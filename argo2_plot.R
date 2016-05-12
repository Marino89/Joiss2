library(dplyr)
library(tidyr)

load("argo_joiss.RData", verbose=T) # 관할해역 argo 자료일부
argo_data <- argo_joiss  %>% filter(obssite_id =="FLO0000932" & obs_depth > 0) %>% 
  select(obs_dtime, obs_depth, obs_value, lat_text, lon_text, obs_item_cd, obssite_id)
argo_data <- argo_data[!duplicated(argo_data[,c(1, 2, 4, 5, 6)]), ]  # 중복자료 삭제
argo_wide <- argo_data %>% spread(obs_item_cd, obs_value)

# 자료를 관측일자별로 split
df <- split(argo_wide,argo_wide$obs_dtime)
# df에서 관측수심 추출
df1 <- lapply(df, "[", "obs_depth")
df2 <- lapply(df1, t)
# df에서 염분자료 추출
df.sal1 <- lapply(df, '[',"COI0067")
df.sal2 <- lapply(df.sal1, t)
# df에서 수온자료 추출
df.temp1 <- lapply(df, '[',"COI0074")
df.temp2 <- lapply(df.temp1, t)
# 관측일별로 수심에 따른 관측자료수가 다르기 때문에 행렬자료를 만들었을 때 자료수가 적은 컬럼에는 많은 결측값으로 채워짐으로 인해
# 내삽시 오류발생함
# 관측일별 평균 관측건수를 구하여 해당 건수만큼 수온, 염분자료를 추출하여 내삽오류 방지
n <- round(min(mean(sapply(df.sal2, length)), mean(sapply(df.temp2, length))),0)
depth <- sapply(df2, '[', seq(n))
depth <- unname(depth)
sal <- sapply(df.sal2, '[', seq(n))
sal <- unname(sal)
temper <- sapply(df.temp2, '[', seq(n))
temper <- unname(temper)
dtime <- unique(argo_wide$obs_dtime)
lonlat <- unique(data.frame(lon=as.numeric(argo_wide$lon_text), lat=as.numeric(argo_wide$lat_text)))
obssite_id <- unique(argo_wide$obssite_id)

# 수온, 염분 데이타프레임에서 전 수심에 걸쳐 NA인 컬럼 제거
allmisscols1 <- apply(temper, 2, function(x) all(is.na(x)))
allmisscols2 <- apply(sal, 2, function(x) all(is.na(x)))

allmisscols <- as.logical(allmisscols1 + allmisscols2)
colswithallmiss <- which(allmisscols > 0)

if(length(colswithallmiss) == 0){
  dtime; lonlat; sal; temper; depth;
} else{
  dtime <- dtime[-colswithallmiss]; 
  lonlat <- lonlat[-colswithallmiss, ]; 
  sal <- sal[, -colswithallmiss]; 
  temper <- temper[, -colswithallmiss]; 
  depth <- depth[, -colswithallmiss]
}

library(oce)
# argo class 개체생성하기
argo.joiss <- as.argo(time = dtime, longitude = lonlat$lon, latitude = lonlat$lat, 
                      salinity = sal, temperature = temper, depth, id=rep(obssite_id, length(dtime)))

# grid로 자료 내삽
g <- argoGrid(argo.joiss)

# 이미지 생성
op <- par(mfrow=c(3, 1))
t <- g[["time"]]
z <- -g[["pressure"]][,1]
## Set zlim because of spurious temperatures.
imagep(t, z, t(g[['temperature']]), zlim=c(0, 30), ylab="Depth(m)", zlab="Temperature")
imagep(t, z, t(g[['salinity']]), zlim=c(34, 35), ylab="Depth(m)", zlab="Salinity")
plot(argo.joiss, which="trajectory")
op
