#============================================================================================
#  추출을 위한 DB쿼리
#--------------------------------------------------------------------------------------------
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "JURISD_4th",
                 host = "211.232.105.56", port = 54321,
                 user = "postgres", password = "postgres")
# 풍향(COI0094), 풍속(COI0095)
wind_joiss <- dbGetQuery(con, "select * from p_data_proj.p_rsch_statn_dt 
                         where obs_item_cd in ('COI0094', 'COI0095') and 
                         obs_dtime >= '2012-01-01' and obs_dtime <= '2012-12-31' 
                         order by obs_item_cd, obs_dtime")

#------------------------------------------------------------------------------------
#바람장미는 어떤 관측지점에 대하여 해당 기간동안 방위별 풍향출현빈도와 
#풍향별 풍속계급 빈도를 그래프로 나타낸 것
#기상연보에서는 일년동안의 시간별 바람자료를 사용하여 표현함
#막대는 바람이 불어오는 방향을 나타내며, 길이는 해당등급의 바람이 불어온 
#빈도를 백분율로 나타냄
#고요(무풍/calm)의 출현빈도는 별도 그래프의 중심이나 옆 자리에 표시
#-------------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(openair)
library(RGraphics)

wind_data <- wind_joiss %>% select(obssite_id, obs_dtime, obs_item_cd, obs_value) %>% spread(obs_item_cd, obs_value)
# 풍향 및 풍속 자료 중 음수자료 제거
colnames(wind_data)[c(3, 4)] <- c("WDirect", "WSpeed")
wind_data <- wind_data[!(wind_data$WDirect < 0 | wind_data$WSpeed < 0),]
wind_data1 <- wind_data[complete.cases(wind_data),]

# 계절 & 정점별 패턴
print(
  with(wind_data1,
       windRose(data.frame(ws=WSpeed, wd=WDirect, 
                           date=obs_dtime, station=factor(obssite_id)),
                paddle=FALSE, type=c("season", "station"), width=2))
)

# 월별 & 정점별 패턴
print(
  with(wind_data1,
       windRose(data.frame(ws=WSpeed, wd=WDirect, 
                           date=obs_dtime, station=factor(obssite_id)),
                paddle=T, type=c("month", "station"), width=1))
)


# # 정점별 패턴 (paddle =T, with= 1)
# print(
#   with(wind_data1,
#        windRose(data.frame(ws=WSpeed, wd=WDirect, 
#                            date=obs_dtime, station=factor(obssite_id)),
#                 paddle=T, type=c("station"), width=1))
# )

