#--------------------------------------------------------------------------------
#  T-S diagram
#--------------------------------------------------------------------------------

library(marelac)
library(lattice)
library(latticeExtra)
library(dplyr)
library(reshape2)

load("nfrdi_wq.RData", verbose = TRUE)
nfrdi.wq01 <- nfrdi.wq %>% mutate(Seasons=quarter(obsTime), date=format(nfrdi.wq$obsTime, "%Y-%m-%d"))  
nfrdi_sline <- nfrdi.wq01 %>% filter(sline=="207") # 정점간 비교
Seasons.f <-factor(nfrdi_sline$Seasons,levels=c(1,2,3,4), 
                   labels=c("Winter", "Spring", "Summer", "Autumn")) # 계절별

Sal <- seq(0, 40, by=0.5)   # x축 염분범위
Temp <- seq(-5, 40, by=0.5) # y축 수온범위
Val <- outer(X=Sal, Y=Temp, FUN= function(X, Y) sw_dens(S = X, t = Y))  # 염분, 수온을 이용한 밀도자료 생성
Val3d <- melt(Val)
Temp.x <- rep(seq(-5, 40, by=0.5), each= length(Sal))
Sal.x <- rep(seq(0, 40, by=0.5), length(Temp))
Val3d1 <- data.frame(x=Sal.x, y=Temp.x, z=Val3d$value)

p1 <- xyplot(temp~sal|Seasons.f, data=nfrdi_sline, xlim=c(0,40), ylim=c(-5,40),
             xlab="Salinity", ylab="Temperature", main="T-S diagram")  
p1+layer_(panel.contourplot(x = x, y = y, z = z,
                            contour = T, subscripts = T, region=F, labels=T), data = Val3d1)
