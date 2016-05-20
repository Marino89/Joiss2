#============================================================================================
#  추출을 위한 DB쿼리
#--------------------------------------------------------------------------------------------
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "JURISD_4th",
                 host = "211.232.105.56", port = 54321,
                 user = "postgres", password = "postgres")
# COI0075;유속(U), COI0076;유속(V), COI0077;"유속(W)"
adcp_joiss <- dbGetQuery(con, "select * from p_data_proj.p_rsch_statn_dt 
                         where obssite_id = 'FPO0000012' and obs_dtime >= '2007-09-04 00:00:00' and 
                         obs_dtime <= '2007-09-05 23:59:59'order by obs_dtime, obs_depth")



library(dplyr)
library(tidyr)
library(lubridate)

custom_depth <- c(0, 40) # 관측수심 사용자 선택

adcp_data <- adcp_joiss %>% filter(obs_depth >= custom_depth[1] & obs_depth <= custom_depth[2]) %>% 
  select(obssite_id, obs_dtime, obs_item_cd, obs_depth, obs_value) %>% 
  spread(obs_item_cd, obs_value)

# 풍향 및 풍속 자료 중 음수자료 제거
colnames(adcp_data)[4:6] <- c("Ucomp", "Vcomp", "Wcomp")
adcp_data$xx=seq(1,nrow(adcp_data))
adcp_data$yy=0
adcp_data$obs_depth <- as.factor(adcp_data$obs_depth) # 수심을 factor로
xlp <- seq(1,nrow(adcp_data), by=30)
dtime <- adcp_data$obs_dtime[xlp]
curVector=ggplot(data=adcp_data, aes(x=xx,y=yy))+
  geom_segment(aes(xend=xx+Ucomp,yend=yy+Vcomp))+
  theme_bw()+
  xlab("")+ylab("")+geom_line(y=0, lwd=0.1)+
  scale_x_continuous(breaks=xlp, labels=dtime)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(obs_depth~.)

curVector

png("curVector.png",width=4000,height=800,res=200)
print(curVector)
dev.off()
