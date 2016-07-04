library(dplyr)
library(ggmap)
library(ggplot2)
library(RPostgreSQL)
library(colorRamps)
####################################################################################
####################################################################################

## Step.1 데이터 불러오기
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname="JURISD_4th",host="211.232.105.56",port=54321
                 ,user="postgres"
                 ,password="postgres")

#### DB에서 정선해양데이터 불러오기 and (항목,날짜(시작/종료))지정 수심설정 미정
data1 <- dbGetQuery(con, "select * from p_data_proj.p_rline_obs_dt 
                    where obs_item_cd = 'COI0074' 
                    AND obs_dtime between 
                    TO_DATE('2006-01-01' ,'YYYY-MM-DD') and 
                    TO_DATE('2016-12-31' ,'YYYY-MM-DD')")
#### DB에서 정선해양 관측정보(정점/위loc_lon) 불러오기
data2 <- dbGetQuery(con, "select * from meta_com_info.obssite_if
                    where project_id = 'FPJ0000024'")
data2 <- select(data2, obssite_id, loc_lat, loc_lon)

data.merged <- merge(data1, data2, by="obssite_id") ## 정점명 기준 데이터 병합


## Step.2 데이터 다듬기
data.merged <- data.merged %>% 
  mutate(season = format(data.merged$obs_dtime, "%m")) %>%
  mutate(month = paste(season, "월", sep="")) %>%
  mutate(year = paste(format(data.merged$obs_dtime, "%Y"),"년",sep="")) 
data.merged$season <- gsub("01|02|03", "winter", data.merged$season) ## 1~3월   =>  겨울
data.merged$season <- gsub("04|05|06", "spring", data.merged$season) ## 4~6월   =>   봄
data.merged$season <- gsub("07|08|09", "summer", data.merged$season) ## 7~9월   =>  여름
data.merged$season <- gsub("10|11|12", "autumn", data.merged$season) ## 10~12월 =>  가을
data.merged$season <- factor(data.merged$season, levels=c("spring","summer","autumn","winter"))

## Step.3 구글맵 설정 및 불러오기
cent <- c(lon=(min(data.merged$loc_lon)+max(data.merged$loc_lon))/2, 
          lat=(min(data.merged$loc_lat)+max(data.merged$loc_lat))/2) ## 맵 중심 좌표설정
map <- ggmap(get_googlemap(center = cent, zoom = 6, maptype = "roadmap",
                           color = "bw", scale = 2
                           ,style = c(feature = "all"
                                      , element = "labels"
                                      , visibility = "off")
), extent = "device")
## Step.4 분포도 Plotting 하기
set.category <- unique(data.merged$obs_item_cd)
set.start.date <- min(data.merged$obs_dtime) 
set.end.date <- max(data.merged$obs_dtime)

data.forAnalysis <- data.merged %>%
  group_by(obssite_id) %>% mutate(values.Mean = mean(obs_value)) %>% ungroup()

### Case.1 Plot single item
D1 <- unique(data.forAnalysis %>% select(obssite_id,loc_lat,loc_lon,values.Mean))
map + 
  geom_point(data = D1, 
             aes(x=loc_lon, y=loc_lat, color = values.Mean, size = D1$values.Mean)) +
  scale_size(range = c(1, 4))+
  ggtitle(paste(min(data.forAnalysis$obs_dtime) ,"~" ,max(data.forAnalysis$obs_dtime) ,set.category," 분포도" ,sep=" ")) +
  theme(plot.title = element_text(face="bold", color="#00619e")) +
  labs(color = set.category)+
  guides(size="none") + 
  scale_colour_gradientn(colours=blue2red(1000)) +
  scale_fill_hue(l=0)


### Case.2 plot single item by season 
data.forAnalysis <- data.merged %>% 
  group_by(obssite_id, season) %>% mutate(values.Mean = mean(obs_value)) %>% ungroup()
D1 <- unique(data.forAnalysis %>% select(obssite_id,loc_lat,loc_lon,values.Mean,season))
map + 
  geom_point(data = D1, 
             aes(x=loc_lon, y=loc_lat, colour = values.Mean ,size = values.Mean)) +
  scale_size(range = c(0.5, 3))+
  scale_colour_gradientn(colours=blue2red(1000)) +
  ggtitle(paste(min(data.forAnalysis$obs_dtime) ,"~" ,max(data.forAnalysis$obs_dtime) ,set.category ,sep=" ")) +
  theme(plot.title = element_text(face="bold", color="#00619e"),
        strip.background = element_rect(colour="#377195", fill="#00619e"),
        strip.text.x = element_text(size=10, color="#ffffe6", face="bold")) +
  guides(size="none") +
  facet_wrap( ~ season, nrow=2)+
  labs(color = set.category)

### Case.3 plot single item by month
data.forAnalysis <- data.merged %>% 
  group_by(obssite_id, month) %>% mutate(values.Mean = mean(obs_value)) %>% ungroup()
D1 <- unique(data.forAnalysis %>% select(obssite_id,loc_lat,loc_lon,values.Mean,month)) %>% filter(values.Mean<50)
map + 
  geom_point(data = D1, 
             aes(x=loc_lon, y=loc_lat, colour = values.Mean ,size = values.Mean)) +
  scale_size(range = c(0.5, 3))+
  scale_colour_gradientn(colours=blue2red(1000)) +
  ggtitle(paste(min(data.forAnalysis$obs_dtime) ,"~" ,max(data.forAnalysis$obs_dtime) ,set.category ,sep=" ")) +
  theme(plot.title = element_text(face="bold", color="#00619e"),
        strip.background = element_rect(colour="#377195", fill="#00619e"),
        strip.text.x = element_text(size=10, color="#ffffe6", face="bold")) +
  guides(size="none") +
  facet_wrap( ~ month, nrow=3)+
  labs(color = set.category)

### Case.4 plot single item by year
data.forAnalysis2 <- data.merged %>% 
 group_by(obssite_id, year) %>% mutate(values.Mean = mean(obs_value))
D1 <- unique(data.forAnalysis %>% select(obssite_id,loc_lat,loc_lon,values.Mean,year)) %>% filter(values.Mean<50)
map + 
  geom_point(data = D1, 
             aes(x=loc_lon, y=loc_lat, colour = values.Mean ,size = values.Mean)) +
  scale_size(range = c(0.1, 4))+
  scale_color_gradientn(colours=blue2red(600)) +
  ggtitle(paste(min(data.forAnalysis$obs_dtime) ,"~" ,max(data.forAnalysis$obs_dtime) ,set.category ,sep=" ")) +
  theme(plot.title = element_text(face="bold", color="#00619e"),
        strip.background = element_rect(colour="#377195", fill="#00619e"),
        strip.text.x = element_text(size=10, color="#ffffe6", face="bold")) +
  guides(size="none") +
  facet_wrap( ~ year, nrow=3) +
  labs(color = set.category)
####################################################################################
####################################################################################


