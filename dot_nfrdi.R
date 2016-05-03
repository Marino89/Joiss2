# Dot plot
# 관측변수: DO(용존산소) 선택
# 관측시간을 연도별

require(ggplot2)
require(dplyr)
require(plyr)
require(lubridate)

# 관측정점 중 200번 라인의 정점들에 대한 평균 DO 계산
load("nfrdi_wq.RData", verbose = TRUE)
nfrdi.wq01 <- nfrdi.wq %>% mutate(Seasons=quarter(obsTime), date=format(nfrdi.wq$obsTime, "%Y-%m-%d"))   # format이 아닌 as.Date로 날짜를 바꾸면 time zone 영향을 받음
mDO1 <- filter(nfrdi.wq01, line_gr=="200s") %>% ddply("point_name", summarise, meanDO= mean(DO, na.rm=T))

ggplot(mDO1, aes(x=reorder(point_name, meanDO), y=meanDO)) + 
  geom_point(size=3) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.x=element_line(colour="grey60", linetype="dashed"))