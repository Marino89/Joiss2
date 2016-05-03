# 막대그래프
# 관측시간을 계절로 그룹핑(겨울:1, 2, 3월; 봄: 4, 5, 6월; 여름: 7, 8, 9월; 가을:10, 11, 12월)

require(ggplot2)
require(dplyr)
require(lubridate)


load("nfrdi_wq.RData", verbose = TRUE)
# Seasons = quarter(obsTime) 열 생성, 즉 계절 열 생성
nfrdi.wq01 <- nfrdi.wq %>% mutate(Seasons=quarter(obsTime), date=format(nfrdi.wq$obsTime, "%Y-%m-%d"))   # format이 아닌 as.Date로 날짜를 바꾸면 time zone 영향을 받음

# 계절에 따른 평균 DO 
ggplot(nfrdi.wq01, aes(x=factor(Seasons), y=DO)) + 
  geom_bar(stat="summary", fun.y="mean", fill="darkcyan") + theme_bw() + 
  scale_x_discrete("Seasons", labels=c("Winter", "Spring", "Summer", "Autumn")) + 
  labs(y="DO (mg/L)") + 
  theme(text=element_text(size=16))