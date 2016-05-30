# box plot
# 관측변수: DO(용존산소) 선택
# 관측시간을 연도별 또는 계절(겨울:1, 2, 3월; 봄: 4, 5, 6월; 여름: 7, 8, 9월; 가을:10, 11, 12월)로 그룹핑

require(ggplot2)
require(dplyr)
require(lubridate)


load("nfrdi_wq.RData", verbose = TRUE)
# Seasons = quarter(obsTime) 열 생성, 즉 계절 열 생성
nfrdi.wq01 <- nfrdi.wq %>% mutate(Seasons=quarter(obsTime), date=format(nfrdi.wq$obsTime, "%Y-%m-%d"))   # format이 아닌 as.Date로 날짜를 바꾸면 time zone 영향을 받음

# boxplot
# Seasons 컬럼 선택
#ggplot(nfrdi.wq01, aes(x=factor(Seasons), DO)) + geom_boxplot() +
#  theme_bw() + scale_x_discrete("Seasons", labels=c("Winter", "Spring", "Summer", "Autumn"))

# year 컬럼 선택
ggplot(nfrdi.wq01, aes(x=factor(year), DO)) + 
  geom_boxplot(fill='#E69F00', color="black") +
  theme_bw() + scale_x_discrete("Year") + 
  theme(axis.text.x=element_text(angle=90, hjust=1))