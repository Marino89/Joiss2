# x(관측변수)-Y(수심) 프로파일
# 관측변수: 수온(temp) 컬럼 선택
# 정점선택: 207-05
require(ggplot2)
require(plyr)
require(dplyr)
require(cowplot)

load("nfrdi_wq.RData", verbose = TRUE)
nfrdi.wq01 <- nfrdi.wq %>% mutate(Seasons=quarter(obsTime), date=format(nfrdi.wq$obsTime, "%Y-%m-%d"))   # format이 아닌 as.Date로 날짜를 바꾸면 time zone 영향을 받음
mDO2 <- filter(nfrdi.wq01, sline=="207" & date >= "1990-01-01" & date <= "2014-01-01")
mDO2 <- mDO2 %>% filter(point_name=="207-05")

theme_set(theme_cowplot(font_size=12))
p1 <- ggplot(mDO2, aes(x=temp, y=obs_depth)) +
  geom_path(aes(colour=factor(date))) +
  scale_y_reverse() + theme_classic()+
  labs(x = "Temperature [°C]", y = "Depth [m]", colour = "Site")

ggdraw(switch_axis_position(p1+theme(axis.ticks.length=unit(0.3, "cm"),
                                     axis.text.x=element_text(margin=margin(0.2, unit="cm"))), axis='x'))