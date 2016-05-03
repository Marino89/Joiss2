require(lattice)
require(latticeExtra)
require(zoo)
require(dplyr)

# 자료불러오기
load("nfrdi_wq.RData", verbose = TRUE)
nfrdi.wq01 <- nfrdi.wq %>% mutate(Seasons=quarter(obsTime), date=format(nfrdi.wq$obsTime, "%Y-%m-%d"))  

#===============================================================================
#
# 정점선택, 다중변수 시계열 자료
#  (패키지 zoo)
#-------------------------------------------------------------------------------
nfrdi_20701 <- filter(nfrdi.wq01, point_name=="207-01" & obs_depth == 0) # 정점 207-01 & 표층
nfrdi_20701.zoo <- zoo(nfrdi_20701[, 4:11], nfrdi_20701$obsTime)
xyplot(nfrdi_20701.zoo, layout=c(1, ncol(nfrdi_20701.zoo)))
