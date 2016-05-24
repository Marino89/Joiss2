require(RPostgreSQL)

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname="JURISD_4th",host="211.232.105.56",port=54321,user="postgres",password="postgres")



res <- dbSendQuery(con, statement = paste(
  
  " WITH OBSSITE AS (  ",
  
  "    SELECT a.project_id,b.obs_item_cd, a.obssite_name, b.obs_dtime, b.obs_depth::numeric ,b.obs_value::numeric ",
  
  "      FROM meta_com_info.obssite_if a,  ",
  
  "            p_data_proj.p_rline_obs_dt  B    ",
  
  "     WHERE a.obssite_id = b.obssite_id    ",
  
  "       AND a.project_id = 'FPJ0000024'   ",
  
  "      AND b.OBS_DTIME between TO_DATE('2010-01-01' ,'YYYY-MM-DD') and TO_DATE('2014-12-31' ,'YYYY-MM-DD') ",
  
  "      )     ",
  
  "  SELECT  ",
  
  "       obs_dtime,obssite_name,obs_depth ",
  
  "       ,min(case when obs_item_cd = 'COI0067' then obs_value end  ) v1  ",
  
  "       ,min(case when obs_item_cd = 'COI0074' then obs_value end  ) v2  ",
  
  "       ,min(case when obs_item_cd = 'COI0095' then obs_value end  ) v3  ",
  
  "       ,min(case when obs_item_cd = 'COI0099' then obs_value end  ) v4  ",
  
  "       ,min(case when obs_item_cd = 'COI0116' then obs_value end  ) v5  ",
  
  "       ,min(case when obs_item_cd = 'COI0117' then obs_value end  ) v6  ",
  
  "       ,min(case when obs_item_cd = 'COI0128' then obs_value end  ) v7  ",
  
  "       ,min(case when obs_item_cd = 'COI0129' then obs_value end  ) v8  ",
  
  "       ,min(case when obs_item_cd = 'COI0134' then obs_value end  ) v9  ",
  
  "       ,min(case when obs_item_cd = 'COI0135' then obs_value end  ) v10  ",
  
  "       ,min(case when obs_item_cd = 'COI0138' then obs_value end  ) v11 ",
  
  "       ,min(case when obs_item_cd = 'COI0139' then obs_value end  ) v12 ",
  
  "       ,min(case when obs_item_cd = 'COI0140' then obs_value end  ) v13 ",
  
  "   FROM  obssite       ",
  
  "   group by obs_dtime,obssite_name,obs_depth  ",
  
  "   order by obs_dtime asc,obs_depth asc "))

# we now fetch the first 100 records from the resultSet into a data.frame

data1 <- fetch(res, n = -1)
dbDisconnect(con) 
on.exit(dbUnloadDriver(drv), add = TRUE)


require(ggplot2)
require(dplyr)
require(lubridate)

#=============================================
# 월별 변수값의 변동에 대한 꺽은선 그래프
#
# x축: 월, y축: 변수
#---------------------------------------------


# 선택한 임의의 변수(조사항목) v1에 대한 월별 평균 및 표준오차값 산출
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
data2 <- data1 %>% mutate(Seasons=quarter(obs_dtime), Month=month(obs_dtime)) 
data3 <- tapply(data2$v1, factor(data2$Month), stderr)
data4 <- tapply(data2$v1, factor(data2$Month), mean, na.rm=TRUE)
data5 <- data.frame(Means= data4, SE= data3)

require(ggplot2)
require(grid)
# line and point plot
f1 = ggplot(data = data5, aes(x = as.numeric(rownames(data5)), y = Means) ) 
f2 <- f1 + geom_errorbar(aes(ymin = Means - SE, ymax = Means + SE), width=0.3) +
  geom_line() + geom_point(size=5)
 
#f1 = ggplot(data = data5, aes(x = rownames(data5), y = Means, group = group) )  # group이 필요할 시
#f2 <- f1 + geom_errorbar(aes(ymin = Means - SE, ymax = Means + SE), width=0.3) +
#  geom_line() + geom_point(aes(shape=group, fill=group), size=5)


f3 <- f2 +  scale_x_continuous("Month", breaks=1:12) +
  #scale_y_continuous("Y (units)", limits = c(0, 40), breaks=seq(0, 40, by = 5)) +
  scale_y_continuous("Y (units)") +
  scale_shape_manual(values=c(24,21)) +
  scale_fill_manual(values=c("white","black")) +
  #stat_abline(intercept=0, slope=0, linetype="dotted") +
  #annotate("text", x=11, y=10, label="X") +
  theme_bw()

optns <- theme (
  plot.title = element_text(face="bold", size=14),
  axis.title.x = element_text(face="bold", size=12),
  axis.title.y = element_text(face="bold", size=12, angle=90),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = c(0.2,0.8),
  legend.title = element_blank(),
  legend.text = element_text(size=12),
  legend.key.size = unit(1.5, "lines"),
  legend.key = element_blank()
)
f3 +  ggtitle ( "Title") + optns
