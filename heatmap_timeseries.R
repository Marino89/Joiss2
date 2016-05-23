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
  
  "      AND b.OBS_DTIME between TO_DATE('2000-01-01' ,'YYYY-MM-DD') and TO_DATE('2014-12-31' ,'YYYY-MM-DD') ",
  
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

#=======================================
# Heatmap_Time series
#---------------------------------------

require(lattice)
require(dplyr)
require(lubridate)
library(RColorBrewer)

nfrdi.wq01 <- data1 %>% mutate(Year=year(obs_dtime), Month=month(obs_dtime), Day=day(obs_dtime))
nfrdi_20701 <- nfrdi.wq01 %>% filter(obssite_name=="207-01" & obs_depth == 0)

# 변수항목 선택 : 예) v1

data_xtab <- xtabs(v1 ~ Year + Month, nfrdi_20701)
max_legend <- round(max(nfrdi_20701[,"v1"]), 0)
min_legend <- round(min(nfrdi_20701[,"v1"]), 0)
levelplot(data_xtab, 
          aspect='iso', xlab='Year', ylab='Month', main='v1', 
          col.regions=colorRampPalette(brewer.pal(9, 'YlGnBu')),
          at=seq(min_legend, max_legend, length=100), # specify breaks for the colour ramp
          scales=list(alternating=FALSE, tck=1:0, rot=90),border='white')
