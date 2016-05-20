require(RPostgreSQL)

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname="JURISD_4th",host="211.232.105.56",port=54321,user="postgres",password="postgres")



res <- dbSendQuery(con, statement = paste(
  
  " WITH OBSSITE AS (  ",
  
  "    SELECT a.project_id,b.obs_item_cd, a.obssite_name, a.loc_lat::numeric, a.loc_lon::numeric, b.obs_dtime, b.obs_depth::numeric ,b.obs_value::numeric ",
  
  "      FROM meta_com_info.obssite_if a,  ",
  
  "            p_data_proj.p_rline_obs_dt  b    ",
  
  "     WHERE a.obssite_id = b.obssite_id    ",
  
  "       AND a.project_id = 'FPJ0000024'   ",
  
  "      AND b.OBS_DTIME between TO_DATE('1970-01-01' ,'YYYY-MM-DD') and TO_DATE('2014-12-31' ,'YYYY-MM-DD') ",
  
  "      )     ",
  
  "  SELECT  ",
  
  "       obs_dtime,obssite_name, loc_lat, loc_lon, obs_depth ",
  
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
  
  "   group by obs_dtime,obssite_name,loc_lat, loc_lon,obs_depth  ",
  
  "   order by obs_dtime asc,obs_depth asc "))

# we now fetch the first 100 records from the resultSet into a data.frame

data1 <- fetch(res, n = -1)


require(openair)
require(dplyr)
require(lubridate)


site_name <- "310-03"  # 정점선택
sdepth <- 0        # 수심선택
edepth <- 10
param <- "v1"      # 항목선택

nfrdi.wq01 <- data1 %>% filter(obssite_name==site_name & (obs_depth >= sdepth & obs_depth <= edepth) & v1 < 99) %>% 
  select(date=obs_dtime, starts_with("v"))

nfrdi.wq02 <- aggregate(nfrdi.wq01, by=list(factor(nfrdi.wq01$date)), FUN="mean", na.rm=TRUE)

TheilSen(nfrdi.wq02, param)


dbDisconnect(con) 

on.exit(dbUnloadDriver(drv), add = TRUE)
