#============================================================================================
# 퇴적물 입도자료 추출을 위한 DB쿼리
#--------------------------------------------------------------------------------------------
# library(RPostgreSQL)
# drv <- dbDriver("PostgreSQL")
# con <- dbConnect(drv, dbname = "JURISD_4th",
#                  host = "211.232.105.56", port = 54321,
#                  user = "postgres", password = "postgres")
# gravel <- dbGetQuery(con, "select * from p_data_proj.p_cst_geohzd_dt where obssite_id in (
#                      select obssite_id from meta_com_info.obssite_if
#                      where project_id = 'FPJ0000009') and obs_item_cd = 'COI0025'
#                      order by obssite_id, obs_depth")
# sand <- dbGetQuery(con, "select * from p_data_proj.p_cst_geohzd_dt where obssite_id in (
#                    select obssite_id from meta_com_info.obssite_if
#                    where project_id = 'FPJ0000009') and obs_item_cd = 'COI0026'
#                    order by obssite_id, obs_depth")
# silt <- dbGetQuery(con, "select * from p_data_proj.p_cst_geohzd_dt where obssite_id in (
#                    select obssite_id from meta_com_info.obssite_if
#                    where project_id = 'FPJ0000009') and  obs_item_cd = 'COI0027'
#                    order by obssite_id, obs_depth")
# clay <- dbGetQuery(con, "select * from p_data_proj.p_cst_geohzd_dt where obssite_id in (
#                    select obssite_id from meta_com_info.obssite_if
#                    where project_id = 'FPJ0000009') and  obs_item_cd = 'COI0028'
#                    order by obssite_id, obs_depth")
# std <- dbGetQuery(con, "select * from p_data_proj.p_cst_geohzd_dt where obssite_id in (
#                   select obssite_id from meta_com_info.obssite_if
#                   where project_id = 'FPJ0000009') and  obs_item_cd = 'COI0022'
#                   order by obssite_id, obs_depth")
# mud <- silt$obs_value+clay$obs_value
#  
# sedi_tex <- data.frame(gravel[, c(2, 4, 5)], gravel=gravel$obs_value, sand=sand$obs_value, mud, 
#                     silt=silt$obs_value, clay=clay$obs_value, std=std$obs_value)


#===============================================================
# 퇴적물 입도 삼각다이어그램
#--------------------------------------------------------------
library(dplyr) 
library(rysgran)
library(RColorBrewer)

# 퇴적물 깊이 0-100 사이의 퇴적물 입도
sedi_tex_100 <-  sedi_tex %>% filter(obs_depth >=0 & obs_depth <= 100) 
depth.f <- as.factor(sedi_tex_100$obs_depth)
op <- par(mfrow=c(1,2))
# gravel-sand-mud(silt+clay)
rysgran.ternary(x = sedi_tex_100[,c("gravel","sand","mud")], method = "shepard",
                col = brewer.pal(n=length(levels(depth.f)), name='GnBu')[depth.f], pch = 20, show.grid = TRUE, show.legend = FALSE,
                show.lines = TRUE, show.names = TRUE)
legend("topright", paste(levels(depth.f), " m", sep=""), pch=15, col=brewer.pal(n=length(levels(depth.f)), name='GnBu'))
# sand-silt-clay
rysgran.ternary(x = sedi_tex_100[,c("sand","silt","clay")], method = "shepard",
                col = brewer.pal(n=length(levels(depth.f)), name='GnBu')[depth.f], pch = 20, show.grid = TRUE, show.legend = FALSE,
                show.lines = TRUE, show.names = TRUE, show.labels=FALSE)
legend("topright", paste(levels(depth.f), " m", sep=""), pch=15, col=brewer.pal(n=length(levels(depth.f)), name='GnBu'))
op

#===================================================================================================================================