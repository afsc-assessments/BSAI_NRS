#Query catch by strata and write the main control file for sam.tpl

library(tidyverse)
library(RODBC)
library(lubridate)
                
AKFIN<- odbcConnect("AKFIN","","") #cmcgilliard
FmpArea <- 'BSAI' 
SpeciesCode<-'RSOL' #and also 120

#YFS StrataMap (strata are times of year here, but can also be NMFS_AREA or other)
#StrataMap<-data.frame(STRATA =c(1,1,1,1,2,2,2,2,3,3,3,3),
#                      MONTH = seq(from = 1,to = 12,by = 1)) #YFS: 3 strata over the months of the year

#NRS StrataMap (only one strata for BSAI NRS right now)
StrataMap<-data.frame(STRATA =rep(1,n = 12),
                      MONTH = seq(from = 1,to = 12,by = 1)) #NRS: 1 strata

MyQuery<-paste0("SELECT council.comprehensive_blend_ca.week_end_date,\n ",
"council.comprehensive_blend_ca.catch_activity_date,\n ",
"council.comprehensive_blend_ca.reporting_area_code,\n ",
"council.comprehensive_blend_ca.agency_species_code,\n ",
"council.comprehensive_blend_ca.species_group_code,\n ",
"council.comprehensive_blend_ca.retained_or_discarded,\n ",
"council.comprehensive_blend_ca.weight_posted,\n ",
"council.comprehensive_blend_ca.agency_gear_code,\n ",
"council.comprehensive_blend_ca.year,\n ",
"council.comprehensive_blend_ca.fmp_area,\n ",
"council.comprehensive_blend_ca.fmp_subarea,\n ",
"council.comprehensive_blend_ca.fmp_gear,\n ",
"council.comprehensive_blend_ca.species_name\n ",
"FROM council.comprehensive_blend_ca\n ",
"WHERE council.comprehensive_blend_ca.species_group_code =",SpeciesCode,"\n ",
"AND council.comprehensive_blend_ca.fmp_area = ",FmpArea)

catchbio<-sqlQuery(AKFIN,MyQuery)