#Query catch by strata and write the main control file for sam.tpl

library(tidyverse)
library(RODBC)
                
AKFIN<- odbcConnect("AFSC","","") #mcgilliardc
FmpArea <- 'BSAI' 
SpeciesCode<-'RSOL' #and also 120


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
"WHERE council.comprehensive_blend_ca.species_group_code = 'RSOL'\n ",
"AND council.comprehensive_blend_ca.fmp_area = 'BSAI'")

catchbio<-sqlQuery(AKFIN,MyQuery)