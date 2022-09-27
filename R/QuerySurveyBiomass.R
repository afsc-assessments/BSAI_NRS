# read in the survey biomass data for NRS
#This is unidentified rock sole + Northern rock sole
#Only standard area EBS trawl survey data are used

library(RODBC) 
library(dplyr)

#Example:
mybiom<-QuerySurveyBiomass(user = "myuser",pwd = "mypwd",outdir = "C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2022\\NRS\\Data\\Survey_Biomass\\")

QuerySurveyBiomass<-function(user,pwd,outdir) {
AFSC <- odbcConnect("AFSC",user,pwd) #mcgilliardc

thequery<-paste0("SELECT haehnr.biomass_ebs_standard.species_code,\n ",
"haehnr.biomass_ebs_standard.species_name,\n ",
"haehnr.biomass_ebs_standard.year,\n ",
"haehnr.biomass_ebs_standard.stratum,\n ",
"haehnr.biomass_ebs_standard.biomass,\n ",
"haehnr.biomass_ebs_standard.varbio,\n ",
"haehnr.biomass_ebs_standard.upperb,\n ",
"haehnr.biomass_ebs_standard.lowerb\n ",
"FROM haehnr.biomass_ebs_standard\n ",
"WHERE ( haehnr.biomass_ebs_standard.species_code = 10261\n ",
"  AND haehnr.biomass_ebs_standard.stratum = 999 )\n ",
"OR ( haehnr.biomass_ebs_standard.species_code = 10260\n ",
"     AND haehnr.biomass_ebs_standard.stratum = 999 )")


biom <- sqlQuery(AFSC,thequery,believeNRows = FALSE)
combo <- biom %>%
  group_by( YEAR) %>%
  summarise(CBio = sum( BIOMASS)/1000, CVar = sqrt(sum( VARBIO))/1000)

close(AFSC)

write.csv(combo,file = file.path(outdir,"SurveyBiomass.csv"))
return(combo)
}



