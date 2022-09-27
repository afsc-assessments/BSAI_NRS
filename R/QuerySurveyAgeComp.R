#Query the survey age comp data for unidentified and NRS in the EBS trawl survey



SurveyAgeComp<-function(user,pwd) {

AFSC <- odbcConnect("AFSC","mcgilliardc","oopscircle12$") #mcgilliardc

thequery<-paste0("SELECT haehnr.agecomp_ebs_standard_stratum.species_code,\n ",
"haehnr.agecomp_ebs_standard_stratum.year,\n ",
"haehnr.agecomp_ebs_standard_stratum.stratum,\n ",
"haehnr.agecomp_ebs_standard_stratum.sex,\n ",
"haehnr.agecomp_ebs_standard_stratum.age,\n ",
"haehnr.agecomp_ebs_standard_stratum.agepop,\n ",
"haehnr.agecomp_ebs_standard_stratum.meanlen,\n ",
"haehnr.agecomp_ebs_standard_stratum.sdev\n ",
"FROM haehnr.agecomp_ebs_standard_stratum\n ",
"WHERE ( haehnr.agecomp_ebs_standard_stratum.species_code = 10261\n ",
"  AND haehnr.agecomp_ebs_standard_stratum.stratum = 999999 )\n ",
"OR ( haehnr.agecomp_ebs_standard_stratum.species_code = 10260\n ",
"     AND haehnr.agecomp_ebs_standard_stratum.stratum = 999999 )")

ages<-sqlQuery(AFSC,thequery,believeNRows = FALSE)

ages<-ages%>% filter(AGE!=-9)
agecomp<-ages %>% group_by(YEAR,SEX,AGE) %>% summarise(CAges = sum( AGEPOP))
agetot<-ages %>% group_by(YEAR,SEX) %>% summarise(TAges = sum( AGEPOP))
Pagecomp<-full_join(agecomp,agetot) %>% mutate(Prop = CAges/TAges)

#Look up whether it's females then males or vice versa for input to dat file
return(Pagecomp)
}