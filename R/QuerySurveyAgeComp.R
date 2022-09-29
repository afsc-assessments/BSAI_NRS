#Query the survey age comp data for unidentified and NRS in the EBS trawl survey
library(RODBC)
library(dplyr)
library(tidyr)
outdir = "C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2022\\NRS\\Data\\Survey_Comps"
#Example:
myages<-SurveyAgeComp(user = "mcgilliardc",pwd = "oopscircle12$",outdir = outdir)
SurveyAgeComp<-function(user,pwd,outdir) {

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

ages<-ages%>% filter(AGE!=-9,AGE!=-99)
#Bin plus group:
ages <- ages %>% mutate(AGE = replace(AGE, AGE > 20, 20))

#sum within year, sex, and age over unidentified and NRS and over plus group observations
agecomp<-ages %>% group_by(YEAR,SEX,AGE) %>% summarise(CAges = sum( AGEPOP))

#sum to get a total over ages to use for figuring out proportions at age
agetot<-ages %>% group_by(YEAR,SEX) %>% summarise(TAges = sum( AGEPOP))

#Turn age numbers into proportions
Pagecomp<-full_join(agecomp,agetot) %>% mutate(Prop = CAges/TAges)
Pagecomp<-Pagecomp %>% select(YEAR,SEX,AGE,Prop)
#Look up whether it's females then males or vice versa for input to dat file


#organize by females on left and males on right, bin at the plus group age
thegrid<-expand.grid(AGE=seq(from =1,to=20,by=1),SEX = c(1,2),YEAR = unique(Pagecomp$YEAR))
ExpandComp<-full_join(Pagecomp,thegrid) 
ExpandComp<-ExpandComp %>% replace_na(list(Prop=0))
ExpandComp$Prop<-format(round(ExpandComp$Prop,7),nsmall=7)

WideComp<-ExpandComp %>% group_by(SEX,YEAR,AGE) %>% select(YEAR,AGE,Prop) %>% spread(AGE,Prop)
write.csv(WideComp,file.path(outdir,"SurveyAgeComp.csv"))
return(WideComp)
}