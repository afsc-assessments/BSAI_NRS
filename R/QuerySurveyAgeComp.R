#Query the survey age comp data for unidentified and NRS in the EBS trawl survey
library(RODBC)
library(dplyr)
library(tidyr)
AFSC <- odbcConnect("AFSC","mcgilliardc","oopscircle12$") #mcgilliardc
outdir = "C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2022\\NRS\\Data\\Survey_Comps"
#Example:
myages<-SurveyAgeComp(DB = AFSC,user = "mcgilliardc",pwd = "oopscircle12$",outdir = outdir)
SurveyAgeComp<-function(DB,user,pwd,outdir) {

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

ages<-sqlQuery(DB,thequery,believeNRows = FALSE)

ages<-ages%>% filter(AGE!=-9,AGE!=-99,SEX!=3,SEX!=9)
#Bin plus group:
ages <- ages %>% mutate(AGE = replace(AGE, AGE > 20, 20))

#sum within year, sex, and age over unidentified and NRS and over plus group observations
agecomp<-ages %>% group_by(YEAR,SEX,AGE) %>% summarise(CAges = sum( AGEPOP))

#sum to get a total over ages to use for figuring out proportions at age
agetot<-ages %>% group_by(YEAR) %>% summarise(TAges = sum( AGEPOP))

#Turn age numbers into proportions
Pagecomp<-full_join(agecomp,agetot) %>% mutate(Prop = CAges/TAges)
Pagecomp<-Pagecomp %>% select(YEAR,SEX,AGE,Prop)


#organize by females on left and males on right, bin at the plus group age
#add 100 to male ages to denote they are males
Pagecomp<-Pagecomp %>% mutate(AGE = replace(AGE, SEX==2,100+AGE))
Pagecomp<-Pagecomp %>% group_by(YEAR,AGE) %>% select(YEAR,AGE,Prop)

#thegrid<-expand.grid(AGE=seq(from =1,to=20,by=1),SEX = c(1,2),YEAR = unique(Pagecomp$YEAR))
thegrid<-expand.grid(AGE=c(seq(from =1,to=20,by=1),seq(from = 101,to=120,by=1)),YEAR = unique(Pagecomp$YEAR))
ExpandComp<-full_join(Pagecomp,thegrid) 
ExpandComp<-ExpandComp %>% replace_na(list(Prop=0))
ExpandComp$Prop<-format(round(ExpandComp$Prop,7),nsmall=7)

#WideComp<-ExpandComp %>% group_by(SEX,YEAR,AGE) %>% select(YEAR,AGE,Prop) %>% spread(AGE,Prop)
WideComp<-ExpandComp %>% group_by(YEAR,AGE) %>% select(YEAR,AGE,Prop) %>% spread(AGE,Prop)
write.table(WideComp,file.path(outdir,"SurveyAgeComp.dat"), row.names = FALSE, quote = FALSE, sep = " ")
return(WideComp)
}