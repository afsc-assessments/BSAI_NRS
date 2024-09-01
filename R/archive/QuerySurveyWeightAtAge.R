#Query survey weight-at-age data

#Query the survey age comp data for unidentified and NRS in the EBS trawl survey
library(RODBC)
library(dplyr)
library(tidyr)
source("C:/GitProjects/BSAI_NRS/R/BIN_AGE_DATA.R")
AFSC <- odbcConnect("AFSC","mcgilliardc","oopstriangle13$") #mcgilliardc
outdir = "C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2022\\NRS\\Data\\Survey_Comps"
#Example:
wtage<-SurveyWtAge(DB = AFSC,outdir = outdir,age_bins = 1:20)
SurveyWtAge<-function(DB,outdir,age_bins) {
  
  thequery<-paste0("SELECT racebase.specimen.hauljoin,\n ",
"racebase.specimen.region,\n ",
"racebase.specimen.specimenid,\n ",
"racebase.specimen.species_code,\n ",
"racebase.specimen.length,\n ",
"racebase.specimen.sex,\n ",
"racebase.specimen.weight,\n ",
"racebase.specimen.age,\n ",
"racebase.specimen.age_determination_method,\n ",
"racebase.haul.performance,\n ",
"racebase.haul.haul_type,\n ",
"racebase.haul.stratum,\n ",
"racebase.haul.start_time\n ",
"FROM racebase.specimen\n ",
"INNER JOIN racebase.haul ON racebase.haul.hauljoin = racebase.specimen.hauljoin\n ",
"WHERE ( racebase.specimen.region = 'BS'\n ",
"  AND racebase.specimen.species_code = 10261 )\n ",
"OR ( racebase.specimen.region = 'BS'\n ",
"     AND racebase.specimen.species_code = 10260)" )
  

  
  AL.df<-sqlQuery(DB,thequery,believeNRows = FALSE)
  

  
  #Make year variable
  AL.df$START_TIME2 <- as.Date(AL.df$START_TIME,format = "%Y-%m-%d") 
  AL.df$Year<- lubridate::year(AL.df$START_TIME2)

  #Get rid of observations without lengths AND ages:
  AL.df<-AL.df[complete.cases(AL.df$AGE) & complete.cases(AL.df$WEIGHT),]
  
  #Make a unique ID since specimenid is repeated within hauls and maybe years
  AL.df$ID<-paste0(AL.df$SPECIMENID,"_",AL.df$HAULJOIN,"_",AL.df$Year)
  if (length(unique(AL.df$ID))==nrow(AL.df)) {
    print("unique IDs match number of rows")
  } else { print("unique IDs do not match number of rows")}
  
  #Consider selecting for a certain performance value and age determination method and haul type and stratum (??)
  AL.df<-AL.df %>% filter(SEX %in% c(1,2), PERFORMANCE>=0, HAUL_TYPE==3, STRATUM %in% c(10,20,31,32,41,42,43,50,61,62,82,90), Year!=1988)
  
  
  #bin ages:
  AL.df<-BIN_AGE_DATA(AL.df,age_bins)  
  wtage.df<-AL.df %>% select(c(Year,ID,SEX,WEIGHT,aBIN))
  wtage.df<-wtage.df %>% rename(AGE = aBIN)
  
  
  #calculate mean weight-at-age by sex and year
  meanwt.df<-wtage.df %>% group_by(Year,SEX,AGE) %>% summarise(MeanWt = mean(WEIGHT))
  
  #call male ages 101, 102, etc.
  meanwt.df<-meanwt.df %>% mutate(AGE = replace(AGE, SEX==1,100+AGE))
  meanwt.df<-meanwt.df %>% group_by(Year,AGE) %>% select(Year,AGE,MeanWt)
  
  #make a space ages, etc, for which no weight data exist
  thegrid<-expand.grid(AGE=c(seq(from =1,to=max(age_bins),by=1),seq(from = 101,to=100+max(age_bins),by=1)),Year = seq(from = min(meanwt.df$Year),to = max(meanwt.df$Year),by = 1))
  ExpandWts<-full_join(meanwt.df,thegrid) 
  
  
  #For ages 15-20 average 3 years of mean weights-at-age
  
  
  
  #if there is an NA, fill it in with 1.weight-at-age from previous year or 2.average of age below and age above info from same sex and year, or 3. 3 yr average
# if there is an NA find the MeanWt for the year before and the year after, average them, and fill in this blank.  
ages <-sort(unique(ExpandWts$AGE))
years<-sort(unique(ExpandWts$Year))

#for year 1 and age 1's (male and female)
if (is.na(ExpandWts$MeanWt[ExpandWts$Year==years[1] & ExpandWts$AGE==1])==TRUE) {
  ExpandWts$MeanWt[ExpandWts$Year==years[1] & ExpandWts$AGE==1]<-0
}

if (is.na(ExpandWts$MeanWt[ExpandWts$Year==years[1] & ExpandWts$AGE==101])==TRUE) {
  ExpandWts$MeanWt[ExpandWts$Year==years[1] & ExpandWts$AGE==101]<-0
}


#year 1 and any age:
for (iage in 1:length(ages)) {
if (is.na(ExpandWts$MeanWt[ExpandWts$Year==years[1] & ExpandWts$AGE==ages[iage]])==TRUE) {
  ExpandWts$MeanWt[ExpandWts$Year==years[1] & ExpandWts$AGE==ages[iage]]<-
    mean(c(ExpandWts$MeanWt[ExpandWts$Year==years[2] & ExpandWts$AGE==ages[iage]],ExpandWts$MeanWt[ExpandWts$Year==years[3] & ExpandWts$AGE==ages[iage]],ExpandWts$MeanWt[ExpandWts$Year==years[4] & ExpandWts$AGE==ages[iage]]),na.rm = TRUE)
}
}

#year 2+ and any age: 
for (y in 2:length(years)) {
  if (is.na(ExpandWts$MeanWt[ExpandWts$Year==years[y] & ExpandWts$AGE==1])==TRUE) {
    ExpandWts$MeanWt[ExpandWts$Year==years[y] & ExpandWts$AGE==1]<-0
  }
  for (iage in 1:length(unique(ExpandWts$AGE))) {
    if (is.na(ExpandWts$MeanWt[ExpandWts$Year==years[y] & ExpandWts$AGE==ages[iage]])==TRUE) {
      ExpandWts$MeanWt[ExpandWts$Year==years[y] & ExpandWts$AGE==ages[iage]]<-
        mean(c(ExpandWts$MeanWt[ExpandWts$Year==years[y-1] & ExpandWts$AGE==ages[iage]],ExpandWts$MeanWt[ExpandWts$Year==years[y+1] & ExpandWts$AGE==ages[iage]]),na.rm = TRUE)
        
    }
  }
}

#Rolling average for ages 15-20
moldages<-115:120
foldages<-15:20

# Three year running average for all wts at ages 15-20, except for 1st and last yrs:
for (y in 2:(length(years)-1)) {
  
  for (iage in 1:length(foldages)) {
    ExpandWts$MeanWt[ExpandWts$Year==years[y] & ExpandWts$AGE==foldages[iage]] <-
      mean(c(ExpandWts$MeanWt[ExpandWts$Year==years[y-1] & ExpandWts$AGE==foldages[iage]],ExpandWts$MeanWt[ExpandWts$Year==years[y] & ExpandWts$AGE==foldages[iage]],ExpandWts$MeanWt[ExpandWts$Year==years[y+1] & ExpandWts$AGE==foldages[iage]]),na.rm = TRUE)
    
  }

  for (iage in 1:length(moldages)) {
    ExpandWts$MeanWt[ExpandWts$Year==years[y] & ExpandWts$AGE==moldages[iage]] <-
      mean(c(ExpandWts$MeanWt[ExpandWts$Year==years[y-1] & ExpandWts$AGE==moldages[iage]],ExpandWts$MeanWt[ExpandWts$Year==years[y] & ExpandWts$AGE==moldages[iage]],ExpandWts$MeanWt[ExpandWts$Year==years[y+1] & ExpandWts$AGE==moldages[iage]]),na.rm = TRUE)
    
  }
}

#Divide weights by 1000
ExpandWts<-ExpandWts %>% mutate(MeanWt = MeanWt/1000)

#Flip the data for formatting
WideComp<-ExpandWts %>% group_by(Year,AGE) %>% select(Year,AGE,MeanWt) %>% spread(AGE,MeanWt)
write.table(WideComp,file.path(outdir,"SurveyWtAge.dat"), row.names = FALSE, quote = FALSE, sep = " ")

#In the next assessment perhaps a random effects model could be used to smooth and fill in gaps here.
#Also note: only 2001+ empirical weight-at-age is used, and so 1999, and 2000 only exist here to contribute to rolling average calculations.
#Also, there is some 1988 empirical data, but this is not used, consistent with previous assessments. Maybe it was a special project??  
return(WideComp)
}
