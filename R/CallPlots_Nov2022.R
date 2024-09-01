#Notes:
# Units of measure inputs fm.tpl
# Catches: tons/1000 = kg
# Weight-at-age: (0.45 for age 20s): g/1000 = kg
# 
# Units of measure outputs fm.tpl
# Recruitment: log(actual numbers in the form of deviations) (exp(mean_log_rec+devs) = actual numbers), on the order of 700ish.
#   Numbers-at-age: actual numbers
#   Catches: kt (t/1000)
#   Weight-at-age: g/1000
  


#Make plots for fm.tpl for NRS in 2022
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(ggridges)
library(patchwork)

#2022
rundir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2022/NRS/Runs/"


codedir<-"C:/GitProjects/BSAI_NRS/R"
.OVERLAY <-TRUE
.THEME<- theme_few()
maxage = 20
endyr = 2022

#source(file.path(codedir,"prelims.R")) #libraries and ggthemes - may need some refining
source(file.path(codedir,"read-admb.R"))
source(file.path(codedir,"plot-catch.R"))
source(file.path(codedir,"plot-bts.R"))
source(file.path(codedir,"plot_rec.R"))
source(file.path(codedir,"plot_ssb.R"))
source(file.path(codedir,"plot-age-comps-Carey.R"))
source(file.path(codedir,"plot-fsh-sel.R"))
source(file.path(codedir,"plot-srv-sel.R"))
source(file.path(codedir,"plot-srr.R"))
source(file.path(codedir,"do-francis-weighting.R"))
source("C:/GitProjects/BSAI_NRS/R/plot-Fs.R", echo=TRUE)
source("C:/GitProjects/BSAI_NRS/R/plot_sex_ratio.R", echo=TRUE)

setwd(rundir)

#Runs to compare
M2022<-read_admb("c1mod4_alldata_2022/fm")
M2022_sept2024<-read_admb("c1mod4_alldata_2022_sept2024/fm")


# M2020<-read_admb("c1mod4_2020_final_run/fm")
# M2022<-read_admb("c1mod4_alldata_2022/fm")
# MF2022<-read_admb("c1mod4_francis_2022/fm")
# #M2020<-read_admb("c1mod4_2020_final_run/fm")
# #MM2022<-read_admb("c1mod4_francis_estM/fm")
# MBigM2022<-read_admb("c1mod4_francis_estMQbigpriors/fm")

mylist<-list()
mylist$M18.3<-M2022
#mylist$M2020<-M2020
#mylist$MM2022<-MM2022
mylist$M22.1<-MF2022
mylist$M22.2<-MBigM2022

#Specify the base case
BaseDir<-"c1mod4_alldata_2022"
BaseList<-list()
BaseList$M18.3<-M2022
BaseRun<-M2022

#Time-series runs
tslist<-list()
tslist$Current<-BaseRun
tslist$Previous<-M2020

#Make plot directories if they don't exist:
if (!dir.exists(file.path(BaseDir,"plots"))) {
  dir.create(file.path(BaseDir,"plots"))
}

if (!dir.exists("plots")) {
  dir.create("plots")
}

#Make table directories if they don't exist:
if (!dir.exists(file.path(BaseDir,"tables"))) {
  dir.create(file.path(BaseDir,"tables"))
}

if (!dir.exists("tables")) {
  dir.create("tables")
}

#Make and save plots (comparison plots and base case plots)
a<-plot_catch(modlst=mylist,themod=1,obspred = FALSE)
ggsave(filename = "plots/catch.png",plot = a,device = "png")
ggsave(filename = file.path(BaseDir,"plots","catch.png"),plot = a,device = "png")

b<-plot_bts(M = mylist)
bb<-plot_bts(M = BaseList)
ggsave(filename = file.path("plots","bts_compare.png"),plot = b,device = "png")
ggsave(filename = file.path(BaseDir,"plots","bts.png"),plot = bb,device = "png")


c<-plot_rec(M=mylist)
cc<-plot_rec(M=BaseList)
ggsave(filename = file.path("plots","recruits_compare.png"),plot = c,device = "png")
ggsave(filename = file.path(BaseDir,"plots","recruits.png"),plot = cc,device = "png")

d<-plot_ssb(M = mylist)
dd<-plot_ssb(M = BaseList)
ggsave(filename = "plots/ssb_compare.png",plot = d,device = "png")
ggsave(filename = file.path(BaseDir,"plots","ssb.png"),plot = dd,device = "png")


e<-plot_srr(M=mylist, ylab = "Recruits (age 1, billions)", xlab = "Female spawning biomass (kt)", 
            ylim = NULL, xlim=NULL, alpha = 0.05,ebar="FALSE",leglabs=NULL,styr=1978,endyr=2016)
ee<-plot_srr(M=BaseList, ylab = "Recruits (age 1, billions)", xlab = "Female spawning biomass (kt)", 
             ylim = NULL, xlim=NULL, alpha = 0.05,ebar="FALSE",leglabs=NULL,styr=1978,endyr=2016)

ggsave(filename = file.path("plots","srr_compare.png"),plot = e,device = "png")
ggsave(filename = file.path(BaseDir,"plots","srr.png"),plot = ee,device = "png")

ff<-plot_srv_sel(theM=BaseList,themod=1, title="Survey selectivity",bysex=TRUE,maxage = maxage)
ggsave(filename = file.path(BaseDir,"plots","srv_sel.png"),plot = ff,device = "png")

gg<-plot_age_comps(M = BaseList, xlab = "Age (yrs)", ylab = "Proportion", 
                           nages=maxage,type="fishery",sex="split",title="Fishery age compositions")
ggsave(filename = file.path(BaseDir,"plots","age_comps_fsh.png"),plot = gg,device = "png")


hh<-plot_age_comps(M = BaseList, xlab = "Age (yrs)", ylab = "Proportion", 
               nages=maxage,type="survey",sex="split",title="Survey age compositions")
ggsave(filename = file.path(BaseDir,"plots","age_comps_srv.png"),plot = hh,device = "png")


ii<-plot_fsh_sel(mod=BaseRun, title=NULL,alpha=0.3,styr=1991,endyr=endyr,bysex=TRUE,sexoverlay=TRUE)
ggsave(filename = file.path(BaseDir,"plots","fsh_sel.png"),plot = ii,device = "png")


ff<-plotFs(modlst=BaseList,thismod = 1,thismodname = "M18.3")
ggsave(file.path(BaseDir,"plots","FatAge_MeanF.png"),plot=ff,width=8,height=8.0,units="in")

jj<-plot_sex_ratio(M = BaseList) #I don't understand why this doesn't work

# Calculate Francis Weights - spits out new vectors for input sample size (apply manually)
srvwts<-francis(repfile=MF2022,minage=1,maxage=20,nsexes=2,datatype = "srv")
fshwts<-francis(repfile=MF2022,minage=1,maxage=20,nsexes=2,datatype = "fsh")
write.table(t(srvwts$NewNsamp),file = "c1mod4_francis_2022/francis_nsmpl_srv_s.dat",quote = FALSE,row.names = F, col.names = F)
write.table(t(fshwts$NewNsamp),file = "c1mod4_francis_2022/francis_nsmpl_fsh_s.dat",quote = FALSE,row.names = F, col.names = F)


#Some tables:
#Likelihood values


#Fs, recdevs


#Times series tables:
#Numbers-at-age
write.csv(BaseRun$natage_f,file.path(BaseDir,"tables","NumbersAtAgeF.csv"),quote = FALSE,row.names = FALSE)
write.csv(BaseRun$natage_m,file.path(BaseDir,"tables","NumbersAtAgeM.csv"),quote = FALSE,row.names = FALSE)

#SSB, Recruitment, Total Biomass
CurrSSB<-as.data.frame(tslist$Current$SSB) %>% rename(Year = V1,SSB_Curr = V2, SSB_SD_Curr = V3,SSB_LB_Curr = V4, SSB_UB_Curr= V5)
PrevSSB<-as.data.frame(tslist$Previous$SSB) %>% rename(Year = V1,SSB_Prev = V2, SSB_SD_Prev = V3,SSB_LB_Prev = V4, SSB_UB_Prev= V5)


SSB_Table<-full_join(PrevSSB,CurrSSB) %>% select(Year,SSB_Prev,SSB_SD_Prev,SSB_Curr,SSB_SD_Curr)
write.csv(SSB_Table,file.path(BaseDir,"tables","TimeSeriesSSB.csv"),quote = FALSE,row.names = FALSE)
#Note: still need to add projections from Exec summary table. Do it manually; make sure they match

CurrTotBiom<-as.data.frame(tslist$Current$TotBiom) %>% rename(Year = V1,TotBiom_Curr = V2, TotBiom_SD_Curr = V3,TotBiom_LB_Curr = V4, TotBiom_UB_Curr= V5)
PrevTotBiom<-as.data.frame(tslist$Previous$TotBiom) %>% rename(Year = V1,TotBiom_Prev = V2, TotBiom_SD_Prev = V3,TotBiom_LB_Prev = V4, TotBiom_UB_Prev= V5)

TotBiom_Table<-full_join(PrevTotBiom,CurrTotBiom) %>% select(Year,TotBiom_Prev,TotBiom_SD_Prev,TotBiom_Curr,TotBiom_SD_Curr)
write.csv(TotBiom_Table,file.path(BaseDir,"tables","TimeSeriesTotBiom.csv"),quote = FALSE,row.names = FALSE)
#Note this is for all ages - ugh.


likes.df<-data.frame(model = "M18.3",
                     total = BaseRun$obj_fun,
                     survey = BaseRun$survey_likelihood,
                     fshry_age = BaseRun$age_likelihood_for_fishery,
                     surv_age =  BaseRun$age_likeihood_for_survey)
write.csv(t(likes.df),file.path(BaseDir,"tables","Likelihoods.csv"),quote = FALSE,row.names = FALSE)

likesM.df<-data.frame(model = "M22.2",
                     total = MBigM2022$obj_fun,
                     survey = MBigM2022$survey_likelihood,
                     fshry_age = MBigM2022$age_likelihood_for_fishery,
                     surv_age =  MBigM2022$age_likeihood_for_survey)

likesF.df<-data.frame(model = "M22.1",
                      total = MF2022$obj_fun,
                      survey = MF2022$survey_likelihood,
                      fshry_age = MF2022$age_likelihood_for_fishery,
                      surv_age =  MF2022$age_likeihood_for_survey)

Alikes.df<-rbind(likes.df,likesF.df,likesM.df)

write.csv(t(Alikes.df),file.path("tables","Likelihoods.csv"),quote = FALSE,row.names = FALSE)



