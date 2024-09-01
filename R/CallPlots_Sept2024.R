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
#rundir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2022/NRS/Runs/"

#2024
rundir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2024/bsai_nrs/runs/"

codedir<-"C:/GitProjects/BSAI_NRS/R"
.OVERLAY <-TRUE
.THEME<- theme_few()
maxage = 20
endyr = 2024

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
source("C:/GitProjects/BSAI_NRS/R/plot-aggregated-comps.R", echo=TRUE)

setwd(rundir)
mydirs<-list()
mydirs$M18.3<-"c1mod4_alldata_2022"
mydirs$M18.3_new<-"c1mod4_alldata_2022_sept2024"
mydirs$M22.1<-"c2mod4_francis_2022_sept2024"
mydirs$M24.1<-"c3mod4_francis_ISS_sept2024"
mydirs$M24.2<-"c3mod5_ISS_francis_mq_sept2024"

#Runs to compare
m_2022<-read_admb("c1mod4_alldata_2022/fm")
m_sept2024<-read_admb("c1mod4_alldata_2022_sept2024/fm")
m_francis_sept2024<-read_admb("c2mod4_francis_2022_sept2024/fm")
#ran the next one but not needed for comparison
#m_mq_sept2024<-read_admb("c1mod4_bigpriors_MQ_sept2024/fm")
m_iss_francis_sept2024<-read_admb("c3mod4_francis_ISS_sept2024/fm")
m_iss_mq_sept2024<-read_admb("c3mod5_ISS_francis_mq_sept2024/fm")

mylist<-list()
mylist$M18.3<-m_2022
mylist$M18.3_new<-m_sept2024 #2022 accepted model with new data up to sept 2024
mylist$M22.1<-m_francis_sept2024 #2022 model with OFL used as a basis for the ABC
mylist$M24.1<-m_iss_francis_sept2024 #uses afscISS to define input sample sizes prior to applying Francis data weighting
mylist$M24.2<-m_iss_mq_sept2024 #As for M24.1 but estimates female M and q with broad priors


#Specify the base case
BaseDir<-"c1mod4_alldata_2022_sept2024"
BaseList<-list()
BaseList$M18.3<-m_sept2024
BaseRun<-m_sept2024

#Time-series runs
tslist<-list()
tslist$Current<-BaseRun
tslist$Previous<-m_2022

#Make plot directories if they don't exist:
for (i in 1:length(mydirs)) {
if (!dir.exists(file.path(mydirs[[i]],"plots"))) {
  dir.create(file.path(mydirs[[i]],"plots"))
}
}
if (!dir.exists("plots")) {
  dir.create("plots")
}

for (i in 1:length(mydirs)) {
#Make table directories if they don't exist:
if (!dir.exists(file.path(mydirs[[i]],"tables"))) {
  dir.create(file.path(mydirs[[i]],"tables"))
}
}

if (!dir.exists("tables")) {
  dir.create("tables")
}

#Do data weighting
srvwts<-francis(repfile=m_iss_francis_sept2024,minage=1,maxage=20,nsexes=2,datatype = "srv")
fshwts<-francis(repfile=m_iss_francis_sept2024,minage=1,maxage=20,nsexes=2,datatype = "fsh")

# write.csv(t(srvwts$NewNsamp),file = file.path("c3mod4_francis_ISS_sept2024","srvwts.csv"))
# write.csv(t(fshwts$NewNsamp),file = file.path("c3mod4_francis_ISS_sept2024","fshwts.csv"))


#Make and save plots (comparison plots and base case plots)
a<-plot_catch(modlst=mylist,themod=1,obspred = FALSE)
ggsave(filename = "plots/catch.png",plot = a,device = "png",height = 7, width = 10,units = "in")
ggsave(filename = file.path(BaseDir,"plots","catch.png"),plot = a,device = "png",height = 7, width = 10,units = "in")

b<-plot_bts(M = mylist)
bb<-plot_bts(M = BaseList)
ggsave(filename = file.path("plots","bts_compare.png"),plot = b,device = "png",height = 10, width = 9,units = "in")
ggsave(filename = file.path(BaseDir,"plots","bts.png"),plot = bb,device = "png",height = 10, width = 9,units = "in")


c<-plot_rec(M=mylist)
cc<-plot_rec(M=BaseList)
ggsave(filename = file.path("plots","recruits_compare.png"),plot = c,device = "png",height = 6, width = 10,units = "in")
ggsave(filename = file.path(BaseDir,"plots","recruits.png"),plot = cc,device = "png",height = 6, width = 10,units = "in")

d<-plot_ssb(M = mylist)
dd<-plot_ssb(M = BaseList)
ggsave(filename = "plots/ssb_compare.png",plot = d,device = "png",height = 6, width = 10,units = "in")
ggsave(filename = file.path(BaseDir,"plots","ssb.png"),plot = dd,device = "png",height = 6, width = 10,units = "in")


e<-plot_srr(M=mylist, ylab = "Recruits (age 1, billions)", xlab = "Female spawning biomass (kt)", 
            ylim = NULL, xlim=NULL, alpha = 0.05,ebar="FALSE",leglabs=NULL,styr=1978,endyr=2016)
ee<-plot_srr(M=BaseList, ylab = "Recruits (age 1, billions)", xlab = "Female spawning biomass (kt)", 
             ylim = NULL, xlim=NULL, alpha = 0.05,ebar="FALSE",leglabs=NULL,styr=1978,endyr=2016)

ggsave(filename = file.path("plots","srr_compare.png"),plot = e,device = "png",height = 6, width = 10,units = "in")
ggsave(filename = file.path(BaseDir,"plots","srr.png"),plot = ee,device = "png",height = 6, width = 10,units = "in")

ff<-plot_srv_sel(theM=mylist,themod=1, title="Survey selectivity",bysex=TRUE,maxage = maxage)
ggsave(filename = file.path(BaseDir,"plots","srv_sel.png"),plot = ff$single_model,device = "png",height = 6, width = 10,units = "in")
ggsave(filename = file.path("plots","srv_sel_compare.png"),plot = ff$compare_models,device = "png",height = 6, width = 10,units = "in")
ggsave(filename = file.path("plots","srv_sel_all_models.png"),plot = ff$all_models,device = "png",height = 6, width = 10,units = "in")

jj<-plot_sex_ratio(M = mylist, type = "Fishery") #I don't understand why this doesn't work
ss<-plot_sex_ratio(M = mylist, type = "Survey") #I don't understand why this doesn't work
pp <-plot_sex_ratio(M = mylist, type = "Population")

all_sr<-jj/ss/pp
ggsave(file.path("plots","sex_ratios.png"),plot = all_sr,width = 8, height = 10.5,units = "in",device = "png")

agg_comps_plot<-plot_agg_comps(the_runs=mylist,max_age=max_age) 
ggsave(file.path("plots","aggregate_comps.png"),plot = agg_comps_plot,width = 11,height = 6,units = "in",device = "png")

ii<-list()

for (i in 1:length(mylist)) {
  onelist<-list()
  onelist$M<-mylist[[i]]
  gg<-plot_age_comps(M = onelist, xlab = "Age (yrs)", ylab = "Proportion", 
                           nages=maxage,type="fishery",sex="split",title="Fishery age compositions")
  ggsave(filename = file.path(mydirs[[i]],"plots","age_comps_fsh.png"),plot = gg,device = "png",width = 11,height = 8.5,units = "in")

  hh<-plot_age_comps(M = onelist, xlab = "Age (yrs)", ylab = "Proportion", 
               nages=maxage,type="survey",sex="split",title="Survey age compositions")
  ggsave(filename = file.path(mydirs[[i]],"plots","age_comps_srv.png"),plot = hh,device = "png",width = 11,height = 8.5,units = "in")

  zz<-plotFs(modlst=onelist,thismod = 1,thismodname = "M18.3")
  ggsave(file.path(mydirs[[i]],"plots","FatAge_MeanF.png"),plot=zz,width=8,height=8.0,units="in")

  ii[[i]]<-plot_fsh_sel(mod=mylist[[i]], title=names(mylist[i]),alpha=0.3,styr=1991,endyr=endyr,bysex=TRUE,sexoverlay=TRUE,legend_position = "bottom")
  ggsave(filename = file.path(mydirs[[i]],"plots","fsh_sel.png"),plot = ii[[i]],device = "png",height = 11,width = 6)
  
}

#for this plot might need to re-run ii above with legend_position = "none"
all_fish_sel_plot<-ii[[2]] + ii[[4]] +ii[[5]]
ggsave(filename = file.path("plots","fsh_sel.png"),plot = all_fish_sel_plot,device = "png",height = 8.5,width = 11)

# Calculate Francis Weights - spits out new vectors for input sample size (apply manually)
srvwts<-francis(repfile=MF2022,minage=1,maxage=20,nsexes=2,datatype = "srv")
fshwts<-francis(repfile=MF2022,minage=1,maxage=20,nsexes=2,datatype = "fsh")
write.table(t(srvwts$NewNsamp),file = "c1mod4_francis_2022/francis_nsmpl_srv_s.dat",quote = FALSE,row.names = F, col.names = F)
write.table(t(fshwts$NewNsamp),file = "c1mod4_francis_2022/francis_nsmpl_fsh_s.dat",quote = FALSE,row.names = F, col.names = F)


#Some tables:
#Parameter estimates
std<-list()
q_srv<-list()
rec_dev<-list()

for (i in 1:length(mylist)) {
std[[i]]<-read.table(file.path(mydirs[[i]],"fm.std"),header = TRUE) %>%
     mutate(model = names(mylist[i]))
q_srv[[i]]<- std[[i]] %>% filter(name == "q_srv")
q_srv[[i]]<-q_srv[[i]][1,]
std[[i]] <-std[[i]] %>% filter(name != "q_srv")
std[[i]]<-std[[i]] %>% bind_rows(q_srv[[i]])

rec_dev[[i]]<-std[[i]] %>% filter(name == "rec_dev")
rec_dev[[i]]$year<-mylist[[i]]$Yr
}

std_t<-bind_rows(std[[1]],std[[2]])

for (i in 3:length(mylist)) {
  std_t<-std_t %>% bind_rows(std[[i]])
}



main_ps<-std_t %>% filter(name == "q_srv" | name == "natmort_m" | name== "natmort_f" |
                          name == "mean_log_rec" | name == "mean_log_init" | name == "log_avg_fmort" |
                          name == "R_logalpha" | name == "R_logbeta")



derived_ps<-std_t %>% filter(name == "log_msy_sel_f" | name == "log_msy_sel_m" | name == "msy" |
                             name == "Fmsy" | name == "logFmsy" | name =="Fmsyr" | name== "logFmsyr" |
                             name == "Bmsy" | name == "Bmsyr")
                         

sel_ps<-std_t %>% filter( name == "sel_slope_fsh_f" | name == "sel50_fsh_f" | name == "sel_slope_fsh_m" |
                            name == "male_sel_offset" | name == "sel_slope_srv" | name=="sel50_srv" |
                            name == "sel_slope_srv" | name == "sel_slope_srv_m")




  
  
#Times series tables:
for (i in 1:length(mylist)) {
#Numbers-at-age
write.csv(mylist[[i]]$natage_f,file.path(mydirs[[i]],"tables","NumbersAtAgeF.csv"),quote = FALSE,row.names = FALSE)
write.csv(mylist[[i]]$natage_m,file.path(mydirs[[i]],"tables","NumbersAtAgeM.csv"),quote = FALSE,row.names = FALSE)

#SSB, Recruitment, Total Biomass
CurrSSB<-as.data.frame(mylist[[i]]$SSB) %>% rename(Year = V1,SSB_Curr = V2, SSB_SD_Curr = V3,SSB_LB_Curr = V4, SSB_UB_Curr= V5)
PrevSSB<-as.data.frame(tslist$Previous$SSB) %>% rename(Year = V1,SSB_Prev = V2, SSB_SD_Prev = V3,SSB_LB_Prev = V4, SSB_UB_Prev= V5)


SSB_Table<-full_join(PrevSSB,CurrSSB) %>% select(Year,SSB_Prev,SSB_SD_Prev,SSB_Curr,SSB_SD_Curr)
write.csv(SSB_Table,file.path(mydirs[[i]],"tables","TimeSeriesSSB.csv"),quote = FALSE,row.names = FALSE)
#Note: still need to add projections from Exec summary table. Do it manually; make sure they match

CurrTotBiom<-as.data.frame(mylist[[i]]$TotBiom) %>% rename(Year = V1,TotBiom_Curr = V2, TotBiom_SD_Curr = V3,TotBiom_LB_Curr = V4, TotBiom_UB_Curr= V5)
PrevTotBiom<-as.data.frame(tslist$Previous$TotBiom) %>% rename(Year = V1,TotBiom_Prev = V2, TotBiom_SD_Prev = V3,TotBiom_LB_Prev = V4, TotBiom_UB_Prev= V5)

TotBiom_Table<-full_join(PrevTotBiom,CurrTotBiom) %>% select(Year,TotBiom_Prev,TotBiom_SD_Prev,TotBiom_Curr,TotBiom_SD_Curr)
write.csv(TotBiom_Table,file.path(mydirs[[i]],"tables","TimeSeriesTotBiom.csv"),quote = FALSE,row.names = FALSE)
#Note this is for all ages - ugh.
}

#Likelihoods
likes<-list()
for (i in 1:length(mylist)) {
likes[[i]]<-data.frame(model = names(mylist)[i],
                     total = mylist[[i]]$obj_fun,
                     survey = mylist[[i]]$survey_likelihood,
                     fshry_age = mylist[[i]]$age_likelihood_for_fishery,
                     surv_age =  mylist[[i]]$age_likeihood_for_survey)
write.csv(t(likes[[i]]),file.path(mydirs[[i]],"tables","Likelihoods.csv"),quote = FALSE,row.names = FALSE)
}
Alikes.df<-rbind(likes[[1]],likes[[2]])

for (i in 3:length(mylist)) {
  Alikes.df<-rbind(Alikes.df,likes[[i]])
}

write.csv(t(Alikes.df),file.path("tables","Likelihoods.csv"),quote = FALSE,row.names = FALSE)



