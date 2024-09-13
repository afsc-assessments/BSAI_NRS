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
library(compResidual)

#2022
#rundir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2022/NRS/Runs/"

#2024
rundir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2024/bsai_nrs/runs/"

codedir<-"C:/GitProjects/BSAI_NRS/R"
.OVERLAY <-TRUE
.THEME<- theme_few() + theme(text=element_text(size=20))
maxage = 20
endyr = 2024

plot_purpose<-"plot_presentation"
#plot_purpose<-"plots"

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
source("C:/GitProjects/BSAI_NRS/R/plot_osa_comps.R", echo=TRUE)


setwd(rundir)
mydirs<-list()
mydirs$M18.3<-"c1mod4_alldata_2022"
mydirs$M18.3_new<-"c1mod4_alldata_2022_sept2024"
mydirs$M22.1<-"c2mod4_francis_2022_sept2024"
mydirs$M24.1<-"c3mod4_francis_ISS_sept2024"
mydirs$M24.2<-"c3mod5_ISS_francis_mq_sept2024"

sens_dirs<-list()
sens_dirs$M24.2<-"c3mod5_ISS_francis_mq_sept2024"
sens_dirs$M24.2b<-"c3mod5_toms_ages"
sens_dirs$M24.2c<-"c3mod5_LOO_fsh_ages"

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
if (!dir.exists(plot_purpose)) {
  dir.create(plot_purpose)
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
ggsave(filename = file.path(plot_purpose,"catch.png"),plot = a,device = "png",height = 5, width = 7,units = "in")
ggsave(filename = file.path(BaseDir,plot_purpose,"catch.png"),plot = a,device = "png",height = 5, width = 7,units = "in")

b<-plot_bts(M = mylist)
bb<-plot_bts(M = BaseList)
ggsave(filename = file.path(plot_purpose,"bts_compare.png"),plot = b,device = "png",height = 10, width = 9,units = "in")
ggsave(filename = file.path(BaseDir,plot_purpose,"bts.png"),plot = bb,device = "png",height = 10, width = 9,units = "in")


c<-plot_rec(M=mylist)
cc<-plot_rec(M=BaseList)
ggsave(filename = file.path(plot_purpose,"recruits_compare.png"),plot = c,device = "png",height = 6, width = 10,units = "in")
ggsave(filename = file.path(BaseDir,plot_purpose,"recruits.png"),plot = cc,device = "png",height = 6, width = 10,units = "in")

d<-plot_ssb(M = mylist)
dd<-plot_ssb(M = BaseList)
ggsave(filename = file.path(plot_purpose,"ssb_compare.png"),plot = d,device = "png",height = 6, width = 10,units = "in")
ggsave(filename = file.path(BaseDir,plot_purpose,"ssb.png"),plot = dd,device = "png",height = 6, width = 10,units = "in")


e<-plot_srr(M=mylist, ylab = "Recruits (age 1, billions)", xlab = "Female spawning biomass (kt)", 
            ylim = NULL, xlim=NULL, alpha = 0.05,ebar="FALSE",leglabs=NULL,styr=1978,endyr=2016)
ee<-plot_srr(M=BaseList, ylab = "Recruits (age 1, billions)", xlab = "Female spawning biomass (kt)", 
             ylim = NULL, xlim=NULL, alpha = 0.05,ebar="FALSE",leglabs=NULL,styr=1978,endyr=2016)

ggsave(filename = file.path(plot_purpose,"srr_compare.png"),plot = e,device = "png",height = 6, width = 10,units = "in")
ggsave(filename = file.path(BaseDir,plot_purpose,"srr.png"),plot = ee,device = "png",height = 6, width = 10,units = "in")

ff<-plot_srv_sel(theM=mylist,themod=1, title="Survey selectivity",bysex=TRUE,maxage = maxage)
ggsave(filename = file.path(BaseDir,plot_purpose,"srv_sel.png"),plot = ff$single_model,device = "png",height = 4, width = 7,units = "in")
ggsave(filename = file.path(plot_purpose,"srv_sel_compare.png"),plot = ff$compare_models,device = "png",height = 4, width = 7,units = "in")
ggsave(filename = file.path(plot_purpose,"srv_sel_all_models.png"),plot = ff$all_models,device = "png",height = 4, width = 7,units = "in")

jj<-plot_sex_ratio(M = mylist, type = "Fishery") #I don't understand why this doesn't work
ss<-plot_sex_ratio(M = mylist, type = "Survey") #I don't understand why this doesn't work
pp <-plot_sex_ratio(M = mylist, type = "Population")

all_sr<-jj/ss/pp
ggsave(file.path(plot_purpose,"sex_ratios.png"),plot = all_sr,width = 8, height = 10.5,units = "in",device = "png")

agg_comps_plot<-plot_agg_comps(the_runs=mylist,max_age=max_age) 
ggsave(file.path(plot_purpose,"aggregate_comps.png"),plot = agg_comps_plot,width = 7,height = 3,units = "in",device = "png")

ii<-list()
iii<-list()

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

  iii[[i]]<-plot_fsh_sel(mod=mylist[[i]], title=names(mylist[i]),alpha=0.3,styr=1975,endyr=1990,bysex=TRUE,sexoverlay=TRUE,legend_position = "bottom")
  ggsave(filename = file.path(mydirs[[i]],"plots","fsh_sel.png"),plot = ii[[i]],device = "png",height = 11,width = 6)
  

}

#for this plot might need to re-run ii above with legend_position = "none"
all_fish_sel_plot<-ii[[2]] + ii[[4]] +ii[[5]]
ggsave(filename = file.path("plots","fsh_sel.png"),plot = all_fish_sel_plot,device = "png",height = 8.5,width = 11)

#early fishery selectivity
early_fish_sel_plot<-iii[[2]] + iii[[4]] + iii[[5]]
ggsave(filename = file.path("plots","early_fsh_sel.png"),plot = early_fish_sel_plot,device = "png",height = 8.5,width = 11)



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
                          name == "R_logalpha" | name == "R_logbeta" | name== "logFmsyr" | name == "Bmsy" ) %>%
                   select(-c(index)) %>% 
                   pivot_wider(names_from = model,values_from = c(value, std.dev))

write.csv(main_ps,file.path("tables","main_parameters.csv"))

#These can be written out, but it's logFmsyr and Bmsy that are used in management calculations and reporting.
# derived_ps<-std_t %>% filter( name == "msy" |
#                              name == "Fmsy" | name == "logFmsy" | name =="Fmsyr" | name== "logFmsyr" |
#                              name == "Bmsy" | name == "Bmsyr") %>%
#                       select(-c(index)) %>%
#                       pivot_wider(names_from = model,values_from = c(value, std.dev))
#
#write.csv(derived_ps,file.path("tables","derived_parameters.csv"))

sel_ps<-std_t %>% filter( name == "sel_slope_fsh_f" | name == "sel50_fsh_f" | name == "sel_slope_fsh_m" |
                            name == "male_sel_offset" | name == "sel_slope_srv" | name=="sel50_srv" |
                            name == "sel_slope_srv" | name == "sel_slope_srv_m") %>%
                  select(-c(index)) %>%
                  pivot_wider(names_from = model,values_from = c(value, std.dev))
write.csv(sel_ps,file.path("tables","selex_parameters.csv"))

#selectivity curve used for projections in logspace
ages<-seq(from = 1, to = maxage, by = 1)
long_ages<-rep(ages, 2*length(mylist))
projection_sel_t <-std_t %>% filter(name == "log_msy_sel_f" | name == "log_msy_sel_m") %>%
  select(-c(index))

projection_sel_t$age<-long_ages
#plot the fishery selex used for projections
projection_sel_t<-projection_sel_t %>% mutate(selex = exp(value))

# p_proj_sel<-ggplot() %>%
#             geom_line(data = projection_sel_t,aes(x = age,y = selex,color = model)) +
#             facet_wrap(~)


projection_sel_wide<-projection_sel_t %>%
  pivot_wider(names_from = model,values_from = c(value, std.dev))




write.csv(projection_sel_wide,file.path("tables","projection_selex_parameters.csv"))


  
  
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

#Note this is for all ages - ugh.
TotBiom_Table<-full_join(PrevTotBiom,CurrTotBiom) %>% select(Year,TotBiom_Prev,TotBiom_SD_Prev,TotBiom_Curr,TotBiom_SD_Curr)
write.csv(TotBiom_Table,file.path(mydirs[[i]],"tables","TimeSeriesTotBiom.csv"),quote = FALSE,row.names = FALSE)

CurrRec<-as.data.frame(mylist[[i]]$R) %>% rename(Year = V1,Rec_Curr = V2, Rec_SD_Curr = V3,Rec_LB_Curr = V4, Rec_UB_Curr= V5)
PrevRec<-as.data.frame(tslist$Previous$R) %>% rename(Year = V1,Rec_Prev = V2, Rec_SD_Prev = V3,Rec_LB_Prev = V4, Rec_UB_Prev= V5)

Rec_Table<-full_join(PrevRec,CurrRec) %>% select(Year,Rec_Prev,Rec_SD_Prev,Rec_Curr,Rec_SD_Curr)
write.csv(Rec_Table,file.path(mydirs[[i]],"tables","TimeSeriesRecruits.csv"),quote = FALSE,row.names = FALSE)

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

#OSA residuals
library(TMB)

#TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
#remotes::install_github("fishfollower/compResidual/compResidual", INSTALL_opts=c("--no-multiarch"), force=TRUE)

library(compResidual)
#idea for setting this up later
source("C:/GitProjects/BSAI_NRS/R/get_osas.R", echo=TRUE)
#puts plots into the folder for each run/plots
get_osas_2(mylist,mydirs,max_age=20,type = "survey")
get_osas_2(mylist,mydirs,max_age=20,type = "fishery")




