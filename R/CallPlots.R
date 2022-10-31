#Make plots for fm.tpl for NRS in 2022
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(ggridges)

rundir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2022/NRS/Runs/"
codedir<-"C:/GitProjects/BSAI_NRS/R"
.OVERLAY <-FALSE
.THEME<- theme_few()
maxage = 20
endyr = 2022

source(file.path(codedir,"prelims.R"))
source(file.path(codedir,"read-admb.R"))
source(file.path(codedir,"plot-catch.R"))
source(file.path(codedir,"plot-bts.R"))
source(file.path(codedir,"plot-recruitment.R"))
source(file.path(codedir,"plot-ssb.R"))
source(file.path(codedir,"plot-age-comps-Carey.R"))
source(file.path(codedir,"plot-fsh-sel.R"))
source(file.path(codedir,"plot-srv-sel.R"))
source(file.path(codedir,"plot-srr.R"))
source(file.path(codedir,"do-francis-weighting.R"))



setwd(rundir)
MF2022<-read_admb("c1mod4_francis_2022/fm")
M2022<-read_admb("c1mod4_alldata_2022/fm")
M2020<-read_admb("c1mod4_2020_final_run/fm")
mylist<-list()
mylist$MF2022<-MF2022
mylist$M2022<-M2022
mylist$M2020<-M2020

MF2022list<-list()
MF2022list$MF2022<-MF2022

g<-plot_catch(modlst=mylist,themod=1,obspred = TRUE)
ggsave(filename = "../plots/catch.png",plot = g,device = "png")

plot_bts(M = mylist)
plot_recruitment(M=mylist)
plot_ssb(M = mylist)
plot_age_comps(M = MF2022list, xlab = "Age (yrs)", ylab = "Proportion", 
                           nages=maxage,type="fishery",sex="split",title="Fishery age compositions")
plot_age_comps(M = MF2022list, xlab = "Age (yrs)", ylab = "Proportion", 
               nages=maxage,type="survey",sex="split",title="Survey age compositions")
plot_fsh_sel(mod=M2022, title=NULL,alpha=0.3,styr=1991,endyr=endyr,bysex=TRUE,sexoverlay=TRUE)
plot_srv_sel(theM=mylist,themod=1, title="Survey selectivity",bysex=TRUE,maxage = maxage)
plot_srr(M=mylist, ylab = "Recruits (age 1, billions)", xlab = "Female spawning biomass (kt)", 
                     ylim = NULL, xlim=NULL, alpha = 0.05,ebar="FALSE",leglabs=NULL,styr=1978,endyr=2016)

srvwts<-francis(repfile=MF2022,minage=1,maxage=20,nsexes=2,datatype = "srv")
fshwts<-francis(repfile=MF2022,minage=1,maxage=20,nsexes=2,datatype = "fsh")
write.table(t(srvwts$NewNsamp),file = "c1mod4_francis_2022/francis_nsmpl_srv_s.dat",quote = FALSE,row.names = F, col.names = F)
write.table(t(fshwts$NewNsamp),file = "c1mod4_francis_2022/francis_nsmpl_fsh_s.dat",quote = FALSE,row.names = F, col.names = F)


