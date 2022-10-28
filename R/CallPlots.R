#Make plots for fm.tpl for NRS in 2022
library(ggplot2)
library(ggthemes)

rundir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2022/NRS/Runs/c1mod4_2022"
codedir<-"C:/GitProjects/BSAI_NRS/R"
.OVERLAY <-FALSE
.THEME<- theme_few()


source(file.path(codedir,"read-admb.R"))
source(file.path(codedir,"plot-recruitment.R"))
source(file.path(codedir,"plot-ssb.R"))


setwd(rundir)
myrep<-read_admb("fm")
mylist<-list()
mylist$myrep<-myrep


plot_recruitment(M=mylist)
plot_ssb(M = mylist)
