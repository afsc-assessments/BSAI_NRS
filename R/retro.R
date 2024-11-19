###########1. Required packages ##########
library(devtools)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)

# Set your local directory--------
#	mydir <- "C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2022/NRS/Runs"  #change to mine.
  mydir <- "C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2024/bsai_nrs/runs"
  codedir<-"C:/GitProjects/BSAI_NRS/R"
# Get tools to read-write control file
#	set up a file in mydir called "retro" and put a folder in there called "orig" with the basics of your favorit model, mod.ctl, fm.dat, makefile,...
source(file.path(codedir,"read-admb.R"))
.OVERLAY <-TRUE
.THEME<- theme_few()
  
master <-(file.path(mydir,"run1_m18.3_new_retro"))

assess_LY=2024					#assessment terminal year
endyrvec <-1:10		    #retrospective peels (Subtracted from endyr in tpl)

# Values used to downweight survey data when invoked written to retro.dat
#Specify values here
#does this get used??
CV_inc=100	#CPUE CV used to downweight 
comp_n=.001	#input sample size  for length and age composition data used to downweight

do_run <- TRUE  #very important to make the runs go!
df_res <- NULL
filestocopy=c("fm.exe","fm.dat","mod.ctl","c1.dat","fut_temp.dat","future_catch.dat")

for(i in 0:length(endyrvec)){
	# Start in "retro" directory (runs/retro)
	  setwd(master)
	# get new run location and copy files from orig
	  rundir <- 	paste0(master,"/retro_",i )
	  dir.create(rundir)
    setwd("orig")
		file.copy(filestocopy, to=rundir,overwrite = TRUE,recursive = FALSE,copy.mode = FALSE,copy.date = FALSE)
	  
	# Go to that directory, update mod.ctl to this model's configuration
	  setwd(rundir)
	  ctl<- read_dat("mod.ctl")
	  #ctl <- read_ctl("mod.ctl")
	  ctl$n_retro     <- i
	  #ctl$surv_dwnwt  <- j-1
	  write_dat(ctl,"mod.ctl")
	# run model
#	  if (do_run) system("rm fm.std; make") 
	  if (do_run) system("fm") 
	  
	# Save results
	  dftmp        <- data.frame(read.table("ABC_OFL.rep",header=TRUE))
	  dftmp$peel   <- -i
	  df_res       <- rbind(df_res,dftmp)
}	

##6. save results
#write.csv(df_res,file.path(master,"retro_results_ABC_OFL.csv"))
#names(df_res)

# Pool up retro results------------------
df_retro <- data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("Year","Peel","SSB","SSB_sd","Totbio","Totbio_sd","Rec","Rec_sd","run"))))
j=1; i=4
for(i in 0:length(endyrvec)){
  retro_dir <- 	paste0(master,"/retro_",i )
  rep <- read_rep(paste0(retro_dir,"/fm.rep"))
  df_tmp <- data.frame(
    Year      = rep$SSB[,1],
    Peel      = i,
    SSB       = rep$SSB[,2],
    SSB_sd    = rep$SSB[,3],
    Totbio    = rep$TotBiom[,2],
    Totbio_sd = rep$TotBiom[,3],
    Rec       = rep$R[,2],
    Rec_sd    = rep$R[,3]
  )
  # Append run results
  # "Year Peel SSB ssb_stdev Tot_bio  totbio_stdev Rec rec_stdev run"
  df_retro <- rbind(df_retro,df_tmp)
}

#double lb=value(SSB(i)/exp(2.*sqrt(log(1+square(SSB.sd(i))/square(SSB(i))))));
#double ub=value(SSB(i)*exp(2.*sqrt(log(1+square(SSB.sd(i))/square(SSB(i))))));

df_retro<-df_retro %>% mutate(SSB_lb = SSB/exp(2*sqrt(log(1+(SSB_sd/SSB)^2))),SSB_ub = SSB*exp(2*sqrt(log(1+(SSB_sd/SSB)^2)))) %>% 
  mutate(Rec_lb = Rec/exp(2*sqrt(log(1+(Rec_sd/Rec)^2))),Rec_ub = Rec*exp(2*sqrt(log(1+(Rec_sd/Rec)^2)))) %>%
  mutate(Totbio_lb = Totbio/exp(2*sqrt(log(1+(Totbio_sd/Totbio)^2))),Totbio_ub = Totbio*exp(2*sqrt(log(1+(Totbio_sd/Totbio)^2))))

setwd(master)
write.csv(df_retro,file.path(master,"retro_results_yr.csv"),row.names = FALSE)
#df_retro<-read.csv(file = file.path(master,"retro_results_yr.csv"))
###--jim stopped here...-------------------------------------

#Meaghan's:

#RETROSPECTIVE PLOTS
retro.labs=c("")
names(retro.labs)=c("retr")
#txt_df=data.frame(label=paste0("rho = ",rho_SSB)) # I have a text file that have the rho values

SSBretro_withCI=ggplot(df_retro,aes(x=Year,y=SSB,color=as.factor(Peel)))+geom_point()+geom_line(size=1)+
  geom_ribbon(aes(ymin=SSB_lb,ymax=SSB_ub,fill=as.factor(Peel)),colour=NA,alpha=0.15) + #+
  # facet_grid(retro~.,labeller=labeller(retro=retro.labs))+theme_bw()+
  # theme(strip.text.x=element_text(size=12,color="black",face="bold.italic"),strip.background=element_rect(fill="white"))+
   xlab("Year")+ylab("Spawning Biomass (mt)")
  # scale_color_viridis(name="Peel",discrete=TRUE,option="D")+
  # scale_fill_viridis(name="Peel",discrete=TRUE,option="D")
ggsave(file.path(master,"SSBretro_withCI.png"))

Recretro_withCI=ggplot(df_retro,aes(x=Year,y=Rec,color=as.factor(Peel)))+geom_point()+geom_line(size=1)+
  geom_ribbon(aes(ymin=Rec_lb,ymax=Rec_ub,fill=as.factor(Peel)),colour=NA,alpha=0.15) +
  xlab("Year")+ylab("Recruitment") #+
ggsave(file.path(master,"Recretro_withCI.png"))

Totretro_withCI=ggplot(df_retro,aes(x=Year,y=Totbio,color=as.factor(Peel)))+geom_point()+geom_line(size=1)+
  geom_ribbon(aes(ymin=Totbio_lb,ymax=Totbio_ub,fill=as.factor(Peel)),colour=NA,alpha=0.15) + 
  xlab("Year")+ylab("Total Biomass (mt)") #+
ggsave(file.path(master,"Totbioretro_withCI.png"))

#calculate AFSC rho
N <-11
endyrvec<-2022:2012
mohnSSB<-rep(NA,length = (length(endyrvec)-1))
for (i in 1:(N - 1)) {
  #ind = which(df_retro$Year == endyrvec[i+1])
   ssbpeel.df<-df_retro %>%  filter(Year==endyrvec[i+1],Peel == i) %>% select(Year,Peel,SSB)
   ssb1.df<-df_retro %>% filter(Year==endyrvec[i+1],Peel == 0) %>% select(Year,Peel,SSB)
  
   ssbpeel <-ssbpeel.df$SSB
   ssb1 <- ssb1.df$SSB
  mohnSSB[i] = (ssbpeel - ssb1)/ssb1
}
mohn.out = list()
mohn.out$SSB = sum(mohnSSB)/length(mohnSSB)
write.csv(mohn.out,file = file.path(master,"AFSC_MohnsRho.csv"))









