###########1. Required packages ##########
library(devtools)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)

# Set your local directory--------
	mydir <- "C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2022/NRS/Runs"  #change to mine.
  codedir<-"C:/GitProjects/BSAI_NRS/R"
# Get tools to read-write control file
#	set up a file in mydir called "retro" and put a folder in there called "orig" with the basics of your favorit model, mod.ctl, fm.dat, makefile,...
source(file.path(codedir,"read-admb.R"))
.OVERLAY <-TRUE
.THEME<- theme_few()
  
master <-(file.path(mydir,"c1mod4_francis_2022_retro"))

assess_LY=2022					#assessment terminal year
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
write.csv(df_retro,file.path(master,"retro_results_yr.csv"))

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













#names(mod1$SSB)
#rn = "mod1"
#dim(tdf)
p1 <- ggplot() + scale_y_continuous(limits=c(0,max(df_retro$SSB_ub))) + ylab("Spawning biomass") + xlab("Year") + ggthemes::theme_base() + geom_point(data=df_retro,aes(x=Year,y=SSB),size=4) +
  geom_ribbon(data=df_retro ,aes(x=Year,y=SSB,ymin=SSB_lb,ymax=SSB_ub),fill="tan",col="grey",alpha=.6) # + guides(fill=FALSE,alpha=FALSE,col=FALSE) 
for (i in 1:10) {
#  rn=paste("retro",i,sep="");
 # tdf <- data.frame(get(rn)$SSB); names(tdf) <- c("yr","SSB","SE","lb","ub"); tdf <- filter(tdf,yr>1977)
  p1 <- p1 + geom_line(data=tdf,aes(x=yr,y=SSB),col=i,linetype=i,size=1.25)
  #p1 <- p1 + geom_segment(data=tdf,aes(x=yr,xend=yr,yend=SSB,y=SSB),arrow=arrow(angle=90,length=unit(.2,"cm")),size=2,col=i)
  tdf <- tdf[dim(tdf)[1],]
  p1 <- p1 + geom_point(data=tdf,aes(x=yr,y=SSB),size=4,col=i)
  #p1 <- p1 + geom_point(get(rn)$SSB[lr,1],get(rn)$SSB[lr,2],pch=19,col=i)
}
p1






#A bunch of Ingrid's stuff
YFS2022=read.rep("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_0/fm.rep")
YFS2021=read.rep("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_1/fm.rep")
YFS2020=read.rep("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_2/fm.rep")
YFS2019=read.rep("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_3/fm.rep")
YFS2018=read.rep("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_4/fm.rep")
YFS2017=read.rep("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_5/fm.rep")
YFS2016=read.rep("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_6/fm.rep")
YFS2015=read.rep("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_7/fm.rep")
YFS2014=read.rep("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_8/fm.rep")
YFS2013=read.rep("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_9/fm.rep")
YFS2012=read.rep("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_10/fm.rep")
a=-1*(YFS2022$SSB[1:68,2]-YFS2021$SSB[,2])/YFS2022$SSB[1:68,2]  
b=-1*(YFS2022$SSB[1:67,2]-YFS2020$SSB[,2])/YFS2022$SSB[1:67,2]
c=-1*(YFS2022$SSB[1:66,2]-YFS2019$SSB[,2])/YFS2022$SSB[1:66,2]
d=-1*(YFS2022$SSB[1:65,2]-YFS2018$SSB[,2])/YFS2022$SSB[1:65,2]
e=-1*(YFS2022$SSB[1:64,2]-YFS2017$SSB[,2])/YFS2022$SSB[1:64,2]
f=-1*(YFS2022$SSB[1:63,2]-YFS2016$SSB[,2])/YFS2022$SSB[1:63,2]
g=-1*(YFS2022$SSB[1:62,2]-YFS2015$SSB[,2])/YFS2022$SSB[1:62,2]
h=-1*(YFS2022$SSB[1:61,2]-YFS2014$SSB[,2])/YFS2022$SSB[1:61,2]
i=-1*(YFS2022$SSB[1:60,2]-YFS2013$SSB[,2])/YFS2022$SSB[1:60,2]
j=-1*(YFS2022$SSB[1:59,2]-YFS2012$SSB[,2])/YFS2022$SSB[1:59,2]

Rho22_0=(a[length(a)]+b[length(b)]+c[length(c)]+d[length(d)]+e[length(e)]+
          f[length(f)]+g[length(g)]+h[length(h)]+i[length(i)]+j[length(j)])/10;


std2022=read.table("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_0/fm.std")
std2021=read.table("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_1/fm.std")
std2020=read.table("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_2/fm.std")
std2019=read.table("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_3/fm.std")
std2018=read.table("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_4/fm.std")
std2017=read.table("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_5/fm.std")
std2016=read.table("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_6/fm.std")
std2015=read.table("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_7/fm.std")
std2014=read.table("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_8/fm.std")
std2013=read.table("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_9/fm.std")
std2012=read.table("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/retro22_0/retro_10/fm.std")

ret22=data.frame(cbind(c(1954:2022),YFS2022$SSB[,2],YFS2022$SSB[,2]+1.96*as.numeric(as.vector(std2022[['V4']][622:690])),YFS2022$SSB[,2]-1.96*as.numeric(as.vector(std2022[['V4']][622:690]))))

ret21=data.frame(cbind(c(1954:2021),YFS2021r$SSB[,2],YFS2021r$SSB[,2]+1.96*as.numeric(as.vector(std2021[['V4']][615:682])),YFS2021r$SSB[,2]-1.96*as.numeric(as.vector(std2021[['V4']][615:682]))))

ret20=data.frame(cbind(c(1954:2020),YFS2020$SSB[,2],YFS2020$SSB[,2]+1.96*as.numeric(as.vector(std2020[['V4']][608:674])),YFS2020$SSB[,2]-1.96*as.numeric(as.vector(std2020[['V4']][608:674]))))

ret19=data.frame(cbind(c(1954:2019),YFS2019$SSB[,2],YFS2019$SSB[,2]+1.96*as.numeric(as.vector(std2019[['V4']][601:666])),YFS2019$SSB[,2]-1.96*as.numeric(as.vector(std2019[['V4']][601:666]))))

ret18=data.frame(cbind(c(1954:2018),YFS2018$SSB[,2],YFS2018$SSB[,2]+1.96*as.numeric(as.vector(std2018[['V4']][594:658])),YFS2018$SSB[,2]-1.96*as.numeric(as.vector(std2018[['V4']][594:658]))))

ret17=data.frame(cbind(c(1954:2017),YFS2017$SSB[,2],YFS2017$SSB[,2]+1.96*as.numeric(as.vector(std2017[['V4']][587:650])),YFS2017$SSB[,2]-1.96*as.numeric(as.vector(std2017[['V4']][587:650]))))

ret16=data.frame(cbind(c(1954:2016),YFS2016$SSB[,2],YFS2016$SSB[,2]+1.96*as.numeric(as.vector(std2016[['V4']][580:642])),YFS2016$SSB[,2]-1.96*as.numeric(as.vector(std2016[['V4']][580:642]))))

ret15=data.frame(cbind(c(1954:2015),YFS2015$SSB[,2],YFS2015$SSB[,2]+1.96*as.numeric(as.vector(std2015[['V4']][573:634])),YFS2015$SSB[,2]-1.96*as.numeric(as.vector(std2015[['V4']][573:634]))))

ret14=data.frame(cbind(c(1954:2014),YFS2014$SSB[,2],YFS2014$SSB[,2]+1.96*as.numeric(as.vector(std2014[['V4']][566:626])),YFS2014$SSB[,2]-1.96*as.numeric(as.vector(std2014[['V4']][566:626]))))

ret13=data.frame(cbind(c(1954:2013),YFS2013$SSB[,2],YFS2013$SSB[,2]+1.96*as.numeric(as.vector(std2013[['V4']][559:618])),YFS2013$SSB[,2]-1.96*as.numeric(as.vector(std2013[['V4']][559:618]))))

ret12=data.frame(cbind(c(1954:2012),YFS2012$SSB[,2],YFS2012$SSB[,2]+1.96*as.numeric(as.vector(std2012[['V4']][552:610])),YFS2012$SSB[,2]-1.96*as.numeric(as.vector(std2012[['V4']][552:610]))))


retro22_0=as.data.frame(rbind(ret12,ret13,ret14,ret15,ret16,ret17,ret18,ret19,ret20,ret21,ret22))
colnames(retro22_0)=c("Year","Female_Spawning_Biomass","UI","LI")

year=rev(c(rep("2022",length(c(1954:2022))),rep("2021",length(c(1954:2021))),rep("2020",length(c(1954:2020))),rep("2019",length(c(1954:2019))),rep("2018",length(c(1954:2018))),rep("2017",length(c(1954:2017))),rep("2016",length(c(1954:2016))),rep("2015",length(c(1954:2015))),rep("2014",length(c(1954:2014))),rep("2013",length(c(1954:2013))),rep("2012",length(c(1954:2012)))))
retro=cbind(retro22_0,year)


theme_set(theme_gray(base_size = 18))
p22_0=ggplot(data=retro,aes(x=Year,y=Female_Spawning_Biomass,ymin=LI,ymax=UI))+geom_ribbon(aes(fill=year),alpha=0.3)+geom_line(aes(color=year))+ylab("Female Spawning Biomass (t)")+theme_bw()+theme(legend.text=element_text(size=11),axis.text=element_text(size=9),axis.title.x = element_text(size=14),axis.title.y = element_text(size=14))+scale_x_continuous(breaks = seq(1954,2022, by = 2))+theme(axis.text.x=element_text(angle=90,hjust=-1))

p22_0+ggtitle("Model 22.0")

#Save as retro22_0a.pdf

#Now do the difference plot because it uses the same objects.

Years=c(c(1954:2021),c(1954:2020),c(1954:2019),c(1954:2018),c(1954:2017),c(1954:2016),c(1954:2015),c(1954:2014),c(1954:2013),c(1954:2012))


Difference=c(-1*(YFS2022$SSB[1:68,2]-YFS2021r$SSB[,2])/YFS2022$SSB[1:68,2],-1*(YFS2022$SSB[1:67,2]-YFS2020$SSB[,2])/YFS2022$SSB[1:67,2],-1*(YFS2022$SSB[1:66,2]-YFS2019$SSB[,2])/YFS2022$SSB[1:66,2],-1*(YFS2022$SSB[1:65,2]-YFS2018$SSB[,2])/YFS2022$SSB[1:65,2],-1*(YFS2022$SSB[1:64,2]-YFS2017$SSB[,2])/YFS2022$SSB[1:64,2],-1*(YFS2022$SSB[1:63,2]-YFS2016$SSB[,2])/YFS2022$SSB[1:63,2],-1*(YFS2022$SSB[1:62,2]-YFS2015$SSB[,2])/YFS2022$SSB[1:62,2],-1*(YFS2022$SSB[1:61,2]-YFS2014$SSB[,2])/YFS2022$SSB[1:61,2],-1*(YFS2022$SSB[1:60,2]-YFS2013$SSB[,2])/YFS2022$SSB[1:60,2],-1*(YFS2022$SSB[1:59,2]-YFS2012$SSB[,2])/YFS2022$SSB[1:59,2]) 

Year=(c(rep("2021",length(c(1954:2021))),rep("2020",length(c(1954:2020))),rep("2019",length(c(1954:2019))),rep("2018",length(c(1954:2018))),rep("2017",length(c(1954:2017))),rep("2016",length(c(1954:2016))),rep("2015",length(c(1954:2015))),rep("2014",length(c(1954:2014))),rep("2013",length(c(1954:2013))),rep("2012",length(c(1954:2012)))))

dat=data.frame(Years,Difference,Year)

p22_0=ggplot(data=dat,aes(x=Years,y=Difference,type=Year))+geom_line(aes(x=Years,y=Difference, color=Year))+ylab("Difference")+xlab("Year")+theme_bw()+theme(legend.text=element_text(size=11),axis.text=element_text(size=9),axis.title.x = element_text(size=14),axis.title.y = element_text(size=14))+scale_x_continuous(breaks = seq(1954,2022, by = 2))+theme(axis.text.x=element_text(angle=90,hjust=-1))

p22_0+ggtitle("Model 22.0")

#Save as retro22_0b.pdf

p22_0_sm=ggplot(data=dat[which(dat$Years>1974),],aes(x=Years,y=Difference,type=Year))+geom_line(aes(x=Years,y=Difference, color=Year))+ylab("Difference")+xlab("Year")+theme_bw()+theme(legend.text=element_text(size=11),axis.text=element_text(size=9),axis.title.x = element_text(size=14),axis.title.y = element_text(size=14))+scale_x_continuous(breaks = seq(1974,2022, by = 2))+theme(axis.text.x=element_text(angle=90,hjust=-1))

p22_0_sm+ggtitle("Model 22.0")

#Save as retro22_0b_sm.pdf
