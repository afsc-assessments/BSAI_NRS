###########1. Required packages ##########
if(!require("devtools")) install.packages("devtools"); library(devtools)
if(!require("dplyr")) install.packages("dplyr"); library(dplyr)
library(ggplot2)
library(ggthemes)
# Set your local directory--------
	mydir <- "/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs"  #change to mine.
# Get tools to read-write control file
#	set up a file in mydir called "retro" and put a folder in there called "orig" with the basics of your favorit model, mod.ctl, fm.dat, makefile,...
source(paste0(mydir,"/../../R/read-admb.R"))
master <-(paste0(mydir,"/retro22_0"))

assess_LY=2022					#assessment terminal year
endyrvec <-1:10		    #retrospective peels (Subtracted from endyr in tpl)

# Values used to downweight survey data when invoked written to retro.dat
#Specify values here
CV_inc=100	#CPUE CV used to downweight 
comp_n=.001	#input sample size  for length and age composition data used to downweight

do_run <- TRUE  #very important to make the runs go!
df_res <- NULL
for(i in 0:length(endyrvec)){
	# Start in "retro" directory (runs/retro)
	  setwd(master)
	# get new run location and copy files from orig
	  rundir <- 	paste0(master,"/retro_",i )
	  fc <- paste0("mkdir -p ",rundir," ; cp orig/* ",rundir)
	  system(fc)
	# Go to that directory, update mod.ctl to this model's configuration
	  setwd(rundir)
	  ctl <- read_ctl("mod.ctl")
	  ctl$`#n_retro`     <- i
	  #ctl$surv_dwnwt  <- j-1
	  write_dat(ctl,"mod.ctl")
	# run model
	  if (do_run) system("rm fm.std; make") 
	# Save results
	  dftmp        <- data.frame(read.table("ABC_OFL.rep",header=TRUE))
	  dftmp$peel   <- -i
	  df_res       <- rbind(df_res,dftmp)
}	

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
