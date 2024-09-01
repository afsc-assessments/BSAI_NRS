#BSAI NRS partial assessment plots

library(dplyr)
library(tidyverse)
library(ggthemes)
species <-"BSAI_NRS"
in_dir <-file.path("C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2023",species)

#survey biomass prior to 1996 is unidentified rock sole.
#Survey biomass with confidence intervals plot:
biomass.df<-read.csv(file.path(in_dir,"ebs_shelf_standard_nrs_full_survey_biomass.csv"))

biomass.df<-biomass.df %>% mutate(cv = sqrt(Variance)/Biomass) %>%
             mutate(sd_log_pred = sqrt(log(cv^2+1))) %>%
             mutate(lci = exp(log(Biomass)-qnorm(0.975)*sd_log_pred), uci = exp(log(Biomass)+qnorm(0.975)*sd_log_pred))

p<-biomass.df %>% ggplot(aes(Year,Biomass)) + 
                  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "cadetblue", alpha = 0.3) +
                  geom_line(color = "black",linewidth = 1) +
                  theme_classic() +
                  theme(axis.text.x=element_text(size=12), axis.text.y = element_text(size=12))

ggsave(filename = file.path(in_dir,"survey_biomass_plot.png"),p)

#Catch to biomass ratio plot (uses total biomass age 6+ from the assessment and the projections)
#Borrowing code from file.path(codedir,"CallPlots.R") to read in data from a run.
rundir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2023/BSAI_NRS/Partial_Run_2023/"
codedir<-"C:/GitProjects/BSAI_NRS/R"
.OVERLAY <-TRUE
.THEME<- theme_few()
maxage = 20
endyr = 2022
LastProjYr = 2025

#source(file.path(codedir,"prelims.R")) #libraries and ggthemes - may need some refining
source(file.path(codedir,"read-admb.R"))
setwd(rundir)
M2023<-read_admb("c1mod4_alldata_proj2023/fm")

hist_results.df<-data.frame(Year = M2023$TotBiom[,1],
                            TotBiom = M2023$TotBiom[,2],
                            Catches = M2023$Obs_catch) %>%
                mutate(Ratio = Catches/TotBiom,Type = "Historical") %>%
                select(c(Year,Ratio,Type))
#Grab the total biomass from projection years
proj_results.df<-read.table(file.path("c1mod4_alldata_proj2023","ABC_OFL.rep"),header = TRUE) %>%
                  select(c(Year,GM_Biom,Catch_Assump,ABC_HM)) %>%
                  rename(TotBiom = GM_Biom,Catches = Catch_Assump) %>%
                  mutate(Ratio = Catches/TotBiom, Type = "Projection") %>%
                  select(c(Year,Ratio,Type))
all.df<-rbind(hist_results.df,proj_results.df) %>% 
        filter(Year<=LastProjYr) %>%
        group_by(Type)

p2<-all.df %>% ggplot(aes(x = Year,y = Ratio,group = Type, color = Type)) + 
    geom_line(linewidth = 1.3) + theme_classic() +
    scale_color_manual(values = c("black","cadetblue")) + ylim(c(0,0.2)) +
    theme(axis.text.x=element_text(size=12), axis.text.y = element_text(size=12))

ggsave(filename = file.path(in_dir,"catch_biomass_ratio.png"),p2)