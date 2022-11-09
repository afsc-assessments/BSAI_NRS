# Fishing mortality

plotFs<-function(modlst,thismod =1,thismodname) {
  #needs work to generalize ages for these plots across species
  #library(patchwork) # this does p1/p2 to plot one above another.
M<-modlst[[thismod]]
df <-rbind(data.frame(Year=M$SSB[,1],M$F_m,sex="Male"), data.frame(Year=M$SSB[,1],M$F_f,sex="Female") )
names(df) <- c("Year",1:20,"sex"); df.g <- gather(df,age,F,2:21,-Year)
#matrix of Fs -----------------------------------
p1 <- df.g %>% mutate(age=as.numeric(age)) %>% filter(age<26,age>4)%>% ggplot(aes(y=age,x=Year,fill=F)) + geom_tile() + 
  ylab("Age")+ geom_contour(aes(z=F),color="darkgrey",size=.5,alpha=.4) + theme_few() +
  scale_fill_gradient(low = "white", high = "red") + scale_x_continuous(breaks=seq(min(M$Yr),max(M$Yr),5)) + 
  scale_y_continuous(breaks=seq(5,20,5)) +facet_grid(sex~.);

#mean of Fs -------------------------------------
p2 <- df.g %>% filter(as.numeric(age)>9) %>% group_by(Year,sex) %>% summarise(Apical_F=max(F),Mean=mean(F)) %>%
  ggplot(aes(x=Year,y=Mean,color=sex)) + geom_line(size=1) + theme_few() #+ ggtitle("Age 10-20 Mean F")
p1/p2
p3 <-p1/p2
return(p3)

# R packages outside of tidyverse
# include "patchwork" (best layout pkg ever)
# and ggthemes
# guess you never looked at my main script.
# nrs_20.R
# only 200 lines
}