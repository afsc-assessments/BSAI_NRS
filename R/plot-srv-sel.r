plot_srv_sel <- function(theM,themod, title="Survey selectivity",bysex=TRUE,maxage = 20){

  M <- theM[[themod]]; names(M[]) #to see the names of what's in an object
  df <- rbind(data.frame(Age=1:maxage,sel=M$sel_srv_m,sex="Male"),data.frame(Age=1:maxage,sel=M$sel_srv_f,sex="Female"))
base_plot<- ggplot(df,aes(x=Age,y=sel,color=sex)) + geom_line(linewidth=2) + theme_few() + ylab("Selectivity") + ggtitle(paste0("Survey selectivity; (Model ",names(theM[themod]),")"))
  
 n <- length(theM)
   mdf <- NULL
   for (i in 1:n)
   {
       A   <- theM[[i]]
       #length(A$sel_srv_f)
       #length(A$sel_srv_m)
       mdf <- rbind(mdf, data.frame(Model= names(theM)[i],sex="males",  selectivity=A$sel_srv_m,age=1:length(A$sel_srv_m)))
       mdf <- rbind(mdf, data.frame(Model= names(theM)[i],sex="females",selectivity=A$sel_srv_f,age=1:length(A$sel_srv_f)))
   }
   names(mdf) <- c("Model","sex","selectivity","age")
# 

the_plot<-ggplot(mdf,aes(x = age,y = selectivity,color = Model)) + 
    geom_line(linewidth =2) + theme_few() + ylab("Selectivity") + facet_wrap(~sex)

if (bysex) { 
plot2<-ggplot(mdf,aes(x = age,y = selectivity,color = sex)) + 
    geom_line(linewidth =2) + theme_few() + ylab("Selectivity") + facet_wrap(~Model)
}

the_plots<-list()
the_plots$single_model<-base_plot
the_plots$compare_models<-the_plot
the_plots$all_models<-plot2
return(the_plots)
}

