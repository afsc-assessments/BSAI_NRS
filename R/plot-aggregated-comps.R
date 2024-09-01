library(dplyr)
library(ggplot2)
library(tidyr)


plot_agg_comps<-function(the_runs,max_age) {
#make a long dataset of all the runs and mulitply by the adjusted sample size
max_age<-20

long_eac<-list()
long_oac<-list()
big_eac_t<-tibble()
big_oac_t<-tibble()
 for (i in 1:length(the_runs)) {
  #reformat fishery expected comps
  nsamp<-the_runs[[i]]$nsmpl_fsh_s[,1]

  fem_eac<-the_runs[[i]]$eac_fsh_s[,1:max_age]
  male_eac<-the_runs[[i]]$eac_fsh_s[,(1+max_age):(max_age+max_age)]

  colnames(fem_eac)<-seq(from = 1,to = max_age,by = 1)
  rownames(fem_eac)<-the_runs[[i]]$yrs_fsh_age_s

  colnames(male_eac)<-seq(from = 1,to = max_age,by = 1)
  rownames(male_eac)<-the_runs[[i]]$yrs_fsh_age_s

  fem_t<-as.data.frame(fem_eac) %>% 
       mutate(year= rownames(fem_eac),nadj = nsamp,sex="F") 

  long_fem<-fem_t %>% pivot_longer(1:max_age,names_to = "age",values_to = "expected")

  male_t<-as.data.frame(male_eac) %>%
        mutate(year = rownames(male_eac),nadj = nsamp,sex = "M")

  long_male<-male_t %>% pivot_longer(1:max_age,names_to = "age",values_to = "expected")

  long_eac_fsh<-long_fem %>% bind_rows(long_male) %>% 
                       mutate(type = "Fishery",model = names(the_runs[i]))
  
  #reformat fishery observed comps
  #nsamp<-the_runs[[i]]$nsmpl_fsh_s[,1]
  
  fem_oac<-the_runs[[i]]$oac_fsh_s[,1:max_age]
  male_oac<-the_runs[[i]]$oac_fsh_s[,(1+max_age):(max_age+max_age)]
  
  colnames(fem_oac)<-seq(from = 1,to = max_age,by = 1)
  rownames(fem_oac)<-the_runs[[i]]$yrs_fsh_age_s
  
  colnames(male_oac)<-seq(from = 1,to = max_age,by = 1)
  rownames(male_oac)<-the_runs[[i]]$yrs_fsh_age_s
  
  fem_t<-as.data.frame(fem_oac) %>% 
    mutate(year= rownames(fem_oac),nadj = nsamp,sex="F") 
  
  long_fem<-fem_t %>% pivot_longer(1:max_age,names_to = "age",values_to = "observed")
  
  male_t<-as.data.frame(male_oac) %>%
    mutate(year = rownames(male_oac),nadj = nsamp,sex = "M")
  
  long_male<-male_t %>% pivot_longer(1:max_age,names_to = "age",values_to = "observed")
  
  long_oac_fsh<-long_fem %>% bind_rows(long_male) %>% 
    mutate(type = "Fishery",model = names(the_runs[i]))
  
  #survey comp fits (process the same way)
  nsamp<-the_runs[[i]]$nsmpl_srv_s[,1]
  
  fem_eac<-the_runs[[i]]$eac_srv_s[,1:max_age]
  male_eac<-the_runs[[i]]$eac_srv_s[,(1+max_age):(max_age+max_age)]
  
  colnames(fem_eac)<-seq(from = 1,to = max_age,by = 1)
  rownames(fem_eac)<-the_runs[[i]]$yrs_srv_age_s
  
  colnames(male_eac)<-seq(from = 1,to = max_age,by = 1)
  rownames(male_eac)<-the_runs[[i]]$yrs_srv_age_s
  
  fem_t<-as.data.frame(fem_eac) %>% 
    mutate(year= rownames(fem_eac),nadj = nsamp,sex="F") 
  
  long_fem<-fem_t %>% pivot_longer(1:max_age,names_to = "age",values_to = "expected")
  
  male_t<-as.data.frame(male_eac) %>%
    mutate(year = rownames(male_eac),nadj = nsamp,sex = "M")
  
  long_male<-male_t %>% pivot_longer(1:max_age,names_to = "age",values_to = "expected")
  
  long_eac_srv<-long_fem %>% bind_rows(long_male) %>% 
    mutate(type = "Survey",model = names(the_runs[i]))
  
  #Survey comp data
  nsamp<-the_runs[[i]]$nsmpl_srv_s[,1]
  
  fem_oac<-the_runs[[i]]$oac_srv_s[,1:max_age]
  male_oac<-the_runs[[i]]$oac_srv_s[,(1+max_age):(max_age+max_age)]
  
  colnames(fem_oac)<-seq(from = 1,to = max_age,by = 1)
  rownames(fem_oac)<-the_runs[[i]]$yrs_srv_age_s
  
  colnames(male_oac)<-seq(from = 1,to = max_age,by = 1)
  rownames(male_oac)<-the_runs[[i]]$yrs_srv_age_s
  
  fem_t<-as.data.frame(fem_oac) %>% 
    mutate(year= rownames(fem_oac),nadj = nsamp,sex="F") 
  
  long_fem<-fem_t %>% pivot_longer(1:max_age,names_to = "age",values_to = "observed")
  
  male_t<-as.data.frame(male_oac) %>%
    mutate(year = rownames(male_oac),nadj = nsamp,sex = "M")
  
  long_male<-male_t %>% pivot_longer(1:max_age,names_to = "age",values_to = "observed")
  
  long_oac_srv<-long_fem %>% bind_rows(long_male) %>% 
    mutate(type = "Survey",model = names(the_runs[i]))

 #join datasets:
#  long_srv<-long_eac_srv %>% full_join(long_oac_srv)
#  long_fsh<-long_eac_fsh %>% full_join(long_eac_fsh)
  
  long_eac[[i]]<-long_eac_fsh %>% bind_rows(long_eac_srv)
  long_oac[[i]]<-long_oac_fsh %>% bind_rows(long_oac_srv)
  if (i==1) {
    big_eac_t<-long_eac[[i]]
    big_oac_t<-long_oac[[i]]
  } else {
    big_eac_t<-big_eac_t %>% bind_rows(long_eac[[i]])
    big_oac_t<-big_oac_t %>% bind_rows(long_oac[[i]])
  }
 }
  
  #for obs and exp: multiply by nadj and make proportions
big_oac_t<-big_oac_t %>% mutate(obs_adj = observed*nadj)
big_eac_t<-big_eac_t %>% mutate(exp_adj = expected*nadj)

  tot_oac_t<-big_oac_t %>% group_by(type,sex,model) %>%
    summarize(tot=sum(obs_adj))
  
  tot_eac_t<-big_eac_t %>% group_by(type,sex,model) %>%
    summarize(tot = sum(exp_adj))
  
  prop_oac_t<-big_oac_t %>% group_by(type,sex,model,age) %>%
    summarize(sum_obs_adj=sum(obs_adj)) %>%
    left_join(tot_oac_t) %>%
    mutate(prop = sum_obs_adj/tot) %>%
    select(c(type,sex,model,age,prop))
  
  prop_eac_t<-big_eac_t %>% group_by(type,sex,model,age) %>%
    summarize(sum_exp_adj = sum(exp_adj)) %>%
    left_join(tot_eac_t) %>%
    mutate(prop = sum_exp_adj/tot) %>%
    select(c(type,sex,model,age,prop))
  
  check<-prop_oac_t %>% group_by(type,sex,model) %>% 
         mutate(check_prop=sum(prop))

  check2<-prop_eac_t %>% group_by(type,sex,model) %>% 
    mutate(check_prop=sum(prop))  
  
  
# need some separate datasets by run for these quirky plots
  prop_oac_by_run<-list()
  for (i in 1:length(the_runs)) {
    prop_oac_by_run[[i]] <- prop_oac_t %>% filter(model==names(the_runs)[i])
  }




combo <- ggplot(big_eac_t) +
  geom_col(data = prop_oac_by_run[[1]],aes(x =  as.numeric(age), y =  prop),fill = "#FDE725FF",  alpha = 0.6) +
  geom_col(data = prop_oac_by_run[[2]],aes(x =  as.numeric(age), y =  prop),fill = "#7AD151FF",  alpha = 0.6) +
  geom_col(data = prop_oac_by_run[[3]],aes(x =  as.numeric(age), y =  prop),fill = "#22A384FF",  alpha = 0.6) +
  geom_col(data = prop_oac_by_run[[4]],aes(x =  as.numeric(age), y =  prop), fill = "#2A788EFF", alpha = 0.6) +
  geom_col(data = prop_oac_by_run[[5]],aes(x =  as.numeric(age), y =  prop), fill = "#440154FF", alpha = 0.6) +
  
    #    geom_bar(aes(x =  Bin, y =  Obs,color =  mname),stat='identity', alpha = 0.4) +
  facet_grid(sex~type) +
  geom_line(data = prop_eac_t,aes(x = as.numeric(age), y =  prop, color =  model)) +
  scale_color_manual(values=c('#FDE725FF','#7AD151FF','#22A384FF','#2A788EFF','#440154FF')) +
  labs(x = "Age", y = "Proportion",color = "Model") +
    facet_grid(sex~type)


return(combo)
}