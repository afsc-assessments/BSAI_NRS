#format age comps from afscISS program for BSAI NRS (which is as for ss3 for the main data columns)
#Can basically copy this for formatting length comps from afscISS
library(dplyr)
fmt_age_comps<-function(dat,max_age,iss) {

  #make an independent expanded grid and then merge it with dat before adding 100 for males.
  sex<-c(1,2)
  age<-seq(from = 1, to = max_age, by = 1)
  year<-seq(from = 1982,to = 2022)
  stuff<-expand_grid(year,sex,age)
  
  dat<-dat %>% select(-c(species_code,sex_c,q2_5th,q97_5th)) %>% 
               full_join(stuff) %>% 
               replace(is.na(.), 0)
  
  #add 100 to the male comps to make them go to the right and in order when everything goes wide
  male_comps<-dat %>% filter(sex==1) %>% 
                      mutate(age = age+100)
  
  comps<-dat %>% filter(sex==2) %>% 
                 bind_rows(male_comps) %>% 
                 select(-c(sex)) %>%
                 group_by(year,age) %>%
                 arrange(.by_group = TRUE) %>%
                 pivot_wider(names_from = age,values_from = prop)
  
return(comps)
}