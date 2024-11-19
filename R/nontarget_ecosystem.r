eco.df<-read.csv(file = "C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2024\\bsai_nrs\\data\\nontarget_ecosystem.csv")

eco_long<-eco.df %>% replace(is.na(.), 0) %>%
  pivot_longer(cols = 3:ncol(eco.df),names_to = "species",values_to = "values") %>%
  pivot_wider(names_from= Type, values_from = values) %>% 
  rename("conf" = 'Conf Flag')

names(eco_long)<-c("year", "species", "conf", "mt", "count")

eco<-eco_long %>% mutate(mt = if_else(conf == 1, NA, mt),count = if_else(conf==1,NA,count)) %>%
  select(-c(conf,count)) %>%
  pivot_wider(names_from = year,values_from = mt)

write.csv(eco,"C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2024\\bsai_nrs\\writeup\\tables\\nontarget_ecosystem_table.csv")