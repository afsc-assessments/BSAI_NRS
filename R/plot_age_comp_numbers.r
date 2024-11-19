#get the age comp in plain numbers
mydir<-"C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2024\\bsai_nrs\\data\\output"
ages<-read.csv(file = file.path(mydir,"answers_long_survey_age_comp.csv"))

ages$Age.Pop %>% replace_na(0)
ages_t<-ages %>% select(c(Year,Stratum,Sex,Age,Age.Pop)) %>% 
         filter(Age >0) %>%
         group_by(Year,Sex,Age) %>%
         summarise(tot_age = sum(Age.Pop)) %>%
         pivot_wider(names_from = c(Sex,Age),values_from = tot_age) 