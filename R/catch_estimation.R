# rex harvest projections 2024
library(dplyr)
library(spmR)
library(tidyr)
library(ggplot2)
library(stringr)

# #read in weekly data:
# SELECT
# council.comprehensive_blend_ca.week_end_date,
# council.comprehensive_blend_ca.retained_or_discarded,
# council.comprehensive_blend_ca.weight_posted,
# council.comprehensive_blend_ca.year,
# council.comprehensive_blend_ca.fmp_area,
# council.comprehensive_blend_ca.agency_species_code,
# council.comprehensive_blend_ca.species_group_code,
# council.comprehensive_blend_ca.fmp_subarea,
# council.comprehensive_blend_ca.species_name,
# council.comprehensive_blend_ca.species_group_name
# FROM
# council.comprehensive_blend_ca
# WHERE
# council.comprehensive_blend_ca.fmp_area = 'BSAI'
# AND council.comprehensive_blend_ca.species_group_code = 'RSOL'

the_dir<-"C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2024\\bsai_nrs\\data\\fishery"
run_dir<-"C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2024\\bsai_nrs\\"
endyr<-2024
the_data<-read.csv(file.path(the_dir,"bsai_nrs_catches_oct1_2024.csv"),header = TRUE) %>%
  rename_with(tolower) %>%
  mutate(day = substring(week_end_date,1,2),month = substring(week_end_date,4,6))

the_months<-tibble(month_num = seq(from = 1, to = 12, by = 1),month = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))

the_data<-left_join(the_data,the_months)
#find the latest data (this needs to be generalized to work every year)
late<-the_data %>% filter(year == endyr) %>%
         mutate(end_month = max(as.numeric(month_num))) %>%
         filter(month_num==end_month) %>%
         mutate(end_date= max(as.numeric(day))) %>%
         filter(day==end_date) %>%
         distinct(day,month_num)

end_day<-as.numeric(unique(late$day))
end_month<-as.numeric(unique(late$month_num))

endyr_dat<-the_data %>% filter(month_num>= end_month) %>%
  select(c(year,weight_posted)) %>%
  group_by(year) %>%
  summarize(endyr_weight = sum(weight_posted))


past_dat<-the_data %>% select(c(year,weight_posted)) %>%
  group_by(year) %>%
  summarize(tot_weight = sum(weight_posted)) %>%
  left_join(endyr_dat) %>%
  mutate(prop_endyr = endyr_weight/tot_weight) %>%
  filter(!is.na(prop_endyr))

mean_prop<-mean(past_dat$prop_endyr)


est_catch<-the_data %>% group_by(year) %>%
         summarize(tot_weight = sum(weight_posted)) %>%
         filter(year == endyr) %>%
         mutate(est_weight = tot_weight/(1-mean_prop))

write.csv(past_dat,file.path(the_dir,"past_catch.csv"))
write.csv(est_catch,file.path(the_dir,"current_estimated_catch.csv"))

#Find average complete catches for the last 5 years
avg_catches<-past_dat %>% filter(year>=endyr-10) %>% ungroup() %>%
  summarize(avg_catches = mean(tot_weight))

write.csv(avg_catches,file = file.path(the_dir,"ten_yr_avg_catch.csv"))

#enter estimated catches to tier 3 projection file
exec<-make_exec_table(run_dir,endyr=2024,the_scalar=1000)





