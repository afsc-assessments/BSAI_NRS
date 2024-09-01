# open up fishery age specimens and grab# of hauls:
in_dir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2024/bsai_nrs/data/fishery"

load(file.path(in_dir,"BigFisheryAges.Rdata"))

ages_t<-Ages.df %>% drop_na(AGE,LENGTH) %>%
                    filter(AGE >0) %>% 
                    group_by(YEAR)

hauls_with_ages<-ages_t %>% summarise(num_hauls = n_distinct(HAUL_JOIN))

#problem is that this does not include pre-1990 fishery ages.
