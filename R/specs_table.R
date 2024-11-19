# read in data in format that I made from un-useable AKRO table
specsdir<-"C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2024\\bsai_nrs\\data\\fishery"

specs_t<-read.csv(file.path(specsdir,"bsai_nrs_historical_specs.csv")) %>%
         pivot_wider(names_from = specs,values_from = value) %>%
         select(-c(iTAC,CDQ)) %>% arrange(year)

write.csv(noquote(specs_t),file = file.path(specsdir,"specs_table_formatted.csv"))

