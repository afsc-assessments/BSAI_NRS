#Compare new data to that from the last assessment:
#easiest to get the data into the assessment, all formatted
#Read in everything from the last assessment:
codedir<-"C:/GitProjects/BSAI_NRS/R"
source(file.path(codedir,"read-admb.R"))
M2022<-read_admb("c1mod4_alldata_2022/fm")

#Compare survey age comps
previous_age_comps<-M2022$oac_srv_s



#Compare survey weight-at-age




#Compare catches



#Compare fishery ages from Sampler



#Compare weight-at-ge from Sampler