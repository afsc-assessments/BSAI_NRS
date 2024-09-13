library(TMB)
library(compResidual)
# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
# remotes::install_github("fishfollower/compResidual/compResidual", INSTALL_opts=c("--no-multiarch"), force=TRUE)


get_osas_2<-function(the_models,the_dir,max_age,type = "survey") {

for (tt in 1:length(the_models)) {
  #Instead: adjusting plot_osa_comps within bsai_nrs repo
 # source("C:\\GitProjects\\osa_resid\\R\\plot_osa_comps.R")
  
  if (type == "survey") {
    nsamp_vec<-t(the_models[[tt]]$nsmpl_srv_s)
    the_years<-the_models[[tt]]$yrs_srv_age_s
  
    obs_f<-the_models[[tt]]$oac_srv_s[,1:max_age]
    obs_m<-the_models[[tt]]$oac_srv_s[,(max_age+1):(max_age*2)]
    eac_f<-the_models[[tt]]$eac_srv_s[,1:max_age]
    eac_m<-the_models[[tt]]$eac_srv_s[,(max_age+1):(max_age*2)]
  } else {
    nsamp_vec<-the_models[[tt]]$nsmpl_fsh_s
    the_years<-the_models[[tt]]$yrs_fsh_age_s

    obs_f<-the_models[[tt]]$oac_fsh_s[,1:max_age]
    obs_m<-the_models[[tt]]$oac_fsh_s[,(max_age+1):(max_age*2)]
    eac_f<-the_models[[tt]]$eac_fsh_s[,1:max_age]
    eac_m<-the_models[[tt]]$eac_fsh_s[,(max_age+1):(max_age*2)]
  }
  
  
#  new_obs_f<-matrix(nrow = nrow(obs_f),ncol = ncol(obs_f))
#  new_obs_m<-matrix(nrow = nrow(obs_m),ncol = ncol(obs_m))
  #new_eac_f<-matrix(nrow = nrow(eac_f),ncol = ncol(eac_f))
  #new_eac_m<-matrix(nrow = nrow(eac_m),ncol = ncol(eac_m))
  # for (i in 1:length(nsamp_vec)) {
  #   new_obs_f[i,]<- round(nsamp_vec[i]*obs_f[i,])
  #   new_obs_m[i,]<-round(nsamp_vec[i]*obs_m[i,])
  # }
  # colnames(new_obs_f)<-paste0("obsF",seq(from = 1, to = max_age, by= 1))
  # colnames(new_obs_m)<-paste0("obsM",seq(from = 1, to = max_age, by= 1))
  colnames(obs_f)<-paste0("obsF",seq(from = 1, to = max_age, by= 1))
  colnames(obs_m)<-paste0("obsM",seq(from = 1, to = max_age, by= 1))
  
  colnames(exp_f)<-paste0("expF",seq(from = 1, to = max_age, by= 1))
  colnames(exp_m)<-paste0("expM",seq(from = 1, to = max_age, by= 1))
  

  pearson_f <- as.vector(nsamp_vec)*(obs_f-eac_f)/sqrt(eac_f*as.vector(nsamp_vec))
  plot_osa_comps(obs_f,eac_f, pearson_f, drop_index=1, index=1:max_age, years=the_years, index_label='age', Neff=as.vector(nsamp_vec),
                 stock='bsai_nrs', survey=type,sex= "female", outpath=file.path('C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2024/bsai_nrs/runs',the_dir[[tt]],"plots"))

  pearson_m <- as.vector(nsamp_vec)*(obs_m-eac_m)/sqrt(eac_m*as.vector(nsamp_vec))
  plot_osa_comps(obs_m,eac_m, pearson_m, drop_index=1, index=1:max_age, years=the_years, index_label='age', Neff=as.vector(nsamp_vec),
                 stock='bsai_nrs', survey=type,sex= "male", outpath=file.path('C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2024/bsai_nrs/runs',the_dir[[tt]],"plots"))
  
  
}

}












# #Haddock example:
# #try the haddock example from the repo to see how it works
# load("C:\\GitProjects\\compResidual\\GOMhaddock.Rdata")
# head(GOMhaddock)
# 
# ## extract observations fleet 1:
# obs<-GOMhaddock[GOMhaddock$Fleet==1, grep("^obsP",colnames(GOMhaddock))]
# 
# ## multiply by effective sample size and round:
# obs<-round(obs*GOMhaddock[GOMhaddock$Fleet==1, "ESS"])
# 
# ## extract predictions fleet 1:
# pred<-GOMhaddock[GOMhaddock$Fleet==1, grep("^predP",colnames(GOMhaddock))] 
# 
# ## load library:
# library(compResidual) 
# set.seed(123)
# ## calculate residuals:
# res<-resMulti(t(obs), t(pred)) 
# ## Add names to sample number for plotting:
# colnames(res) <- GOMhaddock[GOMhaddock$Fleet==1,"Year"]
# ## plot diagnostics:
# plot(res) 
# 
