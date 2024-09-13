library(TMB)
library(compResidual)
# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
# remotes::install_github("fishfollower/compResidual/compResidual", INSTALL_opts=c("--no-multiarch"), force=TRUE)


get_osas<-function(the_models,the_dir,max_age,type = "survey") {

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
  
  
  new_obs_f<-matrix(nrow = nrow(obs_f),ncol = ncol(obs_f))
  new_obs_m<-matrix(nrow = nrow(obs_m),ncol = ncol(obs_m))
  #new_eac_f<-matrix(nrow = nrow(eac_f),ncol = ncol(eac_f))
  #new_eac_m<-matrix(nrow = nrow(eac_m),ncol = ncol(eac_m))
  for (i in 1:length(nsamp_vec)) {
    new_obs_f[i,]<- round(nsamp_vec[i]*obs_f[i,])
    new_obs_m[i,]<-round(nsamp_vec[i]*obs_m[i,])
    # new_eac_f[i,]<-eac_f[i,]
    # new_eac_m[i,]<-eac_m[i,]
  }
  colnames(new_obs_f)<-paste0("obsF",seq(from = 1, to = max_age, by= 1))
  colnames(new_obs_m)<-paste0("obsM",seq(from = 1, to = max_age, by= 1))
  
  
  new_obs_f<-cbind(new_obs_f[,-ind],new_obs_f[,ind])
  new_obs_m<-cbind(new_obs_m[,-ind],new_obs_m[,ind])
  
  eac_f<-cbind(eac_f[,-ind],eac_f[,ind])
  eac_m<-cbind(eac_m[,-ind],eac_m[,ind])
  
  res_f<-resMulti(t(new_obs_f),t(eac_f))
  colnames(res_f) <- the_years
  
  res_m<-resMulti(t(new_obs_m),t(eac_m))
  colnames(res_m) <- the_years
  
  png(file.path(the_dir[[tt]],"plots",paste0("osa_fem_",type,".png")))
  plot(res_f)
  dev.off()
  
  png(file.path(the_dir[[tt]],"plots",paste0("osa_male_",type,".png")))
  plot(res_m)
  dev.off()
  
  

  pearson <- as.vector(nsamp_vec)*(new_obs_f-eac_f)/sqrt(eac_f*as.vector(nsamp_vec))
  plot_osa_comps(new_obs_f,eac_f, pearson, index=1:max_age, years=the_years, index_label='age', Neff=as.vector(nsamp_vec),
                 stock='bsai_nrs', survey=type, outpath=file.path('C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2024/bsai_nrs/runs',the_dir[[tt]],"plots"))
  

  
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
