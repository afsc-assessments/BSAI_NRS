#Francis data weighting
#Authors: Chris Francis, Ingrid Spies, Carey McGilliard
#Eqn TA1.8

#Model-based age composition data were weighted using the methodology of Francis (2011). Specifically stage 2 weighting incorporated stage 1 legacy weights (200) using Equation TA1.8 of Francis (2011).
#example
#srvwts<-francis(repfile=M2022,minage=1,maxage=20,nsexes=2,datatype = "srv")
#fshwts<-francis(repfile=M2022,minage=1,maxage=20,nsexes=2,datatype = "fsh")

francis<-function(repfile=M2022,minage=1,maxage=20,nsexes=2,datatype = "srv") {
  if (datatype == "srv") {
    obs <- repfile$oac_srv_s
    expect <- repfile$eac_srv_s
    nsamp <- repfile$nsmpl_srv_s
  } else if (datatype == "fsh") {
    obs <- repfile$oac_fsh_s 
    expect <- repfile$eac_fsh_s
    nsamp <- repfile$nsmpl_fsh_s
  } else {
    print("datatype must be 'srv' or 'fsh'")
  }

  obar_jy_fm=vector()
  for (i in 1:nrow(obs)){obar_jy_fm[i]=sum(rep(seq(1,20,1),nsexes)*obs[i,])}
  ebar_jy_fm=vector()
  for (i in 1:nrow(expect)){ebar_jy_fm[i]=sum(rep(seq(1,20,1),nsexes)*expect[i,])}
  vjy=vector();
  for (i in 1:nrow(expect)){vjy[i]=sum((rep(seq(1,20,1),nsexes)^2)*expect[i,])};
  vjy_fm=vjy-(ebar_jy_fm^2)
  

  #Weight (I ran this 3 times iteratively).
  wj=1/(var((obar_jy_fm-ebar_jy_fm)/sqrt(vjy_fm/(1*nsamp))))
  
  out<-list()
  out$wj<-wj
  out$NewNsamp<-wj[1]*nsamp
  
  
#NRS fishery
#First Iteration 1.98
#2nd iteration 2.61
#3rd iteration 2.81
  #4th iteration 2.86
  
#NRS survey
  #First iteration 0.274
  #Second iteration 0.162
  #Third iteration 0.139
  #4th iteration 0.133
  
  return(out)
}