library(TMB)

TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
remotes::install_github("fishfollower/compResidual/compResidual", INSTALL_opts=c("--no-multiarch"), force=TRUE)

library(compResidual)

