#' Extract spawning stock biomass (ssb) from fm.tpl run
#'
#' Spawning biomass may be defined as all males or some combination of males and
#' females
#'
#' @param M list object created by read_admb function
#' @return dataframe of spawning biomass
#' @author SJD Martell, D'Arcy N. Webber, Carey McGilliard
#' @export
#' 
#' 
# #'   R_report<<"SSB"<<endl; 
# for (i=styr;i<=endyr;i++) 
# {
#   double lb=value(SSB(i)/exp(2.*sqrt(log(1+square(SSB.sd(i))/square(SSB(i))))));
#   double ub=value(SSB(i)*exp(2.*sqrt(log(1+square(SSB.sd(i))/square(SSB(i))))));
#   R_report<<i<<" "<<SSB(i)<<" "<<SSB.sd(i)<<" "<<lb<<" "<<ub<<endl;
# }
.get_ssb_df <- function(M)
{
    n <- length(M)
    mdf <- NULL
    for(i in 1:n)
    {
        A <- M[[i]]
        df <- data.frame(Model = names(M)[i],
                         par = "SSB",
                         year = A$SSB[,1],
	                       ssb = A$SSB[,2],
                         sd = A$SSB[,3])
        df$lb <- df$ssb - 1.96*df$sd
        df$ub <- df$ssb + 1.96*df$sd
        mdf <- rbind(mdf, df)
    }
    return(mdf)
}


#' Plot predicted spawning stock biomass (ssb)
#'
#' Spawning biomass may be defined as all males or some combination of males and
#' females
#'
#' @param M List object(s) created by read_admb function
#' @return Plot of model estimates of spawning stock biomass 
#' @author SJD Martell, D'Arcy N. Webber
#' @export
#' 
plot_ssb <- function(M, xlab = "Year", ylab = "SSB (kt)")
{
    xlab <- paste0(xlab, "\n")
    ylab <- paste0(ylab, "\n")
    
    mdf <- .get_ssb_df(M)
    
    p <- ggplot(mdf) + labs(x = xlab, y = ylab) + expand_limits(y = 0)
    if (length(M) == 1)
    {
        p <- p + geom_line(aes(x = year, y = ssb)) +
            geom_ribbon(aes(x = year, ymax = ub, ymin = lb), alpha = 0.3)
    } else {
        p <- p + geom_line(aes(x = year, y = ssb, col = Model)) +
            geom_ribbon(aes(x = year, ymax = ub, ymin = lb, fill = Model), alpha = 0.3)
    }
    if(!.OVERLAY) p <- p + facet_wrap(~Model)
    print(p + .THEME)
}
