#' Get recruitment data
#' 
#' Extracts predicted recruitment and approximate asymptotic error-bars
#'
#' @param M list object(s) created by read_admb function
#' @return dataframe of recruitment
#' @author SJD Martell, DN Webber
#' @export
#' 
.get_recruitment_df <- function(M)
{
    n <- length(M)
    mdf <- NULL
    for(i in 1:n)
    {
        A  <- M[[i]]
        if(is.null(A$fit$logDetHess))
        {
            stop("Appears that the Hessian was not positive definite\n
                  thus estimates of recruitment do not exist.\n
                  See this in replist$fit.")
        }
        df <- data.frame(Model   = names(M)[i],
                         par     = A$fit$names,
                         pred_rec = A$fit$est,
                         sd  = A$fit$std)
        df <- subset(df, par == "pred_rec")
        df$year    <- A$Yr
        #df$log_rec <- exp(df$log_rec)
        df$lb      <- df$pred_rec - 1.96*df$sd
        df$ub      <- df$pred_rec + 1.96*df$sd
        mdf <- rbind(mdf, df)
    }
    return(mdf)
}


#' Get recruitment size distribution data
#' 
#' @param M list object(s) created by read_admb function
#' @return dataframe of recruitment size distribution
#' @author DN Webber
#' @export
#' 
.get_recruitment_size_df <- function(M)
{
    n <- length(M)
    mdf <- NULL
    for(i in 1:n)
    {
        A  <- M[[i]]
        if(is.null(A$fit$logDetHess))
        {
            stop("Appears that the Hessian was not positive definite\n
                  thus estimates of recruitment do not exist.\n
                  See this in replist$fit.")
        }
        df <- data.frame(Model = names(M)[i],
                         mid_points = A$mid_points,
                         rec_sdd = A$rec_sdd,
                         rec_ini = A$rec_ini)
        mdf <- rbind(mdf, df)
    }
    return(mdf)
}


#' Plot predicted recruitment and approximate asymptotic error-bars
#'
#' @param M list object created by read_admb function
#' @param xlab the x-axis label for the plot
#' @param ylab the y-axis label for the plot
#' @return Plot of predicted recruitment
#' @author SJD Martell, DN Webber
#' @export
#' 
plot_recruitment <- function(M, xlab = "Year", ylab = "Recruitment in numbers")
{
    xlab <- paste0(xlab, "\n")
    ylab <- paste0(ylab, "\n")
    mdf <- .get_recruitment_df(M)
    if (length(M) == 1)
    {
        p <- ggplot(mdf, aes(x = year, y = pred_rec)) +
            geom_bar(stat = "identity", alpha = 0.4, position = "dodge") +
            geom_pointrange(aes(year, pred_rec, ymax = ub, ymin = lb), position = position_dodge(width = 0.9))
    } else {
        p <- ggplot(mdf, aes(x = year, y = pred_rec, col = Model, group = Model)) +
            geom_bar(stat = "identity", alpha = 0.4, aes(fill = Model), position = "dodge") +
            geom_pointrange(aes(year, pred_rec, col = Model, ymax = ub, ymin = lb), position = position_dodge(width = 0.9))
    }
    p <- p + labs(x = xlab, y = ylab)
    if(!.OVERLAY) p <- p + facet_wrap(~Model)
    print(p + .THEME)
}


#' Plot recruitment size distribution
#'
#' @param M list object created by read_admb function
#' @param xlab the x-axis label for the plot
#' @param ylab the y-axis label for the plot
#' @return plot of recruitment size distribution
#' @author DN Webber
#' @export
#' 
plot_recruitment_size <- function(M, xlab = "Size", ylab = "Density")
{
    xlab <- paste0("\n", xlab)
    ylab <- paste0(ylab, "\n")
    mdf <- .get_recruitment_size_df(M)
    p <- ggplot(mdf, aes(x = mid_points, y = rec_sdd)) + labs(x = xlab, y = ylab)
    if (length(M) == 1)
    {
        p <- p + geom_line()
    } else {
        p <- p + geom_line(aes(col = Model))
    }
    if(!.OVERLAY) p <- p + facet_wrap(~Model)
    print(p + .THEME)
}


#' Plot predicted recruitment across model runs
#'
#' @param data A list of multiple objects created by read_admb function
#' @param modnames A vector of model names included in \code{data}
#' @return Plot of predicted recruitment compared across models
#' @author Cole Monnahan Kelli Johnson
#' @export
#' 
plot_models_recruitment <- function(data, modnames=NULL )
{
  if (is.null(modnames))
    modnames = paste("Model ",1:length(data))
  if (length(data)!=length(modnames)) 
    stop("Holy moly, unequal object lengths") 

  recs <- lapply(data, get_recruitment)
  df <- do.call("rbind", Map(cbind, recs, modname = modnames))

  p <- ggplot(df,aes(x=factor(year),y=exp(log_rec), group=modname, colour=modname))
  p <- p + geom_line(stat = "identity", alpha=0.4)
  p <- p + geom_pointrange(aes(factor(year),exp(log_rec),ymax=ub,ymin=lb))
  p <- p + labs(x="Year", y="Recruitment")
  pRecruitment <- p + ggtheme
  return(pRecruitment)
}
