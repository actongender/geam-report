#' Is difference between means significant? 
#' 
#' @param df.aov result object from aov()
#' @param thres significance thresheld. If other than numeric, function returns p.value 
#' 
isig <- function(df.aov, thres=.05){
    
    #tmp <- sjstats::anova_stats(df.aov)    
    tmp <- broom::tidy(df.aov)    

    if (any((tmp[,6] <= thres), na.rm=T)){
        return(T)
    } else if (all((tmp[,6] > thres), na.rm=T)){
        return(F)
    } else {
        return(tmp[,6])
    }
    
}