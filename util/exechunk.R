#' @title Check several conditions in relation to variable
#' 
#' @description Checks for several conditions related to the execution of a knitr chunk of 
#'  code. Basic check includes if varible exists. 
#' 
#' @param variable String. Name of the variable of the corresponding code
#' @param data Data frame. Usually df.geam 
#' 
#' @return logit 
#' 
exechunk <- function(variable, data=NULL){
    
    if (variable %in% skipsubquestion){
        return(FALSE)
    }
    
    exec <- TRUE
    
    if (is.null(data) & exists("df.geam") ){
        cnames <- names(df.geam)        
    } else if (!is.null(data)){
        cnames <- names(data)
    } else {
        cnames <- ""
    }

    
    exec <- var_exists(variable, cnames, data)

    return(exec)
}