#' @title Check if variable name exists in data frame. 
#' 
#' @description Useful to avoid error messages before generating figures and tables. Performs exact match.  
#' 
#' @param needle String of one or more column names.  
#' @param stack String of all column names
#' 
#' @return logit 
#' 
var_exists <- function(needle, stack="", data=NULL){
    
    
    # if no stack provided, check if globally defined column names exist (in index.Rmd)
    if (length(stack) == 1 & stack[1] == "" & exists("cnames")){
        stack <- cnames
    } else if (length(stack) == 1 & stack[1] == "" & !is.null(data)){
        stack <- names(data)
    } else if (length(stack) == 1 & stack[1] == "" & is.null(data) & exists("df.geam")){
        stack <- names(df.geam)
    } 
    
    exists <- all(needle %in% stack)
    
    exists
}