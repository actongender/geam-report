#' @title Retrieve question text
#' 
#' @description Retrieves question label for variable.  
#'  
#' @param var_name String of variable name (ls question code)
#' 
var_label <- function(var_name, data=NULL){
    
    if (is.null(data) & exists("df.geam")){
        data <- df.geam
    } 
    
    if(is.null(data)){
        return("Missing dataframe in var_label()")
    }
    
    
    #default label 
    label <- paste0("Label for '",var_name,"' not found") 
    
    # if variable exists, get actual label
    if (var_exists(var_name, stack=names(data) )){
            
        label <- attr(data[,var_name], which="label")    
        
    } 
    
    # label exists but has no attribute set
    if (is.null(label)){
        label <- ""
    }
    
    
    label
}