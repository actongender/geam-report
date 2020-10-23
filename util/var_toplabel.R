#' @title Retrieve question text
#' 
#' @description Retrieves top question label for variable. In case
#'  variable is a sub-question, retrieves the label of the parent (top) level question stored with attribute "question_label". 
#'  In case it is a normal question, retrieve the "label" attribute. 
#'  
#'  Never return sub-question label, but always the top-level (parent) question text. Useful for inserting
#'  the top level question before sub-questions automatically. 
#' 
#' @param var_name String of variable name (ls question code)
#' 
var_toplabel <- function(var_name, data=NULL){
    
    if (is.null(data) & exists("df.geam")){
        data <- df.geam
    }
    
    if (is.null(data)){
        return("Missing dataframe in var_toplabel()!")
    }
    
    #default label 
    label <- paste0("Label for '",var_name,"' not found") 
    
    # if variable exists, get actual label
    if (var_exists(var_name, stack=names(data))){
        
        # matrix answer items
        if (stringr::str_detect(var_name, ".SQ")){
            
            label <- attr(data[,var_name], which="parent_label")
            
            # all other questions
        } else {
            
            label <- attr(data[,var_name], which="label")    
        }
    } 
    
    label
}