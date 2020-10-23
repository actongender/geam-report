#' @title Shortens labels 
#'
#' @description Retrieves variable label and shortes them to the max length indicated. Before cutting 
#'  the length of the label, tries to remove any text withing parenthesis, often examples. 
#' 
#' @param data Data frame or selection of columns
#' @param maxlength Integer. Maximum number of characters for the label. Rest will be removed
#' @param morestr String. Will be pasted to cut strings. 
#'
#'
shorten_label <- function(data, maxlength=30, morestr=" ..."){
    
    
    xlabels <- data %>% 
        sjlabelled::get_label() %>% 
        stringr::str_remove(pattern="\\(.*\\)") %>% 
        stringr::str_sub(1,maxlength) %>% 
        sjlabelled::var_labels()
    
    
    data %>% 
        sjlabelled::set_label(paste0(xlabels, morestr))

}