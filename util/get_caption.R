#' @title Construct a caption 
#' 
#'  @description Construct caption for figures or tables.  
#' 
#' @param var1 String. Name of variable 1 
#' @param var2 String. Name of variable 2
#' @param topl Logit. If top label should be used. 
#' 
#' @return translated string. 
#'
#'
get_caption <- function(var1, var2=NULL, topl=c(T,T), data=NULL){

    if (topl[1]){
        caption_txt1 <- var_toplabel(var1, data = data)
    } else {
        caption_txt1 <- var_label(var1, data = data) 
    }
    
    if (var1 == "SDEM004.bin"){
        caption_txt1 <- get_i18n("Gender")
    }
    
    tcap <- caption_txt1
    
    if (!is.null(var2)){
        
        if (topl[2]){
            caption_txt2 <- var_toplabel(var2, data = data) 
        } else {
            caption_txt2 <- var_label(var2, data = data) 
        }
        
        if (var2 == "SDEM004.bin"){
            caption_txt2 <- get_i18n("Gender")
        }
        
       
        tcap <- paste0("'", caption_txt1, "'  ·  ", get_i18n("Crosstab by") ,"  ·  '", caption_txt2, "'")
    }
    
    tcap
}
