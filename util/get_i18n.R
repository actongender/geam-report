#' @title Translate text 
#' 
#' @param key String. Corresponds to first column in i18n/ translation files
#' @param plang String. Specify language if other than default. 
#' 
#' @return translated string. 
#'
#'
get_i18n <- function(key, plang=NULL){

        
    # Default, use lsLangCode from config.R
    if (is.null(plang) & exists("lsLangCode")){
        plang <- lsLangCode 
        
        # if no plang is specified and no lsLangCode available fall back to English
    }  else if (is.null(plang) & !exists("lsLangCode")){
        plang <- "en"
    }
    
    
    plangfile <- paste0("i18n/geam-report-",plang,".csv")
    
    # if language file is available
    if (file.exists(plangfile)){
        
        i18n <- read_tsv(plangfile, 
                         col_types = cols())
        
    # otherwise use english
    } else {
        
        i18n <- read_tsv(paste0("i18n/geam-report-en.csv"), 
                         col_types = cols())
        
    }
        
    txt <- i18n %>% 
        filter(msgid == key) %>%
        select(2)
 
    as.character(txt)   

}
