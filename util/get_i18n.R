#' @title Translate text 
#' 
#' @param key String. Corresponds to first column in i18n/ translation files
#' @param plang String. Specify language if other than default. 
#' 
#' @return translated string. 
#'
#'
get_i18n <- function(key, plang=NULL){

        
    if (is.null(plang) & exists("lsLangCode")){
        plang <- lsLangCode 
        
    # default is english    
    }  else if (is.null(plang) & !exists("lsLangCode")){
        plang <- "en"
    }
    
    plangfile <- paste0("i18n/geam-report-",plang,".csv")
    

    if (file.exists(plangfile)){
        i18n <- read_tsv(plangfile, 
                         col_types = cols())
    } else {
        warning("No language file for ", plang, " found. Using default.")
    }
        
    txt <- i18n %>% 
        filter(msgid == key) %>%
        select(2)
 
    as.character(txt)   

}
