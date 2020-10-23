#' Prints frequency tables depending on output format. 
#' 
#' library(flextable) is used for word document outputs. Kabel for pdf and html output. 
#' 
#' @param x column of data frame with numeric variable
#' @para fsize font size used in flextable. 
#' 
#' 
print_frqtable <- function(data, 
                           vars, 
                           fsize=11, 
                           tcap= "",
                           lsort.frq=NULL, 
                           ...){
    

    # get auto numbering of tables 
    caption_auto <- officer::run_autonum(seq_id = "tab", 
                                         pre_label = paste0(get_i18n("Table"), " " ))
    
    if (tcap == ""){
        caption_txt <- var_label(vars, data = data)        
    } else {
        caption_txt <- tcap
    } 
    

    
    # overwrite
    if (is.null(lsort.frq)){
        lsort.frq <- frqtsort
    }


    frqtbl <- data %>%
        sjmisc::frq({{vars}}, sort.frq = lsort.frq, ...)
    
    # number of rows of table
    tnum_rows <- dim(frqtbl[[1]])[1]
    
    # missing string
    missing_str <- paste0("(", get_i18n("Missing"), ")")
    

    isHTML <- knitr::is_html_output()

    #html / pdf
    if (isHTML) {

        ftb <- knitr::kable(frqtbl, col.names=c("value", "", "count", "%", "valid %", "cum %"))

    # word document
    } else {
        
        ftb <- flextable(frqtbl[[1]], col_keys = c("val", "frq", "raw.prc", "valid.prc", "cum.prc")) %>% 
                    flextable::set_header_labels(val=" ", #get_i18n("value"), 
                                                 frq=get_i18n("count"), 
                                                 raw.prc="%", 
                                                 valid.prc=paste0("% ",get_i18n("valid")), 
                                                 cum.prc=paste0("% ",get_i18n("cum"))) %>% 
                    flextable::font(fontname="Roboto", part="all") %>% 
                    flextable::fontsize(size=fsize, part = "all") %>% 
                    flextable::bold(bold=T, part = "header") %>% 
                    flextable::width(j=c(1:5), width=c(3,1,.6,1,1)) %>% 
                    flextable::set_caption(caption = caption_txt, autonum=caption_auto) %>% 
                    flextable::compose(i = tnum_rows, j = 1, as_paragraph(as_chunk(missing_str)))
    }

    ftb
}