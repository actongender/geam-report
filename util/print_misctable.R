#' Prints anova tables depending on output format. 
#' 
#' library(flextable) is used for word document outputs. Kabel for pdf and html output. 
#' 
#' @param df.aov anova result object
#' @param fsize font size used in flextable. 
#' @param tcap String Table caption
#' @param include Vector of Strings of anova coefficients to calculate. Possible are: 
#'  c("term", "df", "sumsq", "meansq", "statistic", "p.value", "eta.sq", "partial.etasq", "omegasq", "partial.omegasq", "cohens.f", "powe")
#' 
#' 
print_misctable <- function(data, 
                           fsize=11, 
                           tcap= "",
                           ...){
    

    # get auto numbering of tables 
    caption_auto <- officer::run_autonum(seq_id = "tab", 
                                         pre_label = paste0(get_i18n("Table"), " " ))
    
    if (tcap == ""){
        caption_txt <- var_label(vars, data = data)        
    } else {
        caption_txt <- tcap
    } 
    
    
    tnum_cols <- dim(data)[2]
    

    isHTML <- knitr::is_html_output()

    #html / pdf
    if (isHTML) {

        ftb <- knitr::kable(data)

    # word document
    } else {
        
        ftb <- flextable(data) %>% 
                    flextable::font(fontname="Roboto", part="all") %>% 
                    flextable::fontsize(size=fsize, part = "all") %>% 
                    flextable::bold(bold=T, part = "header") %>% 
                    #flextable::width(j=c(1:tnum_cols), width=c(2,rep(4.4/(tnum_cols-1), (tnum_cols-1)))) %>% 
                    flextable::set_caption(caption = caption_txt, autonum=caption_auto)
                    #flextable::compose(i = tnum_rows, j = 1, as_paragraph(as_chunk(missing_str)))
        
        if (class(data)[1] == "easycorrelation"){
            ftb <- ftb %>% 
                flextable::width(j=c(1:tnum_cols), width=c(1,1,rep(4.6/(tnum_cols-2), (tnum_cols-2))))
        } else {
            ftb <- ftb %>% 
                flextable::width(j=c(1:tnum_cols), width=c(2,rep(4.4/(tnum_cols-1), (tnum_cols-1))))
        }

    }

    ftb
}