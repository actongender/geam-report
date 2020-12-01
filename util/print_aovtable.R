#' Prints anova tables depending on output format. 
#' 
#' library(flextable) is used for word document outputs. Kabel for pdf and html output. 
#' 
#' @param df.aov anova result object or tukey post-test result 
#' @param fsize font size used in flextable. 
#' @param tcap String Table caption
#' @param include Vector of Strings of anova coefficients to calculate. Possible are: 
#'  c("term", "df", "sumsq", "meansq", "statistic", "p.value", "eta.sq", "partial.etasq", "omegasq", "partial.omegasq", "cohens.f", "powe")
#' 
#' 
print_aovtable <- function(df.aov, 
                           fsize=11, 
                           tcap= "",
                           include=c("term", "df", "sumsq", "meansq", "statistic", "p.value", "etasq"),
                           ...){
    

    # get auto numbering of tables 
    caption_auto <- officer::run_autonum(seq_id = "tab", 
                                         pre_label = paste0(get_i18n("Table"), " " ))
    
    if (tcap == ""){
        #caption_txt <- var_label(vars, data = data)        
    } else {
        caption_txt <- tcap
    } 
    

    
    # print the TukeyHSD table 
    if (class(df.aov)[1] == "TukeyHSD"){
        
        tbl <- broom::tidy(df.aov) %>% 
            select(term, contrast, difference=estimate, p.val=adj.p.value) %>% 
            mutate(difference = round(difference,3),
                   p.val = round(p.val, 3))
    
    # or ANOVA result table         
    } else if (class(df.aov)[1] == "aov"){
        
        tbl <- sjstats::anova_stats(df.aov) %>% 
            select(all_of(include))
        
        # get rid of rownames
        rownames(tbl) <- NULL
        
    }
    

    tnum_cols <- dim(tbl)[2]


    # missing string
    missing_str <- paste0("(", get_i18n("Missing"), ")")

    
    #html / pdf
    if (knitr::is_html_output()) {

        ftb <- knitr::kable(tbl)

    # word document
    } else {
        
        ftb <- flextable(tbl) %>% 
                    flextable::font(fontname="Roboto", part="all") %>% 
                    flextable::fontsize(size=fsize, part = "all") %>% 
                    flextable::bold(bold=T, part = "header") %>% 
                    flextable::set_caption(caption = caption_txt, autonum=caption_auto) 
        
        if (class(df.aov)[1] == "TukeyHSD"){
            ftb <- ftb %>% 
                flextable::width(j=c(1:tnum_cols), width=c(2,2,rep(2.2/(tnum_cols-2), (tnum_cols-2)))) 
            
        } else if (class(df.aov)[1] == "aov"){
            ftb <- ftb %>% 
                flextable::width(j=c(1:tnum_cols), width=c(1.4,rep(5/(tnum_cols-1), (tnum_cols-1)))) 
            
        }

    }

    ftb
}