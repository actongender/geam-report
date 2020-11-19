#' Prints contigency tables depending on output format. 
#' 
#' library(flextable) is used for word document outputs. Kabel for pdf and html output. 
#' 
#' @param x column of data frame with numeric variable
#' @para fsize font size used in flextable. 
#' 
#' 
print_xtable <- function(data, 
                         var1, 
                         var2, 
                         fsize=11,
                         tcap = "", ...){


    # get auto numbering of tables 
    caption_auto <- officer::run_autonum(seq_id = "tab", 
                                         pre_label = paste0(get_i18n("Table"), " " ))
    
    # caption text 
    if (tcap == ""){
        caption_txt <- get_caption(var1=var1, var2=var2, data = data)
        
    } else {
        caption_txt <- tcap
    }
    
    
    
    isHTML <- knitr::is_html_output()


    #html / pdf
    if (isHTML) {
        
        ft <- tryCatch({
            sjPlot::tab_xtab(data[[var1]], data[[var2]], ... )            
        }, 
        error = function(e){
            return(" WARNING! Cell-entries likely contain 0 cases")
        })

    # word document
    } else {

        # get cross tabs
        xt <-  xtabs(formula = ~ data[[var1]] + data[[var2]])
        
        # add margins (sum)
        xtm <- addmargins(xt)
        
        # percentages per row and per column
        xtp_r <- round(prop.table(xt, 1), 3)
        xtp_c <- round(prop.table(xt, 2), 3)
        
        # chqui squared 
        cst <- tryCatch({
                chisq.test(data[[var1]], data[[var2]])            
            }, 
            error = function(e){
                cst <- data.frame(statistic=NA, p.value=NA, parameter=NA)
                return(cst)
            }) 
        
        #fisher test
        fpval <- tryCatch({
                fst <- fisher.test(data[[var1]], data[[var2]])
                round(fst$p.value, 3)
                
            }, 
            error = function(e){
                return(" WARNING! ")
            })
         
        
        
        stats_str <- paste0("Chi-squared=", round(cst$statistic,3), " (p-value=", round(cst$p.value,3) ,") · df=", cst$parameter, " · ", " Fisher's p=", fpval)
        
        
        # get dimensios of table. 
        tall_cols <- dim(xtm)[2]+1
        tnum_cols <- dim(xtm)[2]
        
        # create xtable 
        xtab <- xtable(xtm)
    
        # to pass to flextable
        # for word, a good approx for overall table width is 6.4inch = 16.3cm.  
        ft <- flextable::as_flextable(xtab) %>% 
                flextable::font(fontname="Roboto", part="all") %>% 
                flextable::fontsize(size=fsize, part="all") %>% 
                flextable::colformat_num(digits=0, j=c(2:tall_cols)) %>%
                flextable::width(j=c(1:tall_cols), width=c(3,rep(3.4/tnum_cols, tnum_cols))) %>% 
                flextable::align(align="right", part="header") %>% 
                flextable::add_footer_row(colwidths = tall_cols , values=stats_str) %>%
                flextable::align(align="right", part="footer") %>% 
                flextable::set_caption(caption=caption_txt, autonum=caption_auto)
            
        
        
        ft
    }
    
    
    ft
}