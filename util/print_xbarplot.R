#' @title Make cross-tab barplot 
#' 
#' @param data data frame
#' @param var1 first vector 
#' @param var2 second vector
#' @param lrm double. percentage labels from bars are removed when smaller than lrm. 
#'
#' @return cross tab bar plot 
#'
print_xbarplot <- function(data, 
                           var1, 
                           var2, 
                           lrm=.01, 
                           lwidth=30, 
                           xlangle=0,
                           ltitle=T,
                           lcpal=NULL,
                           legpos = "bottom",
                           legdir = "horizontal", ...){
    
    # use local color palette or global 
    if (is.null(lcpal) & !exists("cpal")){
        lcpal <- RColorBrewer::brewer.pal(9, "Set1")
    } else if (is.null(lcpal) & exists("cpal")){
        lcpal <- cpal
    }
    
    
    
    # get cross tabs
    xt <-  xtabs(formula = ~ data[[var1]] + data[[var2]])
    
    xtp_cell <- prop.table(xt)
    
    xtp_cell <- as.data.frame(xtp_cell)
    
    names(xtp_cell) <- c("xvar", "byvar", "freq")
    
    
    g <- xtp_cell %>% 
        mutate(freq1 = if_else(freq < lrm, "" , paste0(round(freq*100,0),"%"))) %>% 
        ggplot(aes(x=xvar, y=freq, fill=byvar)) + 
        geom_bar(stat="identity", position=position_fill()) + 
        scale_y_continuous(labels=scales::percent) + 
        scale_x_discrete(labels = function(x) str_wrap(x,width=lwidth), guide = guide_axis(angle = xlangle)) +
        geom_text(aes(label = freq1 , y=freq), position=position_fill(vjust=.60)) +
        scale_fill_manual(values=lcpal, labels = function(x) str_wrap(x,width=lwidth)) +
        theme_light() +
        theme(axis.title.x = element_blank(),
              legend.position = legpos,
              legend.direction = legdir, 
              plot.margin = margin(.6, .5, .1, .1, "cm"))
    
    # show legend title 
    if (ltitle){
        g <- g + labs(x="", y="", fill=var_toplabel(var2, data))
    } else {
        g <- g + labs(x="", y="", fill="")
    }
    
    g
    
}
