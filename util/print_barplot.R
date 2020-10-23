#'
#' @title Prints customzied bar plot
#' 
#' @description Plots either percentage or total count barplots for GEAM report. 
#' 
#' @param data GEAM result data frame
#' @param vars The variable for the bar plot
#' @param lplotperc Indicates if plot should use asolute count or percentage. 
#' @param lhideNA Logit. Show or hide NA values in plot. 
#' @param coords90 Logit Flip coords 90 degrees (useful if labels don't fit)
#' @param lwidth Integer Wrapping width for x-axis labels. 
#' @param xlfsize Integer. Size of font for x-axis labels. 
#' @param xlangle Integer. Angle for rotating x-axis labels
#' @param lcpal Vector of color values for manual fill. If null, will try to use global cpal variable. 
#'
print_barplot <- function(data, 
                          vars, 
                          lplotperc=NULL, 
                          lhideNA=NULL, 
                          coords90=FALSE, 
                          lwidth=10, 
                          xlfsize=11, 
                          xlangle=0, 
                          lcpal=NULL){
    
    # use global settings if available
    if (is.null(lplotperc) & !exists("plotperc")){
        lplotperc <- T
    } else if (is.null(lplotperc) & exists("plotperc")){
        lplotperc <- plotperc
    }
    
    
    # use global hideNA settings if available 
    if (is.null(lhideNA) & !exists("hideNA")){
        lhideNA <- T
    } else if (is.null(lhideNA) & exists("hideNA")){
        lhideNA <- hideNA
    }

    # use local color palette or global 
    if (is.null(lcpal) & !exists("cpal")){
        lcpal <- RColorBrewer::brewer.pal(9, "Set1")
    } else if (is.null(lcpal) & exists("cpal")){
        lcpal <- cpal
    }
    
    

    # make bar plot of percentages
    if (lplotperc){
        
        g <- data %>% 
                filter(!is.na({{vars}}) & lhideNA) %>%
                group_by({{vars}}) %>% 
                summarize(gtotal = n()) %>% 
                mutate(perc = gtotal/sum(gtotal)) %>% 
                ggplot(aes(x={{vars}}, fill={{vars}})) +
                    geom_bar(aes(y=perc), stat="identity") +
                    geom_text(aes(label = scales::percent(perc, accuracy=1.0), y=perc), vjust = -.5) +
                    scale_y_continuous(labels=scales::percent, expand=c(0.15,0)) +
                    labs(y="")

    # make bar plot of absolute counts
    } else {
        
        g <- data %>%  
                filter(!is.na({{vars}}) & lhideNA) %>% 
                ggplot(aes(x=fct_infreq({{vars}}), fill={{vars}})) + 
                    geom_bar(width=.5) +
                    labs(y=get_i18n("Total count"))
          
    }
    
    
    # check if factor has less than 9 colors and set brewerscale accordingly
    nlev <- data %>% 
        filter(!is.na({{vars}}) & lhideNA) %>%
        distinct({{vars}}) %>% 
        nrow()

    # fewer levels than colors use provided scale, otherwise use default. 
    if (nlev <= length(lcpal)) {
        g <- g + scale_fill_manual(values=lcpal)
    }

        
    # flip entire bar 90 degrees
    if (coords90) {
        g <- g +  coord_flip()
    }
    
    
    # add the rest 
    g <- g +    scale_x_discrete(labels = function(x) str_wrap(x,width=lwidth), guide = guide_axis(angle = xlangle)) +
                guides(fill=F) +
                theme_light() +
                theme(axis.title.x = element_blank(),
                      axis.text = element_text(size = xlfsize),
                      plot.margin = margin(.6, .5, .1, .1, "cm"))
    
    
    return(g)
    
}
