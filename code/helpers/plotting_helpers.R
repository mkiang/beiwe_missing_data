## Plotting helpers


## Misc helpers ----
mkdir_p <- function(dir_name) {
    ## Mimics mkdir -p
    dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
}


## Themes ----
mk_classic <- function(...) {
    ## Just a shortcut for serif fonts and classic theme with legend in upper
    ## left by default. 
    theme_classic(base_size = 10, base_family = "Times") + 
        theme(title = element_text(family = "Times"), 
              legend.key = element_rect(fill = NA, color = NA), 
              legend.position = c(0.01, 1.01), 
              legend.justification = c(0, 1), 
              legend.background = element_rect(fill = alpha("white", .75), 
                                               color = NA))
}


mk_x90 <- function(...) {
    ## Makes x-axis text 90 degrees
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), 
          ...)
}


mk_legend_ur <- function(...) {
    ## Moves legend to upper right
    theme(legend.position = c(0.98, 0.98), 
          legend.justification = c(1, 1))
}



mk_nyt <- function(...) {
    ## http://minimaxir.com/2015/02/ggplot-tutorial/
    ## paste0('https://timogrossenbacher.ch/2016/12/', 
    ##        'beautiful-thematic-maps-with-ggplot2-only/')
    ## https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
    
    ## Colos — stick with the ggplot2() greys
    c_bg    <- "white"
    c_grid  <- "grey80"
    c_btext <- "grey5"
    c_mtext <- "grey30"
    
    # Begin construction of chart
    theme_bw(base_size = 11, base_family = "Arial Narrow") +
        
        # Region
        theme(panel.background = element_rect(fill = c_bg, color = c_bg), 
              plot.background  = element_rect(fill = c_bg, color = c_bg), 
              panel.border     = element_blank()) +
        
        # Grid
        theme(panel.grid.major.y = element_blank(), 
              panel.grid.major.x = element_blank(),
              panel.grid.minor   = element_blank(), 
              axis.ticks         = element_line(color = c_grid, size = .15, 
                                                linetype = "solid"), 
              axis.ticks.length  = unit(.15, "cm")) +
        
        # Legend
        theme(legend.position      = c(0, 1), 
              legend.justification = c(0, 1), 
              legend.direction     = "vertical",
              legend.key           = element_rect(fill = NA, color = NA), 
              legend.background    = element_rect(fill = "transparent", color = NA), 
              legend.text          = element_text(color = c_mtext)) +
        
        # Titles, labels, etc.
        theme(plot.title     = element_text(color = c_btext, vjust = 1.25, 
                                            face = "bold", size = 11), 
              axis.text      = element_text(size = 8, color = c_mtext), 
              axis.line.x    = element_line(color = c_grid, linetype = "solid"),
              axis.text.x    = element_text(size = 8, color = c_mtext, 
                                            hjust = .5),
              axis.title.x   = element_text(size = 9, color = c_mtext,
                                            hjust = 1), 
              axis.title.y   = element_text(size = 9, color = c_mtext, 
                                            hjust = 1)) +
        # Facets
        theme(strip.background = element_rect(fill = c_grid, color = c_btext), 
              strip.text = element_text(size = 8, color = c_btext)) + 
        
        # Plot margins
        theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) + 
        
        # Additionals
        theme(...)
}



turn_off_clipping <- function(ggplot_grob, draw = FALSE) {
    x <- ggplot_gtable(ggplot_build(ggplot_grob))
    x$layout$clip[x$layout$name == "panel"] <- "off"
    x$layout$clip = "off"
    
    if (draw) {
        grid.draw(x)
    }
    
    return(x)
}

