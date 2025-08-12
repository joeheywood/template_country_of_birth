set_year_of_arrival_plot <- function() {
    filename <- "lond_21_yoa"
    
    var_name <- "yoa"
    

    
    ## setting up
    pal <- gla_pal(gla_theme = "default", n = 10)
    theme_set(theme_gla(gla_theme = "default"))
    
    
    ## reading in the data
    
    to_read <- paste0("report_data/",filename,".csv")
    
    plot_dat <- fread(to_read)
    
    ## small bit of data work - rounding
    
    plot_dat$percs <- round(plot_dat$percs,1)
    
    ## end of small bit of custom data work
    save(plot_dat, file = "year_of_arrival_plot.RData")
    
    
    
}

get_year_of_arrival_plot <- function() {
    load("year_of_arrival_plot.RData")
    ## year of arrival plot
    tooltip_var <- "Year of Arrival: "
    pal <- gla_pal(gla_theme = "default", n = 10)
    theme_set(theme_gla(gla_theme = "default"))   
    var_name <- "yoa"
    positions <- c(
        "Arrived before 1951",
        "Arrived 1951 to 1960",
        "Arrived 1961 to 1970",
        "Arrived 1971 to 1980",
        "Arrived 1981 to 1990",
        "Arrived 1991 to 2000",
        "Arrived 2001 to 2010",
        "Arrived 2011 to 2013",
        "Arrived 2014 to 2016",
        "Arrived 2017 to 2019",
        "Arrived 2020 to 2021"
    )
    ## inputs
    ## plotting
    
    out_plot <- ggplot(data = plot_dat,
                       aes(x = .data[[var_name]],y = percs,
                           text = paste0(tooltip_var, .data[[var_name]],
                                         "<br>",
                                         "Percentage: ", percs,"%",
                                         "<br>",
                                         "Count: ", value))) + 
        geom_bar(stat = "identity", fill = pal[1]) + 
        #scale_colour_manual(values = pal) + 
        theme(axis.text.x = element_text(angle = 45,size = 10,hjust = 0.5,vjust = 2)) + 
        scale_y_continuous(labels = dollar_format(prefix = "",suffix = "%")) + 
        scale_x_discrete(limits = positions)
    
    tf <- list(family = "Arial",
               size = 12)
    
    fin_plot <- ggplotly(out_plot,height = 450, width = 650, tooltip = "text") %>%
        layout(
            xaxis = list(tickfont = tf,
                         showgrid = FALSE),
            yaxis = list(tickfont = tf,
                         showgrid = TRUE))
    
    fin_plot
    
    
    
}
