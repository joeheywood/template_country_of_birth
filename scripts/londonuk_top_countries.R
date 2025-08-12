set_londonuk_top_countries <- function() {
    ## inputs
    file_name <- "lond_vs_rest_21" # name of files to be used
    
    var_name <- "cob_new" # name of variable to be plotted
    
    tooltip_var <- "Country/Region: " # name of the variable as it will appear in the tooltip
    
    # positions <- c("Romania",
    #                "Poland",
    #                "Italy",
    #                "Ireland",
    #                "France",
    #                "Other Europe",
    #                "India",
    #                "Bangladesh",
    #                "Pakistan",
    #                "Sri Lanka",
    #                "Other Middle East and Asia",
    #                "Nigeria",
    #                "Somalia",
    #                "North Africa",
    #                "Ghana",
    #                "Kenya",
    #                "Other Africa",
    #                "All South American countries",
    #                "Jamaica",
    #                "United States",
    #                "Other Americas and the Caribbean",
    #                "Other Antarctica and Oceania")
    
    suf <- "%"
    
    ## reading in data and setting up
    
    to_read <- paste0("report_data/",file_name, ".csv")
    plot_dat <- fread(to_read)
    
    theme_set(theme_gla(gla_theme = "default"))
    
    
    ## custom code for cob
    
    uk <- c("England","Northern Ireland","Scotland","Wales")
    
    plot_dat[plot_dat$cob %in% uk,]$cob_region <- "United Kingdom"
    
    plot_dat$cob_region <- gsub("The ","",plot_dat$cob_region,ignore.case = FALSE)
    
    subreg_agg <- c("Africa not otherwise specified","Other Central and Western Africa",
                    "Other South and Eastern Africa","Other Antarctica and Oceania",
                    "Other Australasia","All other European countries","Other Europe non-EU",
                    "Other EU 2001-2011 members","Other EU 2001 members",
                    "Great Britain not otherwise specified",
                    "United Kingdom not otherwise specified","Other Eastern Asia","Other Middle East",
                    "Other South-East Asia","Other Southern Asia","All Central American countries","Other",
                    "Other North America","Other Caribbean")
    
    
    lond <- plot_dat$geography == "London"
    rest <- plot_dat$geography == "England"
    
    lond_dat <- plot_dat[lond,]
    rest_dat <- plot_dat[rest,]
    
    # london
    
    lond_dat$cob_new <- "blank"
    
    lond_dat[lond_dat$percs >= 0.66,]$cob_new <- lond_dat[lond_dat$percs >= 0.66,]$cob
    
    lond_dat[lond_dat$percs < 0.66,]$cob_new <- paste0("Other"," ",lond_dat[lond_dat$percs < 0.66,]$cob_region)
    
    lond_dat[lond_dat$cob %in% subreg_agg,]$cob_new <- paste0("Other"," ",lond_dat[lond_dat$cob %in% subreg_agg,]$cob_region)
    
    # really awful workaround below, to address the issue of the fake numbers that I generated leading to different countries. Will have to be manual on the day, and then change positions and colours
    # should set seed when I generate the data. Then on day positions has to be manually set
    lond_dat <- fread("report_data/spec_dat.csv")
    
    # matching with rest
    
    lond_dat <- lond_dat[order(lond_dat$cob),]
    rest_dat <- rest_dat[order(rest_dat$cob),]
    
    lond_dat$cob == rest_dat$cob
    
    rest_dat$cob_new <- lond_dat$cob_new
    
    # aggregating both
    
    lond_dat <- lond_dat[,.(percs = sum(percs),value = sum(value)),
                         by = list(cob_new,
                                   cob_region,
                                   geography)]
    
    lond_dat <- lond_dat[!(lond_dat$cob_new %in% c("England","Other Other",
                                                   "Scotland","Other United Kingdom",
                                                   "Wales","Northern Ireland")),]
    
    rest_dat <- rest_dat[,.(percs = sum(percs),value = sum(value)),
                         by = list(cob_new,
                                   cob_region,
                                   geography)]
    
    rest_dat <- rest_dat[!(rest_dat$cob_new %in% c("England","Other Other",
                                                   "Scotland","Other United Kingdom",
                                                   "Wales","Northern Ireland")),]
    
    # putting them together
    
    plot_dat <- rbind(lond_dat,
                      rest_dat)
    
    # lond <- plot_dat$geography == "London"
    # rest <- plot_dat$geography == "England"
    
    ## end of custom code
    
    
    ## custom code - should automate here
    plot_dat$Percentage <- plot_dat$percs
    plot_dat$`Country of Birth` <- plot_dat[,..var_name]
    
    save(plot_dat, file = "report_data/RDA/londonuk_top_countries_plot.RData")
    
    
}

get_londonuk_top_countries <- function() {
    load("report_data/RDA/londonuk_top_countries_plot.RData")
    var_name <- "Country of Birth"
    to_plot <- "percs"
    lond <- plot_dat$geography == "London"
    rest <- plot_dat$geography == "England"
    
    pal <- gla_pal(gla_theme = "default", n = 10)
    positions <- c("Romania",
                   "Poland",
                   "Italy",
                   "Ireland",
                   "France",
                   "Other Europe",
                   "India",
                   "Bangladesh",
                   "Pakistan",
                   "Sri Lanka",
                   "Other Middle East and Asia",
                   "Nigeria",
                   "Somalia",
                   "North Africa",
                   "Ghana",
                   "Kenya",
                   "Other Africa",
                   "All South American countries",
                   "Jamaica",
                   "United States",
                   "Other Americas and the Caribbean",
                   "Other Antarctica and Oceania")
    suf <- "%"
    ## end of custom code
    
    out_plot <- ggplot() + 
        geom_segment(
            aes(x = unlist(plot_dat[rest,..var_name]), y = unlist(plot_dat[rest,..to_plot]),
                xend = unlist(plot_dat[lond,..var_name]), yend = unlist(plot_dat[lond,..to_plot])),
            colour = "gray"
        ) + 
        geom_point(data = plot_dat[lond,],
                   aes(x = .data[[var_name]],
                       y = .data[[to_plot]],
                       fill = .data[[var_name]],
                       text = paste0("London",
                                     "<br>",
                                     .data[[var_name]],
                                     "<br>",
                                     .data[[to_plot]],"%",
                                     "<br>",
                                     value)),
                   shape = 21,
                   stroke = 0.5,
                   size = 2) + 
        geom_point(data = plot_dat[rest,],
                   aes(x = .data[[var_name]],
                       y = .data[[to_plot]],
                       fill = .data[[var_name]]),
                   shape = 21,
                   size = 2, 
                   colour = "white",
                   fill = "white") + 
        geom_point(data = plot_dat[rest,],
                   aes(x = .data[[var_name]],
                       y = .data[[to_plot]],
                       fill = .data[[var_name]],
                       text = paste0("Rest of England",
                                     "<br>",
                                     .data[[var_name]],
                                     "<br>",
                                     .data[[to_plot]],"%",
                                     "<br>",
                                     value)),
                   shape = 1,
                   size = 2) + 
        coord_flip() + 
        theme(axis.text.y = element_text(angle = 0,size = 10,hjust = 1,vjust = 0.5),
              legend.position = "none") + 
        scale_y_continuous(labels = dollar_format(prefix = "",suffix = suf)) + 
        scale_x_discrete(limits = rev(positions)) + 
        scale_fill_manual(values = c(rep(pal[5],6),
                                     rep(pal[1],5),
                                     rep(pal[10],6),
                                     rep(pal[3],4),
                                     rep(pal[2],2),
                                     rep(pal[7],1)),
                          limits = positions) + 
        geom_point(aes(x = 3.5,
                       y = 2.15),
                   shape = 21,
                   size = 2,
                   fill = "gray") + 
        geom_point(aes(x = 2.5,
                       y = 2.15),
                   shape = 1,
                   stroke = 0.5,
                   size = 2,
                   colour =  "gray")
    
    t <- list(family = "Arial",
              size = 20)
    tf <- list(family = "Arial",
               size = 12)
    
    fin_plot <- ggplotly(out_plot, height = 500, width = 700, tooltip = c("text")) %>%
        layout(
            xaxis = list(tickfont = tf,
                         showgrid = TRUE),
            yaxis = list(tickfont = tf,
                         showgrid = FALSE),
            font = tf,
            annotations = list(
                x = c(2.25,2.25), 
                y = c(3.5,2.5),
                text = c("London","Rest of England"),
                showarrow = FALSE,
                xanchor = "left"
            )
        )
    
    fin_plot
    
    
}
