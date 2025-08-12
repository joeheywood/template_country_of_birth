set_londonchange_top_countries_plot <- function() {
    ## inputs (finalise later)
    
    filename_11 <- "lond"
    filename_21 <- "lond_21_com"
    
    var_name <- "cob_new"
    
    high_var_name <- "cob_region"
    
    to_plot <- "percs"
    
    suf <- "%"
    
    
    
    ## TEMPLATE STARTS HERE
    
    to_read_11 <- paste0("report_data/",filename_11,".csv")
    to_read_21 <- paste0("report_data/",filename_21,".csv")
    
    plot_dat_11 <- fread(to_read_11)
    plot_dat_21 <- fread(to_read_21)
    
    
    
    # custom code below
    
    
    # 2021 dataset fixing
    uk <- c("England","Northern Ireland","Scotland","Wales")
    
    plot_dat_21[plot_dat_21$cob %in% uk,]$cob_region <- "United Kingdom"
    
    plot_dat_21$cob_region <- gsub("The ","",plot_dat_21$cob_region,ignore.case = FALSE)
    
    subreg_agg <- c("Africa not otherwise specified","Other Central and Western Africa",
                    "Other South and Eastern Africa","Other Antarctica and Oceania",
                    "Other Australasia","All other European countries","Other Europe non-EU",
                    "Other EU 2001-2011 members","Other EU 2001 members",
                    "Great Britain not otherwise specified",
                    "United Kingdom not otherwise specified","Other Eastern Asia","Other Middle East",
                    "Other South-East Asia","Other Southern Asia","All Central American countries","Other",
                    "Other North America","Other Caribbean")
    
    plot_dat_21$cob_new <- "blank"
    
    int_count <- plot_dat_21[plot_dat_21$percs >= 0.66,]$cob
    
    plot_dat_21[plot_dat_21$percs >= 0.66,]$cob_new <- plot_dat_21[plot_dat_21$percs >= 0.66,]$cob
    
    plot_dat_21[plot_dat_21$percs < 0.66,]$cob_new <- paste0("Other"," ",plot_dat_21[plot_dat_21$percs < 0.66,]$cob_region)
    
    plot_dat_21[plot_dat_21$cob %in% subreg_agg,]$cob_new <- paste0("Other"," ",plot_dat_21[plot_dat_21$cob %in% subreg_agg,]$cob_region)
    
    
    plot_dat_21 <- plot_dat_21[,.(percs = sum(percs),value = sum(value)),
                               by = list(cob_new,
                                         cob_region)]
    
    plot_dat_21 <- plot_dat_21[!(plot_dat_21$cob_new %in% c("England","Other Other",
                                                            "Scotland","Wales",
                                                            "Northern Ireland","Other United Kingdom")),]
    
    
    # 2011 dataset fixing (another terrible workaround - manually changing the 2011 dataset now. Fully automate later.)
    
    uk <- c("England","Northern Ireland","Scotland","Wales")
    
    plot_dat_11[plot_dat_11$cob %in% uk,]$cob_region <- "United Kingdom"
    
    plot_dat_11$cob_region <- gsub("The ","",plot_dat_11$cob_region,ignore.case = FALSE)
    
    
    
    plot_dat_11$cob_new <- "blank"
    
    plot_dat_11[plot_dat_11$cob %in% int_count,]$cob_new <- plot_dat_11[plot_dat_11$cob %in% int_count,]$cob
    
    plot_dat_11[!(plot_dat_11$cob %in% int_count),]$cob_new <- paste0("Other"," ",plot_dat_11[!(plot_dat_11$cob %in% int_count),]$cob_region)
    
    plot_dat_11[plot_dat_11$cob %in% subreg_agg,]$cob_new <- paste0("Other"," ",plot_dat_11[plot_dat_11$cob %in% subreg_agg,]$cob_region)
    
    
    plot_dat_11 <- plot_dat_11[,.(percs = sum(percs),value = sum(value)),
                               by = list(cob_new,
                                         cob_region)]
    
    plot_dat_11 <- plot_dat_11[!(plot_dat_11$cob_new %in% c("England","Other Other",
                                                            "Scotland","Wales",
                                                            "Northern Ireland","Other United Kingdom")),]
    
    
    # end of custom code below
    
    ## custom code - should automate here
    plot_dat_21$Percentage <- round(plot_dat_21$percs,2)
    plot_dat_21$`Country of Birth` <- plot_dat_21[,..var_name]
    
    plot_dat_11$Percentage <- round(plot_dat_11$percs,2)
    plot_dat_11$`Country of Birth` <- plot_dat_11[,..var_name]
    save(plot_dat_11, plot_dat_21, file = "report_data/RDA/londonchange_top_countries_plot.RData")
    
    
}


get_londonchange_top_countries_plot <- function() {
    load("report_data/RDA/londonchange_top_countries_plot.RData")
    var_name <- "Country of Birth"
    to_plot <- "Percentage"
    suf <- "%"
    
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
    ## end of custom code
    
    pal <- gla_pal(gla_theme = "default", n = 10)
    theme_set(theme_gla(gla_theme = "default"))
    
    out_plot <- ggplot() + 
        geom_segment(
            aes(x = unlist(plot_dat_11[,..var_name]), y = unlist(plot_dat_11[,..to_plot]),
                xend = unlist(plot_dat_21[,..var_name]), yend = unlist(plot_dat_21[,..to_plot])),
            colour = "gray"
        ) + 
        geom_point(data = plot_dat_21,
                   aes(x = .data[[var_name]],
                       y = .data[[to_plot]],
                       fill = .data[[var_name]],
                       text = paste0("2021",
                                     "<br>",
                                     .data[[var_name]],
                                     "<br>",
                                     .data[[to_plot]],"%",
                                     "<br>",
                                     value)),
                   shape = 21,
                   stroke = 0.5,
                   size = 2) + 
        geom_point(data = plot_dat_11,
                   aes(x = .data[[var_name]],
                       y = .data[[to_plot]],
                       fill = .data[[var_name]]),
                   shape = 21,
                   size = 2, 
                   colour = "white",
                   fill = "white") + 
        geom_point(data = plot_dat_11,
                   aes(x = .data[[var_name]],
                       y = .data[[to_plot]],
                       fill = .data[[var_name]],
                       text = paste0("2011",
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
                       y = 2.2),
                   shape = 21,
                   size = 2,
                   fill = "gray") + 
        geom_point(aes(x = 2.5,
                       y = 2.2),
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
                x = c(2.325,2.325), 
                y = c(3.5,2.5),
                text = c("2021","2011"),
                showarrow = FALSE,
                xanchor = "left"
            )
        )
    
    fin_plot
} 