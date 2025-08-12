
## inputs

set_top_countries <- function() {
    file_name <- "lond_21" # name of files to be used
    
    var_name <- "cob_new" # name of variable to be plotted
    
    tooltip_var <- "Country/Region: " # name of the variable as it will appear in the tooltip
    
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
    
    ## setting up
    
    pal <- gla_pal(gla_theme = "default", n = 10)
    theme_set(theme_gla(gla_theme = "default"))
    
    ## reading in data
    
    to_read <- paste0("report_data/",file_name, ".csv")
    plot_dat <- fread(to_read)
    
    ## custom - light data manipulation (much of this should be a custom section in data processing)
    
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
    
    sum(plot_dat$percs >= 0.67) # make 0.7 the threshold
    
    plot_dat$cob_new <- "blank"
    
    plot_dat[plot_dat$percs >= 0.66,]$cob_new <- plot_dat[plot_dat$percs >= 0.66,]$cob
    
    plot_dat[plot_dat$percs < 0.66,]$cob_new <- paste0("Other"," ",plot_dat[plot_dat$percs < 0.66,]$cob_region)
    
    plot_dat[plot_dat$cob %in% subreg_agg,]$cob_new <- paste0("Other"," ",plot_dat[plot_dat$cob %in% subreg_agg,]$cob_region)
    
    fwrite(plot_dat,
           "report_data/spec_dat.csv")
    
    plot_dat <- plot_dat[,.(percs = sum(percs),value = sum(value)),
                         by = list(cob_new,
                                   cob_region)]
    
    plot_dat <- plot_dat[!(plot_dat$cob_new %in% c("England","Other Other",
                                                   "Scotland","Other United Kingdom",
                                                   "Wales","Northern Ireland")),]
    save(plot_dat, var_name, tooltip_var, positions, pal, file = "report_data/RDA/london_top_countries.RData")
    
}


### Making the plot

get_top_countries_plot <- function() {
    load("report_data/RDA/london_top_countries.RData")
    ldn_top_countries_plot <- ggplot(data = plot_dat,
                                     aes(x = .data[[var_name]],
                                         fill = .data[[var_name]],
                                         y = value,
                                         text = paste0(tooltip_var, .data[[var_name]],
                                                       "<br>",
                                                       "Percentage: ", percs,"%",
                                                       "<br>",
                                                       "Count: ", value))) + 
        geom_bar(stat = "identity") + 
        coord_flip() + 
        theme(axis.text.x = element_text(angle = 0,size = 10,hjust = 1,vjust = 2),
              legend.position = "none") + 
        scale_y_continuous(labels = dollar_format(prefix = "",suffix = "")) + 
        scale_x_discrete(limits = rev(positions)) + 
        scale_fill_manual(values = c(rep(pal[5],6),
                                     rep(pal[1],5),
                                     rep(pal[10],6),
                                     rep(pal[3],4),
                                     rep(pal[2],2),
                                     rep(pal[7],1)),
                          limits = positions)
    
    tf <- list(family = "Arial",
               size = 12)
    
    
    ggplotly(ldn_top_countries_plot,height = 500, width = 700, tooltip = "text") %>%
        layout(
            xaxis = list(tickfont = tf,
                         showgrid = TRUE),
            yaxis = list(tickfont = tf,
                         showgrid = FALSE))
    
    
}

