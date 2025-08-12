
set_year_of_arrival_table <- function() {
    ## year of arrival plot
    
    ## inputs
    filename <- "lond_21_yoa"
    
    var_name <- "yoa"
    tooltip_var <- "Year of Arrival: "
    
    ## setting up
    pal <- gla_pal(gla_theme = "default", n = 10)
    theme_set(theme_gla(gla_theme = "default"))
    
    
    ## reading in the data
    
    to_read <- paste0("report_data/",filename,".csv")
    
    tab_dat <- fread(to_read)
    
    ## small bit of data work - rounding, selecting, finalising
    
    tab_dat$percs <- round(tab_dat$percs,1)
    
    keep <- c("yoa","value","percs")
    
    tab_dat <- tab_dat[,..keep]
    
    colnames(tab_dat) <- c("Year of Arrival","Count","Percentage")
    
    tab_dat <- tab_dat[c(12,11,1:10),]
    save(tab_dat, file = "report_data/RDA/year_of_arrival_tab.RData")
    
    
}

get_year_of_arrival_table <- function() {
    load("report_data/RDA/year_of_arrival_tab.RData")
    font.size = "9.5pt"
    datatable(tab_dat,
              extensions = "Buttons",
              rownames = "",
              filter = "top",
              width = "75%",
              options =  list(buttons = c('csv'),
                              dom = "Brftip",
                              columnDefs = list(list(className = 'dt-center', targets = 2:3)),
                              initComplete = htmlwidgets::JS(
                                  "function(settings, json) {",
                                  paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
                                  "}"))
    )
    
}

