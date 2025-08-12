set_di_tab <- function() {
    data <- data.frame(st_read("report_data/country_of_birth_12A_change.csv", 
                               quiet = TRUE))
    
    keep <- c(
        "lad22nm",
        "ward_name",
        "Simpson.s.standardised.2021",
        "Simpson.s.standardised.2011",
        "Simpson.s.standardised.change"
    )
    
    data <- data[,keep]
    
    data$Simpson.s.standardised.2021 <- 
        round(as.numeric(data$Simpson.s.standardised.2021), 3)
    
    data$Simpson.s.standardised.2011 <- 
        round(as.numeric(data$Simpson.s.standardised.2011), 3)
    
    data$Simpson.s.standardised.change <- 
        round(as.numeric(data$Simpson.s.standardised.change), 3)
    
    colnames(data) <- c("Borough","Ward","2021 diversity","2011 diversity","Change")
    
    tab_dat <- data
    save(tab_dat, file = "report_data/RDA/di_tab.RData")
    
}

get_di_tab <- function() {
    load("report_data/RDA/di_tab.RData")
    font.size <- "9.5pt"
    datatable(tab_dat,
              extensions = "Buttons",
              rownames = "",
              filter = "top",
              width = "75%",
              options =  list(buttons = c('csv'),
                              dom = "Brftip",
                              columnDefs = list(list(className = 'dt-center', targets = 3:5)),
                              initComplete = htmlwidgets::JS(
                                  "function(settings, json) {",
                                  paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
                                  "}"))
    )
    
}


