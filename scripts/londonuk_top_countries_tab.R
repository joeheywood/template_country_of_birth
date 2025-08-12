
set_londonuk_top_countries_tab <- function() {
    file_name <- "lond_vs_rest_21"
    
    var_name <- "cob"
    
    var_name_out <- "Country"
    
    high_var_name <- "cob_region"
    
    high_var_name_out <- "Region"
    
    
    ## reading in, setting up data
    
    to_read <- paste0("report_data/",file_name,".csv")
    
    data <- fread(to_read)
    
    if(high_var_name != ""){
        keep <- c(high_var_name, var_name, "geography","value","percs")
    }else{
        keep <- c(var_name,"geography","value","percs")
    }
    
    tab_dat <- data[,..keep]
    
    if(high_var_name_out != ""){
        colnames(tab_dat) <- c(high_var_name_out,var_name_out,"Geography",
                               "Count","Percentage")
    }else{
        colnames(tab_dat) <- c(var_name_out,"Geography",
                               "Count","Percentage")
    }
    
    font.size = "9.5pt"
    
    
    ## aggregating the UK
    
    uk <- c(
        "England",
        "Great Britain not otherwise specified",
        "Northern Ireland",
        "Scotland",
        "United Kingdom not otherwise specified",
        "Wales"
    )
    
    tab_dat$Country[tab_dat$Country %in% uk] <- "United Kingdom"
    
    tab_dat <- tab_dat[,.(Count = sum(Count), Percentage = sum(Percentage)),
                       by = list(
                           Region,
                           Country,
                           Geography
                       )]
    
    ## changing geography to wide
    
    tab_dat <- data.table(pivot_wider(
        data = tab_dat,
        names_from = Geography,
        values_from = c(Count, Percentage)
    ))
    
    colnames(tab_dat)[3:6] <- c("London Count","Rest of England Count",
                                "London Percentage"," Rest of England Percentage")
    
    
    ## ordering
    
    oth_ind <- grep("other",tab_dat$Country,ignore.case = TRUE)
    
    tab_end_dat <- tab_dat[oth_ind,]
    
    tab_end_dat <- tab_end_dat[order(tab_end_dat$`London Percentage`, decreasing = TRUE),]
    
    tab_top_dat <- tab_dat[-oth_ind,]
    
    tab_top_dat <- tab_top_dat[order(tab_top_dat$`London Percentage`, decreasing = TRUE),]
    
    tab_dat <- rbind(tab_top_dat, tab_end_dat)
    save(tab_dat, file = "report_data/RDA/londonuk_top_countries_tab.RData")
    
}

get_londonuk_top_countries_tab <- function() {
    font.size = "9.5pt"
    load("report_data/RDA/londonuk_top_countries_tab.RData")
    datatable(tab_dat,
              extensions = "Buttons",
              rownames = "",
              filter = "top",
              width = "100%",
              options =  list(buttons = c('csv'),
                              dom = "Brftip",
                              columnDefs = list(list(className = 'dt-center', targets = 3:6)),
                              initComplete = htmlwidgets::JS(
                                  "function(settings, json) {",
                                  paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
                                  "}"))
    )
    
}




