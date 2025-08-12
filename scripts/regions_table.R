set_regions_table <- function() {
    ## regions table
    
    ## inputs 
    
    filename <- "regions_21"
    
    ## initial data work
    
    to_read <- paste0("report_data/",filename,".csv")
    
    data <- fread(to_read)
    
    keep <- c("geography","cob_region","cob","value","percs")
    
    data <- data[,..keep]
    
    
    ## aggregate UK countries
    
    uk <- c(
        "England",
        "Great Britain not otherwise specified",
        "Northern Ireland",
        "Scotland",
        "United Kingdom not otherwise specified",
        "Wales"
    )
    
    data$cob[data$cob %in% uk] <- "United Kingdom"
    
    data <- data[,.(value = sum(value), percs = sum(percs)),
                 by = list(
                     geography,
                     cob_region,
                     cob
                 )]
    
    colnames(data) <- c("Region","Region of Birth","Country of Birth",
                        "Count","Percentage")
    
    tab_dat <- data
    
    ## ordering
    
    tab_end_cats <- c(
        "All other European countries",
        "Other South and Eastern Africa",
        "Other Middle East",
        "Other South-East Asia",
        "Other Central and Western Africa",
        "Other Caribbean",
        "Other Southern Asia",
        "Other Eastern Asia",
        "Other Australasia",
        "Other North America",
        "Africa not otherwise specified",
        "Other Antarctica and Oceania",
        "United Kingdom not otherwise specified",
        "Great Britain not otherwise specified",
        "Other"
    )
    
    oth_ind <- grep("other",tab_dat$Country, ignore.case = TRUE)
    
    tab_dat$tag <- 0
    
    tab_dat$tag[oth_ind] <- 1
    
    tab_dat$tag[-oth_ind] <- 2
    
    tab_dat <- tab_dat[order(tab_dat$Region,tab_dat$tag,tab_dat$Percentage, decreasing = TRUE),]
    
    tab_dat <- tab_dat[,-"tag"]
    save(tab_dat, file = "report_data/RDA/regions_table.RData")
    
    
    
}

get_regions_table <- function() {
    load("report_data/RDA/regions_table.RData")
    font.size = "9.5pt"
    datatable(tab_dat,
              extensions = "Buttons",
              rownames = "",
              filter = "top",
              width = "75%",
              options =  list(buttons = c('csv'),
                              dom = "Brftip",
                              columnDefs = list(list(className = 'dt-center', targets = 4:5)),
                              initComplete = htmlwidgets::JS(
                                  "function(settings, json) {",
                                  paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
                                  "}"))
    )
    
}
