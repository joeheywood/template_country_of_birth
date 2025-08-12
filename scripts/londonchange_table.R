set_londonchange_table <- function() {
    ## inputs
    
    filename_11 <- "lond"
    
    filename_21 <- "lond_21_com"
    
    
    ## 
    
    to_read_11 <- paste0("report_data/",filename_11,".csv")
    
    to_read_21 <- paste0("report_data/",filename_21,".csv")
    
    data_11 <- fread(to_read_11)
    
    data_21 <- fread(to_read_21)
    
    keep <- c("cob_region","cob","percs","value")
    
    data_11 <- data_11[,..keep]
    
    data_21 <- data_21[,..keep]
    
    data_21$cob_region == data_11$cob_region
    
    data_21$cob == data_11$cob
    
    data_both <- data_21
    
    colnames(data_both)[3:4] <- c("percs_21","value_21")
    
    data_both$percs_11 <- data_11$percs
    
    data_both$value_11 <- data_11$value
    
    # aggregate UK countries
    uk <- c(
        "England",
        "Great Britain not otherwise specified",
        "Northern Ireland",
        "Scotland",
        "United Kingdom not otherwise specified",
        "Wales"
    )
    
    colnames(data_both)
    data_both$cob[data_both$cob %in% uk] <- "United Kingdom"
    
    data_both <- data_both[,.(percs_21 = sum(percs_21),
                              value_21 = sum(value_21),
                              percs_11 = sum(percs_11),
                              value_11 = sum(value_11)),
                           by = list(
                               cob_region,
                               cob
                           )]
    
    data_both$percs_change <- round((data_both$percs_21 - data_both$percs_11),2)
    
    data_both$value_change <- data_both$value_21 - data_both$value_11
    
    colnames(data_both) <- c(
        "Region of Birth",
        "Country of Birth",
        "2021 percentage",
        "2021 count",
        "2011 percentage",
        "2011 count",
        "% point change",
        "Count change"
    )
    
    data_both <- data_both[,c(1,2,3,5,7,4,6,8)]
    
    tab_dat <- data_both
    
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
    
    tab_end_dat <- tab_dat[oth_ind,]
    
    tab_end_dat <- tab_end_dat[order(tab_end_dat$`2021 percentage`, decreasing = TRUE),]
    
    tab_top_dat <- tab_dat[-oth_ind,]
    
    tab_top_dat <- tab_top_dat[order(tab_top_dat$`2021 percentage`, decreasing = TRUE),]
    
    tab_dat <- rbind(tab_top_dat, tab_end_dat)
    save(tab_dat, file = "report_data/RDA/londonchange_table.RData")
    
}

get_londonchange_table <- function() {
    load("report_data/RDA/londonchange_table.RData")
    font.size <- "9.5pt"
    
    datatable(tab_dat,
              extensions = "Buttons",
              rownames = "",
              filter = "top",
              width = "100%",
              options =  list(buttons = c('csv'),
                              dom = "Brftip",
                              columnDefs = list(list(className = 'dt-center', targets = 3:8)),
                              initComplete = htmlwidgets::JS(
                                  "function(settings, json) {",
                                  paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
                                  "}"))
    )
}
