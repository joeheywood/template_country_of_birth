
set_london_table <- function() {
    
    
    file_name <- "lond_21"
    
    var_name <- "cob"
    
    var_name_out <- "Country"
    
    high_var_name <- "cob_region"
    
    high_var_name_out <- "Region"
    
    
    ## reading in, setting up data
    
    to_read <- paste0("report_data/",file_name,".csv")
    
    data <- fread(to_read)
    
    
    if(high_var_name != ""){
        keep <- c(high_var_name, var_name, "value","percs")
    }else{
        keep <- c(var_name,"value","percs")
    }
    
    tab_dat <- data[,..keep]
    
    ords <- order(tab_dat$percs,decreasing = TRUE)
    
    tab_dat <- tab_dat[ords,]
    
    if(high_var_name_out != ""){
        colnames(tab_dat) <- c(high_var_name_out,var_name_out,
                               "Count","Percentage")
    }else{
        colnames(tab_dat) <- c(var_name_out,
                               "Count","Percentage")
    }
    
    font.size = "9.5pt"
    
    ## ordering
    
    oth_ind <- grep("other",tab_dat$Country,ignore.case = TRUE)
    
    tab_end_dat <- tab_dat[oth_ind,]
    
    tab_top_dat <- tab_dat[-oth_ind,]
    
    tab_dat <- rbind(tab_top_dat, tab_end_dat)
    save(tab_dat, file = "report_data/RDA/tab_dat.RData")
    
    
}

get_london_table <- function(){
    load("report_data/RDA/tab_dat.RData")
    tab_dat
}


