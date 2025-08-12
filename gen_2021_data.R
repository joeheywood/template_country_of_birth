## Making fake 2021 data

### Setting up
library(data.table)

### Region-level data

reg_data_21 <- fread("report_data/regions_11.csv")

inc_facts <- rnorm(nrow(reg_data_21),mean = 1.07, sd = 0.1)

reg_data_21$value <- round(reg_data_21$value*inc_facts)

tots <- tapply(X = reg_data_21$value,
          INDEX = list(reg_data_21$geography),
          FUN = sum)

reg_data_21$total_pop <- tots[reg_data_21$geography]

reg_data_21$percs <- round(100*(reg_data_21$value/reg_data_21$total_pop),1)

fwrite(reg_data_21,
       "report_data/regions_21.csv")

### London

lond_data_21 <- fread("report_data/lond.csv")

inc_facts <- rnorm(nrow(lond_data_21),mean = 1.07, sd = 0.1)

lond_data_21$value <- round(lond_data_21$value*inc_facts)

tots <- tapply(X = lond_data_21$value,
               INDEX = list(lond_data_21$geography),
               FUN = sum)

lond_data_21$total_pop <- tots[lond_data_21$geography]

lond_data_21$percs <- round(100*(lond_data_21$value/lond_data_21$total_pop),1)

lond_data_21$percs <- c(
  43.4,
  1.3,
  0.0,
  14.6,
  0.9,
  0.6,
  1,
  1.4,
  7,
  3,
  3.8,
  1.4,
  3.6,
  7.9,
  3.5,
  0.7,
  1,
  5
)

fwrite(lond_data_21,
       "report_data/lond_21.csv")

### London vs Rest of UK

lond_uk_data_21 <- fread("report_data/lond_vs_rest_11.csv")

inc_facts <- rnorm(nrow(lond_uk_data_21),mean = 1.07, sd = 0.1)

lond_uk_data_21$value <- round(lond_uk_data_21$value*inc_facts)

tots <- tapply(X = lond_uk_data_21$value,
               INDEX = list(lond_uk_data_21$geography),
               FUN = sum)

lond_uk_data_21$total_pop <- tots[lond_uk_data_21$geography]

lond_uk_data_21$percs <- round(100*(lond_uk_data_21$value/lond_uk_data_21$total_pop),1)

fwrite(lond_uk_data_21,
       "report_data/lond_vs_rest_21.csv")


### Borough-level data

borough_data_21 <- fread("report_data/boroughs_11.csv")

inc_facts <- rnorm(nrow(borough_data_21),mean = 1.07, sd = 0.1)

borough_data_21$value <- round(borough_data_21$value*inc_facts)

tots <- tapply(X = borough_data_21$value,
               INDEX = list(borough_data_21$geography),
               FUN = sum)

borough_data_21$total_pop <- tots[borough_data_21$geography]

borough_data_21$percs <- round(100*(borough_data_21$value/borough_data_21$total_pop),1)

fwrite(borough_data_21,
       "report_data/boroughs_21.csv")


## Making the change datasets

### Region-level

regs <- fread("report_data/regions_11.csv")
regs_21 <- fread("report_data/regions_21.csv")
regs_change <- regs

regs_change$total_pop <-  
  regs_21$total_pop - regs$total_pop

regs_change$value <- 
  regs_21$value - regs$value

regs_change$percs <-
  regs_21$percs - regs$percs

fwrite(regs_change,
       "report_data/regions_change.csv")

### London

lond_uk <- fread("report_data/lond.csv")
lond_uk_21 <- fread("report_data/lond_21.csv")
lond_uk_change <- lond_uk

lond_uk_change$total_pop <-  
  lond_uk_21$total_pop - lond_uk$total_pop

lond_uk_change$value <- 
  lond_uk_21$value - lond_uk$value

lond_uk_change$percs <-
  lond_uk_21$percs - lond_uk$percs

fwrite(lond_uk_change,
       "report_data/lond_change.csv")


### London vs rest of UK

lond_uk <- fread("report_data/lond_vs_rest_11.csv")
lond_uk_21 <- fread("report_data/lond_vs_rest_21.csv")
lond_uk_change <- lond_uk

lond_uk_change$total_pop <-  
  lond_uk_21$total_pop - lond_uk$total_pop

lond_uk_change$value <- 
  lond_uk_21$value - lond_uk$value

lond_uk_change$percs <-
  lond_uk_21$percs - lond_uk$percs

fwrite(lond_uk_change,
       "report_data/lond_vs_rest_change.csv")

### Boroughs

boroughs <- fread("report_data/boroughs_11.csv")
boroughs_21 <- fread("report_data/boroughs_21.csv")
boroughs_change <- boroughs

boroughs_change$total_pop <-  
  boroughs_21$total_pop - boroughs$total_pop

boroughs_change$value <- 
  boroughs_21$value - boroughs$value

boroughs_change$percs <-
  boroughs_21$percs - boroughs$percs

fwrite(boroughs_change,
       "report_data/boroughs_change.csv")



