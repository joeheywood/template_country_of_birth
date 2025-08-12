## Initial data processing
## This whole thing should be automated with just a few inputs from the user. Shouldn't be too difficult.

rm(list = ls())

# 18-CATEGORY DATA # 

## REGION DATA ## 

library(gglaplot)
library(dplyr)
library(tidyverse)
library(plotly)
library(data.table)
library(scales)

regions <- fread("input_data/regions_18.csv")

## fixing up names, removing sub-totals, pivoting to wide dataset

colnames(regions) <- gsub("Ethnic Group: ","",colnames(regions))
colnames(regions) <- gsub("; measures: Value","",colnames(regions))
colnames(regions) <- gsub(" ","_",colnames(regions))

#rem <- grep("total",colnames(regions),ignore.case = TRUE)

rem <- c("White","Mixed","Asian","Black","Other")

regions <- regions[,-..rem]

colnames(regions)[5] <- "total_pop"

names_vec <- colnames(regions)[6:ncol(regions)]

reg_long <- pivot_longer(data = regions,
                         cols = names_vec)

reg_long <- data.table(reg_long)

## shortening names
reg_long$name <- 
  gsub("Mixed/multiple_ethnic_group","Mixed",reg_long$name)

reg_long$name <- 
  gsub("Asian/Asian_British","Asian",reg_long$name)

reg_long$name <- 
  gsub("Black/African/Caribbean/Black_British","Black",reg_long$name)

reg_long$name <- 
  gsub("Other_ethnic_group","Other",reg_long$name)

reg_long$name <- 
  gsub("English/Welsh/Scottish/Northern_Irish/British","British",reg_long$name)

reg_long$name <- 
  gsub("Any_other_ethnic_group","Any_other",reg_long$name)


## splitting the variables into categories
split_names <- tstrsplit(x = reg_long$name,
                         split = ":_")

reg_long$eth_group <- split_names[[1]]
reg_long$eth <- split_names[[2]]
reg_long$eth_all <- reg_long$name

#reg_long[is.na(reg_long$lang),]$lang <- reg_long[is.na(reg_long$lang),]$lang_group


## creating percentages

reg_long$percs <- 100*(reg_long$value/reg_long$total_pop)
reg_long$percs <- round(reg_long$percs,1)

reg_long$eth <- gsub("_"," ",reg_long$eth)
reg_long$eth_all <- gsub("_"," ",reg_long$eth_all)

reg_long <- reg_long[,-"name"]

## getting rid of wales

reg_long <- reg_long[reg_long$geography != "Wales",]

## writing the dataset

fwrite(x = reg_long,
       file = "report_data/regions_11.csv")


## LONDON DATA vs England and Wales ##

lond_long <- reg_long[reg_long$geography == "London",]

fwrite(x = lond_long,
       file = "report_data/lond.csv")

rest_long <- reg_long[reg_long$geography != "London"]

to_agg <- c("date","Rural_Urban","name","lang_group","lang")

list(data.frame(rest_long[,..to_agg]))

rest_agg <- data.table(aggregate(x = rest_long$value,
                      by = list(rest_long$date,
                                rest_long$Rural_Urban,
                                rest_long$eth_all,
                                rest_long$eth_group,
                                rest_long$eth),
                      FUN = sum))

colnames(rest_agg) <- c("date","Rural_Urban","eth_all","eth_group","eth","value")

rest_agg$geography <- "England and Wales (excluding London)"
rest_agg$geography_code <- NA

rest_agg$total_pop <- sum(unique(reg_long$total_pop[reg_long$geography != "London"]))

rest_agg$percs <- 100*(rest_agg$value/rest_agg$total_pop)

col_ords <- colnames(lond_long)

rest_agg <- rest_agg[,..col_ords]

colnames(lond_long) == colnames(rest_agg) # checking that they're matched and aligned correctly

lond_vs_rest <- rbind(lond_long,rest_agg)

lond_vs_rest$percs <- round(lond_vs_rest$percs,1)

lond_vs_rest$geography[lond_vs_rest$geography == "England and Wales (excluding London)"] <- "England & Wales"

fwrite(x = lond_vs_rest,
       file = "report_data/lond_vs_rest_11.csv")

## BOROUGH DATA ## 

## Reading in data, narrowing to London
lookup <- read.csv("geo/oa_ward_address_lookup_2021.csv")

las_london <- unique(lookup$gss_code_la)

las <- fread("input_data/las_18.csv")

las_london <- las[las$`geography code` %in% las_london,]


## fixing up names, making non-English, pivoting to wide dataset

colnames(las_london) <- gsub("Ethnic Group: ","",colnames(las_london))
colnames(las_london) <- gsub("; measures: Value","",colnames(las_london))
colnames(las_london) <- gsub(" ","_",colnames(las_london))

#rem <- grep("total",colnames(las_london),ignore.case = TRUE)

rem <- c("White","Mixed","Asian","Black","Other")

las_london <- las_london[,-..rem]

colnames(las_london)[5] <- "total_pop"

names_vec <- colnames(las_london)[6:ncol(las_london)]

las_long <- pivot_longer(data = las_london,
                         cols = names_vec)

las_long <- data.table(las_long)


## shortening names
las_long$name <- 
  gsub("Mixed/multiple_ethnic_group","Mixed",las_long$name)

las_long$name <- 
  gsub("Asian/Asian_British","Asian",las_long$name)

las_long$name <- 
  gsub("Black/African/Caribbean/Black_British","Black",las_long$name)

las_long$name <- 
  gsub("Other_ethnic_group","Other",las_long$name)

las_long$name <- 
  gsub("English/Welsh/Scottish/Northern_Irish/British","British",las_long$name)

las_long$name <- 
  gsub("Any_other_ethnic_group","Any_other",las_long$name)


## splitting the variables into categories
## prob need a function

split_names <- tstrsplit(x = las_long$name,
                         split = ":_")

las_long$eth_group <- split_names[[1]]
las_long$eth <- split_names[[2]]
las_long$eth_all <- las_long$name

#las_long[is.na(las_long$lang),]$lang <- las_long[is.na(las_long$lang),]$lang_group


## creating percentages

las_long$percs <- 100*(las_long$value/las_long$total_pop)
las_long$percs <- round(las_long$percs,1)

las_long$eth <- gsub("_"," ",las_long$eth)
las_long$eth_all <- gsub("_"," ",las_long$eth_all)

las_long <- las_long[,-"name"]

## writing the dataset

fwrite(x = las_long,
       file = "report_data/boroughs_11.csv")


