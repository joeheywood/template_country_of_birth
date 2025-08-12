## Regions plots

filename <- "regions_21"

geog_file <- "gb_regions_simpr" ## either "gb_regions_simpr" or "london_boroughs_simp"

default_category <- "United Kingdom"

var_name <- "cob"

var_name_out <- "Country of Birth"

geog <- " Region" # either Borough or Region


# source("temp/app_template_1.R") # try to figure out a way to make "source" work

### START OF TEMPLATE

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

wd <- getwd()
setwd("../..")

to_read_dat <- paste0("report_data/",filename,".csv")

data <- fread(to_read_dat)

to_read_geog <- paste0("geo_data/",geog_file,".shp")

borders <- st_read(to_read_geog, quiet = TRUE)

setwd(wd)

borders <- data.table(borders[,c("GSS_CODE","NAME")])
setkey(borders,"GSS_CODE")

keep <- c("geography",var_name,"value","percs")

tab_dat <- data[,..keep]

colnames(tab_dat) <- c("Region",var_name_out,"Count","Percentage")


## custom data manipulation

uk <- c("England","Great Britain not otherwise specified",
        "Northern Ireland","Scotland",
        "United Kingdom not otherwise specified","Wales")

data$cob[data$cob %in% uk] <- "United Kingdom"

data <- data[,.(percs = sum(percs),value = sum(value)),
             by = list(cob,
                       cob_region,
                       geography,
                       geography_code)]


keep <- c("United Kingdom",
          "Romania",
          "Poland",
          "Italy",
          "Ireland",
          "France",
          "India",
          "Bangladesh",
          "Pakistan",
          "Sri Lanka",
          "Nigeria",
          "Somalia",
          "North Africa",
          "Ghana",
          "Kenya",
          "Other Africa",
          "Jamaica",
          "United States")

data <- data[data$cob %in% keep,]

## end of custom data manipulation



ui <- fluidPage(
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  selectInput("cat","Choose Country or sub-Region",unique(data[,..var_name]),selected = default_category),
  
  plotly::plotlyOutput("plot")
  
)

server <- function(input,output,session) {
  
  output$plot <- plotly::renderPlotly({
    plot_cat <- input$cat
    
    plot_dat <- data[unlist(data[,..var_name]) == plot_cat,]
    
    ords <- order(plot_dat$percs, decreasing = TRUE)
    
    positions <- rev(plot_dat$geography[ords])
    
    ### Making the plot
    
    out_plot <- ggplot(data = plot_dat,
                       aes(x = geography,y = percs, 
                           text = paste0("Region: ", geography,
                                         "<br>",
                                         "Count: ", value,
                                         "<br>",
                                         "Percentage: ", percs, "%"))) + 
      geom_bar(stat = "identity", fill = pal[1]) + 
      coord_flip() + 
      #scale_colour_manual(values = pal) + 
      theme(axis.text.x = element_text(angle = 0,size = 10,hjust = 1,vjust = 2)) + 
      scale_y_continuous(labels = dollar_format(prefix = "",suffix = "%")) + 
      scale_x_discrete(limits = positions)
    
    tf <- list(family = "Arial",
               size = 12)
    
    ggplotly(out_plot,height = 400, width = 700, tooltip = "text") %>%
      layout(
        xaxis = list(tickfont = tf,
                     showgrid = TRUE),
        yaxis = list(tickfont = tf,
                     showgrid = FALSE))
    
  })
  
  
}


### END OF TEMPLATE

shinyApp(
  ui = ui,
  server = server
)
