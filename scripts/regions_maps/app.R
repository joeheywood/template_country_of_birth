

## Regions maps

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
  
  plotly::plotlyOutput("map")
  
)

server <- function(input,output,session) {
  
  output$map <- plotly::renderPlotly({
    map_cat <- input$cat
    
    map_dat <- data[unlist(data[,..var_name]) == map_cat,]
    
    setkey(map_dat,"geography_code")
    
    map_dat <- st_as_sf(borders[map_dat])
    
    #### Making the map
    
    tf <- list(family = "Arial",
               size = 12)
    
    map_pl <- ggplot(data = map_dat, aes(text = paste("Area:",NAME))) + 
      geom_sf(aes(fill = percs, color = geography,
                  text = paste0("Region: ", geography,
                                "<br>",
                                "Count: ", value,
                                "<br>",
                                "Percentage: ", percs, "%"))) + 
      theme(
        panel.ontop = TRUE,
        panel.grid = element_blank(), 
        line = element_blank(), 
        rect = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        text = element_blank(), 
        legend.title = element_blank(),
        plot.background = element_rect(fill = "white")) + 
      labs(caption = "Source: 2011 Census, ONS
                  Crown Copyright. Ordanance Survey 100032216 GLA",
           title = "") + 
      scale_fill_gradient(low = "#56B1F7", high = "#132B43", name = NULL, labels = ~paste0(.x, "%"))
    
    out_map <- ggplotly(map_pl, height = 500,width = 600, tooltip = "text") %>%
      layout(
        xaxis = list(tickfont = tf),
        yaxis = list(tickfont = tf))
    
    out_map %>% style(hoverlabel = list(bgcolor = "white"),
                      hoveron = "fills",
                      traces = seq.int(2, length(out_map$x$data)),
                      line.color = toRGB("gray40")) %>%
      hide_legend()
    
  })
  
}

help(style)

### END OF TEMPLATE

shinyApp(
  ui = ui,
  server = server
)


