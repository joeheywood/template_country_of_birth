
# inputs

filename <- "boroughs_21"

geog_file <- "london_boroughs_simp" ## either "gb_regions_simpr" or "london_boroughs_simp"

default_category <- "United Kingdom"

var_name <- "cob"

var_name_out <- "Country of Birth"

geog <- " Borough" # either Borough or Region

plot_title <- "Plot X: Percentage born in selected Country/sub-Region in each"

map_title <- "Map X: Percentage born in selected Country/sub-Region in each"

table_title <- "Table X: Percentage born in selected Country/sub-Region in each"


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

  ## aggregate UK

unique(data$cob)

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
        by = list(cob_region,
                  cob,
                  geography_code,
                  geography)]

ui <- fluidPage(
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  selectInput("cat","Choose Country or sub-Region",unique(data[,..var_name]),selected = default_category),
  
  plotly::plotlyOutput("map")
  
)


server <- function(input,output,session){
  
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
    
    out_map <- ggplotly(map_pl, height = 500,width = 700, tooltip = "text") %>%
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

help("scale_fill_gradient")
scale_color_discrete(labels = ~paste(.x, "(S)"))

shinyApp(
  ui = ui,
  server = server
)

