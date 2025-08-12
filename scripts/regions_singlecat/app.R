
filename <- "regions_21"

geog_file <- "gb_regions_simpr" ## either "gb_regions_simpr" or "london_boroughs_simp"

default_category <- "England"

var_name <- "cob"

var_name_out <- "Country of Birth"

geog <- " Region" # either Borough or Region

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

plot_text <- paste0(
  plot_title,
  geog
)

tab_text <- paste0(
  table_title,
  geog
)

map_text <- paste0(
  map_title,
  geog
)

cap_text <- paste0("Source: 2011 Census, ONS",
                   br(),
                   "Crown Copyright. Ordanance Survey 100032216 GLA")

ui <- fluidPage(
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  tabsetPanel(
    tabPanel("Plot",
             fluidRow(column(br(),div(plot_text,style="font-family: arial; font-size:13pt; font-weight:bold"),width = 8)),
             fluidRow(column(plotly::plotlyOutput("plot"),width = 12)),
             fluidRow(column(br(),br(),br(),br(),
                             div("Source: 2011 Census, ONS",
                                 br(),
                                 "Crown Copyright. Ordanance Survey 100032216 GLA",
                                 style = "font-family: arial; font-size:9.5pt"),width = 8))),
    
    tabPanel("Map",
             fluidRow(column(br(),div(map_text,style="font-family: arial; font-size:13pt; font-weight:bold"),width = 8)),
             fluidRow(column(plotly::plotlyOutput("map"),width = 12)),
             fluidRow(column(br(),br(),br(),br(),
                             div("Source: 2011 Census, ONS",
                                 br(),
                                 "Crown Copyright. Ordanance Survey 100032216 GLA",
                                 style = "font-family: arial; font-size:9.5pt"),width = 8))),
    
    tabPanel("Table",
             fluidRow(column(br(),div(tab_text,style="font-family: arial; font-size:13pt; font-weight:bold"),br(),width = 8)),
             fluidRow(column(DT::DTOutput("table"),width = 12)),
             fluidRow(column(
               div("Source: 2011 Census, ONS",
                   br(),
                   "Crown Copyright. Ordanance Survey 100032216 GLA",
                   style = "font-family: arial; font-size:9.5pt"),width = 8)))
  ),
  
  fluidRow(column(
    br(),
    selectInput("cat","Choose Category",unique(data[,..var_name]),selected = default_category),
    width = 12)),
  
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
    
    ggplotly(out_plot,height = 500, width = 700, tooltip = "text") %>%
      layout(
        xaxis = list(tickfont = tf,
                     showgrid = TRUE),
        yaxis = list(tickfont = tf,
                     showgrid = FALSE))
    
  })
  
  
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
      scale_fill_gradient(low = "#56B1F7", high = "#132B43", name = NULL)
    
    out_map <- ggplotly(map_pl, height = 500,width = 700, tooltip = "text") %>%
      layout(
        xaxis = list(tickfont = tf),
        yaxis = list(tickfont = tf))
      
      out_map %>% style(hoveron = "fills",
                        traces = seq.int(2, length(out_map$x$data)),
                        line.color = toRGB("gray40")) %>%
        hide_legend()
    
  })
  
  
  output$table <- DT::renderDataTable({
    
    tab_dat[unlist(tab_dat[,..var_name_out]) == input$cat,]
    
  })
  
}


### END OF TEMPLATE

shinyApp(
  ui = ui,
  server = server
)
