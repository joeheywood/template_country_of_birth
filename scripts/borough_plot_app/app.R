
filename <- "boroughs_21"

default_category <- "England"

var_name <- "cob"

var_name_out <- "Country of Birth"

geog <- " Borough" # either Borough or Region


# source("temp/app_template_1.R") # try to figure out a way to make "source" work

### START OF TEMPLATE

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

wd <- getwd()
setwd("../..")

to_read_dat <- paste0("report_data/",filename,".csv")

data <- fread(to_read_dat)

setwd(wd)

  ## custom data manipulation

uk_agg <- c("England","Great Britain not otherwise specified",
            "Northern Ireland","Scotland",
            "United Kingdom not otherwise specified","Wales")

data$cob[data$cob %in% uk_agg] <- "United Kingdom"

data <- data[,.(percs = sum(percs),value = sum(value)),
             by = list(geography,
                       cob_region,
                       cob)]

rem <- c(
  "Africa not otherwise specified",
  "Other Central and Western Africa",
  "Other South and Eastern Africa",
  "Other Antarctica and Oceania",
  "Other Australasia",
  "All other European countries",
  "Other Eastern Asia",
  "Other Middle East",
  "Other South-East Asia",
  "Other Southern Asia",
  "Other North America",
  "Other Caribbean"
)

data <- data[!(data$cob %in% rem),]

data <- data[data$cob != "United Kingdom",]

  ## end of custom data manipulation


ui <- fluidPage(
  
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  selectInput("cat","Choose Borough",unique(data[,"geography",])),

  plotly::plotlyOutput("plot")
  
)


server <- function(input,output,session) {
  
  output$plot <- plotly::renderPlotly({
    
    plot_cat <- input$cat
    
    plot_dat <- data[unlist(data[,"geography"]) == plot_cat,]
    
    ords <- order(plot_dat$percs, decreasing = TRUE)
    
    positions <- plot_dat[,..var_name][ords][1:20]
    
    positions <- unlist(positions)
    
    ### Making the plot
    
    out_plot <- ggplot(data = plot_dat,
                       aes(x = .data[[var_name]],y = percs, 
                           text = paste0("Country of Birth: ", .data[[var_name]],
                                         "<br>",
                                         "Count: ", value,
                                         "<br>",
                                         "Percentage: ", percs, "%"))) + 
      geom_bar(stat = "identity", fill = pal[1]) + 
      coord_flip() + 
      #scale_colour_manual(values = pal) + 
      theme(axis.text.x = element_text(angle = 0,size = 10,hjust = 1,vjust = 2)) + 
      scale_y_continuous(labels = dollar_format(prefix = "",suffix = "%")) + 
      scale_x_discrete(limits = rev(positions))
    
    tf <- list(family = "Arial",
               size = 12)
    
    ggplotly(out_plot,height = 500, width = 700, tooltip = "text") %>%
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

