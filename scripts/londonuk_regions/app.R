

## inputs

filename <- "lond_vs_rest_21"

default_category <- "Regions"

var_name <- "cob"

tooltip_var <- "Country/Region of Birth: "

suf <- "%"

## setting up

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

wd <- getwd()
setwd("../..")

to_read_dat <- paste0("report_data/",filename,".csv")

data <- fread(to_read_dat)

setwd(wd)

  ## custom code - initial small bit of data manipulation

to_change <- c("England","Great Britain not otherwise specified",
               "Northern Ireland","Scotland",
               "United Kingdom not otherwise specified",
               "Wales")

data$cob_region[data$cob %in% to_change] <- "United Kingdom"
pop <- data$total_pop[1]

data[data$cob_region == "Europe",]$cob_region <- "Rest of Europe"

data <- data[data$cob_region != "Other",]

regs_dat <- data[,.(value = sum(value)),
     by = list(data$cob_region,
            data$geography,
            data$total_pop)]

colnames(regs_dat) <- c("cob","geography","total_pop","value")

regs_dat$percs <- 100*(regs_dat$value/regs_dat$total_pop)

regs_dat$percs <- round(regs_dat$percs,1)

regs_dat[c(3,9),1] <- "Rest of Europe"

to_rem <- c("Other Australasia",
            "United Kingdom not otherwise specified",
            "Great Britain not otherwise specified")

data <- data[!(data$cob %in% to_rem),]

  ## end of custom code

tf <- list(family = "Arial",
           size = 12)

## the app


ui <- fluidPage(
  
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  selectInput("cat", "Choose Plot", c("UK & World Regions",unique(data$cob_region[data$cob_region != "United Kingdom" & data$cob_region != "Other"])), selected = default_category),
  
  plotly::plotlyOutput("plot")
  
)

server <- function(input,output,session){

output$plot <- plotly::renderPlotly({
  
  input <- input$cat
  
  # select
  if(input == "UK & World Regions"){
    plot_dat <- regs_dat
  }else{
    plot_dat <- data[data$cob_region == input]
  }
  
  # order + setup
  
  lond <- plot_dat$geography == "London"
  rest <- plot_dat$geography == "England"
  
  plot_dat$Percentage <- plot_dat$percs
  plot_dat$`Country of Birth` <- plot_dat[,..var_name]
  
  var_name <- "Country of Birth"
  to_plot <- "Percentage"

  ords <- order(plot_dat$percs[lond], decreasing = TRUE)
  
  positions <- rev(unlist(plot_dat[lond,][ords,][,..var_name]))
  
  # plot
  
  out_plot <- ggplot() + 
    geom_segment(
      aes(x = unlist(plot_dat[rest,..var_name]), y = unlist(plot_dat[rest,..to_plot]),
          xend = unlist(plot_dat[lond,..var_name]), yend = unlist(plot_dat[lond,..to_plot])),
      colour = "gray"
    ) + 
    geom_point(data = plot_dat[lond,],
               aes(x = .data[[var_name]],
                   y = .data[[to_plot]],
                   fill = .data[[var_name]],
                   text = paste0("London",
                                 "<br>",
                                 .data[[var_name]],
                                 "<br>",
                                 .data[[to_plot]],"%",
                                 "<br>",
                                 value)),
               shape = 21,
               stroke = 0.5,
               size = 2,
               fill = pal[1],
               colour = pal[1]) + 
    geom_point(data = plot_dat[rest,],
               aes(x = .data[[var_name]],
                   y = .data[[to_plot]],
                   fill = .data[[var_name]],
                   text = paste0("Rest of England",
                                 "<br>",
                                 .data[[var_name]],
                                 "<br>",
                                 .data[[to_plot]],"%",
                                 "<br>",
                                 value)),
               shape = 21, 
               size = 2,
               fill = pal[10],
               colour = pal[10]) + 
    coord_flip() + 
    theme(axis.text.y = element_text(angle = 0,size = 10,hjust = 1,vjust = 0.5),
          legend.position = "none") + 
    scale_y_continuous(labels = dollar_format(prefix = "",suffix = suf)) + 
    scale_x_discrete(limits = positions) + 
    scale_fill_manual(values = rep(pal[1], length(unique(unlist(plot_dat[,..var_name])))),
                      limits = rev(positions))

  fin_plot <- ggplotly(out_plot, height = 500, width = 700, tooltip = c("text")) %>%
    layout(
      xaxis = list(tickfont = tf,
                   showgrid = TRUE),
      yaxis = list(tickfont = tf,
                   showgrid = FALSE),
      font = tf
    )
  
  fin_plot
  
})
  
}

shinyApp(ui = ui,
         server = server,
         options = list(width = 750, 
                        height = 550))


