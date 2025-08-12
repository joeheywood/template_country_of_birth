

## inputs

filename_11 <- "lond"
filename_21 <- "lond_21_com"

default_category <- "Regions"

var_name <- "cob"

tooltip_var <- "Country/Region of Birth: "

suf <- "%"


## setting up

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

t <- list(family = "Arial",
          size = 20)
tf <- list(family = "Arial",
           size = 12)

wd <- getwd()
setwd("../..")

to_read_dat_11 <- paste0("report_data/",filename_11,".csv")
to_read_dat_21 <- paste0("report_data/",filename_21,".csv")

data_11 <- fread(to_read_dat_11)
data_21 <- fread(to_read_dat_21)

setwd(wd)

  ## custom code - initial small bit of data manipulation

to_change <- c("England","Great Britain not otherwise specified",
               "Northern Ireland","Scotland",
               "United Kingdom not otherwise specified",
               "Wales")
  # 2011
data_11$cob_region[data_11$cob %in% to_change] <- "United Kingdom"
pop <- data_11$total_pop[1]

data_11[data_11$cob_region == "Europe",]$cob_region <- "Rest of Europe"

data_11 <- data_11[data_11$cob_region != "Other",]

regs_dat_11 <- data_11[,.(value = sum(value)),
     by = list(data_11$cob_region,
            data_11$geography,
            data_11$total_pop)]

colnames(regs_dat_11) <- c("cob","geography","total_pop","value")

regs_dat_11$percs <- 100*(regs_dat_11$value/regs_dat_11$total_pop)

regs_dat_11$percs <- round(regs_dat_11$percs,1)

regs_dat_11[3,1] <- "Rest of Europe"

to_rem <- c("Other Australasia",
            "United Kingdom not otherwise specified",
            "Great Britain not otherwise specified")

data_11 <- data_11[!(data_11$cob %in% to_rem),]

  # 2021
data_21$cob_region[data_21$cob %in% to_change] <- "United Kingdom"
pop <- data_21$total_pop[1]

data_21[data_21$cob_region == "Europe",]$cob_region <- "Rest of Europe"

data_21 <- data_21[data_21$cob_region != "Other",]

regs_dat_21 <- data_21[,.(value = sum(value)),
                 by = list(data_21$cob_region,
                           data_21$geography,
                           data_21$total_pop)]

colnames(regs_dat_21) <- c("cob","geography","total_pop","value")

regs_dat_21$percs <- 100*(regs_dat_21$value/regs_dat_21$total_pop)

regs_dat_21$percs <- round(regs_dat_21$percs,1)

regs_dat_21[3,1] <- "Rest of Europe"

to_rem <- c("Other Australasia",
            "United Kingdom not otherwise specified",
            "Great Britain not otherwise specified")

data_21 <- data_21[!(data_21$cob %in% to_rem),]

  ## end of custom code


## the app

ui <- fluidPage(
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  selectInput("cat", "Choose Plot", c("UK & World Regions",unique(data_21$cob_region[data_21$cob_region != "United Kingdom" & data_21$cob_region != "Other"])), selected = default_category),
  
  plotly::plotlyOutput("plot")
  
)

server <- function(input,output,session){

output$plot <- plotly::renderPlotly({

  input <- input$cat
  
  # select
  if(input == "UK & World Regions"){
    plot_dat_11 <- regs_dat_11
  }else{
    plot_dat_11 <- data_11[data_11$cob_region == input]
  }
  
  if(input == "UK & World Regions"){
    plot_dat_21 <- regs_dat_21
  }else{
    plot_dat_21 <- data_21[data_21$cob_region == input]
  }
  
  # order + setup
  
  plot_dat_21$Percentage <- plot_dat_21$percs
  plot_dat_21$`Country of Birth` <- plot_dat_21[,..var_name]
  
  plot_dat_11$Percentage <- plot_dat_11$percs
  plot_dat_11$`Country of Birth` <- plot_dat_11[,..var_name]
  
  var_name <- "Country of Birth"
  to_plot <- "Percentage"
  
  ords <- order(plot_dat_21$percs, decreasing = TRUE)
  
  plot_dat_21 <- plot_dat_21[ords,]
  
  positions <- rev(unlist(plot_dat_21[,..var_name]))
  
  plot_dat_11 <- plot_dat_11[ords,]

  # plot
  
  out_plot <- ggplot() + 
    geom_segment(
      aes(x = unlist(plot_dat_11[,..var_name]), y = unlist(plot_dat_11[,..to_plot]),
          xend = unlist(plot_dat_21[,..var_name]), yend = unlist(plot_dat_21[,..to_plot])),
      colour = "gray"
    ) + 
    geom_point(data = plot_dat_21,
               aes(x = .data[[var_name]],
                   y = .data[[to_plot]],
                   fill = .data[[var_name]],
                   text = paste0("2021",
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
    geom_point(data = plot_dat_11,
               aes(x = .data[[var_name]],
                   y = .data[[to_plot]],
                   fill = .data[[var_name]],
                   text = paste0("2011",
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
    scale_x_discrete(limits = positions)

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

