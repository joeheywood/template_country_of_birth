
## inputs

filename <- "lond_21"

default_category <- "Regions"

selvar_name <- "cob_region"

var_name <- "cob"

tooltip_var <- "Country/Region of Birth: "


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
     by = cob_region]

regs_dat$percs <- 100*(regs_dat$value/pop)

regs_dat$percs <- round(regs_dat$percs,1)

colnames(regs_dat)[1] <- "cob"

regs_dat[3,1] <- "Rest of Europe"

to_rem <- c("Other Australasia",
            "United Kingdom not otherwise specified",
            "Great Britain not otherwise specified")

data <- data[!(data$cob %in% to_rem),]

  ## end of custom code

## the app

ui <- fluidPage(
 
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  selectInput("cat", "Choose Plot", c("UK & World Regions",unique(data$cob_region)), selected = default_category),
  
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
  
  # order
  ords <- order(plot_dat$percs, decreasing = TRUE)
  
  plot_dat <- plot_dat[ords,]
  
  positions <- unlist(plot_dat[,..var_name])
  
  # plot
  out_plot <- ggplot(data = plot_dat,
                     aes(x = .data[[var_name]],y = percs,
                         text = paste0(tooltip_var, .data[[var_name]],
                                       "<br>",
                                       "Percentage: ", percs,"%",
                                       "<br>",
                                       "Count: ", value))) + 
    geom_bar(stat = "identity", fill = pal[1]) + 
    coord_flip() + 
    #scale_colour_manual(values = pal) + 
    theme(axis.text.x = element_text(angle = 0,size = 10,hjust = 1,vjust = 2)) + 
    scale_y_continuous(labels = dollar_format(prefix = "",suffix = "%")) + 
    scale_x_discrete(limits = rev(positions))
  
  tf <- list(family = "Arial",
             size = 12)
  
  fin_plot <- ggplotly(out_plot,height = 500, width = 700, tooltip = "text") %>%
    layout(
      xaxis = list(tickfont = tf,
                   showgrid = TRUE),
      yaxis = list(tickfont = tf,
                   showgrid = FALSE))
  
  fin_plot
  
})
  

}

shinyApp(ui = ui,
         server = server,
         options = list(width = 750, 
                        height = 550))





