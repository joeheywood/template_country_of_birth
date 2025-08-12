## make a stacked barchart

## x is Borough, y is percs (remember axis flip), and fill is cob, narrowed to a region

## inputs (finalise later)

filename <- "boroughs_21"

var_name <- "cob"

to_plot <- "percs"

suf <- "%"

## TEMPLATE STARTS HERE

to_read <- paste0("report_data/",filename,".csv")

data <- fread(to_read)

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

  ## custom data manipulation
unique(data$cob)

uk_agg <- c("England","Great Britain not otherwise specified",
            "Northern Ireland","Scotland",
            "United Kingdom not otherwise specified","Wales")

data$cob[data$cob %in% uk_agg] <- "United Kingdom"

data <- data[,.(percs = sum(percs),value = sum(value)),
     by = list(geography,
               cob_region,
               cob)]

  ## end of custom data manipulation 
unique(data$cob_region)

plot_dat <- data[data$cob_region == "Middle East and Asia",]

regs <- plot_dat[,.(percs = sum(percs)),
         by = list(cob_region,
                   geography)]

ords <- order(regs$percs, decreasing = TRUE)

positions <- regs[ords,]$geography

cols <- c(pal[10],pal[3],pal[1],pal[2],pal[5])

out_plot <- ggplot(data = plot_dat,
                   aes(x = geography,
                       y = .data[[to_plot]],
                       fill = .data[[var_name]],
                       text = paste0(geography,"<br>",
                                     .data[[var_name]],"<br>",
                                     .data[[to_plot]],"%"))) + 
  geom_bar(position = "stack", stat = "identity") + 
  coord_flip() + 
  theme(axis.text.y = element_text(angle = 0,size = 10,hjust = 1,vjust = 1)) + 
  scale_y_continuous(labels = dollar_format(prefix = "",suffix = suf)) + 
  scale_x_discrete(limits = rev(positions))# +
  #scale_fill_manual(values = c(
   # cols
  #)) 


ggplotly(out_plot,tooltip = "text") %>% hide_legend()



