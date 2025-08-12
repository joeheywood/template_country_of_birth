

library(ggthemes)

data <- st_read("geo_data/country_of_birth_12A_change.shp", quiet = TRUE)

wards_simp <- st_read("geo_data/wards_simp.shp", quiet = TRUE)

colnames(data)[1] <- "gss_code_ward"

tf <- list(family = "Arial",
           size = 12)

## adding City to the simplified ward boundaries

wards_simp[wards_simp$la_distric == "City of London",]$ward_code <- "E09000001"
wards_simp[wards_simp$la_distric == "City of London",]$ward_name <- "City of London"

## adding the simpler ward boundaries

data <- data.table(data)[,-"geometry"]
setkey(data, "gss_code_ward")

wards_simp <- data.table(wards_simp)
setkey(wards_simp, "ward_code")

keep <- c("ward_code","geometry")

wards_simp <- wards_simp[,..keep]

data <- wards_simp[data]

colnames(data)[1] <- "gss_code_ward"

data <- st_as_sf(data)

data$Smps.sc <- round(data$Smps.sc,2)

map_21 <- ggplot(data = data, 
                 aes(text = paste0("Ward: ", ward_nm,
                                   "<br>",
                                   "Borough: ",lad22nm,
                                   "<br>",
                                   "Index change: ",Smps.sc))) + 
  geom_sf(aes(fill = Smps.sc,color = gss_code_ward), lwd = 0.2) + 
  theme(
    panel.ontop = TRUE,
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    text = element_blank(), 
    plot.background = element_rect(fill = "white")) + 
  scale_fill_gradient2_tableau(palette = "Orange-Blue-White Diverging",
                               limits = c(-0.25,0.25),
                               name = NULL)
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43", name = NULL)

tf <- list(family = "Arial",
           size = 12)

out_map <- ggplotly(map_21, height = 500,width = 700, tooltip = "text")

out_map <- out_map %>%
  style(hoverlabel = list(bgcolor = "white"),
        hoveron = "fills",
        traces = seq.int(2, length(out_map$x$data)),
        line.color = toRGB("gray40")) %>%
  hide_legend()

out_map

htmlwidgets::saveWidget(
  out_map,"fragments/di_map_21.html"
)


