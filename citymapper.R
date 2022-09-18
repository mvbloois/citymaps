library(osmdata)
library(ggplot2)
library(stringr)
library(showtext)

# fonts
font_add_google(name = "Crimson Pro", family = "font")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# city
city <- "Gouda"

city_bb <- getbb(city)
center <- colMeans(t(city_bb))

title <- str_to_upper(city)
subtitle <- paste0(round(center[2], 2), "°N ",   round(center[1], 2), "°E\n\n")

city_roads <- city_bb %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# city_rivers <- city_bb %>%
#   opq() %>%
#   add_osm_feature(key = "waterway") %>%
#   osmdata_sf()
# 
# city_water <- city_bb %>%
#   opq() %>%
#   add_osm_feature(key = "water") %>%
#   osmdata_sf()
# 
# city_natural <- city_bb %>%
#   opq() %>%
#   add_osm_feature(key = "natural") %>%
#   osmdata_sf()


city_railway <- city_bb %>%
  opq() %>%
  add_osm_feature(key = "railway") %>%
  osmdata_sf()

map <- ggplot() +
#   geom_sf(data = city_rivers$osm_lines,
#           colour = "#7fc0ff", alpha = 0.6) +
#   geom_sf(data = city_water$osm_polygons, fill = "#7fc0ff") +
# geom_sf(data = city_natural$osm_polygons, fill = "#7fc0ff") +
  geom_sf(data = city_railway$osm_lines, 
          colour = "#6a5a00",
          alpha = 0.9) +
  geom_sf(data = city_roads$osm_lines,
          colour = "#6a5a00",
          alpha = 0.6) +
  scale_x_continuous(limits = c(city_bb[1], city_bb[3])) +
  scale_y_continuous(limits = c(city_bb[2], city_bb[4])) +
  labs(title = title,
       subtitle = subtitle,
       caption = "Source: OpenStreetMap\n@prancke") +
  theme_void() +
  theme(text = element_text(family = "font",
                            colour = "#6a5a00"),
        plot.title = element_text(face = "bold",
                                  size = 24,
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5),
        plot.margin = margin(0,1,1.2,1, unit = "cm"),
        plot.background = element_rect(fill = "#FCF5E5",
                                       colour = "#FCF5E5")
  )

ggsave(paste0("plots/", city, "_", Sys.Date(), ".png"),
       map,
       width = 6, height = 6, dpi = 300,
       device = "png")
        