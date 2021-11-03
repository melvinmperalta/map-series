library(tidyverse)
library(sf)
library(ggplot2)
library(ambient)
library(purrr)
library(tmaptools)
library(Rcpp)
library(cowplot)
library(osmdata)
library(osmextract)

#noise algorithms: https://blog.djnavarro.net/posts/2021-09-07_water-colours/
field <- function(points, frequency = .1, octaves = 1) {
  ambient::curl_noise(
    generator = ambient::fracture,
    fractal = ambient::billow,
    noise = ambient::gen_simplex,
    x = points$x,
    y = points$y,
    frequency = frequency,
    octaves = octaves,
    seed = 1
  )
}

shift <- function(points, amount, ...) {
  vectors <- field(points, ...)
  points <- points %>%
    mutate(
      x = x + vectors$x * amount,
      y = y + vectors$y * amount,
      time = time + 1,
      id = id
    )
  return(points)
}

#shift coordinates
iterate <- function(pts, time, step, ...) {
  bind_rows(
    accumulate(
      .x = rep(step, time),
      .f = shift,
      .init = pts,
      ...
    )
  )
}

map_size <- function(x, y) {
  x * (max(y)^2 - y^2) / y^2
}

map_alpha <- function(x) {
  ambient::normalise(x, to = c(0.1, 0))
}

#load map data and save
dc_points <- oe_read(
  file.choose(),
  layer = "points",
  stringsAsFactors = FALSE,
  max_file_size = 5e+10
)
save(dc_points, file = "dc.RData")
load("dc.RData")

#build bounding box
min_lon <- -77.120498; max_lon <- -76.909698
min_lat <- 38.802760; max_lat <- 38.996739

#build color palette
c <- c("#FCD116", "#0038A8", "#CE1126")

#crop map and label educational points
dc_cropped <- st_crop(dc_points, xmin = min_lon, xmax = max_lon, ymin = min_lat, ymax = max_lat)
et <- c("\"amenity\"=>\"college\"",
        "\"amenity\"=>\"driving_school\"",
        "\"amenity\"=>\"kindergarten\"",
        "\"amenity\"=>\"language_school\"",
        "\"amenity\"=>\"library\"",
        "\"amenity\"=>\"toy_library\"",
        "\"amenity\"=>\"music_school\"",
        "\"amenity\"=>\"school\"",
        "\"amenity\"=>\"university\"")

#normalize coordinates, populate with id, time, colors, and extracted channels
dcc <- st_coordinates(dc_cropped) %>% as_tibble()
dcc <- dcc %>% rename(x = X, y = Y)
dcc$s <- grepl(paste(et, collapse="|"), dc_cropped$other_tags)
dcc$x <- normalise(dcc$x, to = c(1, 50))
dcc$y <- normalise(dcc$y, to = c(1, 50))
dcc$id <- 1:nrow(dcc)
dcc$time <- 1
dcc <- dcc %>% mutate(
  shade = ifelse(s == TRUE, c[3],
                 sample(c(c[1], c[2]), nrow(dcc), replace = TRUE))
)

#iterate map
pts <- dcc %>% filter(s == FALSE) %>% iterate(time = 20, step = 0.1, 
                        octaves = 30, frequency = 0.05)
schools <- dcc %>% filter(s == TRUE) %>% iterate(time = 40, step = 0.2,
                                                 octaves = 10, frequency = 0.05)

#create TIFF
tiff("dc.tiff", height = 6000, width = 6000, units = "px", pointsize = 12, res = 300, compression = 'lzw')

ggplot() +
  geom_point(data = pts, aes(x = x, y = y, 
                 colour = shade, 
                 size = map_size(1, time),
                 alpha = map_alpha(time)),
             shape = 46) +
  geom_point(data = schools,  
             aes(x = x, y = y, 
                 colour = shade),
             alpha = 0.5,
             size = 0.75,
             inherit.aes = FALSE) +
  coord_equal() +
  scale_size_identity() +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black"))

dev.off()
