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
library(scales)

grid <- long_grid(x = seq(0, 1, length.out = 1000),
                  y = seq(0, 1, length.out = 1000))

#noise algorithms: https://blog.djnavarro.net/posts/2021-09-07_water-colours/
gensimplex <- function(points, frequency = .1, octaves = 1) {
  ambient::curl_noise(
    generator = ambient::fracture,
    fractal = ambient::billow,
    noise = ambient::gen_simplex,
    x = points$x,
    y = points$y,
    frequency = frequency,
    octaves = octaves,
    seed = 1,
  )
}

genwaves <- function(points, frequency = .1, octaves = 1) {
  ambient::curl_noise(
    generator = ambient::fracture,
    fractal = ambient::ridged,
    noise = ambient::gen_waves,
    x = points$x,
    y = points$y,
    frequency = frequency,
    octaves = octaves,
    seed = 1
  )
}

gencheckerboard <- function(points, frequency = .1, octaves = 1) {
  ambient::curl_noise(
    generator = ambient::fracture,
    fractal = ambient::billow,
    noise = ambient::gen_checkerboard,
    x = points$x,
    y = points$y,
    frequency = frequency,
    octaves = octaves,
    seed = 1
  )
}

genspheres <- function(points, frequency = .1, octaves = 1) {
  ambient::curl_noise(
    generator = ambient::fracture,
    fractal = ambient::billow,
    noise = ambient::gen_spheres,
    x = points$x,
    y = points$y,
    frequency = frequency,
    octaves = octaves,
    seed = 123456
  )
}

gencubic <- function(points, frequency = .1, octaves = 1) {
  ambient::curl_noise(
    generator = ambient::fracture,
    fractal = ambient::billow,
    noise = ambient::gen_cubic,
    x = points$x,
    y = points$y,
    frequency = frequency,
    octaves = octaves,
    seed = 1
  )
}

genperlin <- function(points, frequency = .1, octaves = 1) {
  ambient::curl_noise(
    generator = ambient::fracture,
    fractal = ambient::billow,
    noise = ambient::gen_perlin,
    x = points$x,
    y = points$y,
    frequency = frequency,
    octaves = octaves,
    seed = 1
  )
}

genvalue <- function(points, frequency = .1, octaves = 1) {
  ambient::curl_noise(
    generator = ambient::fracture,
    fractal = ambient::billow,
    noise = ambient::gen_value,
    x = points$x,
    y = points$y,
    frequency = frequency,
    octaves = octaves,
    seed = 1
  )
}

shift <- function(points, amount, field, ...) {
  vectors <- field(points, ...)
  points <- points %>%
    mutate(
      x = x + vectors$x * amount,
      y = y + vectors$y * amount,
      time = time + 1,
      id = id,
      shade = shade
    )
  return(points)
}

#shift coordinates
iterate <- function(pts, time, step, field, ...) {
  fieldlist <- list()
  for(i in 1:time) fieldlist[[i]] <- field
  bind_rows(
    accumulate2(
      .x = rep(step, time),
      .y = fieldlist, 
      .f = shift,
      .init = pts,
      ...
    )
  )
}

map_size <- function(x) {
  ambient::normalise(x, to = c(0.25, 1))
}

map_alpha <- function(x) {
  ambient::normalise(x, to = c(0.1, 0))
}

#load map data and save
# dc_points <- oe_read(
#   file.choose(),
#   layer = "points",
#   stringsAsFactors = FALSE,
#   max_file_size = 5e+10
# )
# save(dc_points, file = "dc.RData")
load("dc.RData")

#build bounding box
min_lon <- -77.120498; max_lon <- -76.909698
min_lat <- 38.802760; max_lat <- 38.996739

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

#build color palette and set colors
c <- c("#FFE298", "#577C5B", "#E26783")
c <- c("#58a6a6", "#421e22", "#efa355")
c <- c("#BF3413", "#FFC818", "#020202")
c <- c("#FFC818", "#fafafa", "#BDD5EA")
dcc <- dcc %>% mutate(
  shade = ifelse(s == TRUE, c[3],
                 sample(c(c[1], c[2]), nrow(dcc), replace = TRUE))
)

#iterate gencubic
pts <- dcc %>% filter(s == FALSE) %>% iterate(time = 5, step = 0.1, field = gencubic,
                                              octaves = 5, frequency = 1)
schools <- dcc %>% filter(s == TRUE) %>% iterate(time = 40, step = 0.1, field = gencubic,
                                                 octaves = 5, frequency = 1)

#iterate gensimplex
pts <- dcc %>% filter(s == FALSE) %>% iterate(time = 20, step = 0.1, field = gensimplex,
                        octaves = 30, frequency = 0.05)
schools <- dcc %>% filter(s == TRUE) %>% iterate(time = 40, step = 0.1, field = gensimplex,
                                                 octaves = 10, frequency = 0.05)

#iterate genwaves
pts <- dcc %>% filter(s == FALSE) %>% iterate(time = 20, step = 0.1, field = genwaves,
                                              octaves = 5, frequency = 0.05)
schools <- dcc %>% filter(s == TRUE) %>% iterate(time = 40, step = 0.1, field = genwaves,
                                                 octaves = 5, frequency = 0.05)

#iterate gencheckerboard
pts <- dcc %>% filter(s == FALSE) %>% iterate(time = 2, step = 0.1, field = gencheckerboard,
                                              octaves = 5, frequency = 0.05)
schools <- dcc %>% filter(s == TRUE) %>% iterate(time = 40, step = 0.1, field = gensimplex,
                                                 octaves = 5, frequency = 0.1)

#iterate genspheres
pts <- dcc %>% filter(s == FALSE) %>% iterate(time = 10, step = 0.1, field = genspheres,
                                              octaves = 2, frequency = 5)
schools <- dcc %>% filter(s == TRUE) %>% iterate(time = 20, step = 0.1, field = genwaves,
                                                 octaves = 5, frequency = 0.05)

#iterate genvalue
pts <- dcc %>% filter(s == FALSE) %>% iterate(time = 10, step = 0.1, field = genvalue,
                                              octaves = 5, frequency = 3)
schools <- dcc %>% filter(s == TRUE) %>% iterate(time = 40, step = 0.1, field = genvalue,
                                                 octaves = 5, frequency = 2)

#create TIFF
tiff("dc4.tiff", height = 6000, width = 6000, units = "px", pointsize = 12, res = 300, compression = 'lzw')

ggplot() +
  geom_point(data = pts, aes(x = x, y = y, 
                 colour = shade, 
                 size = map_size(time),
                 alpha = map_alpha(time)),
             stroke = 0) +
  geom_point(data = schools,
             aes(x = x, y = y,
                 colour = shade,
                 size = map_size(time),
                 alpha = map_alpha(time)),
             # alpha = 0.5,
             # size = 0.5,
             inherit.aes = FALSE) +
  coord_equal() +
  scale_size_identity() +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black"))

dev.off()
