# create Kenya full vaccination coverage map using coverage data

# load packages
library (rgdal)
library (data.table)
library (ggplot2)
library (broom)
library (dplyr)
library (raster)
library (maptools)
library (gpclib)
library (rgeos)

# map tutorial
# https://www.r-graph-gallery.com/168-load-a-shape-file-into-r.html

# clear workspace
rm (list = ls())

# create Kenya full vaccination coverage map using coverage data
create_map <- function () {
  
  # give maptools the permission to use gpclib
  gpclibPermit ()
  
  # read Kenya admin level map data
  my_spdf <- readOGR (dsn     = "kenya_region_shapefile",
                      layer   = "kenya_region_shapefile",
                      verbose = FALSE
  )
  # 'fortify' the data to get a dataframe format required by ggplot2
  spdf_fortified <- tidy (my_spdf, region = "name_1")
  
  # coordinates and aspect ratio
  coord <- coord_quickmap (xlim = range (spdf_fortified$long), 
                           ylim = range(spdf_fortified$lat), 
                           expand = F
  )
  asp <- coord$aspect (list (x.range = range (spdf_fortified$long), 
                             y.range = range (spdf_fortified$lat)))
  
  # read Ethiopia basic vaccination coverage data (source: DHS 2016)
  coverage <- fread (file = "ken_coverage.csv")
  
  # join coverage data to map data
  shapefile.df <- right_join (spdf_fortified, coverage, by = "id")
  setDT (shapefile.df)
  
  # admin level-1 data (8 provinces in Ethiopia)
  idList <- my_spdf@data$name_1
  
  # centre of each region
  centroids.df         <- as.data.frame (coordinates(my_spdf))
  names (centroids.df) <- c ("Longitude", "Latitude")
  
  pop.df <- (data.frame(coverage, centroids.df))
  setDT (pop.df)
  
  # # minor update of region names, latitude, and longitude
  # pop.df [id == "Benshangul-Gumaz", "id"] <- "Benishangul"
  # pop.df [id == "Gambela Peoples",  "id"] <- "Gambela"
  # pop.df [id == "Harari People",    "id"] <- "Harari"
  # pop.df [id == "Southern Nations, Nationalities and Peoples",    "id"] <- "SNNPR"
  # pop.df [id == "Oromia",      "Longitude"] <- 40.4
  # pop.df [id == "Harari",      "Latitude"]  <- 9
  # pop.df [id == "Dire Dawa",   "Latitude"]  <- 10
  # pop.df [id == "Addis Ababa", "Latitude"]  <- 9.25
  # pop.df [id == "Tigray",      "Latitude"]  <- 14
  # pop.df [id == "Addis Ababa", "Longitude"] <- 38.4
  pop.df [id == "North-Eastern", "id"] <- "North Eastern"
  pop.df [id == "Eastern", "Longitude"] <- 38.65
  
  # create map
  p <- ggplot() + 
    geom_polygon (data = shapefile.df, 
                  aes (x = long, y = lat, group = group, fill = coverage), 
                  colour = "gold") +
    # labs (title = "     Full vaccination coverage among children aged 12-23 months in 8 provinces of Kenya", 
    #      subtitle = "                 (1-dose BCG, 3-dose DTP-HepB-Hib, 3-dose polio, 1-dose measles, 3-dose PCV)" ) +
    # geom_polygon (data = shapefile.df [id == "Addis Ababa"], aes(x = long, y = lat, group = group, fill = coverage), colour = "gold") +
    # geom_polygon (data = shapefile.df [id == "Harari People"], aes(x = long, y = lat, group = group, fill = coverage), colour = "gold") +
    geom_text (data = pop.df, 
               aes (x = Longitude, y = Latitude, label = id), 
               size = 6, 
               color = "gold") +
    theme_void () +
    theme (plot.title = element_text (size = 20)) +
    theme (plot.subtitle = element_text (size = 18)) +
    theme (legend.title = element_text(size = 16),
           legend.text = element_text(size = 16)) + 
    scale_fill_continuous (name  = "full \nimmunisation \ncoverage (%)", 
                           trans = "reverse", 
                           guide = guide_colourbar(reverse = TRUE))
  
  print (p)
  
  # save map figure to file
  ggsave (filename = "plot_Kenya_vaccination_coverage.jpg",
          width = 11 * 1.11,
          height = 9 * 1.15 * asp,
          units = "in",
          dpi = 600)
  
  ggsave (filename = "plot_Kenya_vaccination_coverage.eps",
          width = 11 * 1.11,
          height = 9 * 1.15 * asp,
          units = "in",
          device = cairo_ps)
  
  return (p)
  
} # end of function -- create_map


# create Ethiopia basic vaccination coverage map using coverage data from DHS 2016
create_map ()

