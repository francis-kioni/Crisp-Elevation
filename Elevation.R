################################################################################
#                 How to create a crisp topographic map in R
################################################################################

windowsFonts(georg = windowsFont('Georgia'))

# libraries we need
libs <- c("elevatr", "terra", "tidyverse", 
          "sf", "giscoR", "marmap")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. GET COUNTRY MAP
#---------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_sf <- function(country_sf, country_transformed) {
  
  country_sf <- giscoR::gisco_get_countries(
    year = "2016",
    epsg = "4326",
    resolution = "10",
    country = "Kenya")
  
  country_transformed <- st_transform(country_sf, crs = crsLONGLAT)
  
  return(country_transformed)
}

country_transformed <- get_sf() 

# 2. GET ELEVATION DATA
#---------

get_elevation_data <- function(country_elevation, country_elevation_df) {
  
  country_elevation <- get_elev_raster(
    locations = country_transformed, 
    z = 5, # Please decrease the z value if you experience R crashing
    clip = "locations") 
  
  country_elevation_df <- as.data.frame(country_elevation, xy = T) %>%
    na.omit()
  
  colnames(country_elevation_df)[3] <- "elevation"
  
  return(country_elevation_df)
}

country_elevation_df <- get_elevation_data()

# 3. MAP
#---------


country_map <- get_elevation_map()

ggsave(filename="Kenya_topo_map.png", width=7, height=8.5, dpi = 600, device='png', country_map)
get_elevation_map <- function(country_map) {
  
  country_map <- ggplot() +
    geom_tile(data = country_elevation_df, 
              aes(x = x, y = y, fill = elevation)) +
    scale_fill_etopo() +
    coord_sf(crs = crsLONGLAT)+
    theme_minimal() +
    theme(text = element_text(family = "georg", color = "#22211d"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.grid.major = element_line(color = "white", size = 0.2),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=18, color="grey20", hjust=1, vjust=-5),
          plot.caption = element_text(size=8, color="grey70", hjust=.15, vjust=20),
          plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
          plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_rect(fill = "white", color = NA),
          panel.border = element_blank()) +
    labs(x = "", 
         y = NULL, 
         title = "Topographic map of KENYA", 
         subtitle = "Francis Kioni", 
         Credits = "©2022 Milos Popovic (https://milospopovic.net)")
  
  return(country_map)
}