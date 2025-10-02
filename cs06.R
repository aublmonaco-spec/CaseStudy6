#cs06: Wind Farm Suitability Analysis with Raster and Vector Data
#' subtitle: "Aggregating High Wind Speed Areas by County using `terra` and `tigris`"

#install.packages("terra")
#install.packages("elevatr")
#install.packages("viridis")
library(terra)
library(tidyverse)
library(sf)
library(tigris)
library(elevatr)
library(viridis)

#Using the counties funciton gets data for the New York county polygons; I had to set cb to TRUE so that it would clip the boundaries around the shorelines and not include water
ny_counties_sf <- counties(state = "NY", cb = TRUE)

#I used the rast function here to put the wind tif file into raster format
wind_raster <- rast("wind100.tif")

#The get_elev_raster function here obtains elevation data within the boundaries of the wind_raster; the zoom argument determines the raster resolution; I clipped the data to the location so it would only show within the wind_raster boundaries
elevation <- get_elev_raster(location = wind_raster, z = 5, clip = "location")

#Using the rast function here converts the elevation data just obtained into raster format
elevation_raster <- rast(elevation)

#Resampling the elevation data with the wind_raster ensures that the resolutions are compatible; I used the bilinear method since it is the default for continuous data; I saved it as a file just to see what it looks like and added the overwrite argument in case I made future changes
elevation_raster <- resample(elevation_raster, wind_raster, method = "bilinear", filename = "re_elevation.tif", overwrite = TRUE)

st_crs(ny_counties_sf)
st_crs(wind_raster)

#I used st_transform to match the counties' crs to the wind_raster's crs; I used st_crs(wind_raster) as an easier way to convert the crs of the counties file
ny_counties_new <- st_transform(ny_counties_sf, st_crs(wind_raster))
st_crs(ny_counties_new)

#Using the mask function here tells the wind-raster to only show values within the counties boundaries
wind_raster <- mask(wind_raster, ny_counties_new)

#The high_wind_raster now contains only values with wind speeds above 8 m/s from the wind_raster
high_wind_raster <- wind_raster > 8

#Resampled to ensure that the elevation_raster is now aligned with the high_wind_raster
re_elevation <- resample(elevation_raster, high_wind_raster)

#Ensured the elevation data only lies within the NY county boundaries
re_elevation <- mask(re_elevation, ny_counties_new)

#Used terrain to find the "slope" and "roughness" data of the re_elevation raster
slope <- terrain(re_elevation, "slope")
roughness <- terrain(re_elevation, "roughness")

#Used logical expressions to specify that suitable areas are contained in the high_wind_raster, in areas with a slope less than 10 degrees, and a roughness less than 100; All other values are NA
suitable_areas <- high_wind_raster & slope < 10 & roughness < 100

#Pixel_area contains the size of the suitable_area pixels in "km"
pixel_area <- cellSize(suitable_areas, unit = "km")

#multiplied the pixel_area by the original suitable_areas to get area in km^2
multiplied_raster <- pixel_area * suitable_areas

#Extract takes the area values from the raster data and applies it to the counties data; Summarize totals the amount of suitable area in each county
summary <- terra::extract(multiplied_raster, ny_counties_new, fun = sum, na.rm = TRUE)

#Can use bind_cols to join the area data from the summary to the county data because the extracted data aligns with the county order
suitable_area <- bind_cols(summary, ny_counties_new)

#Made area into a numeric column in suitable_area so that it would be possible to use a logical expression in the next step
suitable_area <- suitable_area %>%
  mutate(area = as.numeric(area))

#Made a new table (area_suitable) that includes only the area, name, and geometry columns from suitable_area, and only includes counties with suitable areas greater than 0; I used arrange to arrange the table from greatest to least suitable area
area_suitable <- suitable_area %>%
  select(c("area", "NAME", "geometry")) %>%
  filter(suitable_area$area > 0) %>%
  arrange(desc(area)) %>%
  rename(suitable_area_km2 = area)

#Used st_as_sf to convert area_suitable to an sf object and st_set_geometry to get rid of the geometry column in my new table
st_as_sf(area_suitable)
area_suitable <- area_suitable %>% 
  st_set_geometry(NULL)

#Used ggplot with geom_sf and suitable_area to show suitable areas for wind farms; I filled the map by area so it would show the most suitable places that have area with high wind speeds, suitable slopes, and suitable roughness; I added labels and titles, and I removed the x and y-axis labels
chloropleth <- ggplot() + geom_sf(data = suitable_area, aes(fill = area, geometry = geometry)) + 
  labs(title = "Potential Wind Farm Area by NY County", subtitle = "Based on average wind speeds at 100m height", caption = "Data: NREL CONUS Wind Speed & US Census Bureau", fill = expression(paste("Area with Wind Speed > 8 m/s ", ("km"^2)))) + 
  theme(panel.background = element_blank(), legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())

#Used scale_fill_viridis_c to fill the continuous data with a specified color palette
chloropleth + scale_fill_viridis_c(option = "D")