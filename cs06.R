#cs06: Wind Farm Suitability Analysis with Raster and Vector Data
#' subtitle: "Aggregating High Wind Speed Areas by County using `terra` and `tigris`"

#install.packages("terra")
#install.packages("elevatr")
library(terra)
library(tidyverse)
library(sf)
library(tigris)
library(elevatr)

ny_counties_sf <- counties(state = "NY")

wind_raster <- rast("wind100.tif")
elevation <- get_elev_raster(location = wind_raster, z = 5, clip = "location")

#' Then use `rast()` to convert the downloaded raster to a `terra` object and `resample()` to align it with the wind raster.  Resampling a raster is necessary when the two rasters have different resolutions or extents, ensuring that they can be accurately compared or combined in subsequent analyses.
elevation_raster <- rast(elevation)
resample(elevation_raster, wind_raster, method = "bilinear", filename = "re_elevation.tif", overwrite = TRUE)

#' An equal-area projection is ideal for calculating area, so we will transform the county vector data to match the raster's CRS.

st_crs(ny_counties_sf)
st_crs(wind_raster)

ny_counties_sf <- st_transform(ny_counties_sf, st_crs(wind_raster))
st_crs(ny_counties_sf)

wind_raster <- mask(wind_raster, ny_counties_sf)
high_wind_raster <- wind_raster > 8
terra::plot(high_wind_raster)

re_elevation <- resample(elevation_raster, wind_raster)
re_elevation <- mask(re_elevation, ny_counties_sf)

slope <- terrain(re_elevation, "slope")
roughness <- terrain(re_elevation, "roughness")
terra::plot(slope)
#' ## Find suitable areas for Wind Farms
#' 
#' Locate pixels with high wind speed, low slope (<10), and low roughness (<100). Note that this is a very simplistic model and real-world suitability analysis would be more complex. 
#' 
#' Note you can combine multiple logical conditions using the `&` operator. The result will be a binary raster where cells meeting all criteria are `TRUE` (or 1) and others are `FALSE` (or 0). For example, `x > 5 & y < 10` will return `TRUE` only for cells where both conditions are met. You can work with `rast` objects directly using these logical operations.
#' 
suitable_areas <- high_wind_raster & slope < 10 & roughness < 100
terra::plot(suitable_areas)
#' 
#' ### Vector-Raster Integration: Calculate Area per County
#' 
#' Now, we will summarize the `suitable_area` for each county polygon. This is a classic "zonal statistics" operation where we `extract` raster values within vector polygons.
#' 
#' *  First, calculate the area of each pixel in square kilometers using `cellSize()` with `unit="km"`.
#' 
pixel_area <- cellSize(suitable_areas, unit = "km")
#' *  Multiply the `suitable_area` raster by the pixel area raster to get a new raster where each cell's value represents the area of suitable land in km².
multiplied_raster <- pixel_area * suitable_areas

#' *   Use `terra::extract()` with the `suitable_area` and `ny_counties_proj`.
#'     *   Set `fun='sum'` to total the suitable area in each county.
#'     *   Set `na.rm = TRUE`.
#' *   Join this summary back to the `ny_counties_proj` object using `bind_cols()`.  This only works because the order of the rows in the extracted data matches the order of the counties. 
#' 
summary <- terra::extract(multiplied_raster, ny_counties_sf, fun = sum, na.rm = TRUE)

suitable_area <- bind_cols(summary, ny_counties_sf)
#' 
#' 
#' ### Summarize and Inspect Results
#' Now that we have the total suitable area for wind farms in each county, create a table showing all counties with suitable area greater than 0 km². Sort the table in descending order of suitable area.
#' 
#' To get a table like this, you will likely need to use `select()`, `filter()`, `arrange()`, and `mutate()` from `dplyr`. Use `st_set_geometry()` to drop the geometry column for easier viewing.
#' 
suitable_area <- suitable_area %>%
  mutate(area = as.numeric(area))

area_suitable <- suitable_area %>%
  filter(suitable_area$area > 0) %>%
  arrange(desc(area))

#area_suitable <- st_set_geometry(area_suitable)

#' 
#' ### Communicate Your Results: Create a Choropleth Map
#' 
#' Visualizing the results is a key part of communicating your findings.
#' 
#' *   Use `ggplot()` and `geom_sf()` to create a choropleth map of the results.
#' *   Fill each county based on the `high_wind_area_km2` column.
#' *   Use an appropriate color scale, like `scale_fill_viridis_c()`.
#' *   Add informative titles and labels.
#' 
chloropleth <- ggplot(data = area_suitable) + geom_sf(aes(fill = area, geometry = geometry))

chloropleth

#' ## Deliverables
#' 
#' Submit your final, well-commented `.R` or `.qmd` script to your course repository.
#' 
#' 
