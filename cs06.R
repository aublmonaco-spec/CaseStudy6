#' title: "cs06: Wind Farm Suitability Analysis with Raster and Vector Data"
#' subtitle: "Aggregating High Wind Speed Areas by County using `terra` and `tigris`"
#' 
#' Your goal for this assignment is to **identify the total area within each county of New York State that are suitable for wind turbines**. You will then visualize these results in a choropleth map.
#' 
#' For this case study, we will define suitable areas for wind farms based on the following simplified criteria:
#' *   **Wind Speed**: Areas with average wind speeds greater than 8 m/s at 100m height.
#' *   **Slope**: Areas with slope less than 10 degrees (steeper slopes are
#'     generally unsuitable for construction).
#' *   **Roughness**: Areas with surface roughness less than 100 (lower
#'     roughness values indicate smoother terrain, which is preferable for wind farms).

#install.packages("terra")
#install.packages("elevatr")
library(terra)
library(tidyverse)
library(sf)
library(tigris)
library(elevatr)

ny_counties_sf <- counties(state = "NY")

#' ## Get elevation data
#' 
#' Download elevation data for New York using the `elevatr` package. We will use this to calculate slope and roughness, which are important factors for wind farm siting. 
#' 
#' Use the `get_elev_raster()` function to download elevation data for the extent of the wind raster. Set the zoom level (`z`) to 5 for a balance between resolution and download size. Clip the data to the location (`clip="location"`) of the wind raster.
wind_raster <- rast("wind100.tif")
elevation <- get_elev_raster(wind_raster, z = 5, clip = "location")

#' 
#' Then use `rast()` to convert the downloaded raster to a `terra` object and `resample()` to align it with the wind raster.  Resampling a raster is necessary when the two rasters have different resolutions or extents, ensuring that they can be accurately compared or combined in subsequent analyses.
#' 
#' 
elevation_raster <- rast(elevation)
resample(elevation_raster, wind_raster, method = "bilinear", filename = "re_elevation.tif", overwrite = TRUE)

#' An equal-area projection is ideal for calculating area, so we will transform the county vector data to match the raster's CRS.

st_crs(ny_counties_sf)
st_crs(wind_raster)

ny_counties_sf <- st_transform(ny_counties_sf, 9001)
st_crs(ny_counties_sf)

#' ### Raster Processing: Identify High-Wind Areas
#' 
#' Our goal is to find areas with wind speeds over 10 m/s. We will create a new binary raster where cells meeting this criterion are given a value of 1, and all others are `NA`.
#' 
#' *   **Mask the raster** to the extent of the New York county polygons to make processing faster.
#' *   **Reclassify the raster**: Create a new raster called `high_wind_raster`. Cells in the cropped raster with a value > 8 should become 1. All other cells should become 0. *Hint: a simple logical operator like `wind_raster_ny > 8` will work.*
#' 
mask(wind_raster, ny_counties_sf)
high_wind_raster <- wind_raster > 8

#' 
#' ### Resample and mask the elevation dataset
#' 
#' Resample (`resample()`) the DEM to match the wind raster resolution and extent, then mask it (`mask()`) to New York counties. Next, calculate slope and roughness from the DEM using `terrain()`. Slope should be in degrees, and roughness is a unitless index.
#' 
#' 
re_elevation <- resample(elevation_raster, wind_raster)
mask(re_elevation, ny_counties_sf)

slope <- terrain(re_elevation, "slope")
roughness <- terrain(re_elevation, "roughness")

#' 
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
  select(area == 0) %>%
  arrange(area) %>%
  st_set_geometry(NULL)
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

#' 
#' ## Deliverables
#' 
#' Submit your final, well-commented `.R` or `.qmd` script to your course repository.
#' 
#' 
