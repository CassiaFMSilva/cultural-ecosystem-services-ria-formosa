# -------------------------------------------------------------------------
# 🌿 developed with R, open tools and care for environmental data
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# project: Cultural Ecosystem Services from Geolocated Images
# script: 01_collect_wikimedia_images_ria_formosa.R
# author: Cássia Fernanda Martins da Silva
# github: github.com/CassiaFMSilva
#
# description:
# this script downloads the official protected area boundary for Ria Formosa,
# retrieves geolocated image metadata from Wikimedia Commons, filters records
# by time and study area, and prepares outputs for cultural ecosystem service
# classification.
#
# inputs:
# - ICNF protected areas spatial layer, downloaded from:
#   http://si.icnf.pt/shp/rnap
# - Wikimedia Commons API geosearch results
#
# outputs:
# - data/processed/ria_formosa_icnf.gpkg
# - data/processed/ria_formosa_icnf.shp
# - outputs/tables/wikimedia_images_ria_formosa_20_years.csv
# - outputs/maps/wikimedia_images_ria_formosa_20_years.gpkg
# - outputs/figures/map_wikimedia_images_ria_formosa_20_years.png
# - outputs/figures/plot_wikimedia_images_by_upload_year.png
# - outputs/tables/image_classification_sheet_ria_formosa.csv
# - outputs/tables/adapted_cices_classification_key.csv
# - outputs/tables/image_classification_sheet_cices_ria_formosa.csv
# - outputs/tables/calibration_30_percent_images.csv
# - outputs/tables/classification_batches_70_percent.csv
# - outputs/tables/classification_batches_and_calibration.xlsx
# - outputs/thumbnails/calibration_30_percent/
#
# notes:
# this workflow is part of an ongoing research project on cultural ecosystem
# services, geolocated images, spatial analysis and open science.
#
# the collection date is fixed to support reproducibility.
# set open_viewer <- TRUE when you want selected tables to open while running
# the workflow section by section in RStudio.
# -------------------------------------------------------------------------


##### 01. load packages #####

# list required packages for this workflow
required_packages <- c(
  "sf",
  "dplyr",
  "purrr",
  "httr2",
  "jsonlite",
  "stringr",
  "readr",
  "ggplot2",
  "ggspatial",
  "lubridate",
  "tibble",
  "writexl",
  "here"
)

# identify packages that are not installed
missing_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]
]

# install missing packages only when needed
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# load packages
library(sf)
library(dplyr)
library(purrr)
library(httr2)
library(jsonlite)
library(stringr)
library(readr)
library(ggplot2)
library(ggspatial)
library(lubridate)
library(tibble)
library(writexl)
library(here)


##### 01.1. define repository paths #####

# this script is intended to be run from the repository root
# using here::here() keeps paths stable when the project is shared on GitHub
path_data_raw_icnf <- here::here("data", "raw", "icnf")
path_data_raw_rnap <- here::here("data", "raw", "icnf", "rnap")
path_data_processed <- here::here("data", "processed")
path_outputs_intermediate_queries <- here::here("outputs", "intermediate", "commons_queries")
path_outputs_tables <- here::here("outputs", "tables")
path_outputs_maps <- here::here("outputs", "maps")
path_outputs_figures <- here::here("outputs", "figures")
path_outputs_thumbnails <- here::here("outputs", "thumbnails")

# relative thumbnail paths are saved in tables to keep outputs portable
relative_outputs_thumbnails <- file.path("outputs", "thumbnails")


##### 01.2. configure inspection helpers #####

# set TRUE when you want tables to open in the RStudio Viewer while running the script
open_viewer <- FALSE

# set TRUE only when you want output folders to open automatically
open_output_folders <- FALSE

# set TRUE only when you want the optional mapview check at the end
run_optional_mapview <- FALSE

# inspect regular tables while keeping the workflow readable
inspect_table <- function(data, object_name = "table", open_view = FALSE) {
  message("\n--- check: ", object_name, " ---")
  print(dim(data))
  print(dplyr::glimpse(data))
  print(utils::head(data, 5))
  
  if (interactive() && isTRUE(open_view)) {
    View(data)
  }
}

# inspect spatial objects without hiding their attribute table
inspect_spatial_object <- function(data, object_name = "spatial object", open_view = FALSE) {
  message("\n--- check: ", object_name, " ---")
  print(dim(data))
  print(sf::st_crs(data))
  print(sf::st_bbox(data))
  print(dplyr::glimpse(sf::st_drop_geometry(data)))
  
  if (interactive() && isTRUE(open_view)) {
    View(sf::st_drop_geometry(data))
  }
}

# inspect simple vectors and paths
inspect_vector <- function(x, object_name = "object") {
  message("\n--- check: ", object_name, " ---")
  print(x)
}


##### 01.3. define modern map helpers #####

# soft coastal palette used in the maps
map_colors <- list(
  boundary = "#0F5C64",
  boundary_fill = "#E8F4F1",
  boundary_halo = "#FFFFFF",
  query_points = "#6F7CB9",
  image_points = "#C14D7A",
  grid_line = "#E7ECEF",
  text = "#2F3A3D",
  title = "#172023",
  caption = "#607076"
)

# create an asymmetric map extent with extra space for cartographic elements
create_padded_bbox <- function(sf_object,
                               padding_x = 0.06,
                               padding_bottom = 0.10,
                               padding_top = 0.22) {
  
  bbox <- sf::st_bbox(sf_object)
  
  x_range <- bbox["xmax"] - bbox["xmin"]
  y_range <- bbox["ymax"] - bbox["ymin"]
  
  list(
    raw = bbox,
    x_range = x_range,
    y_range = y_range,
    xlim = c(
      bbox["xmin"] - x_range * padding_x,
      bbox["xmax"] + x_range * padding_x
    ),
    ylim = c(
      bbox["ymin"] - y_range * padding_bottom,
      bbox["ymax"] + y_range * padding_top
    )
  )
}

# add a small custom north arrow in the empty upper-right area of the map
modern_north_arrow <- function(map_bbox) {
  
  x <- map_bbox$raw["xmax"] + map_bbox$x_range * 0.025
  y_start <- map_bbox$raw["ymax"] + map_bbox$y_range * 0.055
  y_end <- map_bbox$raw["ymax"] + map_bbox$y_range * 0.155
  
  list(
    annotate(
      "segment",
      x = x,
      xend = x,
      y = y_start,
      yend = y_end,
      linewidth = 0.45,
      color = map_colors$text,
      arrow = grid::arrow(
        length = grid::unit(0.16, "cm"),
        type = "closed"
      )
    ),
    annotate(
      "text",
      x = x,
      y = y_end + map_bbox$y_range * 0.025,
      label = "N",
      size = 3.2,
      fontface = "bold",
      color = map_colors$text
    )
  )
}

# keep the scale bar small and quiet, away from the data layer
modern_scale_bar <- function() {
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.18,
    line_width = 0.35,
    text_cex = 0.58,
    pad_x = grid::unit(0.40, "cm"),
    pad_y = grid::unit(0.35, "cm"),
    bar_cols = c("#2F3A3D", "#FFFFFF")
  )
}

# clean map theme with light grid lines and no heavy frame
map_theme <- function(base_size = 10.5) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(
        color = map_colors$grid_line,
        linewidth = 0.22
      ),
      panel.grid.minor = element_blank(),
      axis.title = element_text(
        color = map_colors$text,
        size = 9.5
      ),
      axis.text = element_text(
        color = map_colors$text,
        size = 8
      ),
      axis.ticks = element_blank(),
      plot.title = element_text(
        face = "bold",
        size = 13.5,
        color = map_colors$title,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 10,
        color = map_colors$text,
        margin = margin(b = 8)
      ),
      plot.caption = element_text(
        size = 7.8,
        color = map_colors$caption,
        hjust = 0,
        margin = margin(t = 8)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = margin(10, 14, 10, 10)
    )
}


##### 02. create project folders #####

# create input, processed data and output folders used by the workflow
dir.create(path_data_raw_rnap, recursive = TRUE, showWarnings = FALSE)
dir.create(path_data_processed, recursive = TRUE, showWarnings = FALSE)
dir.create(path_outputs_intermediate_queries, recursive = TRUE, showWarnings = FALSE)
dir.create(path_outputs_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(path_outputs_maps, recursive = TRUE, showWarnings = FALSE)
dir.create(path_outputs_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(path_outputs_thumbnails, recursive = TRUE, showWarnings = FALSE)


##### 03. download the official protected areas layer #####

# define the official ICNF RNAP spatial layer URL
rnap_url <- "http://si.icnf.pt/shp/rnap"

# download the compressed spatial layer
download.file(
  url = rnap_url,
  destfile = file.path(path_data_raw_icnf, "rnap.zip"),
  mode = "wb"
)

# unzip the downloaded files
unzip(
  zipfile = file.path(path_data_raw_icnf, "rnap.zip"),
  exdir = path_data_raw_rnap
)


##### 04. locate the downloaded shapefile #####

# search for shapefile files inside the unzipped folder
rnap_shapefiles <- list.files(
  path_data_raw_rnap,
  pattern = "\\.shp$",
  full.names = TRUE,
  recursive = TRUE
)

# stop the workflow if no shapefile is found
if (length(rnap_shapefiles) == 0) {
  stop("No shapefile was found in data/raw/icnf/rnap.")
}

##### 04.1. check downloaded shapefile path #####

# inspect the shapefile path used in the workflow
inspect_vector(rnap_shapefiles[1], "selected RNAP shapefile")


##### 05. read the protected areas layer #####

# read the first shapefile found in the ICNF RNAP folder
rnap <- st_read(rnap_shapefiles[1])

##### 05.1. check protected areas layer #####

# inspect the spatial layer before filtering the study area
inspect_spatial_object(rnap, "ICNF RNAP protected areas layer")

# inspect column names
inspect_vector(names(rnap), "RNAP column names")


##### 06. define text standardization helper #####

# standardize text to support searches across character columns
standardize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_lower(x)
  x <- str_squish(x)
  return(x)
}


##### 07. search for the Ria Formosa feature #####

# create a non-spatial table with standardized text fields
rnap_table <- rnap %>%
  st_drop_geometry() %>%
  mutate(across(where(is.character), standardize_text))

# identify rows that mention Ria Formosa in any character column
ria_rows <- rnap_table %>%
  filter(if_any(where(is.character), ~ str_detect(.x, "ria formosa")))

##### 07.1. check candidate rows for Ria Formosa #####

# inspect candidate rows before filtering the spatial layer
inspect_table(ria_rows, "candidate rows for Ria Formosa", open_view = open_viewer)


##### 08. filter the Ria Formosa boundary #####

# filter the spatial layer to keep the Ria Formosa protected area
ria_formosa <- rnap %>%
  mutate(across(where(is.character), as.character)) %>%
  filter(
    if_any(
      where(is.character),
      ~ str_detect(standardize_text(.x), "ria formosa")
    )
  )

# stop the workflow if the study area was not found
if (nrow(ria_formosa) == 0) {
  stop("Ria Formosa was not found in the ICNF RNAP layer.")
}

# fix geometries when needed
ria_formosa <- st_make_valid(ria_formosa)

# transform the study area to WGS84
ria_formosa <- st_transform(ria_formosa, 4326)


##### 08.1. check filtered study area #####

# inspect the filtered study area before using it in spatial queries
inspect_spatial_object(ria_formosa, "Ria Formosa boundary", open_view = open_viewer)



##### 09. check study area map #####

# define map extent with extra space for scale and north arrow
ria_bbox <- create_padded_bbox(ria_formosa)

# map the study area boundary
check_map_ria_formosa <- ggplot() +
  geom_sf(
    data = ria_formosa,
    fill = map_colors$boundary_fill,
    color = NA
  ) +
  geom_sf(
    data = ria_formosa,
    fill = NA,
    color = map_colors$boundary_halo,
    linewidth = 1.4
  ) +
  geom_sf(
    data = ria_formosa,
    fill = NA,
    color = map_colors$boundary,
    linewidth = 0.55
  ) +
  modern_scale_bar() +
  modern_north_arrow(ria_bbox) +
  coord_sf(
    xlim = ria_bbox$xlim,
    ylim = ria_bbox$ylim,
    expand = FALSE
  ) +
  labs(
    title = "Ria Formosa Natural Park",
    subtitle = "Official protected area boundary",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: ICNF/RNAP. CRS: WGS 84 (EPSG:4326)."
  ) +
  map_theme()

# display the map
check_map_ria_formosa


##### 10. extract study area bounding box #####

# get bounding box values from the study area
bbox_ria <- st_bbox(ria_formosa)

# organize bounding box values for possible API queries
bbox_commons <- c(
  bbox_ria["xmin"],
  bbox_ria["ymin"],
  bbox_ria["xmax"],
  bbox_ria["ymax"]
)

##### 10.1. check study area bounding box #####

# inspect bounding box values
inspect_vector(bbox_commons, "Ria Formosa bounding box")


##### 11. save the study area boundary #####

# save the Ria Formosa boundary as GeoPackage
st_write(
  ria_formosa,
  file.path(path_data_processed, "ria_formosa_icnf.gpkg"),
  delete_dsn = TRUE
)

# save the Ria Formosa boundary as shapefile
st_write(
  ria_formosa,
  file.path(path_data_processed, "ria_formosa_icnf.shp"),
  delete_layer = TRUE
)


##### 12. ensure study area is in WGS84 #####

# Wikimedia Commons geosearch uses latitude and longitude coordinates
ria_formosa <- st_transform(ria_formosa, 4326)

##### 12.1. check study area reference system and extent #####

# inspect reference system and spatial extent
inspect_spatial_object(ria_formosa, "Ria Formosa boundary in WGS84")


##### 13. create query grid #####

# transform the study area to a projected CRS suitable for Portugal
ria_formosa_projected <- st_transform(ria_formosa, 3763)

# create a point grid with 1500 m spacing
query_points <- st_make_grid(
  ria_formosa_projected,
  cellsize = 1500,
  what = "centers"
) %>%
  st_as_sf()

# create a buffer to include images close to protected area boundaries
ria_formosa_buffer <- st_buffer(ria_formosa_projected, 1500)

# keep only grid points intersecting the buffered study area
query_points <- query_points[
  st_intersects(query_points, ria_formosa_buffer, sparse = FALSE)[, 1],
]

# transform the query grid back to WGS84
query_points <- st_transform(query_points, 4326)

# extract point coordinates
query_coordinates <- st_coordinates(query_points)

# organize query point table
query_grid <- query_points %>%
  st_drop_geometry() %>%
  mutate(
    point_id = row_number(),
    longitude = query_coordinates[, 1],
    latitude = query_coordinates[, 2]
  )

##### 13.1. check query grid table #####

# inspect query grid before API requests
inspect_table(query_grid, "query grid", open_view = open_viewer)

# check number of query points
nrow(query_grid)



##### 14. check query grid map #####

# map the points used to query Wikimedia Commons
check_map_query_grid <- ggplot() +
  geom_sf(
    data = ria_formosa,
    fill = map_colors$boundary_fill,
    color = NA
  ) +
  geom_sf(
    data = ria_formosa,
    fill = NA,
    color = map_colors$boundary_halo,
    linewidth = 1.3
  ) +
  geom_sf(
    data = ria_formosa,
    fill = NA,
    color = map_colors$boundary,
    linewidth = 0.5
  ) +
  geom_sf(
    data = query_points,
    shape = 21,
    size = 1.55,
    stroke = 0.35,
    color = map_colors$query_points,
    fill = "white",
    alpha = 0.92
  ) +
  modern_scale_bar() +
  modern_north_arrow(ria_bbox) +
  coord_sf(
    xlim = ria_bbox$xlim,
    ylim = ria_bbox$ylim,
    expand = FALSE
  ) +
  labs(
    title = "Query grid for Wikimedia Commons geosearch",
    subtitle = "Points used to retrieve geolocated image metadata",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: ICNF/RNAP and Wikimedia Commons API. CRS: WGS 84 (EPSG:4326)."
  ) +
  map_theme()

# display the map
check_map_query_grid


##### 15. define metadata helpers #####

# return a character value or NA when the object is null
get_value <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  
  return(as.character(x))
}

# return a metadata value or NA when the metadata field is missing
get_metadata <- function(metadata, field) {
  if (is.null(metadata[[field]]$value)) {
    return(NA_character_)
  }
  
  return(as.character(metadata[[field]]$value))
}


##### 16. define Wikimedia Commons query function #####

# query geolocated images from Wikimedia Commons with request control
query_commons_point <- function(latitude, longitude, radius = 3000, limit = 50, attempts = 5) {
  
  coordinate_text <- paste0(latitude, "|", longitude)
  
  for (attempt in seq_len(attempts)) {
    
    # small pause before each request
    Sys.sleep(runif(1, min = 2, max = 5))
    
    request_object <- request("https://commons.wikimedia.org/w/api.php") %>%
      req_user_agent("RiaFormosaCESResearch/0.1 (academic research)") %>%
      req_url_query(
        action = "query",
        generator = "geosearch",
        ggscoord = coordinate_text,
        ggsradius = radius,
        ggsnamespace = 6,
        ggslimit = limit,
        prop = "coordinates|imageinfo",
        iiprop = "url|extmetadata|mime|timestamp|user",
        iiurlwidth = 600,
        format = "json",
        formatversion = 2
      )
    
    response <- tryCatch(
      req_perform(request_object),
      error = function(e) e
    )
    
    # wait and retry when the request fails
    if (inherits(response, "error")) {
      message("request error. attempt ", attempt, " of ", attempts)
      Sys.sleep(10 * attempt)
      next
    }
    
    status_code <- resp_status(response)
    
    # wait longer when the API returns status 429
    if (status_code == 429) {
      message("status 429: too many requests. waiting before retrying...")
      Sys.sleep(30 * attempt)
      next
    }
    
    # return an empty table for other HTTP errors
    if (status_code != 200) {
      message("request returned status ", status_code)
      return(tibble())
    }
    
    json_response <- resp_body_json(
      response,
      simplifyVector = FALSE
    )
    
    if (is.null(json_response$query$pages)) {
      return(tibble())
    }
    
    pages <- json_response$query$pages
    
    images <- map_dfr(pages, function(page) {
      
      coordinate <- page$coordinates[[1]]
      image_info <- page$imageinfo[[1]]
      metadata <- image_info$extmetadata
      
      tibble(
        pageid = get_value(page$pageid),
        title = get_value(page$title),
        latitude = as.numeric(coordinate$lat),
        longitude = as.numeric(coordinate$lon),
        image_url = get_value(image_info$url),
        thumbnail_url = get_value(image_info$thumburl),
        mime = get_value(image_info$mime),
        upload_timestamp = get_value(image_info$timestamp),
        upload_user = get_value(image_info$user),
        artist = get_metadata(metadata, "Artist"),
        credit = get_metadata(metadata, "Credit"),
        license = get_metadata(metadata, "LicenseShortName"),
        license_terms = get_metadata(metadata, "UsageTerms")
      )
    })
    
    return(images)
  }
  
  # return an empty table if all attempts fail
  return(tibble())
}


##### 17. test query using one grid point #####

# test the query function using the first grid point
commons_test <- query_commons_point(
  latitude = query_grid$latitude[1],
  longitude = query_grid$longitude[1],
  radius = 1500
)

##### 17.1. check single-point query result #####

# inspect test result
inspect_table(commons_test, "single-point Wikimedia Commons test", open_view = open_viewer)

# check number of records returned
nrow(commons_test)


##### 18. test query using the study area centroid #####

# calculate study area centroid using projected coordinates first
ria_centroid <- ria_formosa %>%
  st_transform(3763) %>%
  st_union() %>%
  st_centroid() %>%
  st_transform(4326)

# extract centroid coordinates
centroid_coordinates <- st_coordinates(ria_centroid)

centroid_longitude <- centroid_coordinates[1, 1]
centroid_latitude <- centroid_coordinates[1, 2]

# test query using a larger radius around the centroid
commons_centroid_test <- query_commons_point(
  latitude = centroid_latitude,
  longitude = centroid_longitude,
  radius = 10000
)

##### 18.1. check centroid query result #####

# inspect centroid test result
inspect_table(commons_centroid_test, "centroid Wikimedia Commons test", open_view = open_viewer)

# check number of records returned
nrow(commons_centroid_test)


##### 19. test query using several grid points #####

# test the query function using up to the first 20 grid points
n_test_points <- min(20, nrow(query_grid))

commons_multiple_points_test <- pmap_dfr(
  list(
    query_grid$latitude[seq_len(n_test_points)],
    query_grid$longitude[seq_len(n_test_points)]
  ),
  function(latitude, longitude) {
    
    Sys.sleep(0.5)
    
    query_commons_point(
      latitude = latitude,
      longitude = longitude,
      radius = 3000
    )
  }
)

# remove duplicate images returned by nearby query points
commons_multiple_points_test <- commons_multiple_points_test %>%
  distinct(pageid, .keep_all = TRUE)

##### 19.1. check multiple-point query result #####

# inspect multiple-point test result
inspect_table(commons_multiple_points_test, "multiple-point Wikimedia Commons test", open_view = open_viewer)

# check number of unique records returned
nrow(commons_multiple_points_test)


##### 20. query images across the full grid #####

# query all grid points and save each partial result
for (i in seq_len(nrow(query_grid))) {
  
  # define output file for the current query point
  output_file <- file.path(
    path_outputs_intermediate_queries,
    paste0("point_", query_grid$point_id[i], ".rds")
  )
  
  # skip the point when a partial result already exists
  if (file.exists(output_file)) {
    next
  }
  
  message("querying point ", i, " of ", nrow(query_grid))
  
  # query the current point
  point_result <- query_commons_point(
    latitude = query_grid$latitude[i],
    longitude = query_grid$longitude[i],
    radius = 5000,
    limit = 50
  )
  
  # add query point information when records are returned
  if (nrow(point_result) > 0) {
    
    point_result <- point_result %>%
      mutate(
        query_point_id = query_grid$point_id[i],
        query_point_latitude = query_grid$latitude[i],
        query_point_longitude = query_grid$longitude[i]
      )
  }
  
  # save partial result
  saveRDS(point_result, output_file)
  
  # pause to reduce the risk of API blocking
  Sys.sleep(runif(1, min = 2, max = 5))
}


##### 21. combine query results #####

# list all partial query results
partial_files <- list.files(
  path_outputs_intermediate_queries,
  pattern = "\\.rds$",
  full.names = TRUE
)

# stop the workflow if no partial files are available
if (length(partial_files) == 0) {
  stop("No partial query files were found in outputs/intermediate/commons_queries.")
}

# combine partial results into a single table
commons_images_bbox <- partial_files %>%
  purrr::map_dfr(readRDS)

# remove duplicate images returned by more than one query point
commons_images_bbox <- commons_images_bbox %>%
  distinct(pageid, .keep_all = TRUE)

##### 21.1. check combined query results #####

# inspect combined query results before temporal filtering
inspect_table(commons_images_bbox, "combined Wikimedia Commons results", open_view = open_viewer)

# check total images found in the approximate search area
nrow(commons_images_bbox)


##### 22. filter images from the last 20 years #####

# define a fixed collection date to support reproducibility
collection_date <- as.Date("2026-05-24")

# define the start date for the 20-year period
start_date_20_years <- collection_date %m-% years(20)

# organize upload date
commons_images_bbox <- commons_images_bbox %>%
  mutate(
    upload_date = as.Date(substr(upload_timestamp, 1, 10))
  )

# keep only images made available within the last 20 years
commons_images_bbox_20_years <- commons_images_bbox %>%
  filter(
    !is.na(upload_date),
    upload_date >= start_date_20_years,
    upload_date <= collection_date
  )

##### 22.1. check temporal filtering result #####

# inspect table after filtering images from the last 20 years
inspect_table(commons_images_bbox_20_years, "images from the last 20 years", open_view = open_viewer)

# check total after temporal filtering
nrow(commons_images_bbox_20_years)


##### 23. convert images to spatial points #####

# create a spatial object from image coordinates
commons_images_sf <- commons_images_bbox_20_years %>%
  filter(
    !is.na(latitude),
    !is.na(longitude)
  ) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )


##### 23.1. check image spatial points #####

# inspect image points before filtering inside the study area
inspect_spatial_object(commons_images_sf, "Wikimedia Commons image points", open_view = open_viewer)


##### 24. filter images inside the study area #####

# identify images located inside the Ria Formosa boundary
inside_filter <- lengths(
  st_intersects(commons_images_sf, ria_formosa)
) > 0

# keep only images inside the study area
ria_formosa_images <- commons_images_sf[inside_filter, ]

##### 24.1. check images inside the study area #####

# inspect final image points inside Ria Formosa
inspect_spatial_object(ria_formosa_images, "images inside Ria Formosa", open_view = open_viewer)

# check final number of images inside the study area and time window
nrow(ria_formosa_images)


##### 25. organize final image metadata table #####

# create final non-spatial image metadata table
image_metadata <- ria_formosa_images %>%
  st_drop_geometry() %>%
  mutate(
    platform = "Wikimedia Commons",
    title_url = stringr::str_replace_all(title, " ", "_"),
    page_url = paste0(
      "https://commons.wikimedia.org/wiki/",
      title_url
    )
  ) %>%
  select(
    platform,
    pageid,
    title,
    latitude,
    longitude,
    upload_timestamp,
    upload_date,
    upload_user,
    artist,
    credit,
    license,
    license_terms,
    mime,
    thumbnail_url,
    image_url,
    page_url
  )

##### 25.1. check final image metadata table #####

# inspect final image metadata table
inspect_table(image_metadata, "final image metadata", open_view = open_viewer)

# check final number of images
nrow(image_metadata)


##### 26. save final image metadata and spatial files #####

# save final image metadata table
write_csv(
  image_metadata,
  file.path(path_outputs_tables, "wikimedia_images_ria_formosa_20_years.csv")
)

# save spatial image layer
st_write(
  ria_formosa_images,
  file.path(path_outputs_maps, "wikimedia_images_ria_formosa_20_years.gpkg"),
  delete_dsn = TRUE
)



##### 27. map images inside the study area #####

# map selected Wikimedia Commons images inside the study area
map_wikimedia_images <- ggplot() +
  geom_sf(
    data = ria_formosa,
    fill = map_colors$boundary_fill,
    color = NA
  ) +
  geom_sf(
    data = ria_formosa,
    fill = NA,
    color = map_colors$boundary_halo,
    linewidth = 1.35
  ) +
  geom_sf(
    data = ria_formosa,
    fill = NA,
    color = map_colors$boundary,
    linewidth = 0.52
  ) +
  geom_sf(
    data = ria_formosa_images,
    shape = 21,
    size = 2.15,
    stroke = 0.45,
    color = map_colors$image_points,
    fill = "white",
    alpha = 0.92
  ) +
  modern_scale_bar() +
  modern_north_arrow(ria_bbox) +
  coord_sf(
    xlim = ria_bbox$xlim,
    ylim = ria_bbox$ylim,
    expand = FALSE
  ) +
  labs(
    title = "Geolocated images from Wikimedia Commons",
    subtitle = "Records from the last 20 years located inside Ria Formosa Natural Park",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: Wikimedia Commons API and ICNF/RNAP. CRS: WGS 84 (EPSG:4326)."
  ) +
  map_theme()

# display the map
map_wikimedia_images



##### 28. save image distribution map #####

# save map as PNG
ggsave(
  filename = file.path(path_outputs_figures, "map_wikimedia_images_ria_formosa_20_years.png"),
  plot = map_wikimedia_images,
  width = 7.8,
  height = 5.8,
  dpi = 400,
  bg = "white"
)

# save map as PDF for publication workflows
ggsave(
  filename = file.path(path_outputs_figures, "map_wikimedia_images_ria_formosa_20_years.pdf"),
  plot = map_wikimedia_images,
  width = 7.8,
  height = 5.8,
  device = grDevices::cairo_pdf,
  bg = "white"
)


##### 29. summarize image metadata #####

# create a general summary of selected images
commons_summary <- image_metadata %>%
  summarise(
    total_images = n(),
    total_users = n_distinct(upload_user),
    total_licenses = n_distinct(license),
    first_upload_date = min(upload_date, na.rm = TRUE),
    last_upload_date = max(upload_date, na.rm = TRUE)
  )

##### 29.1. check general image summary #####

# inspect summary
inspect_table(commons_summary, "general image summary", open_view = open_viewer)

# save summary table
write_csv(
  commons_summary,
  file.path(path_outputs_tables, "summary_wikimedia_images_ria_formosa_20_years.csv")
)


##### 30. summarize images by upload year #####

# count images by upload year
summary_by_year <- image_metadata %>%
  mutate(
    upload_year = lubridate::year(upload_date)
  ) %>%
  count(upload_year, name = "n_images") %>%
  arrange(upload_year)

##### 30.1. check yearly image summary #####

# inspect yearly summary
inspect_table(summary_by_year, "images by upload year", open_view = open_viewer)

# save yearly summary
write_csv(
  summary_by_year,
  file.path(path_outputs_tables, "summary_wikimedia_images_by_upload_year.csv")
)


##### 31. plot images by upload year #####

# create temporal distribution plot
plot_images_by_year <- ggplot(
  summary_by_year,
  aes(x = upload_year, y = n_images)
) +
  geom_col() +
  labs(
    title = "Temporal distribution of geolocated images",
    subtitle = "Wikimedia Commons | Ria Formosa | Last 20 years",
    x = "Upload year",
    y = "Number of images"
  ) +
  theme_minimal()

# display the plot
plot_images_by_year

# save temporal distribution plot
ggsave(
  filename = file.path(path_outputs_figures, "plot_wikimedia_images_by_upload_year.png"),
  plot = plot_images_by_year,
  width = 8,
  height = 5,
  dpi = 300
)


##### 32. create preliminary image classification sheet #####

# create a preliminary table for visual classification
preliminary_classification_sheet <- image_metadata %>%
  mutate(
    cultural_service_category = NA_character_,
    subcategory = NA_character_,
    visible_elements = NA_character_,
    notes = NA_character_,
    classify = "yes"
  ) %>%
  select(
    pageid,
    title,
    page_url,
    thumbnail_url,
    latitude,
    longitude,
    upload_date,
    license,
    cultural_service_category,
    subcategory,
    visible_elements,
    notes,
    classify
  )

##### 32.1. check preliminary classification sheet #####

# inspect preliminary classification sheet before saving
inspect_table(preliminary_classification_sheet, "preliminary image classification sheet", open_view = open_viewer)

# save preliminary classification sheet
write_csv(
  preliminary_classification_sheet,
  file.path(path_outputs_tables, "image_classification_sheet_preliminary_ria_formosa.csv")
)


##### 33. download thumbnails for visual classification #####

# organize thumbnail download table
images_to_download <- image_metadata %>%
  filter(!is.na(thumbnail_url)) %>%
  mutate(
    thumbnail_file = file.path(
      relative_outputs_thumbnails,
      paste0(pageid, ".jpg")
    )
  ) %>%
  select(
    pageid,
    thumbnail_url,
    thumbnail_file
  )

# download image thumbnails
for (i in seq_len(nrow(images_to_download))) {
  
  destination <- images_to_download$thumbnail_file[i]
  image_url <- images_to_download$thumbnail_url[i]
  
  # download only when the thumbnail does not already exist
  if (!file.exists(here::here(destination))) {
    
    message("downloading thumbnail ", i, " of ", nrow(images_to_download))
    
    try(
      download.file(
        url = image_url,
        destfile = here::here(destination),
        mode = "wb",
        quiet = TRUE
      ),
      silent = TRUE
    )
    
    # small pause to avoid excessive requests
    Sys.sleep(0.3)
  }
}


##### 34. create image classification sheet with local thumbnail paths #####

# create final image classification table with local thumbnail paths
image_classification_sheet <- image_metadata %>%
  mutate(
    thumbnail_file = file.path(
      relative_outputs_thumbnails,
      paste0(pageid, ".jpg")
    ),
    cultural_service_category = NA_character_,
    subcategory = NA_character_,
    visible_elements = NA_character_,
    notes = NA_character_,
    classify = "yes"
  ) %>%
  select(
    pageid,
    title,
    page_url,
    thumbnail_url,
    thumbnail_file,
    latitude,
    longitude,
    upload_date,
    upload_user,
    artist,
    license,
    cultural_service_category,
    subcategory,
    visible_elements,
    notes,
    classify
  )

##### 34.1. check image classification sheet #####

# inspect image classification sheet before saving
inspect_table(image_classification_sheet, "image classification sheet", open_view = open_viewer)

# save image classification sheet
write_csv(
  image_classification_sheet,
  file.path(path_outputs_tables, "image_classification_sheet_ria_formosa.csv")
)


##### 35. check downloaded thumbnails #####

# list downloaded thumbnails
downloaded_thumbnails <- list.files(
  path_outputs_thumbnails,
  pattern = "\\.jpg$",
  full.names = TRUE
)

# check number of downloaded thumbnails
length(downloaded_thumbnails)

# inspect a few downloaded thumbnail paths
utils::head(downloaded_thumbnails, 10)


##### 36. optionally open thumbnail folder #####

# open thumbnail folder only in interactive sessions
if (interactive() && isTRUE(open_output_folders)) {
  
  thumbnail_folder <- normalizePath(path_outputs_thumbnails)
  
  if (.Platform$OS.type == "windows") {
    shell.exec(thumbnail_folder)
  } else {
    browseURL(paste0("file://", thumbnail_folder))
  }
}


##### 37. create adapted CICES classification key #####

# create an adapted classification key for visual interpretation
adapted_cices_key <- tibble::tribble(
  ~adapted_cices_category, ~description, ~visual_examples,
  
  "outdoor_recreation",
  "active or immersive use of the natural landscape for recreation, leisure or well-being",
  "walking, beach use, trails, cycling, kayaking, boating, bathing, nautical activities",
  
  "aesthetic_landscape_appreciation",
  "visual, aesthetic or scenic appreciation of the landscape",
  "sunset, lagoon, dunes, islands, panoramic view, natural landscape",
  
  "nature_observation",
  "direct observation of natural elements, fauna, flora or habitats",
  "birds, plants, habitats, wetlands, dunes, vegetation, biodiversity",
  
  "education_knowledge_science",
  "use of the landscape or its elements for education, environmental interpretation, knowledge or science",
  "interpretive signs, visitor centers, panels, field activities, monitoring",
  
  "cultural_heritage_identity",
  "cultural, historical, traditional or identity-related elements associated with the landscape",
  "traditional fishing, boats, salt pans, local architecture, traditional practices",
  
  "tourism_leisure_infrastructure",
  "tourism or recreational use mediated by infrastructure associated with the landscape",
  "boardwalks, viewpoints, marinas, piers, restaurants, visitor facilities",
  
  "spiritual_symbolic_values",
  "symbolic, spiritual or religious values associated with the environment",
  "chapels, rituals, memorials, cultural symbols in the landscape",
  
  "not_classifiable",
  "image without enough elements to infer a cultural ecosystem service",
  "isolated portrait, object without context, indoor scene, low-information image",
  
  "exclude",
  "record that should not be included in the analysis",
  "map, logo, drawing, duplicate, image unrelated to the area or without analytical use"
)

##### 37.1. check adapted CICES classification key #####

# inspect classification key
inspect_table(adapted_cices_key, "adapted CICES classification key", open_view = open_viewer)

# save classification key
write_csv(
  adapted_cices_key,
  file.path(path_outputs_tables, "adapted_cices_classification_key.csv")
)


##### 38. create CICES-based image classification sheet #####

# create a classification sheet based on the adapted CICES key
cices_classification_sheet <- image_metadata %>%
  mutate(
    thumbnail_file = file.path(
      relative_outputs_thumbnails,
      paste0(pageid, ".jpg")
    ),
    adapted_cices_category = NA_character_,
    classification_confidence = NA_character_,
    main_observed_element = NA_character_,
    people_presence = NA_character_,
    visible_use = NA_character_,
    notes = NA_character_,
    keep_in_analysis = "yes"
  ) %>%
  select(
    pageid,
    title,
    page_url,
    thumbnail_url,
    thumbnail_file,
    latitude,
    longitude,
    upload_date,
    upload_user,
    artist,
    license,
    adapted_cices_category,
    classification_confidence,
    main_observed_element,
    people_presence,
    visible_use,
    notes,
    keep_in_analysis
  )

##### 38.1. check CICES-based classification sheet #####

# inspect CICES-based classification sheet before saving
inspect_table(cices_classification_sheet, "CICES-based classification sheet", open_view = open_viewer)

# save CICES-based classification sheet
write_csv(
  cices_classification_sheet,
  file.path(path_outputs_tables, "image_classification_sheet_cices_ria_formosa.csv")
)


##### 38.2. create Portuguese classification sheet for group use #####

# create a Portuguese version using the same structure already shared with the group
# this table will be used for calibration and classification batches
planilha_classificacao_ptbr <- cices_classification_sheet %>%
  transmute(
    pageid = pageid,
    titulo = title,
    link_pagina = page_url,
    url_miniatura = thumbnail_url,
    arquivo_miniatura = thumbnail_file,
    latitude = latitude,
    longitude = longitude,
    data_upload_data = upload_date,
    usuario_upload = upload_user,
    autor = artist,
    licenca = license,
    categoria_servico_cultural = NA_character_,
    elementos_visiveis = NA_character_,
    observacao = NA_character_,
    classificar = "sim"
  )

##### 38.3. check Portuguese classification sheet #####

# inspect Portuguese classification sheet before saving
inspect_table(
  planilha_classificacao_ptbr,
  "planilha de classificação em português",
  open_view = open_viewer
)

# save Portuguese classification sheet as CSV
write_csv(
  planilha_classificacao_ptbr,
  file.path(path_outputs_tables, "planilha_classificacao_imagens_ria_formosa_ptbr.csv")
)

# save Portuguese classification sheet as XLSX for group use
writexl::write_xlsx(
  list(
    planilha_classificacao = planilha_classificacao_ptbr
  ),
  file.path(path_outputs_tables, "planilha_classificacao_imagens_ria_formosa_ptbr.xlsx")
)


##### 39. split Portuguese classification sheet for calibration and classification batches #####

# this internal step organizes the image set for group classification
# 30% of the images are randomly selected for shared calibration
# the remaining 70% are divided into four balanced individual classification batches
# the split uses the Portuguese table format already shared with the group

# define percentage of images used for shared calibration
calibration_percent <- 0.30

# calculate number of calibration images
# the final number depends on the current collection result
n_calibration <- ceiling(
  nrow(planilha_classificacao_ptbr) * calibration_percent
)

# check number of calibration images
inspect_vector(
  n_calibration,
  "number of images selected for calibration"
)


##### 39.1. randomly select calibration images #####

# set seed to make the random selection reproducible
set.seed(123)

# randomly select 30% of the images for calibration
calibration_images <- planilha_classificacao_ptbr %>%
  slice_sample(n = n_calibration)

# check calibration images
inspect_table(
  calibration_images,
  "calibration images - 30 percent",
  open_view = open_viewer
)


##### 39.2. create calibration thumbnail folder #####

# create a folder only for copied calibration thumbnails
calibration_thumbnail_folder <- file.path(
  path_outputs_thumbnails,
  "calibration_30_percent"
)

relative_calibration_thumbnail_folder <- file.path(
  relative_outputs_thumbnails,
  "calibration_30_percent"
)

dir.create(
  calibration_thumbnail_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

# check calibration thumbnail folder path
inspect_vector(
  normalizePath(
    calibration_thumbnail_folder,
    winslash = "/",
    mustWork = FALSE
  ),
  "calibration thumbnail folder"
)


##### 39.3. copy calibration thumbnails to the calibration folder #####

# add original and destination paths for copied calibration thumbnails
calibration_images_copy <- calibration_images %>%
  mutate(
    arquivo_miniatura_original = arquivo_miniatura,
    arquivo_miniatura_calibracao = file.path(
      calibration_thumbnail_folder,
      basename(arquivo_miniatura)
    ),
    miniatura_original_existe = file.exists(here::here(arquivo_miniatura_original))
  )

# keep only calibration images whose original thumbnail file exists
calibration_images_to_copy <- calibration_images_copy %>%
  filter(miniatura_original_existe)

# copy thumbnails to the calibration folder
copy_result <- file.copy(
  from = here::here(calibration_images_to_copy$arquivo_miniatura_original),
  to = calibration_images_to_copy$arquivo_miniatura_calibracao,
  overwrite = TRUE
)

# update copy status after copying
calibration_images_copy <- calibration_images_copy %>%
  mutate(
    miniatura_copiada = file.exists(arquivo_miniatura_calibracao)
  )

# update calibration table to point to the copied thumbnails
# this keeps the same column structure as the shared classification table
calibration_images <- calibration_images_copy %>%
  mutate(
    arquivo_miniatura = file.path(
      relative_calibration_thumbnail_folder,
      basename(arquivo_miniatura_original)
    )
  ) %>%
  select(
    pageid,
    titulo,
    link_pagina,
    url_miniatura,
    arquivo_miniatura,
    latitude,
    longitude,
    data_upload_data,
    usuario_upload,
    autor,
    licenca,
    categoria_servico_cultural,
    elementos_visiveis,
    observacao,
    classificar
  )

# list copied calibration thumbnails
copied_calibration_thumbnails <- list.files(
  calibration_thumbnail_folder,
  pattern = "\\.jpg$",
  full.names = TRUE
)

# check number of copied thumbnails
inspect_vector(
  length(copied_calibration_thumbnails),
  "number of calibration thumbnails copied"
)

# check calibration image table after copying thumbnails
inspect_table(
  calibration_images,
  "calibration images with copied thumbnail paths",
  open_view = open_viewer
)


##### 39.4. save calibration table and copy report #####

# save calibration table with the same structure as the shared classification table
write_csv(
  calibration_images,
  file.path(path_outputs_tables, "calibracao_30_percent_imagens.csv")
)

# save calibration table as XLSX for easier group classification
writexl::write_xlsx(
  list(
    calibracao_30_percent = calibration_images
  ),
  file.path(path_outputs_tables, "calibracao_30_percent_imagens.xlsx")
)

# create a small report on thumbnail copy status
calibration_copy_report <- calibration_images_copy %>%
  count(
    miniatura_original_existe,
    miniatura_copiada,
    name = "n_imagens"
  )

# save copy report
write_csv(
  calibration_copy_report,
  file.path(path_outputs_tables, "calibracao_30_percent_relatorio_copias.csv")
)

# check copy report
inspect_table(
  calibration_copy_report,
  "calibration thumbnail copy report",
  open_view = open_viewer
)

# check saved calibration files
calibration_saved_files <- c(
  file.path(path_outputs_tables, "calibracao_30_percent_imagens.csv"),
  file.path(path_outputs_tables, "calibracao_30_percent_imagens.xlsx"),
  file.path(path_outputs_tables, "calibracao_30_percent_relatorio_copias.csv")
)

inspect_vector(
  calibration_saved_files[file.exists(calibration_saved_files)],
  "saved calibration files"
)


##### 39.5. remove calibration images from classification pool #####

# keep only images that were not selected for calibration
classification_pool <- planilha_classificacao_ptbr %>%
  filter(
    !pageid %in% calibration_images$pageid
  )

# check number of remaining images
inspect_vector(
  nrow(classification_pool),
  "number of images remaining for individual classification"
)

# check classification pool
inspect_table(
  classification_pool,
  "classification pool - remaining 70 percent",
  open_view = open_viewer
)


##### 39.6. divide remaining images into four batches #####

# define people responsible for individual classification batches
responsible_people <- c(
  "classifier_01",
  "classifier_02",
  "classifier_03",
  "classifier_04"
)

# shuffle remaining images before assigning batches
set.seed(456)

classification_pool <- classification_pool %>%
  slice_sample(prop = 1)

# assign balanced batches
classification_batches <- classification_pool %>%
  mutate(
    batch_number = rep(
      seq_along(responsible_people),
      length.out = n()
    ),
    responsavel = responsible_people[batch_number],
    bloco_classificacao = paste0("bloco_", responsavel)
  )

# summarize batches
classification_batch_summary <- classification_batches %>%
  count(
    bloco_classificacao,
    responsavel,
    name = "n_imagens"
  )

# check batch summary
inspect_table(
  classification_batch_summary,
  "classification batch summary",
  open_view = open_viewer
)

# check individual batches
inspect_table(
  classification_batches,
  "classification batches - 70 percent",
  open_view = open_viewer
)


##### 39.7. save classification batches #####

# save full table with all individual batches
write_csv(
  classification_batches,
  file.path(path_outputs_tables, "blocos_classificacao_70_percent.csv")
)

# save batch summary
write_csv(
  classification_batch_summary,
  file.path(path_outputs_tables, "resumo_blocos_classificacao.csv")
)

# save one table per responsible person using the shared table format
for (person in responsible_people) {
  
  person_batch <- classification_batches %>%
    filter(responsavel == person) %>%
    select(
      pageid,
      titulo,
      link_pagina,
      url_miniatura,
      arquivo_miniatura,
      latitude,
      longitude,
      data_upload_data,
      usuario_upload,
      autor,
      licenca,
      categoria_servico_cultural,
      elementos_visiveis,
      observacao,
      classificar
    )
  
  output_file_person_csv <- file.path(
    path_outputs_tables,
    paste0("bloco_classificacao_", person, ".csv")
  )
  
  output_file_person_xlsx <- file.path(
    path_outputs_tables,
    paste0("bloco_classificacao_", person, ".xlsx")
  )
  
  write_csv(
    person_batch,
    output_file_person_csv
  )
  
  writexl::write_xlsx(
    list(
      bloco_classificacao = person_batch
    ),
    output_file_person_xlsx
  )
}

# create clean batches for the Excel workbook
batch_classifier_01 <- classification_batches %>%
  filter(responsavel == "classifier_01") %>%
  select(
    pageid,
    titulo,
    link_pagina,
    url_miniatura,
    arquivo_miniatura,
    latitude,
    longitude,
    data_upload_data,
    usuario_upload,
    autor,
    licenca,
    categoria_servico_cultural,
    elementos_visiveis,
    observacao,
    classificar
  )

batch_classifier_02 <- classification_batches %>%
  filter(responsavel == "classifier_02") %>%
  select(
    pageid,
    titulo,
    link_pagina,
    url_miniatura,
    arquivo_miniatura,
    latitude,
    longitude,
    data_upload_data,
    usuario_upload,
    autor,
    licenca,
    categoria_servico_cultural,
    elementos_visiveis,
    observacao,
    classificar
  )

batch_classifier_03 <- classification_batches %>%
  filter(responsavel == "classifier_03") %>%
  select(
    pageid,
    titulo,
    link_pagina,
    url_miniatura,
    arquivo_miniatura,
    latitude,
    longitude,
    data_upload_data,
    usuario_upload,
    autor,
    licenca,
    categoria_servico_cultural,
    elementos_visiveis,
    observacao,
    classificar
  )

batch_classifier_04 <- classification_batches %>%
  filter(responsavel == "classifier_04") %>%
  select(
    pageid,
    titulo,
    link_pagina,
    url_miniatura,
    arquivo_miniatura,
    latitude,
    longitude,
    data_upload_data,
    usuario_upload,
    autor,
    licenca,
    categoria_servico_cultural,
    elementos_visiveis,
    observacao,
    classificar
  )

# save workbook with calibration and all individual batches
classification_workbook <- list(
  resumo_blocos = classification_batch_summary,
  calibracao_30_percent = calibration_images,
  classifier_01 = batch_classifier_01,
  classifier_02 = batch_classifier_02,
  classifier_03 = batch_classifier_03,
  classifier_04 = batch_classifier_04
)

writexl::write_xlsx(
  classification_workbook,
  file.path(path_outputs_tables, "blocos_classificacao_e_calibracao.xlsx")
)

# check saved batch files
saved_batch_files <- list.files(
  path_outputs_tables,
  pattern = "^(bloco_classificacao|blocos_classificacao|calibracao_30_percent|resumo_blocos).*\\.(csv|xlsx)$",
  full.names = TRUE
)

inspect_vector(
  saved_batch_files,
  "saved calibration and classification batch files"
)


##### 39.8. open folders for visual inspection #####

# open the folder with copied calibration thumbnails
if (interactive() && isTRUE(open_output_folders)) {
  
  calibration_folder_to_open <- normalizePath(
    calibration_thumbnail_folder,
    winslash = "/",
    mustWork = FALSE
  )
  
  if (.Platform$OS.type == "windows") {
    shell.exec(calibration_folder_to_open)
  } else {
    browseURL(paste0("file://", calibration_folder_to_open))
  }
}

# open the folder where calibration and batch tables were saved
if (interactive() && isTRUE(open_output_folders)) {
  
  tables_folder_to_open <- normalizePath(
    path_outputs_tables,
    winslash = "/",
    mustWork = FALSE
  )
  
  if (.Platform$OS.type == "windows") {
    shell.exec(tables_folder_to_open)
  } else {
    browseURL(paste0("file://", tables_folder_to_open))
  }
}


##### 40. inspect tables in interactive sessions #####

# open tables only when the script is running interactively
if (interactive() && isTRUE(open_viewer)) {
  View(cices_classification_sheet)
  View(adapted_cices_key)
  View(image_classification_sheet)
  View(calibration_images)
  View(classification_batch_summary)
  View(classification_batches)
}


##### 41. optional interactive mapview check #####

# quick visual check for one selected image
# this block is optional and does not run unless run_optional_mapview <- TRUE
if (interactive() && isTRUE(run_optional_mapview)) {
  
  if (!requireNamespace("mapview", quietly = TRUE)) {
    message("package 'mapview' is not installed. install it to use this optional check.")
  } else {
    mapview::mapview(
      ria_formosa_images |>
        dplyr::filter(pageid == "47418539"),
      layer.name = "imagem_47418539"
    )
  }
}

# ---------------------------- ⋆⋅🌿⋅⋆ ───-----------------------------------
#    developed with R, open tools and care for environmental data
# -------------------------------------------------------------------------
