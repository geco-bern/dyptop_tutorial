# Define the function
plot_map <- function(
    elevation_file,
    bounding_box,
    cti_crit = NA,
    dir_ne = "~/data/naturalearth/",
    show_countries = TRUE,
    show_legend = FALSE,
    show_inset = TRUE
    ){
  # elevation_file: Path to the elevation raster file
  # bounding_box: A numeric vector in the format c(xmin, xmax, ymin, ymax)

  # Load the elevation data
  elevation <- rast(elevation_file)

  # Crop the elevation data to the specified bounding box
  cropped_elevation <- crop(elevation, ext(bounding_box))

  # Convert the cropped raster to a tidy data frame
  elevation_df <- as.data.frame(cropped_elevation, xy = TRUE, na.rm = TRUE)
  
  # if (!is.na(cti_crit)){
  #   flooded_df <- elevation_df
  #   flooded_df[,3] <- ifelse(flooded_df[,3] > cti_crit, 1, NA)
  # }

  # Helper function to format degrees with N/S and E/W
  format_degrees <- function(value, axis) {
    if (axis == "y") {
      ifelse(value > 0, paste0(value, "°N"), paste0(-value, "°S"))
    } else {
      ifelse(value > 0, paste0(value, "°E"), paste0(-value, "°W"))
    }
  }
  
  if (show_inset){
    
    # Define the orthographic projection focused on region
    mid_lat <- format(mean(bounding_box[c(3,4)]), digits = 2)
    mid_lon <- format(mean(bounding_box[c(1,2)]), digits = 2)
    sphere_proj <- paste0("+proj=ortho +lat_0=", mid_lat, " +lon_0=", mid_lon, " +datum=WGS84 +units=m +no_defs")
    
    # Create the world outline for the globe
    if (show_countries){
      world <- ne_countries(scale = "small", returnclass = "sf")
    } else {
      world <- ne_coastline(scale = "small", returnclass = "sf")
    }
    
    # Validate the geometries to avoid projection errors
    world <- st_make_valid(world)
    
    # Transform the world to the orthographic projection
    world_proj <- st_transform(world, crs = sphere_proj)
    
    # Create the bounding box for region in projected space
    bbox <- st_bbox(c(xmin = bounding_box[1], xmax = bounding_box[2], ymin = bounding_box[3], ymax = bounding_box[4]), crs = st_crs(4326))
    bbox_proj <- st_transform(st_as_sfc(st_bbox(bbox)), crs = sphere_proj)
    
    # Create a circle outline for the Earth's sphere
    sphere <- st_buffer(
      st_sfc(st_point(c(0, 0)), crs = st_crs(sphere_proj)),
      dist = 6371000
    )
    
    # # Validate and clip the world and bounding box to the visible hemisphere
    # world_proj <- tryCatch(
    #   st_intersection(world_proj, sphere),
    #   error = function(e) stop("Intersection with the sphere failed. Please check geometries.")
    # )
    
    # Ensure no invalid or empty geometries after intersection
    world_proj <- world_proj[!st_is_empty(world_proj), ]
    
  }

  # Create the main map
  main_map <- ggplot() +

    # Add elevation layer
    geom_raster(data = elevation_df, aes(x = x, y = y, fill = Band1), show.legend = show_legend) +
    scale_fill_viridis_c(
      option = "magma",
      name = "CTI",
      guide = guide_colorbar(
        title.position = "left",
        title.hjust = 0.5,
        barwidth = unit(7, "cm"),
        barheight = unit(0.4, "cm")
      )
    ) +
    scale_x_continuous(
      expand = c(0, 0), # Remove the buffer for the x-axis
      labels = function(x) format_degrees(x, "x") # Format longitude
    ) +
    scale_y_continuous(
      expand = c(0, 0), # Remove the buffer for the y-axis
      labels = function(y) format_degrees(y, "y") # Format latitude
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(), # Remove axis titles
      axis.title.y = element_blank(), # Remove axis titles
      axis.text = element_text(size = 8), # Reduce tick label size
      axis.ticks = element_line(size = 0.3), # Reduce tick mark size
      axis.ticks.length = unit(2, "pt"), # Shorten tick marks
      legend.position = "bottom", # Position the legend at the bottom of the plot
      legend.title = element_text(size = 10), # Adjust title font size
      legend.text = element_text(size = 8)    # Adjust legend text size
    ) +
    coord_fixed()
  
  if (!is.na(cti_crit)){
    main_map <- main_map +
      geom_raster(
        data = elevation_df |> 
          filter(Band1 > cti_crit), 
        aes(x = x, y = y, fill = Band1), 
        fill = "royalblue"
        )
  }

  if (show_inset){
    # Create the globe inset
    globe_inset <- ggplot() +
      geom_sf(data = sphere, fill = "grey30", color = "black", linewidth = 0.6) + # Earth's sphere outline
      geom_sf(data = world_proj, fill = "gray80", color = "white", linewidth = 0.2) +
      geom_sf(data = bbox_proj, fill = NA, color = "red", linewidth = 1) +
      theme_void() +
      # theme(panel.background = element_rect(fill = "lightblue")) +
      coord_sf(crs = sphere_proj)
    
    # Combine main map and inset
    combined_map <- ggdraw() +
      draw_plot(main_map, 0, 0, 1, 1) +
      draw_plot(globe_inset, 0.75, 0.7, 0.3, 0.3) # Adjusted position and size of the inset
    
    return(combined_map)
  } else {
    return(main_map)
  }
}
