library(ncdf4)
library(here)

# Open NetCDF file
nc_file <- nc_open("~/data/lpx_dyptop_outputs/LPX-Bern_DYPTOP_vars_1990-2020_0.5x0.5_m.nc")

# List available variables
print(nc_file)

# Read a variable (replace "var_name" with the actual variable)
var_name <- "pwtpos"  # Change this based on your data
data <- ncvar_get(nc_file, var_name)

# Read time information
time <- ncvar_get(nc_file, "TIME")
nc_close(nc_file)

# Reshape into (lon, lat, month, year)
data_reshaped <- array(data, dim = c(720, 360, 12, 31))

# Compute the mean along the year dimension (4th dimension)
seasonal_cycle <- apply(data_reshaped, c(1, 2, 3), mean, na.rm = TRUE)

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", seq(-179.75, 179.75, length.out = 720))
lat_dim <- ncdim_def("lat", "degrees_north", seq(-89.75, 89.75, length.out = 360))
time_dim <- ncdim_def("time", "months", 1:12)

# Define variable
var_def <- ncvar_def("pwtpos", "mm", list(lon_dim, lat_dim, time_dim), 
                     missval = NA, longname = "monthly peatland water table position")

# Create NetCDF file
nc_out <- nc_create(here("data/LPX-Bern_DYPTOP_vars_1990-2020_0.5x0.5_m_MONMEAN.nc"), var_def)

# Write data
ncvar_put(nc_out, var_def, seasonal_cycle)

# Close file
nc_close(nc_out)
