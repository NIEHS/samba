# barbara

The development of this library is in progress.

## Spatiotemporal Bayesian model for the creation of an hourly gridded product of air temperature across the US

Key points:

-   Covariates included in the analysis incorporate specificity of the urban environment for a better inference of the urban heat island. Most of them are downloaded thanks to the amadeus R package developed by the NIEHS.

-   Inference results reach 1km\*1km resolution and hourly timesteps

-   Spatiotemporal model is inferred with INLA and incorporates data from WeatherUnderground personal weather stations

## How to use? 

### Step 1: load libary

### Step 2: download all necessary data

- Personal weather stations data comes from WeatherUnderground, and is processed through brassens library 
- Spatial covariates include elevation, forest canopy height and imperviousness
- Meteorological covariates all come from ERA5 reanalysis at single level

### Step 3: run samba function 

Prepare function arguments
```{r}
args <- NULL
args[1] <- "2021-07-01 00:00:00"
args[2] <- "2021-07-31 23:00:00"
args[3] <- "./area.shp"
args[4] <- "./cws_raw.rds"
args[5] <- "../input/forest_height_2019_nam.tif"
args[6] <- "../input/gmted_medianstat_7-5arcsec.tif"
args[7] <- "../input/data_files/nlcd_2021_impervious_l48_20230630.img"
args[8] <- "../input/data_stream-oper_stepType-instant.nc"
args[9] <- "../input/data_stream-oper_stepType-accum.nc"
args[10] <- "example"
args[11] <- "./output/"
```

Create output dir
```{r}
if (!dir.exists(args[11])) {
  dir.create(args[11], recursive = TRUE)
}
```

Run samba function
```{r}
output <- samba(
  as.POSIXct(args[1], tz = "UTC", format = "%Y-%m-%d %H:%M:%S"),
  as.POSIXct(args[2], tz = "UTC", format = "%Y-%m-%d %H:%M:%S"),
  args[3],
  args[4],
  args[5],
  args[6],
  args[7],
  args[8],
  args[9]
)
```

Save prediction mean and sd rasters
```{r}
terra::writeRaster(
  x = output$pred_mean,
  file = paste0(
    args[11],
    args[10],
    "_inference_predmean_",
    format(min(output$inf_out$pred$time), "%Y%m%d%H"),
    "_",
    format(max(output$inf_out$pred$time), "%Y%m%d%H"),
    ".tif"
  )
)
terra::writeRaster(
  x = output$pred_sd,
  file = paste0(
    args[11],
    args[10],
    "_inference_predsd_",
    format(min(output$inf_out$pred$time), "%Y%m%d%H"),
    "_",
    format(max(output$inf_out$pred$time), "%Y%m%d%H"),
    ".tif"
  )
)
``` 