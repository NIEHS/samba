# samba

The development of this library is in progress.

## Spatiotemporal Air-temperature Model with Bayesian Approach 
#### Creation of an hourly gridded product of air temperature across the US

Key points:

-   Covariates included in the analysis incorporate specificity of the urban environment for a better inference of the urban heat island. Most of them are downloaded thanks to the amadeus R package developed by the NIEHS.

-   Inference results reach 1km\*1km resolution and hourly timesteps

-   Spatiotemporal model is inferred with INLA and incorporates data from WeatherUnderground personal weather stations
