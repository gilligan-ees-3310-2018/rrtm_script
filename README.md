# rrtm_script
Script to run RRTM model from http://climatemodels.uchicago.edu/rrtm

```run_rrtm <- function(file = NULL, I_solar = 1360.0, T_surface = 284.42, lapse_rate = 6.0, 
                        tropopause_km = 15.0, co2_ppm = 400.0, ch4_ppm = 1.7, 
                        relative_humidity = 80.0, 
                        low_cloud_frac = 0.0, high_cloud_frac = 0.0, cloud_drop_radius = 10.0,
                        surface_type = "earth's average", aerosols = "none", albedo = NULL)
```
where
* `file` is the file to store the data from the model run (with an extension "`.Rds`")
* `I_solar` is the intensity of the sunlight at the top of the atmosphere (in W/m^2)
* `T_surface` is the surface temperature in Kelvin.
* `lapse_rate` is the environmental lapse rate, in Kelvin/kilometer
* `tropopause_km` is the height of the tropopause, in kilometers.
* `co2_ppm` is the concentration of CO2, in parts per million
* `ch4_ppm` is the concentration of methane, in parts per milllion.
* `relative_humidity` is the relative humidity, in percent (from 0 to 100).
* `low_cloud_frac` is the fraction of the sky covered by low clouds (from 0 to 1).
* `high_cloud_frac` is the fraction of the sky covered by high clouds (from 0 to 1).
* `cloud_drop_radius` is the radius of water droplets in the clouds (in microns).
* `surface_type` is the surface of the earth at the location you're looking at. This is
  used to calculate the albedo. Legal values are:
    * "earth's average"
    * "asphalt"
    * "concrete"
    * "desert"
    * "forest"
    * "grass"
    * "ocean"
    * "snow"
    * "bare earth"
    * "custom"
* `aerosols` is the kind of aerosols in the atmosphere. Legal values are:
    * "none"
    * "ocean"
    * "desert"
    * "city"
    * "city just sulphates"
    * "city just soot"
    * "land"
    * "polluted land"
    * "antarctica"
    * "pinatubo"
* `albedo` is the albedo (from 0 to 1), which is used in case `surface_type` is set to `"custom"`.


```read_rrtm(file)```
Read an RRTM file saved from `run_rrtm`

```plot_heat_flows(rrtm, sw = TRUE, lw = TRUE, total = TRUE, text_size = 10)```
Plot the upward and downward heat flows from an RRTM model run.
* `sw`: `TRUE` or `FALSE` to indicate whether to plot shortwave radiation
* `lw`: `TRUE` or `FALSE` to indicate whether to plot longwave radiation
* `total`: `TRUE` or `FALSE` to indicate whether to plot total radiation
