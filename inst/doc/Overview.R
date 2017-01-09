## ----message = FALSE, echo = FALSE---------------------------------------
library(dplyr)

## ------------------------------------------------------------------------
library(stormwindmodel)
data("floyd_tracks")
head(floyd_tracks)
data("katrina_tracks")
head(katrina_tracks)

## ------------------------------------------------------------------------
data(county_points)
head(county_points)

## ----eval = FALSE, echo = c(1)-------------------------------------------
#  floyd_winds <- get_grid_winds(hurr_track = floyd_tracks, grid_df = county_points)
#  save(floyd_winds, file = "data/floyd_winds.Rdata")

## ----echo = c(2:4)-------------------------------------------------------
load("data/floyd_winds.Rdata")
floyd_winds %>%
  dplyr::select(gridid, vmax_gust, vmax_sust, gust_dur, sust_dur) %>%
  slice(1:6)

## ----fig.width = 8-------------------------------------------------------
map_wind(floyd_winds)

## ----message = FALSE, warning = FALSE------------------------------------
library(tigris)
new_orleans <- tracts(state = "LA", county = c("Orleans")) 

## ----message = FALSE-----------------------------------------------------
library(rgeos)
new_orleans_tract_centers <- gCentroid(new_orleans, byid = TRUE)@coords
head(new_orleans_tract_centers)

## ------------------------------------------------------------------------
new_orleans_tract_centers <- new_orleans_tract_centers %>%
  tbl_df() %>%
  mutate(gridid = unique(new_orleans@data$TRACTCE)) %>%
  rename(glat = y, 
         glon = x)
head(new_orleans_tract_centers)

## ----message = FALSE, warning = FALSE------------------------------------
library(ggplot2)
library(ggmap)
library(sp)

new_orleans_bbox <- bbox(new_orleans)

## ----message = FALSE, warning = FALSE, fig.width = 6, fig.height = 6, eval = FALSE, echo = c(2:10)----
#  png(filename = "figures/new_orleans1.png", height = 400, width = 400)
#  get_map(location = new_orleans_bbox + c(-0.1, -0.1, 0.1, 0.1), source = "stamen") %>%
#    ggmap() +
#    geom_polygon(data = fortify(new_orleans),
#                 aes(x = long, y = lat, group = group),
#                 color = "black", fill = NA) +
#    theme_void() +
#    geom_point(data = new_orleans_tract_centers,
#               aes(x = glon, y = glat, group = NULL),
#               color = "red", size = 1, alpha = 0.5)
#  dev.off()

## ----echo = FALSE, fig.width = 6, fig.height = 6-------------------------
knitr::include_graphics("figures/new_orleans1.png")

## ----eval = FALSE, echo = c(1:2)-----------------------------------------
#  new_orleans_tracts_katrina <- get_grid_winds(hurr_track = katrina_tracks,
#                                               grid_df = new_orleans_tract_centers)
#  save(new_orleans_tracts_katrina, file = "data/new_orleans.Rdata")

## ----echo = c(2)---------------------------------------------------------
load("data/new_orleans.Rdata")
head(new_orleans_tracts_katrina)

## ----message = FALSE, message = FALSE, warning = FALSE, fig.width = 7, fig.height = 6----
library(viridis)

new_orleans <- new_orleans %>% 
  fortify(region = "TRACTCE") %>%
  left_join(new_orleans_tracts_katrina, by = c("id" = "gridid"))

## ----eval = FALSE, echo = c(2:9)-----------------------------------------
#  png("figures/new_orleans2.png", height = 400, width = 500)
#  get_map(location = new_orleans_bbox + c(-0.1, -0.1, 0.1, 0.1), source = "stamen") %>%
#    ggmap() +
#    geom_polygon(data = new_orleans,
#                 aes(x = long, y = lat, group = group,
#                     fill = vmax_sust),
#                 color = "black", alpha = 0.7, size = 0.2) +
#    scale_fill_viridis(name = "Maximum\nsustained\nwind (m / s)") +
#    theme_void()
#  dev.off()

## ----echo = FALSE, fig.width= 7, fig.height = 5--------------------------
knitr::include_graphics("figures/new_orleans2.png")

## ----warning = FALSE, message = FALSE, fig.width = 6, fig.height = 2.5----
dare_county <- county_points %>% # Get grid point information for Dare County
  filter(gridid == "37055")

with_wind_radii <- floyd_tracks %>%
  create_full_track() %>% # Interpolate tracks to every 15 minutes
  add_wind_radii()        # Calculate required inputs for Willoughby wind model

dare_winds <- calc_grid_wind(grid_point = dare_county,          # Model winds at one grid point
                             with_wind_radii = with_wind_radii)

ggplot(dare_winds, aes(x = date, y = windspeed)) + 
  geom_line() + 
  xlab("Observation time (UTC)") + 
  ylab("Modeled surface wind (m / s)") 

## ----fig.width = 8-------------------------------------------------------
floyd_map <- map_wind(floyd_winds)
add_storm_track(floyd_tracks, plot_object = floyd_map)

## ----fig.width = 8-------------------------------------------------------
map_wind(floyd_winds, value = "vmax_gust", wind_metric = "knots")

## ----fig.width = 8-------------------------------------------------------
map_wind(floyd_winds, value = "vmax_sust", wind_metric = "knots", break_point = 34)

