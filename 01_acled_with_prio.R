# data import ----
baseline <- read_dta(paste0(dir_data_raw, "BCS_baseline.dta"))				# baseline data
acled <- read_csv(paste0(dir_data_raw, "acled_2023.csv"))							# ACLED data

# data clean ----
## prio_gid clean
prio_gid <- baseline %>% 
	select(gid,
				 iso3,
				 latitude,
				 longitude) %>% 
	mutate("xcord" = latitude, 
				 "ycord" = longitude) %>% 
	st_as_sf(coords = c("latitude", "longitude"), crs = 4326) %>%
	arrange(gid) %>% 
	distinct()

write.csv(prio_gid, paste0(dir_data_int, "base_prio_gid.csv"), row.names = FALSE)

## acled as spatial feature
acled <- acled  %>% 
	st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# data merge ----
## merge prio gid with a raster of 0.5x0.5 degrees cells 
gid_raster <- (st_bbox(prio_gid) + 0.5/2*c(-1,-1,1,1)) %>% 
  st_make_grid(cellsize = c(0.5, 0.5)) %>% 
  st_sf()

prio_gid <- gid_raster %>% 
  st_join(prio_gid, join = st_contains, left = FALSE) %>% 
  arrange(gid)

ggplot() + 
  geom_sf(data = slice_head(prio_gid, n = 100)) +
  geom_point(data = slice_head(prio_gid, n = 100), aes(xcord, ycord))

## merge prio gid with acled
acled_with_prio <- acled %>% 
  st_join(prio_gid) %>% 
  filter(!is.na(gid)) %>%
  arrange(gid, year)

# data export ----
write_csv(acled_with_prio, file = paste0(dir_data_int, "acled_with_prio_2023.csv"))
