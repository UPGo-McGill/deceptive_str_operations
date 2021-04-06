source("R/01_source.R")


# Load data ---------------------------------------------------------------

property <- qread("output/property.qs")

cities_shp <- 
property %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") %>% 
  count(city, name = "Market size", sort = T) %>% 
  mutate(geometry_center = st_centroid(geometry))

country_shp <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(admin %in% c("Canada", "United States of America", "Mexico")) %>% 
  st_transform(crs = st_crs(cities_shp))


# Map ---------------------------------------------------------------------

# For an addition to the bbox, deciding how zoomed out the map will be
around <- 5

north_america_10_str <-
ggplot(data = country_shp) +
  geom_sf(color = "grey80", fill = "grey95")+
  geom_sf(data = cities_shp, aes(size = `Market size`, fill = `Market size`, 
                                 geometry = geometry_center), 
          shape = 21, color = "transparent")+
  scale_size_continuous(range = c(1,8), breaks=scales::breaks_extended(4)) +
  scale_fill_gradientn(colours = gp_gradient1, breaks=scales::breaks_extended(4)) +
  guides(fill = guide_legend(), size = guide_legend())+
  ggrepel::geom_label_repel(
    data = cities_shp,
    aes(label = city, geometry = geometry_center),
    stat = "sf_coordinates",
    colour = "black",
    segment.colour = "black",
    box.padding = 0.5,
  )+
  coord_sf(xlim = c(st_bbox(cities_shp$geometry_center)["xmin"]-around,
                    st_bbox(cities_shp$geometry_center)["xmax"]+around), 
           ylim = c(st_bbox(cities_shp$geometry_center)["ymin"]-1,
                    st_bbox(cities_shp$geometry_center)["ymax"]+around))+
  theme_void()+
  theme(legend.position = "bottom")

# Get the city name in order of market size for the report.
cities_shp %>% 
  arrange(-`Market size`) %>% 
  pull(city) %>% 
  paste(collapse = ", ")

# Save geometries and graph -----------------------------------------------

ggsave("output/figures/01_north_america_10_str.png", plot = north_america_10_str, width = 8, 
       height = 5, units = "in")

qsavem(cities_shp, country_shp, file = "output/geometries.qsm")
