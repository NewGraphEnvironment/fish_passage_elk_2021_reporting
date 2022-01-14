# source('R/packages.R')
# source('R/functions.R')
source('R/0320-tables-phase2.R')
# source('R/tables.R')

##make your geopackage for mapping
make_geopackage <- function(dat, gpkg_name = 'fishpass_mapping', utm_zone = 9){
  nm <-deparse(substitute(dat))
  dat %>%
    sf::st_as_sf(coords = c("utm_easting", "utm_northing"), crs = 26900 + utm_zone, remove = F) %>%
    st_transform(crs = 4326) %>%
    sf::st_write(paste0("./data/fishpass_mapping/", gpkg_name, ".gpkg"), nm, delete_layer = TRUE)
}


make_geopackage(dat = hab_fish_collect)
make_geopackage(dat = hab_features)
make_geopackage(dat = hab_site_priorities)
make_geopackage(dat = phase1_priorities)

##we do this manually since the
# phase1_priorities %>%
#   st_transform(crs = 3005) %>%
#   sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'phase1_priorities', delete_layer = TRUE)



##add the tracks
sf::read_sf("./data/habitat_confirmation_tracks.gpx", layer = "tracks") %>%
  sf::st_write(paste0("./data/fishpass_mapping/", 'fishpass_mapping', ".gpkg"), 'hab_tracks', append = TRUE)

##study area watersheds
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "postgis",
  host = "localhost",
  port = "5432",
  user = "postgres",
  password = "postgres"
)

# dbGetQuery(conn,
#            "SELECT column_name,data_type
#            FROM information_schema.columns
#            WHERE table_name='fwa_watershed_groups_poly'")

##here is the study area watersheds
wshd_study_areas <- st_read(conn,
                           query = "SELECT * FROM whse_basemapping.fwa_watershed_groups_poly a
                               WHERE a.watershed_group_code  = 'BULK'
                           OR a.watershed_group_code  = 'MORR'"
)

wshd_study_areas %>%
  # select(-wscode_ltree) %>%
  st_cast("POLYGON") %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_write(paste0("./data/fishpass_mapping/", 'fishpass_mapping', ".gpkg"), 'wshd_study_areas', append = F)

dbDisconnect(conn = conn)

####------------add the watersheds-------------------------

##having the watersheds derived is nice so lets try
##make a function to retrieve the watershed info
get_watershed <- function(dat){
  mapply(fwapgr::fwa_watershed_at_measure,
         blue_line_key = dat$blue_line_key,
         downstream_route_measure = dat$downstream_route_measure,
         SIMPLIFY = F) %>%
    purrr::set_names(nm = dat$stream_crossing_id) %>%
    discard(function(x) nrow(x) == 0) %>% ##remove zero row tibbles with https://stackoverflow.com/questions/49696392/remove-list-elements-that-are-zero-row-tibbles
    data.table::rbindlist(idcol="stream_crossing_id") %>%
    distinct(stream_crossing_id, .keep_all = T) %>% ##in case there are duplicates we should get rid of
    st_as_sf()
}

##we needed to remove crossings that are first order - this used to run but doesn't want to anymore
##i wonder if it is because the 1st order watershed is the first one on the list so the api kicks us off...
bcfishpass_phase2_clean <- bcfishpass_phase2 %>%
  filter(stream_order != 1)

wshds <- get_watershed(bcfishpass_phase2_clean)

# ##add to the geopackage
wshds %>%
  sf::st_write(paste0("./data/fishpass_mapping/", 'fishpass_mapping', ".gpkg"), 'hab_wshds', append = F) ##might want to f the append....

#burn to kml as well so we can see elevations
st_write(wshds, append = TRUE, driver = 'kml', dsn = "data/extracted_inputs/wshds.kml")

####--------------------burn geojsons from geopackage-----------------------------------------------------
##we need geojsons to make the mapping convenient so lets pull everything out of the geopackage and write to geojson files
read_gpkg <- function(layers = layer_name){
  sf::st_read(dsn = "./data/fishpass_mapping.gpkg", layer = layers) %>%
    mutate(name = layers)
    # sf::st_transform(crs = 4326)
}

##get the names of the layers you want
layer_names <- sf::st_layers(paste0("./data/fishpass_mapping/", 'fishpass_mapping', ".gpkg")) %>%
  pluck('name')

##grab the layers and give them a name
layers_to_burn <- layer_names %>%
  purrr::map(read_gpkg) %>%
  purrr::set_names(nm = layer_names)

##now burn them to a folder called fishpass_mapping
dir.create('data/fishpass_mapping')

write_geojson <- function(layers){
  layers %>%
  geojsonio::geojson_write(file = paste0("./data/fishpass_mapping/", unique(layers$name), ".geojson"))
}

layers_to_burn %>%
  map(write_geojson)

##make a kml of the planning info
tab_plan <- tab_plan_raw %>%
  filter(!is.na(my_text)) %>%
  arrange(stream_crossing_id, modelled_crossing_id) %>%
  mutate(my_priority = case_when(my_priority == 'mod' ~ 'moderate',
                                 T ~ my_priority)) %>%
  select(
         Priority = my_priority,
         `PSCIS ID` = stream_crossing_id,
         `Modelled ID` = modelled_crossing_id,
         `Species` = observedspp_upstr,
         `Order` = stream_order,
         `Upstream habitat (km)` = wct_network_km,
         `Channel width` = downstream_channel_width,
         `Habitat value` = habitat_value_code,
         `Image link` = image_view_url,
          long,
          lat,
         Comments = my_text)


df <- make_kml_col(tab_plan)

df <- df %>%
  dplyr::group_split(site_id) %>%
  purrr::map(make_html_tbl) %>%
  dplyr::bind_rows()

coords <- df %>%
  # select(easting, northing) %>%
  select(long, lat)
proj4string <- sp::CRS("+init=epsg:3005") #26911
df <- df %>%
  sp::SpatialPointsDataFrame(coords = coords, proj4string = proj4string) %>%
  plotKML::reproject()

# shape = "http://maps.google.com/mapfiles/kml/paddle/A.png"
# shape = "http://maps.google.com/mapfiles/kml/paddle/wht-blank.png"
# shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"


# kml_open("data/outgoing/barrier_assessments.kml")
kml_open("data/Attachment_1_morice_planning.kml")
kml_layer(df, shape = df$shape, colour = df$color, labels = df$label, html.table = df$html_tbl, z.scale = 2, LabelScale = 1, size = 1.5)
kml_close("data/Attachment_1_morice_planning.kml")

##now we will zip up the kml files in the data folder and rename with kmz
files_to_zip <- paste0("data/", list.files(path = "data/", pattern = "\\.kml$"))  ##this used to includes the planning file which we don't want to do so watch out
zip::zipr("data/Attachment_1_morice_planning_kml.zip", files = files_to_zip)  ##it does not work to zip to kmz!!
