source('R/packages.R')
source('R/functions.R')
source('R/tables-phase2.R')
source('R/tables.R')

##make your geopackage for mapping
make_geopackage <- function(dat, gpkg_name = 'fishpass_mapping'){
  nm <-deparse(substitute(dat))
  dat %>%
    sf::st_as_sf(coords = c("utm_easting", "utm_northing"), crs = 26911, remove = F) %>%
    st_transform(crs = 3005) %>%
    sf::st_write(paste0("./data/", gpkg_name, ".gpkg"), nm, delete_layer = TRUE)
}

# ##just scab together the new id's with the old ones to save time
# ##this is built with load-crossings-xref.R file
# xref_pscis_my_crossing_modelled <- readr::read_csv(file = paste0(getwd(), '/data/raw_input/xref_pscis_my_crossing_modelled.csv')) %>%
#   mutate(across(everything(), as.integer))

# phase1_priorities <- left_join(
#   phase1_priorities,
#   select(xref_pscis_my_crossing_modelled, my_crossing_reference, stream_crossing_id),
#   by = 'my_crossing_reference'
# )


phase1_priorities2 <- phase1_priorities %>%
  mutate(pscis_crossing_id = as.character(pscis_crossing_id),
         my_crossing_reference = as.character(my_crossing_reference)) %>%
  mutate(id_combined = case_when(
  !is.na(pscis_crossing_id) ~ pscis_crossing_id,
  T ~ paste0('*', my_crossing_reference
  ))) %>% sf::st_as_sf(coords = c("utm_easting", "utm_northing"), crs = 26911, remove = F) %>%
  st_transform(crs = 3005)



make_geopackage(dat = hab_fish_collect)
make_geopackage(dat = hab_features)
make_geopackage(dat = hab_site_priorities)
# make_geopackage(dat = phase1_priorities2)

##we do this manually so we don't clutter the file with another version and we don't mangle the original file name
phase1_priorities2 %>%
sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'phase1_priorities', delete_layer = TRUE)

##add the tracks
sf::read_sf("./data/habitat_confirmation_tracks.gpx", layer = "tracks") %>%
  sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'hab_tracks', append = TRUE)

####------------add the watersheds-------------------------

##having the watersheds derived is nice so lets try
##make a function to retrieve the watershed info
get_watershed <- function(dat){
  mapply(fwapgr::fwa_watershed_at_measure,
         blue_line_key = dat$blue_line_key,
         downstream_route_measure = dat$downstream_route_measure,
         SIMPLIFY = F) %>%
    purrr::set_names(nm = dat$pscis_crossing_id) %>%
    discard(function(x) nrow(x) == 0) %>% ##remove zero row tibbles with https://stackoverflow.com/questions/49696392/remove-list-elements-that-are-zero-row-tibbles
    data.table::rbindlist(idcol="pscis_crossing_id") %>%
    distinct(pscis_crossing_id, .keep_all = T) %>% ##in case there are duplicates we should get rid of
    st_as_sf()
}



# ##for each site grab a blueline key and downstream route measure
# hab_site_priorities2 <- hab_site_priorities %>%
#   mutate(srid = as.integer(26911))
#
# hab_site_fwa_index <- mapply(fwapgr::fwa_index_point,
#                              x = hab_site_priorities2$utm_easting,
#                              y = hab_site_priorities2$utm_northing,
#                              srid = hab_site_priorities2$srid,
#                              SIMPLIFY = F) %>%
#   purrr::set_names(nm = hab_site_priorities2$alias_local_name) %>%
#   data.table::rbindlist(idcol="alias_local_name") %>%
#   st_as_sf()

# ##now lets get our watersheds
# hab_site_fwa_wshds <- get_watershed(dat = hab_site_fwa_index)

##filter only phase 2 sites that were qa'd to a modelled crossing thorugh
wshed_input <- bcfishpass_phase2 %>%
  filter(source %like% 'phase2' &
           !is.na(modelled_crossing_id)) %>%
  select(-geom, -geometry) %>%
  mutate(downstream_route_measure_chk = case_when(
    downstream_route_measure < 200 ~ 20,
    T ~ NA_real_
  ),
  downstream_route_measure = case_when(
    !is.na(downstream_route_measure_chk) ~ downstream_route_measure_chk,
    T ~ downstream_route_measure
  ))


##now lets get our watersheds
hab_site_fwa_wshds <- left_join(
  get_watershed(dat = wshed_input) %>% mutate(pscis_crossing_id = as.integer(pscis_crossing_id)),
  select(wshed_input, -localcode_ltree, -wscode_ltree),
  by = 'pscis_crossing_id'
)

##add to the geopackage
hab_site_fwa_wshds %>%
  sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'hab_wshds', append = F) ##might want to f the append....

# hab_site_fwa_index %>%
#   sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'hab_fwa_index', append = T) ##might want to f the append....

##get the watersheds for the study area as a whole
##here is the elk
wshd_study_elk <- fwapgr::fwa_watershed_at_measure(blue_line_key = 356570562, downstream_route_measure = 22910, epsg = 3005) %>%
  mutate(study_area = 'elk')

wshd_study_elk %>%
sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'wshd_study_elk', append = F)

##here is the flathead
wshd_study_flathead <- st_read(conn,
                               query = "SELECT * FROM whse_basemapping.fwa_named_watersheds_poly a
                               WHERE a.named_watershed_id = '4600'"
) %>%
  mutate(study_area = 'flathead')

wshd_study_flathead %>%
  # select(-wscode_ltree) %>%
  st_cast("POLYGON") %>%
  sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'wshd_study_flathead', append = F)

##############################################################################
##############################################################################
##############################################################################
##make a kml of the planning info

tab_plan <- tab_plan_raw %>%
  filter(!is.na(my_text) & !my_text %ilike% 'assessed') %>%
  arrange(stream_crossing_id, modelled_crossing_id) %>%
  mutate(my_priority = case_when(my_priority == 'mod' ~ 'moderate',
                                 T ~ my_priority)) %>%
  select(Area = study_area,
         Priority = my_priority,
         `PSCIS ID` = stream_crossing_id,
         `Modelled ID` = modelled_crossing_id,
         `Species` = observedspp_upstr,
         `Order` = stream_order,
         `Upstream habitat (km)` = wct_network_km,
         `Channel width` = downstream_channel_width,
         `Habitat value` = habitat_value_code,
         `Image link` = image_view_url,
         easting,
         northing,
         Comments = my_text)


df <- make_kml_col(tab_plan)

df <- df %>%
  dplyr::group_split(site_id) %>%
  purrr::map(make_html_tbl) %>%
  dplyr::bind_rows()

coords <- df %>% select(easting, northing)
proj4string <- sp::CRS("+init=epsg:26911")
df <- df %>%
  sp::SpatialPointsDataFrame(coords = coords, proj4string = proj4string) %>%
  plotKML::reproject()

# shape = "http://maps.google.com/mapfiles/kml/paddle/A.png"
# shape = "http://maps.google.com/mapfiles/kml/paddle/wht-blank.png"
# shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"


# kml_open("data/outgoing/barrier_assessments.kml")
kml_open("data/Attachment_1_elk_planning.kml")
kml_layer(df, shape = df$shape, colour = df$color, labels = df$label, html.table = df$html_tbl, z.scale = 2, LabelScale = 1, size = 1.5)
kml_close("data/Attachment_1_elk_planning.kml")


##now we will zip up the kml files in the data folder and rename with kmz
files_to_zip <- paste0("data/", list.files(path = "data/", pattern = "\\.kml$"))  ##this used to includes the planning file which we don't want to do so watch out
zip::zipr("data/Attachment_1_elk_planning_kml.zip", files = files_to_zip)  ##it does not work to zip to kmz!!

##add the planning to the goepackage
tab_plan_sf %>%
sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'planning_priorities', append = F)

st_layers(paste0("./data/", 'fishpass_mapping', ".gpkg"))

