source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')

pscis_all <- bind_rows(fpr_import_pscis_all())

# n_distinct(pscis_all$aggregated_crossings_id)

dat <- pscis_all %>%
  sf::st_as_sf(coords = c("easting", "northing"),
               crs = 26911, remove = F) %>% ##don't forget to put it in the right crs buds
  sf::st_transform(crs = 3005) ##get the crs same as the layers we want to hit up


##get the road info from the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
)
#
# ##listthe schemas in the database
# dbGetQuery(conn,
#            "SELECT schema_name
#            FROM information_schema.schemata")
# #
# #
# # # ##list tables in a schema
dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='bcfishpass'")
# # # # #
# # # # # ##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='crossings'") #modelled_stream_crossings #modelled_crossings_closed_bottom


# add a unique id - we could just use the reference number
dat$misc_point_id <- seq.int(nrow(dat))

# dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", "test_hack",";"))
# load to database
sf::st_write(obj = dat, dsn = conn, Id(schema= "ali", table = "misc"))

# we are using fish_passage.modelled_crossings_closed_bottom but this table is deprecated. Should revise to pull from files raw but that's lots of work
# and this data should be fine so whatever
# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON ali.misc USING GIST (geometry)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE ali.misc ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)
# swapped out fish_passage.modelled_crossings_closed_bottom
dat_info <- dbGetQuery(conn, "SELECT
  a.misc_point_id,
  b.*,
  ST_Distance(ST_Transform(a.geometry,3005), b.geom) AS distance
FROM
  ali.misc AS a
CROSS JOIN LATERAL
  (SELECT *
   FROM fish_passage.modelled_crossings_closed_bottom
   ORDER BY
     a.geometry <-> geom
   LIMIT 1) AS b")

##join the modelling data to our pscis submission info
dat_joined <- left_join(
  select(dat, misc_point_id,
         pscis_crossing_id,
         my_crossing_reference,
         aggregated_crossings_id,
         stream_name,
         road_name,
         downstream_channel_width_meters,
         barrier_result,
         fill_depth_meters,
         crossing_fix,
         habitat_value,
         recommended_diameter_or_span_meters,
          source), ##traded pscis_crossing_id for my_crossing_reference
  select(dat_info,
         misc_point_id:utm_northing,
         distance, geom), ##keep only the road info and the distance to nearest point from here
  by = "misc_point_id"
  )

dbDisconnect(conn = conn)

pscis_rd <- dat_joined %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(my_road_class = case_when(is.na(road_class) & !is.na(file_type_description) ~
                                            file_type_description,
                                          T ~ road_class)) %>%
  dplyr::mutate(my_road_class = case_when(is.na(my_road_class) & !is.na(owner_name) ~
                                            'rail',
                                          T ~ my_road_class)) %>%
  dplyr::mutate(my_road_surface = case_when(is.na(road_surface) & !is.na(file_type_description) ~
                                              'loose',
                                            T ~ road_surface)) %>%
  dplyr::mutate(my_road_surface = case_when(is.na(my_road_surface) & !is.na(owner_name) ~
                                              'rail',
                                            T ~ my_road_surface)) %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_class_surface.csv'))


####----tab cost multipliers for road surface-----
tab_cost_rd_mult <- pscis_rd %>%
  select(my_road_class, my_road_surface) %>%
  # mutate(road_surface_mult = NA_real_, road_class_mult = NA_real_) %>%
  mutate(road_class_mult = case_when(my_road_class == 'local' ~ 4,
                                     my_road_class == 'collector' ~ 4,
                                     my_road_class == 'arterial' ~ 15,
                                     my_road_class == 'highway' ~ 15,
                                     my_road_class == 'rail' ~ 15,
                                     T ~ 1))  %>%
  mutate(road_surface_mult = case_when(my_road_surface == 'loose' |
                                         my_road_surface == 'rough' ~
                                         1,
                                       T ~ 2)) %>%
  # mutate(road_type_mult = road_class_mult * road_surface_mult) %>%
  mutate(cost_m_1000s_bridge = road_surface_mult * road_class_mult * 20,  #changed from 12.5 due to inflation
         cost_embed_cv = road_surface_mult * road_class_mult * 40) %>%
  # mutate(cost_1000s_for_10m_bridge = 10 * cost_m_1000s_bridge) %>%
  distinct( .keep_all = T) %>%
  tidyr::drop_na() %>%
  arrange(cost_m_1000s_bridge, my_road_class) %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_cost_mult.csv'))



