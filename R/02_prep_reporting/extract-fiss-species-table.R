##note that this was not run this year - we just used last years to save time

source('R/functions.R')
source('R/packages.R')
source('R/tables-phase2.R')


conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "postgis",
  host = "localhost",
  port = "5432",
  user = "postgres",
  password = "postgres"
)

# ##we are going to use the whole elk for now but should refine for our study area
# fish_species_watershed <- sf::st_read(conn,
#                                       query = "SELECT DISTINCT x.species_code, x.species_name
#                    FROM whse_fish.fiss_fish_obsrvtn_pnt_sp x
#                    INNER JOIN
#                    whse_basemapping.fwa_named_watersheds_poly nws
#                    ON ST_intersects(x.geom, nws.geom)
#                    WHERE nws.gnis_id IN
#                              ('16880')
#                            GROUP BY x.species_code, x.species_name
#                            ORDER BY x.species_code")


##get the elk first
dat <- sf::read_sf('data/fishpass_mapping.gpkg', layer = 'wshd_study_elk')
schema_name <- "working"
table_name <- "misc"
geom_type <- "polygon"
crs <- 3005
sf::st_write(obj = dat, dsn = conn, Id(schema= "working", table = "misc"))
res <- dbSendQuery(conn,
                   paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geom
           Type geometry(", geom_type, ", ", crs, ")
           USING ST_SetSRID(geom, ", crs, ");"))
dbClearResult(res)

query <- "SELECT DISTINCT x.species_code, x.species_name, ws.study_area as Elk
  FROM whse_fish.fiss_fish_obsrvtn_pnt_sp x
INNER JOIN working.misc ws ON (ST_Within(x.geom, ws.geom));"

fish_spp_elk <- st_read(conn, query = query)


##now get the flathead
dat <- sf::read_sf('data/fishpass_mapping.gpkg', layer = 'wshd_study_flathead')
sf::st_write(obj = dat, dsn = conn, Id(schema= "working", table = "misc"))
res <- dbSendQuery(conn,
                   paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geom
           Type geometry(", geom_type, ", ", crs, ")
           USING ST_SetSRID(geom, ", crs, ");"))
dbClearResult(res)

query <- "SELECT DISTINCT x.species_code, x.species_name, ws.study_area as Flathead
  FROM whse_fish.fiss_fish_obsrvtn_pnt_sp x
INNER JOIN working.misc ws ON (ST_Within(x.geom, ws.geom));"

fish_spp_flathead <- st_read(conn, query = query)

fish_spp <- full_join(
  fish_spp_elk,
  fish_spp_flathead,
  by = c('species_code', 'species_name')
)

fish_all <- fishbc::freshwaterfish
fish_cdc <- fishbc::cdc

# fish_species_lookup <- dbGetQuery(conn,
#                                   "Select * from whse_fish.species_codes_lookup")

# fish_species_lookup <- hab_fish_codes %>%
#   select(common_name, species_code, scientific_name)
#
# fish_species_watershed <- left_join(fish_species_watershed,
#                                     fish_species_lookup,
#                                     by = "species_code")

fish_spp2 <- left_join(fish_spp,
                       fish_all,
                       by = c("species_code" = "Code")) %>%
  filter(!is.na(Class) &
           !species_code %in% c('ACT', 'ST', 'TR', 'C', 'GR', 'WF', 'WST', 'DV')) %>%
  mutate(species_name = case_when(species_name == 'Westslope (Yellowstone) Cutthroat Trout' ~ 'Westslope Cutthroat Trout',
                                  T ~ species_name)) %>%
  mutate(CDCode = case_when(species_code == 'BT' ~ 'F-SACO-11', ##pacific population yo
                             T ~ CDCode)) %>%
  select(species_code, species_name, elk, flathead, CDCode)

fish_spp3 <- left_join(
  fish_spp2,
  fish_cdc,
  by = c('CDCode' = 'Species Code')
) %>%
  select(`Scientific Name`,
         'Species Name' = species_name,
         'Species Code' = species_code,
         `BC List`,
         `Provincial FRPA`,
         COSEWIC,
         SARA,
         `Upper Elk` = elk,
         Flathead = flathead) %>%
  mutate(`Upper Elk` = case_when(!is.na(`Upper Elk`) ~ 'Yes',
                         T ~ `Upper Elk`),
         Flathead = case_when(!is.na(Flathead) ~ 'Yes',
                         T ~ Flathead)) %>%
  # mutate(COSEWIC = case_when(`Species Code` == 'BT' ~ NA_character_,
  #                            T ~ COSEWIC)) %>%
  arrange(`Scientific Name`, `Species Name`) %>%
  replace(., is.na(.), "--")


# #use pipes to group
# fish_table <- fish_species_watershed %>%
#   dplyr::group_by(scientific_name, species_name,gnis_name,species_code) %>%
#   dplyr::summarise(count = n()) %>%
#   dplyr::arrange(gnis_name) %>% ##ditch the rare occurance which look like errors
#   dplyr::filter(count > 1 &
#                   species_name != 'Dolly Varden' &
#                   species_name != 'Rainbow Smelt' &
#                   !stringr::str_detect(species_name, "General") &
#                   !species_code %in% 'TR') %>%
#   ungroup() %>%
#   filter(!is.na(scientific_name)) %>%
#   select('Scientific Name' = scientific_name, 'Species Name' = species_name,
#          'Species Code' = species_code) %>%
#   mutate_all(~replace_na(.,"-")) %>%
#   mutate_all(~stringr::str_replace_all(.,"NA", "-"))


##print your table to input_raw for use in the report
fish_spp3 %>% readr::write_csv(file = paste0(getwd(), '/data/raw_input/fiss_species_table.csv'))


dbDisconnect(conn = conn)
