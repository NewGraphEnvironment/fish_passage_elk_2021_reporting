source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')


###these scripts will pull out the utms from bcfishpass https://github.com/smnorris/bcfishpass generated modelled crossings
##we improved on this in the 103-extract-rd-tenure script by add an aggregated_crossings_id.  We could use that method next time!!

##make a dataframe to pull info from the db
##we should probably break each row out and determine the crs by the utm_zone attribute
##lets do both phases at once to create a file for feeding back to bcfishpass


##this is weird but we know these will be dups because we check at the end of this script.
##lets pull these out of these files at the start

# dups <- c(4600183, 4600069, 4600367, 4605732, 4600070)

dat1 <- import_pscis(workbook_name = 'pscis_phase1.xlsm')
dat2 <- import_pscis(workbook_name = 'pscis_phase2.xlsm')
dat3 <- import_pscis(workbook_name = 'pscis_reassessments.xlsm')


dat_prep <- bind_rows(
  dat1,
  dat2
)

dat <- bind_rows(
  dat_prep,
  dat3
)


##get the utm info from the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname_wsl,
  host = host_wsl,
  port = port_wsl,
  user = user_wsl,
  password = password_wsl
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
           WHERE table_name='crossings'")



##pull out the details for the crossings that match the modelled ids in our dat
##derive coordinates useing sf
##burn csvs for each of the input files so we can copy and paste back in

##isolate the id's that we want info for
id <- dat %>%
  # filter(is.na(easting)) %>%
  pull(my_crossing_reference)


#######make sure you change the crs based on the utm zone (ie. add the zone number to 26900)
# ##this is for phase 1
sql <- glue::glue_sql("SELECT x.*, ST_X(ST_TRANSFORM(x.geom, 26911)) as utm_easting_derived, ST_Y(ST_TRANSFORM(x.geom, 26911)) as utm_northing_derived FROM bcfishpass.crossings x WHERE x.modelled_crossing_id IN ({id*})",
                      .con = conn)


query <- DBI::dbSendQuery(conn, sql)
df <- DBI::dbFetch(query)
dbClearResult(query)

id_joined <- left_join(
  dat %>% select(rowid, pscis_crossing_id, my_crossing_reference, source, easting, northing),
  df %>% select(modelled_crossing_id, utm_easting_derived, utm_northing_derived),
  by = c('my_crossing_reference' = 'modelled_crossing_id')
) %>%
  mutate(utm_easting = case_when(!is.na(easting) ~ easting,
         T ~ utm_easting_derived),
         utm_northing = case_when(!is.na(northing) ~ northing,
         T ~ utm_northing_derived)
  ) %>%
  select(-utm_easting_derived, -utm_northing_derived)


##now export csvs for each of the sources
test <- id_joined %>%
  filter(source %like% 'phase1')
  write_csv("data/inputs_extracted/temp/utms_modelled_phase1.csv")


#################------------------------------Phase2 and Reassessments------------------------------------------------------
id <- dat %>%
  # filter(is.na(easting)) %>%
  pull(pscis_crossing_id)

#######make sure you change the crs based on the utm zone (ie. add the zone number to 26900)
##we need to tweak it a bit for the phase 2
##we are using the pscis model combined layer from way back but will work for now
sql <- glue::glue_sql("SELECT x.*, ST_X(ST_TRANSFORM(x.geom, 26911)) as utm_easting_derived, ST_Y(ST_TRANSFORM(x.geom, 26911)) as utm_northing_derived FROM bcfishpass.crossings x WHERE x.stream_crossing_id IN ({id*})",
                      .con = conn)

query <- DBI::dbSendQuery(conn, sql)
df2 <- DBI::dbFetch(query)
dbClearResult(query)

id_joined2 <- left_join(
  id_joined,
  df2 %>% select(stream_crossing_id, utm_easting_derived, utm_northing_derived),
  by = c('pscis_crossing_id' = 'stream_crossing_id')
) %>%
  mutate(utm_easting = case_when(!is.na(utm_easting) ~ utm_easting,
                                 T ~ utm_easting_derived),
         utm_northing = case_when(!is.na(utm_northing) ~ utm_northing,
                                  T ~ utm_northing_derived)
  )%>%
  select(-utm_easting_derived, -utm_northing_derived)


##burn------------------------------------------------------------

id_joined2 %>%
  filter(source %like% 'phase2')  %>%
  write_csv("data/inputs_extracted/temp/utms_modelled_phase2.csv")

id_joined2 %>%
  filter(source %like% 'reassessments') %>%
  write_csv("data/inputs_extracted/temp/utms_modelled_reassessments.csv")


##always disconnect from the database
dbDisconnect(conn = conn)


##here a quick one to get our hab utms for the upstream sites. We need to derive the downstream sites from the hab_con tracks.
# This relies on the data/tables.R file so is maybe cart before the horse but oh well!
source('R/tables.R')
hab_loc_utms <- left_join(
  hab_loc %>%
    tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
    mutate(site_id = paste0(site, '_', location)) %>% mutate(site = as.numeric(site)),
  pscis_phase2 %>% select(pscis_crossing_id, easting, northing),
  by = c('site' = 'pscis_crossing_id')
) %>%
  mutate(easting = case_when(site_id %ilike% '_ds' ~ NA_real_,
                             T ~ easting),
         northing = case_when(site_id %ilike% '_ds' ~ NA_real_,
                              T ~ northing)) %>%
  select(site_id, easting, northing) %>%
  tibble::rowid_to_column() %>%
  write_csv("data/inputs_extracted/temp/utms_hab_loc.csv", na = '')



