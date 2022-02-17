##note that this was not run this year - we just used last years to save time


source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')

##get the road info from the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
)

##ran this in rstudio sever (cmd) to get the latest

# bcdata bc2pg WHSE_FISH.FISS_FISH_OBSRVTN_PNT_SP
# should update our whse_fish.fiss_fish_obsrvtn_events_sp table with https://github.com/NewGraphEnvironment/bcfishobs but not going to get into it right now.


# # ##list tables in a schema
dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='whse_fish'")
# # # # #
# # # # # ##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_schema = 'whse_fish'
           and table_name='fiss_fish_obsrvtn_events_sp'")

dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='streams'")



##get some stats for WCT - we are going to do this from bcbarriers for now since discharge is there. bcfishobs is the local schema
query = "SELECT
  fish_observation_point_id,
  s.gradient,
  s.stream_order,
  s.upstream_area_ha,
  s.mad_m3s,
  s.channel_width,
  s.watershed_group_code,
  s.linear_feature_id,
  s.accessibility_model_wct,
  round((ST_Z((ST_Dump(ST_LocateAlong(s.geom, e.downstream_route_measure))).geom))::numeric) as elevation
FROM whse_fish.fiss_fish_obsrvtn_events_sp e
INNER JOIN bcfishpass.streams s
ON e.linear_feature_id = s.linear_feature_id
WHERE e.species_code = 'WCT'
AND e.downstream_route_measure > s.downstream_route_measure;"
# AND e.watershed_group_code = 'ELKR';" ##looks like only one wct elsewhere anyway which is odd since they show
# get the correct segmented ID with the downstream route measure


wct <- st_read(conn, query = query)

# ggplot(wct, aes(x=channel_width, y=mad_m3s)) +
#   geom_point()

wct_elkr_grad <- wct %>%
  mutate(Gradient = case_when(
    gradient < .03 ~ '0 - 3 %',
    gradient >= .03 &  gradient < .05 ~ '03 - 5 %',
    gradient >= .05 &  gradient < .08 ~ '05 - 8 %',
    gradient >= .08 &  gradient < .15 ~ '08 - 15 %',
    gradient >= .15 &  gradient < .22 ~ '15 - 22 %',
    gradient >= .22  ~ '22+ %')) %>%
  group_by(Gradient)  %>%
  summarise(Count = n()) %>%
  mutate(total = nrow(wct),
         Percent = round(Count/total * 100, 0))

wct_elkr_grad$gradient_id <- c(3,5,8,15,22,99)

##save this for the report
##burn it all to a file we can use later
wct_elkr_grad %>% readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/02_prep_report/wct_mad_chanwidthgrade.csv'))

dbDisconnect(conn = conn)


