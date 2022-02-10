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
           and table_name='fiss_fish_obsrvtn_pnt_sp'")

##get some stats for WCT
query = "SELECT
  fish_observation_point_id,
  s.gradient,
  s.stream_order,
  s.upstream_area_ha,
  round((ST_Z((ST_Dump(ST_LocateAlong(s.geom, e.downstream_route_measure))).geom))::numeric) as elevation
FROM whse_fish.fiss_fish_obsrvtn_events_sp e
INNER JOIN bcfishpass.streams s
ON e.linear_feature_id = s.linear_feature_id
WHERE e.species_code = 'WCT'
AND e.watershed_group_code = 'ELKR';"

wct_elkr <- st_read(conn, query = query)

wct_elkr_grad <- wct_elkr %>%
  mutate(Gradient = case_when(
    gradient < .03 ~ '0 - 3 %',
    gradient >= .03 &  gradient < .05 ~ '03 - 5 %',
    gradient >= .05 &  gradient < .08 ~ '05 - 8 %',
    gradient >= .08 &  gradient < .15 ~ '08 - 15 %',
    gradient >= .15 &  gradient < .22 ~ '15 - 22 %',
    gradient >= .22  ~ '22+ %')) %>%
  group_by(Gradient)  %>%
  summarise(Count = n()) %>%
  mutate(total = nrow(wct_elkr),
         Percent = round(Count/total * 100, 0))

wct_elkr_grad$gradient_id <- c(3,5,8,15,22,99)

##save this for the report
##burn it all to a file we can use later
wct_elkr_grad %>% readr::write_csv(file = paste0(getwd(), '/data/raw_input/wct_elkr_grad.csv'))




