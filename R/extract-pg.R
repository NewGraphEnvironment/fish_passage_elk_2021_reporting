source('R/packages.R')



conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "postgis",
  host = "localhost",
  port = "5432",
  user = "postgres",
  password = "postgres"
)


##get our study area
dat <- fwa_watershed_at_measure(
  blue_line_key = 356570562,
  downstream_route_measure = 22910)

dat <- dat %>% st_transform(3005)

##have a look at it
ggplot2::ggplot() +
  # ggplot2::geom_sf(data = streams, lwd = 0.15) +
  ggplot2::geom_sf(data = dat, lwd = 0.15, fill = 'steelblue', alpha = 0.5)
# ggplot2::geom_sf(data = yakoun, lwd = 0.15)

##get the number of crossings in PSCIS and a summary of how they fair

# load to database
sf::st_write(obj = dat, dsn = conn, Id(schema= "working", table = "misc"))

# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON working.misc USING GIST (geometry)")
dbClearResult(res)
# res <- dbSendQuery(conn, "ALTER TABLE working.misc ADD PRIMARY KEY (misc_point_id)")
# dbClearResult(res)

res <- dbSendQuery(conn,
                   paste0("ALTER TABLE ", "working", ".", "misc", " ALTER COLUMN geometry
           Type geometry(Polygon, ", 3005, ")
           USING ST_SetSRID(geometry, ", 3005, ");"))

dbClearResult(res)

query <- "SELECT p.* FROM whse_fish.pscis_assessment_svw p INNER JOIN working.misc wsg ON ST_Intersects(wsg.geometry,p.geom)"
pscis_ass <-  sf::st_read(conn, query = query)

#make table of results
pscis_ass_sum <-   pscis_ass %>%
  st_set_geometry(NULL) %>%
  select(current_barrier_result_code) %>%
  # filter(watershed_group_code == 'PARS') %>%
  group_by(current_barrier_result_code)  %>%
  summarise(count =n())

pscis_ass_ref <- pscis_ass %>%
  st_set_geometry(NULL) %>%
  # filter(watershed_group_code == 'PARS') %>%
  group_by(consultant_name, year = lubridate::year(lubridate::as_date(assessment_date)), ecocat_url)  %>% ##http://a100.gov.bc.ca/pub/acat/public/viewReport.do?reportId=50797 this is the Hooft 2015 report.  the link in pscis is incorrect
  summarise(count =n())

##listthe schemas in the database
dbGetQuery(conn,
           "SELECT schema_name
           FROM information_schema.schemata")

##list tables in a schema
dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='whse_environmental_monitoring'")

##now do the same for the confirmations
query <- "SELECT p.* FROM whse_fish.pscis_habitat_confirmation_svw p INNER JOIN working.misc wsg ON ST_Intersects(wsg.geometry,p.geom)"
pscis_con <-  sf::st_read(conn, query = query)

##get the number of crossings assessed by masse
length(pscis_con$id)

##find out which hydrometric stations are in our area
query <- "SELECT p.* FROM whse_environmental_monitoring.envcan_hydrometric_stn_sp p
          INNER JOIN working.misc wsg
          ON ST_Intersects(wsg.geometry,p.geom)"
hydro <-  sf::st_read(conn, query = query)

##08NK002 is elk river at Fernie  - 08NK016 is near sparwood

dbDisconnect(conn = conn)
