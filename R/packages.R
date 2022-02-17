# install.packages('pacman')


pacman::p_load(
  plotKML,  #plot kml needed to go ahead of other packages for some reason.... not sure why
  tidyverse,
  readwritesqlite,
  sf,
  readxl,
  janitor,
  leafem,
  leaflet,
  kableExtra,
  httr,
  RPostgres,
  RPostgreSQL,
  DBI,
  magick,
  bcdata,
  jpeg,
  datapasta,
  knitr,
  data.table,
  lubridate,
  forcats,
  bookdown,
  fasstr,
  tidyhydat,
  # flextable,
  english,
  leaflet.extras,
  ggdark,
  pdftools,
  geojsonsf,
  bit64 ##to make integer column type for pg
  # gert  ##to track git moves
  ##leafpop I think
  )

#



pacman::p_load_gh("poissonconsulting/fwapgr",
                  "crsh/citr")
                  # "poissonconsulting/subfoldr2")
