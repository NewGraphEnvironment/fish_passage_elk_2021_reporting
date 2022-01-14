# install.packages('pacman')

pacman::p_load(
  tidyverse,
  readwritesqlite,
  sf,
  readxl,
  janitor,
  leafem,
  leaflet,
  plotKML,
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



pacman::p_load_gh("poissonconsulting/fwapgr",
                  "crsh/citr",
                  "rstudio/pagedown")
                  # "poissonconsulting/subfoldr2")
