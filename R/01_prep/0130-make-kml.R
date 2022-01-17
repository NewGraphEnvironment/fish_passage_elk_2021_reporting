source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')


##here we just want to make a simple kml for the presentation




##get the names of your pscis files
workbooks <-  list.files(path = 'data', pattern = "pscis", all.files = F) %>%
  grep(pattern = '~', invert = T, value = T)

dat1 <- import_pscis(workbook_name = 'pscis_phase1.xlsm')

##the colors don't seem to work yet.  Might need to put a case_when for the actual google icon symbol.  Posting cutom symbols on a url and pointing to them will work too.
make_kml_col <- function(df){
  df %>%
    mutate(pscis_crossing_id = as.integer(pscis_crossing_id),
           my_crossing_reference = as.integer(my_crossing_reference),
           color = case_when(source %like% 'phase1' ~ 'red',
                             source %like% 'phase2' ~'black',
                             T ~ 'white'),
           color = plotKML::col2kml(color),
           site_id = case_when(!is.na(pscis_crossing_id) ~ pscis_crossing_id,
                               is.na(pscis_crossing_id) ~ my_crossing_reference),
           label = paste0(site_id, '-', stream_name)) ##label = paste0(site_id, '-', stream_name, '-', barrier_result, '-', habitat_value, ' habitat value')
  # mutate(across(where(is.numeric), round(.,2)))

}

##make a function to import and add the source
make_pscis_kml_prep <- function(workbook){
  import_pscis(workbook_name = workbook)  %>%
    make_kml_col()  %>%
    # dplyr::group_split(site_id) %>% ##going to do this later
    # purrr::map(make_html_tbl) %>% ##going to do this later
    dplyr::bind_rows() %>%
    dplyr::mutate(source = tools::file_path_sans_ext(workbook))
}

##import and combine all your pscis assessments
df <- workbooks %>%
  map_df(make_pscis_kml_prep) %>%
  dplyr::mutate(sort = case_when(source %like% 'phase1' ~ 2,
                                 source %like% 'phase2' ~ 1,
                                 T ~ 3),
                shape = case_when(source %like% 'phase1' ~ "http://maps.google.com/mapfiles/kml/paddle/A.png",
                                  source %like% 'phase2' ~ "http://maps.google.com/mapfiles/kml/paddle/C.png",
                                  T ~ "http://maps.google.com/mapfiles/kml/paddle/R.png")) %>%
  dplyr::arrange(sort) %>%
  dplyr::distinct(site_id, .keep_all = T)

df <- df %>%
  dplyr::group_split(site_id) %>%
  purrr::map(make_html_tbl) %>%
  dplyr::bind_rows()

coords <- df %>% select(easting, northing)
proj4string <- sp::CRS("+init=epsg:32609")
df <- df %>%
  sp::SpatialPointsDataFrame(coords = coords, proj4string = proj4string) %>%
  plotKML::reproject()


##we need to clear out the old kmls
##now we will zip up the kml files in the data folder and rename with kmz
files_to_clear <- paste0("data/outgoing/", list.files(path = "data/outgoing/", pattern = "\\.kml$"))
file.remove(files_to_clear)


##or alternatively - give the file an identifier
bname <- "2021_skeena"


# kml_open("data/outgoing/barrier_assessments.kml")
kml_open(paste0("data/outgoing/",bname, "_PASSAGE_assessments.kml"))
kml_layer(df, shape = df$shape, colour = df$color, labels = df$label, html.table = df$html_tbl, z.scale = 2, LabelScale = 1, size = 2)
kml_close(paste0("data/outgoing/",bname, "_PASSAGE_assessments", format(Sys.time(),"_%Y%m%d_%H%M.kml")))

files_to_zip <- paste0("data/outgoing/", list.files(path = "data/outgoing/", pattern = "\\.kml$"))  ##this used to includes the planning file which we don't want to do so watch out
zip::zipr("data/outgoing/bulkley_morice_progress_2021_kml.zip", files = files_to_zip)  ##it does not work to zip to kmz!!

