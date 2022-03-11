##here we need to pull all the metadata from all the marked photos so we can use it to have our photos show on the leaflet map
source('R/packages.R')


photo_metadata <- exifr::read_exif('data/photos',recursive=T)  %>%
  janitor::clean_names() %>%
  select(file_name, source_file, gps_latitude, gps_longitude) %>%
  mutate(url  = paste0('https://github.com/NewGraphEnvironment/fish_passage_elk_2021_reporting/raw/master/',
                       source_file)) %>%
         # base = tools::file_path_sans_ext(filename)) %>%
  filter(file_name %like% '_k_' &
           !file_name %like% 'rotated') # here is a hack so that we don't get doubles of the photos from the google flip phone

# 'https://newgraphenvironment.github.io/fish_passage_elk_2021_reporting/fish_passage_elk_2021_reporting/'


conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)
rws_drop_table("photo_metadata", conn = conn) ##now drop the table so you can replace it
rws_write(photo_metadata, exists = F, delete = TRUE,
          conn = conn, x_name = "photo_metadata")
rws_list_tables(conn)
rws_disconnect(conn)

