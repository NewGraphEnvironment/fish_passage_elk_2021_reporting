##here is a test to see if we can automate placing photos in the right folders
###To do
#everything should be functions to pass list of camera_ids
##script backup and resizing as part of process - this will also deal with file ending names!

source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')

pscis_list <- fpr_import_pscis_all()

pscis_all <- bind_rows(pscis_list)

##get date times from input files
time_prep <- pscis_all %>%
  distinct(aggregated_crossings_id, .keep_all = T) %>% ##remove duplicates
  mutate(time_start = str_squish(str_extract(assessment_comment, "[^.]*$")),
         date_time_start = lubridate::ymd_hm(paste0(date, ' ', time_start))) %>%
  mutate(camera_id = stringr::word(crew_members, 1),
         ##identify who had the camera as this is the first initials in the crew_members
         camera_id = case_when(camera_id == 'AI' ~ 'al',  ##need to match the photo file names
                               camera_id == 'KP' ~ 'kyle'))

##because two people are going at the same time we need to do everything seperate for kyle and al - abstract later
time_prep_al <- time_prep %>%
  filter(camera_id != 'kyle') %>%
  arrange(date_time_start) %>%
  tibble::rowid_to_column(var = 'time_idx_start') %>%
  mutate(time_idx_end = time_idx_start + 1)

time_prep_kyle <- time_prep %>%
  filter(camera_id == 'kyle') %>%
  arrange(date_time_start) %>%
  tibble::rowid_to_column(var = 'time_idx_start') %>%
  mutate(time_idx_end = time_idx_start + 1)


##test to see results
# unique(time_prep$camera_id)
##join with the end time
pscis_all_time_al <- left_join(
  time_prep_al,
  select(time_prep_al, time_idx_start, date_end = date, time_end = time_start),
  by = c('time_idx_end' = 'time_idx_start')
) %>%
  mutate(
    ##we have no value for the last date and time so need to insert -- #used 11pm.  Might mess with things!!!!!!!!!!!!!!!
    time_end = case_when(time_idx_end == max(time_idx_end) ~ '23:00', T ~ time_end),
    date_end = case_when(time_idx_end == max(time_idx_end) ~ date, T ~ date_end),
    date_time_end = lubridate::ymd_hm(paste0(date_end, ' ', time_end)),
    time_interv = interval(date_time_start, date_time_end))

pscis_all_time_kyle <- left_join(
  time_prep_kyle,
  select(time_prep_kyle, time_idx_start, date_end = date, time_end = time_start),
  by = c('time_idx_end' = 'time_idx_start')
) %>%
  mutate(
    ##we have no value for the last date and time so need to insert -- #used 11pm.  Might mess with things!!!!!!!!!!!!!!!
    time_end = case_when(time_idx_end == max(time_idx_end) ~ '23:00', T ~ time_end),
    date_end = case_when(time_idx_end == max(time_idx_end) ~ date, T ~ date_end),
    date_time_end = lubridate::ymd_hm(paste0(date_end, ' ', time_end)),
    time_interv = interval(date_time_start, date_time_end))


##this works but grabs the photos from the subfolders too...
# photo_meta_all <- exifr::read_exif("C:/Users/allan/OneDrive/New_Graph/Current/2021-041-nupqu-elk-fish-passage/data/photos/brody" ,
#                                    recursive= T) %>%
#   purrr::set_names(., nm = tolower(names(.)))
#
# test <- fpr_photo_sort_metadat(input_file = "C:/Users/allan/OneDrive/New_Graph/Current/2021-041-nupqu-elk-fish-passage/data/photos/brody")

##lets pass it a list of folders to do the sorting on
##we do not include nupqu becasue their photos are already sorted into folders
lst_folders <- c("C:/Users/allan/OneDrive/New_Graph/Current/2021-041-nupqu-elk-fish-passage/data/photos/al",
                 "C:/Users/allan/OneDrive/New_Graph/Current/2021-041-nupqu-elk-fish-passage/data/photos/kyle",
                 "C:/Users/allan/OneDrive/New_Graph/Current/2021-041-nupqu-elk-fish-passage/data/photos/brody"
                 "C:/Users/allan/OneDrive/New_Graph/Current/2021-041-nupqu-elk-fish-passage/data/photos/tammy"
                 )


##########everything should be functions to pass list of camera_ids
photo_meta <- lst_folders %>%
  map(fpr_photo_sort_metadat) %>%
  purrr::set_names(nm = basename(lst_folders)) %>%
  bind_rows(.id = 'camera_id') %>%
  tibble::rowid_to_column()

photo_meta_al <- photo_meta %>%
  filter(camera_id != 'kyle')

photo_meta_kyle <- photo_meta %>%
  filter(camera_id == 'kyle')
# photo_meta <- photo_meta_all %>%
#   select(sourcefile, datetimeoriginal) %>%
#   mutate(datetimeoriginal = lubridate::ymd_hms(datetimeoriginal)) %>%
#   tibble::rowid_to_column()

##make a list of the times of each photo and run it through our function with our interval list
photo_time_lst_al <- photo_meta_al %>%
  pull(datetimeoriginal)
photo_time_lst_kyle <- photo_meta_kyle %>%
  pull(datetimeoriginal)

##make interval lists
intervs_al <- pscis_all_time_al %>%
  pull(time_interv)
intervs_kyle <- pscis_all_time_kyle %>%
  pull(time_interv)

##see which intervals
# lubridate::int_overlaps(intervs_al, intervs_kyle)



##make a tibbles of the index of the row with the folder containing the interval
photo_folders_idx_al <- photo_time_lst_al %>%
  map(fpr_time_interval_idx, intervs = intervs_al) %>%
  purrr::set_names(nm = photo_meta_al$rowid) %>%
  as_tibble_col(column_name = "site_idx")

photo_meta_tojoin_al <- bind_cols(
  photo_meta_al,
  photo_folders_idx_al
) %>%
  #filter out .mov
  filter(!sourcefile %ilike% '.mov') %>%
  mutate(site_idx = as.numeric(site_idx))  ##this will only work when we only have one result per photo! bound to break

photo_folders_idx_kyle <- photo_time_lst_kyle %>%
  map(fpr_time_interval_idx, intervs = intervs_kyle) %>%
  purrr::set_names(nm = photo_meta_kyle$rowid) %>%
  as_tibble_col(column_name = "site_idx")

photo_meta_tojoin_kyle <- bind_cols(
  photo_meta_kyle,
  photo_folders_idx_kyle
) %>%
  #filter out .mov
  filter(!sourcefile %ilike% '.mov') %>%
  mutate(site_idx = as.numeric(site_idx))  ##this will only work when we only have one result per photo! bound to break


##we have a few special cases no lets make some conditions
#need to be careful because there can be duplicate names from different cameras!
ids_coal <- paste0('kyle_TC_0', 2266:2277, '.JPG')
ids_hartley <- paste0('kyle_TC_0', 2244:2265, '.JPG')
ids_parker_50067 <- paste0('kyle_IMG_', 3678:3684, '.JPG')
ids_bighorn_4605514 <- paste0('kyle_TC_0', 2875:2949, '.JPG')
ids_harmer_dam_1063 <- paste0('al_IMG_', 6763:6767, '.JPG')
ids_elkford_dam_al_2606 <- paste0('al_IMG_', 7187:7193, '.JPG')
ids_elkford_dam_brody_2606 <- c('brody_TimePhoto_20211014_153720.jpg',  ##file name endings!!!!
                                'brody_TimePhoto_20211014_153813.jpg',
                                'brody_TimePhoto_20211014_153820.jpg',
                                'brody_TimePhoto_20211014_153832.jpg',
                                'brody_TimePhoto_20211014_153902.jpg',
                                'brody_TimePhoto_20211014_153907.jpg',
                                'brody_TimePhoto_20211014_153944.jpg')
ids_dont_copy01 <- c(
  paste0('kyle_IMG_', 3062:3142, '.JPG'),
  paste0('kyle_IMG_', 3260:3269, '.JPG'),
  paste0('kyle_IMG_', 3410:3415, '.JPG'),
  paste0('kyle_IMG_', 3476:3479, '.JPG'),
  paste0('kyle_IMG_', 3567:3576, '.JPG'),
  paste0('kyle_IMG_', 3877:3891, '.JPG'),
  paste0('al_IMG_', 6969:6985, '.JPG'),
  paste0('al_IMG_', 6744:6750, '.JPG'),
  paste0('al_IMG_', 7170:7180, '.JPG'),
  paste0('al_IMG_', 7248:7260, '.JPG'),
  paste0('al_IMG_', 7375:7384, '.JPG')
)


##join on the name of the folder we paste to
photo_folder_targets_al <- left_join(
  photo_meta_tojoin_al,
  pscis_all_time_al %>% select(time_idx_start, pscis_crossing_id, my_crossing_reference),
  by = c('site_idx' = 'time_idx_start')  ##rememberthat rowid is for the input excel files!!
)

photo_folder_targets_kyle <- left_join(
  photo_meta_tojoin_kyle,
  pscis_all_time_kyle %>% select(time_idx_start, pscis_crossing_id, my_crossing_reference),
  by = c('site_idx' = 'time_idx_start')  ##rememberthat rowid is for the input excel files!!
)

photo_folder_targets <- bind_rows(
  photo_folder_targets_al,
  photo_folder_targets_kyle
)  %>%
  #make a column to hold all the ids.  there are no duplicates in this dataset
  mutate(
    photo_basename = basename(sourcefile),
    photo_fullname = paste0(camera_id, '_', photo_basename),
    folder_to_id = case_when(!is.na(pscis_crossing_id) ~ pscis_crossing_id,
                             !is.na(my_crossing_reference) ~ my_crossing_reference),
    folder_to_id = as.character(folder_to_id),
    ##we have a few special cases no lets make some conditions
    folder_to_id = case_when(photo_fullname %in% ids_coal ~ '61504',
                             photo_fullname %in% ids_hartley ~ '197542',
                             photo_fullname %in% ids_parker_50067 ~ '50067',
                             photo_fullname %in% ids_bighorn_4605514 ~ '4605514',
                             photo_fullname %in% ids_harmer_dam_1063 ~ '1063',
                             photo_fullname %in% ids_elkford_dam_al_2606 ~ '2606',
                             photo_fullname %in% ids_elkford_dam_brody_2606 ~ '2606',
                             photo_fullname %in% ids_dont_copy01 ~ NA_character_,
                             T ~ folder_to_id),
    folder_to_path = paste0(getwd(), '/data/photos/', folder_to_id, '/', camera_id, '_', photo_basename)  ##we could add the source to the name of the photo if we wanted....
  ) %>%
  filter(
    !sourcefile %ilike% 'not_used'   ##filter out some photos that shouldn't or don't move
  )

photo_folder_targets_transfer <- photo_folder_targets %>%
  filter(
    !is.na(folder_to_id)  ##filter out some photos that shouldn't or don't move
  )

# test <- photo_folder_targets %>%
#   filter(folder_to_id == "2021101302")

####################------------------------CArefuL - TEST first yo-----------------------------------------##############################

##test a bunch first!!!!!!!
# test <- photo_folder_targets %>%
#   filter(folder_to_id == 4600087) #my_crossing_reference


#copy over the photos to their respective folders
# file.copy(from=photo_folder_targets$sourcefile, to=photo_folder_targets$folder_to_path,
#           overwrite = F, recursive = FALSE,
#           copy.mode = TRUE)

# test <- photo_folder_targets %>%
#   head()

##in this one we move them vs. copy.  Need to be sure they are backed up first!!!!
##we will script the backup and resizing to an intermediary file next time
file.rename(from=photo_folder_targets_transfer$sourcefile, to=photo_folder_targets_transfer$folder_to_path)

# unique(photo_folder_targets_transfer$folder_to_id)
# test <- photo_folder_targets_delete %>% head()


##we also can erase the photos we said not to move since they are backed up and
##we want to see any left overs
photo_folder_targets_delete <- photo_folder_targets %>%
  filter(
    is.na(folder_to_id)  ##filter out some photos that shouldn't or don't move
  )

file.remove(photo_folder_targets_delete$sourcefile)





















##here is where we tested the concept
# ##tibble with our intervals and dates
# int_df <- tibble('int' = c(interval(ymd(20090101), ymd(20090201)),
#                            interval(ymd(20100101), ymd(20100201))),
#                  'date' = c(ymd(20090102), ymd(20090202))
# ) %>%
#   tibble::rowid_to_column()
#
# ##pull out vectors
# dates <- int_df %>% pull(date)
# intervs <- int_df %>% pull(int)
#
# ##give index of interval
# fpr_time_interval_idx <- function(date_input){
#   which(date_input %within% intervs)
# }
#
#
# ##make a tibble of the index of the row with the folder containing the interval
# lst <- dates %>%
#   map(fpr_time_interval_idx) %>%
#   purrr::set_names(nm = int_df$rowid) %>%
#   as_tibble_col(column_name = "value")
