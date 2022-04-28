
## QA to be sure that you have all 6 required photos for submission to PSCIS
## convert jpg (or other formats - need to code) to JPG for consistency and to avoid issues with submission/reporting
##move the photos and spreadsheet ready for submission to pscis

source('R/packages.R')
source('R/functions.R')
# source('R/tables.R')

##path to the photos
path <- paste0(getwd(), '/data/photos')


##use the pscis spreadsheet to make the folders to copy the photos to
d <- fpr_import_pscis(workbook_name = 'pscis_phase1.xlsm')

folderstocopy<- d$my_crossing_reference %>% as.character()

path_to_photos <- paste0(getwd(), '/data/photos/', folderstocopy)

##we need to change the extensions and should really do this when we resize the photos.
# this time We do it now in the sorted folders
fpr_photos_ext_to_change <- function(target){
  list.files(path = target,
             pattern = '.*\\.(jpg|png|jpeg)$',
             recursive = TRUE,
             ignore.case = F, ##note this is false!!
             full.names = T,
             include.dirs = T)
  # as_tibble()
}

ext_change_from <- path_to_photos %>%
  purrr::map(fpr_photos_ext_to_change) %>%
  purrr::set_names(folderstocopy)

##could reconsider the name of this function as could be confusing
fpr_photo_change_ext <- function(filenames_to_change){
  gsub(filenames_to_change, pattern = '(.jpg|.png|.jpeg)', replacement = '.JPG') #.*\\.(jpg|png|jpeg)$
}

ext_change_to <- ext_change_from %>%
  map(fpr_photo_change_ext)


fpr_photo_rename_ext <- function(filescopy, filespaste){
  file.rename(from=filescopy, to=filespaste)
}

###do a test!!!!!!!!!!!!!!!!!!!!!
# test_from <- ext_change_from %>% pluck('4600087')
# test_to <- ext_change_to %>% pluck('4600087')
##we actually don't need a function when we have just a vector
# file.rename(from=test_from, to=test_to)
# mapply(fpr_photo_rename_ext,
#        filescopy = test_from,
#        filespaste = test_to)


##########!!!!!!!!!!!!!!run the rename - hashed out for safety!!!!!!!!!!!!!!!!!############
# mapply(fpr_photo_rename_ext,
#        filescopy = ext_change_from,
#        filespaste = ext_change_to)


##########################here we back everything up to the D drive###################################################################
targetdir = paste0("D:/PSCIS/PSCIS_elk_2021_phase1/")
dir.create(targetdir)

folderstocreate<- paste0(targetdir, folderstocopy)

##create the folders
lapply(folderstocreate, dir.create)


fpr_photos_paths_to_copy <- function(target){
  bind_rows(
    list.files(path = target,
               pattern = ".JPG$",
               recursive = TRUE,
               ignore.case = T,
               full.names = T,
               include.dirs = T) %>%
      stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet') %>%
      as_tibble(),
##this section will grab up to 4 more photos that have been add with the 'keep' tag (_k_) as they are in the reporting
    list.files(path = target,
               pattern = ".JPG$",
               recursive = TRUE,
               ignore.case = T,
               full.names = T,
               include.dirs = T) %>%
      stringr::str_subset(., '_k_') %>%
      as_tibble() %>%
      slice(1:4) ##we needed a way to grab only the first 4 photos that have a _k_ in them or else we get too many photos.
  ) %>%
    pull(value)
}


filestocopy_list <- path_to_photos %>%
  purrr::map(fpr_photos_paths_to_copy) %>%
  purrr::set_names(basename(folderstocreate))



##view which files do not have any photos to paste
empty_idx <- which(!lengths(filestocopy_list))

fpr_filter_list <- function(idx){
  filestocopy_list[idx]
}

empty_files <- empty_idx %>% fpr_filter_list()


##-----------------------------rename long names-------------------------------------------
## we have seen an issue with very long photo names getting rejected from PSCIS submisson
## find the long names and truncate them
## hold a record of the orignal and shortened named so we can repeat?
## Maybe not necessary if we run this each time but necessary on this job since we hand bombed earlier (doh)...
fpr_photos_all<- function(target, full_names = T){
  list.files(path = target,
             pattern =  '.*\\.(jpg|png|jpeg)$', #".JPG$"
             recursive = TRUE,
             ignore.case = T,
             full.names = full_names, ##this is false
             include.dirs = T) %>%
    as_tibble()
}

photos_all <- path_to_photos %>%
  purrr::map(fpr_photos_all) %>%
  purrr::set_names(folderstocopy) %>%
  bind_rows(.id = 'folder') %>%
  mutate(photo_name = str_squish(str_extract(value, "[^/]*$")),
         photo_name_length = stringr::str_length(photo_name))


###here we back up a csv that gives us the new location and name of the original JPG photos.
## Not ideal becasue we did some sorting by hand without adding name of camera to the file name but a start on reproducability notheless

##burn to csv
photos_all %>%
  readr::write_csv(file = paste0(getwd(), '/data/photos/photo_sort_tracking.csv'))

fpr_photo_change_name <- function(filenames_to_change){
  gsub(filenames_to_change, pattern = paste0(getwd(), '/data/photos/'), replacement = targetdir)
}


filestopaste_list <- filestocopy_list %>%
  map(fpr_photo_change_name)

# filestopaste_list <- path_to_photos %>%
#   purrr::map2(paths_to_copy)

fpr_copy_over_photos <- function(filescopy, filespaste){
  file.copy(from=filescopy, to=filespaste,
            overwrite = T,
            copy.mode = TRUE)
}


##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
mapply(fpr_copy_over_photos,
       filescopy =  filestocopy_list,
       filespaste = filestopaste_list)

##also move over the pscis file
file.copy(from = 'data/pscis_phase1.xlsm',
          to = paste0(targetdir, 'pscis_phase1.xlsm'))



##!!!!!!!!!!!!!this is a special case !!!!!!!!!!!!!!!!!to test if magick might be better for resizing than powertools
# for now we just resize 4600992  as these are the last photos that won't load to pscis. Next time we need to do everytng at once if this works
##this scales everything and converts everything to jpg

fpr_img_resize_convert <- function(img, path_write = path, folder = '4600992'){
  image <- image_read(img)
  image_scaled <- image_scale(image,"1024!x1024!")
  image_write(image_scaled, path = paste0(path_write, folder, '/', tools::file_path_sans_ext(basename(img)), '.JPG'), format = 'jpg')
}


filestocopy_list %>%
  pluck('4600992') %>% ##we need to filter or files wit 4600992
  purrr::map(fpr_img_resize_convert, path_write = targetdir)

##going to make a few notes here about the submission process
## we need to work in microsoft edge and put sites in "Internet Explorer mode pages" and set exceptions for uploading to soft and esf
## https://www.env.gov.bc.ca/csd/imb/soft/soft.shtml
## https://logon7.gov.bc.ca/clp-cgi/capBceid/logon.cgi?flags=1111:1,8&TARGET=$SM$https%3a%2f%2fapps%2enrs%2egov%2ebc%2eca%2fext%2fesf%2fsubmissionWelcome%2edo

##chack on your submission here https://apps.nrs.gov.bc.ca/ext/esf/submissionSearch.do?action=detail&submissionId=2117684


## phase 2 -----------------------------------------------------------------
##path to the photos
path <- paste0(getwd(), '/data/photos')


##use the pscis spreadsheet to make the folders to copy the photos to
d <- fpr_import_pscis(workbook_name = 'pscis_phase2.xlsm')

# we have sites that are not submitted yet b/c cwf doesn't want to pay for their reporting
sites_exclude <- c(50063, 50061, 62122, 50067)

folderstocopy<- d %>%
  filter(!pscis_crossing_id %in% sites_exclude) %>%
  pull(pscis_crossing_id) %>%
  as.character()

path_to_photos <- paste0(getwd(), '/data/photos/', folderstocopy)

##we need to change the extensions and should really do this when we resize the photos.
# this time We do it now in the sorted folders
fpr_photos_ext_to_change <- function(target){
  list.files(path = target,
             pattern = '.*\\.(jpg|png|jpeg)$',
             recursive = TRUE,
             ignore.case = F, ##note this is false!!
             full.names = T,
             include.dirs = T)
  # as_tibble()
}

ext_change_from <- path_to_photos %>%
  purrr::map(fpr_photos_ext_to_change) %>%
  purrr::set_names(folderstocopy)

##could reconsider the name of this function as could be confusing
fpr_photo_change_ext <- function(filenames_to_change){
  gsub(filenames_to_change, pattern = '(.jpg|.png|.jpeg)', replacement = '.JPG') #.*\\.(jpg|png|jpeg)$
}

ext_change_to <- ext_change_from %>%
  map(fpr_photo_change_ext)


fpr_photo_rename_ext <- function(filescopy, filespaste){
  file.rename(from=filescopy, to=filespaste)
}

###do a test!!!!!!!!!!!!!!!!!!!!! Actually these are the only ones that need renaming!
test_from <- ext_change_from %>% pluck('61504')
test_to <- ext_change_to %>% pluck('61504')

##we actually don't need a function when we have just a vector
# file.rename(from=test_from, to=test_to)
# mapply(fpr_photo_rename_ext,
#        filescopy = test_from,
#        filespaste = test_to)


##########!!!!!!!!!!!!!!run the rename - hashed out for safety!!!!!!!!!!!!!!!!!############
# mapply(fpr_photo_rename_ext,
#        filescopy = ext_change_from,
#        filespaste = ext_change_to)


##########################here we back everything up to the D drive###################################################################
targetdir = paste0("D:/PSCIS/PSCIS_elk_2021_phase2/")
dir.create(targetdir)

folderstocreate<- paste0(targetdir, folderstocopy)

##create the folders
lapply(folderstocreate, dir.create)


fpr_photos_paths_to_copy <- function(target){
  bind_rows(
    list.files(path = target,
               pattern = ".JPG$",
               recursive = TRUE,
               ignore.case = T,
               full.names = T,
               include.dirs = T) %>%
      stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet') %>%
      as_tibble(),
    ##this section will grab up to 4 more photos that have been add with the 'keep' tag (_k_) as they are in the reporting
    list.files(path = target,
               pattern = ".JPG$",
               recursive = TRUE,
               ignore.case = T,
               full.names = T,
               include.dirs = T) %>%
      stringr::str_subset(., '_k_') %>%
      as_tibble() %>%
      slice(1:4) ##we needed a way to grab only the first 4 photos that have a _k_ in them or else we get too many photos.
  ) %>%
    pull(value)
}


filestocopy_list <- path_to_photos %>%
  purrr::map(fpr_photos_paths_to_copy) %>%
  purrr::set_names(basename(folderstocreate))



##view which files do not have any photos to paste
empty_idx <- which(!lengths(filestocopy_list))

fpr_filter_list <- function(idx){
  filestocopy_list[idx]
}

empty_files <- empty_idx %>% fpr_filter_list()


##-----------------------------rename long names-------------------------------------------
## we have seen an issue with very long photo names getting rejected from PSCIS submisson
## find the long names and truncate them
## hold a record of the orignal and shortened named so we can repeat?
## Maybe not necessary if we run this each time but necessary on this job since we hand bombed earlier (doh)...
fpr_photos_all<- function(target, full_names = T){
  list.files(path = target,
             pattern =  '.*\\.(jpg|png|jpeg)$', #".JPG$"
             recursive = TRUE,
             ignore.case = T,
             full.names = full_names, ##this is false
             include.dirs = T) %>%
    as_tibble()
}

photos_all <- path_to_photos %>%
  purrr::map(fpr_photos_all) %>%
  purrr::set_names(folderstocopy) %>%
  bind_rows(.id = 'folder') %>%
  mutate(photo_name = str_squish(str_extract(value, "[^/]*$")),
         photo_name_length = stringr::str_length(photo_name))


###here we back up a csv that gives us the new location and name of the original JPG photos.
## Not ideal becasue we did some sorting by hand without adding name of camera to the file name but a start on reproducability notheless

##burn to csv
photos_all %>%
  readr::write_csv(file = paste0(getwd(), '/data/photos/photo_sort_tracking_phase2.csv'))

fpr_photo_change_name <- function(filenames_to_change){
  gsub(filenames_to_change, pattern = paste0(getwd(), '/data/photos/'), replacement = targetdir)
}


filestopaste_list <- filestocopy_list %>%
  map(fpr_photo_change_name)

# filestopaste_list <- path_to_photos %>%
#   purrr::map2(paths_to_copy)

fpr_copy_over_photos <- function(filescopy, filespaste){
  file.copy(from=filescopy, to=filespaste,
            overwrite = T,
            copy.mode = TRUE)
}


##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
mapply(fpr_copy_over_photos,
       filescopy =  filestocopy_list,
       filespaste = filestopaste_list)

##also move over the pscis file
file.copy(from = 'data/pscis_phase2.xlsm',
          to = paste0(targetdir, 'pscis_phase2.xlsm'))



##!!!!!!!!!!!!!this is a special case !!!!!!!!!!!!!!!!!to test if magick might be better for resizing than powertools
# for now we just resize 4600992  as these are the last photos that won't load to pscis. Next time we need to do everytng at once if this works
##this scales everything and converts everything to jpg

fpr_img_resize_convert <- function(img, path_write = path, folder = '4600992'){
  image <- image_read(img)
  image_scaled <- image_scale(image,"1024!x1024!")
  image_write(image_scaled, path = paste0(path_write, folder, '/', tools::file_path_sans_ext(basename(img)), '.JPG'), format = 'jpg')
}

# https://www.env.gov.bc.ca/perl/soft/dl.pl/20220428143147-14-gp-80d1952c-7fd1-4d6c-817f-f90973d9?simple=y
# https://apps.nrs.gov.bc.ca/ext/esf/submissionSearch.do?action=detail&submissionId=2151117
