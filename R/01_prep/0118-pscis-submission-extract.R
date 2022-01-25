
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
             pattern = ".jpg$",
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
  gsub(filenames_to_change, pattern = 'jpg', replacement = 'JPG')
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

mapply(fpr_copy_over_photos,
       filescopy =  filestocopy_list,
       filespaste = filestopaste_list)

##also move over the pscis file
file.copy(from = 'data/pscis_phase1.xlsm',
          to = paste0(targetdir, 'pscis_phase1.xlsm'))


