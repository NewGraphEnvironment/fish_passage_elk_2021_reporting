##lets use magick to:
## back up original photos ont he d drive
##resize teh photos in our main file - moved to custom size of 1728 x 972 with image resizer (90% of large)
##convert our pngs to jpg
##make a composite image of all the most relevant culvert shots for easy display in leaflet yo
##get rid of the AAE files from the iphone.
##need to move over 61504 for second day of survey with nupqu

source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')


pscis_list <- import_pscis_all()
pscis_phase1 <- pscis_list %>% pluck('pscis_phase1')
pscis_phase2 <- pscis_list %>% pluck('pscis_phase2')
pscis_reassessments <- pscis_list %>% pluck('pscis_reassessments')
pscis_all <- pscis_list %>% pluck('pscis_all')

##back up your photos onto the D drive.
##this function needs to be abstracted more so that we can use it the other way as part of the photo sorting scripts.

fpr_photos_backup(filename = 'al')
fpr_photos_backup(filename = 'tammy')
fpr_photos_backup(filename = 'brody')
fpr_photos_backup(filename = 'kyle')
fpr_photos_backup(filename = 'nupqu')  ##this doesn't work becasue the photos are already in folders - done by hand


#---------------------------convert al------------------------------------------------------------

#
# ##input the name of the file we want to copy.  We should get a list of files next time and then purrr::map the procedure
# filename = "al"
# ##path to the photos
# path <- paste0("C:/Users/allan/OneDrive/New_Graph/Current/", bname, '/data/photos/', filename)
# filestocopy <- list.files(path = path,
#                           full.names = T)
# filestoconvert <- grep('.PNG', filestocopy, value=TRUE)
# filestoconvert %>%
#   purrr::map(img_resize_convert)
# ############ remove the png files that are now converted to jpg
# ##identify all the png files in the folder
# filesremove <- grep('.PNG', filestocopy, value=TRUE)
# file.remove(filesremove)
#
# #####---------------------------------convert kyle-------------------------------------------------------------------
# ##input the name of the file we want to copy.  We should get a list of files next time and then purrr::map the procedure
# filename = "kyle"
# ##path to the photos
# path <- paste0("C:/Users/allan/OneDrive/New_Graph/Current/", bname, '/data/photos/', filename)
# filestocopy <- list.files(path = path,
#                           full.names = T)
# filestoconvert <- grep('.PNG', filestocopy, value=TRUE)
# filestoconvert %>%
#   purrr::map(img_resize_convert)
# ############ remove the png files that are now converted to jpg
# ##identify all the png files in the folder
# filesremove <- grep('.PNG', filestocopy, value=TRUE)
# file.remove(filesremove)



###make the folders that we will drag our photos into

##create the photos folder
dir.create(paste0(getwd(), '/data/photos'))

##first the my_crossing_reference_folders
folderstocreate <- pscis_all %>%
  filter(!is.na(my_crossing_reference)) %>%
  distinct(my_crossing_reference) %>%
  pull(my_crossing_reference) %>%
  as.character()


folderstocreate %>%
  purrr::map(fpr_make_photo_folders)

##do the same for our pscis crossings
folderstocreate <- pscis_all %>%
  filter(!is.na(pscis_crossing_id)) %>%
  distinct(pscis_crossing_id) %>%
  pull(pscis_crossing_id) %>%
  as.character()


folderstocreate %>%
  purrr::map(fpr_make_photo_folders)

##we have special cases that do not have excel inputs

folders_special_cases <- c(197542, 4605826, 1086, 2606, 1063,
                           2021101301, 4600761, 2021101302, 4600992)  ##and some we are hacking in so we don't need to run the whole file

folders_special_cases %>%
  purrr::map(fpr_make_photo_folders)


