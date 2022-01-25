# This file is used to confirm that we have one of each of the photos required for PSCIS upload and to
# build a composite photo for viewing that has the 6 key photos of each crossing
source('R/packages.R')
source('R/functions.R')


##get list of files (site_ids) in the photo folder
site_id_list <- list.files(path = paste0(getwd(), '/data/photos/'),
                           full.names = T) %>%
  basename()



##we had a couple of extra little files in the folder that were a problem. make sure the file is only photo folders

photo_names <- site_id_list %>%
  map(fpr_photo_qa) %>%
  purrr::set_names(site_id_list) %>%
  bind_rows(.id = 'site_id') %>%
  pivot_wider(names_from = x,
              values_from = x)


##if the above df build throws a list column warning then there are duplicates.  Deal with those first (should build a filter next time)
## and then run the filter below to find the sites with missing photos
colnms <- names(photo_names)

photo_missing <- photo_names %>%
  filter(across(all_of(colnms), is.na))  ##filter_at(vars(all_of(colnms)), any_vars(is.na(.)))


# So - fpr_photo_qa will only give us site_ids when at least 1 of the photos we search for are there!
# we are missing folders here because we have some dam sites that do not have
#  for this reason we pull our list of files to do the photo amalgamation on from photo_names dataframe above


##once you review the test results and make sure they are all there you are good then run your function over the list
## test on a small sample
# test <- photo_names$site_id %>% head()
# test %>%
#   map(fpr_photo_amalg_cv)

## make the amalgamated photos for each of the crossings
photo_names$site_id %>%
  map(fpr_photo_amalg_cv)


########################---------------resize our generic image
# im <- image_read(path = paste0(getwd(), '/data/photos/3139/no_photo_road.JPG'))
# im2 <- image_scale(im, "1120x840!")
# image_write(im2, path = paste0(getwd(), '/data/photos/3139/no_photo_road.JPG'), format = 'jpg')
