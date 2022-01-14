##we want a composite photo for viewing that has the 6 key photos of each crossing
source('R/packages.R')
source('R/functions.R')


##get list of files (site_ids) in the photo folder
site_id_list <- list.files(path = paste0(getwd(), '/data/photos/'), full.names = T) %>%
  basename()

##lets look to see that we have all the right photo names - the sorting has nothing to do with it
##but might be useful for QA if we add the total and check that it is equiv - leave for now...
find_photo_names <- function(site_id){
  list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet') %>%
    as_tibble() %>%
    mutate(sort = case_when(
      value %ilike% 'road' ~ 1,
      value %ilike% 'inlet' ~ 2,
      value %ilike% 'upstream' ~ 3,
      value %ilike% 'barrel' ~ 4,
      value %ilike% 'outlet' ~ 5,
      value %ilike% 'downstream' ~ 6,
    )) %>%
    arrange(sort) %>%
    select(-sort)
}


##if you look at this "by hand" you can see tibbles with less than 6 rows so those should be fixed.
##we had a couple of extra litte files in the folder that were a problem. make sure the file is only photo folders
##we should be able to filter the list so that we get a quick read on which ones are short photos but we will need to do that another time.
##this does not identify when there are duplicates and one photo is missing!! need to revise
photo_names <- site_id_list %>%
  map(find_photo_names)


##once you review the test results and make sure they are all there you are good then run your function over the list
site_id_list %>%
  map(make_photo_comp_cv)




#could try to get the names of the photos printed on the photos but might not be worth it...

##------------used this to make the function
# site_id <- '3054'
# # photos_id1 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
# #   stringr::str_subset(., 'barrel|outlet|upstream')
# # photos_id2 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
# #   stringr::str_subset(., 'downstream|road|inlet')
#
# ##read in the photos
# # photos_images1 <- image_read(photos_id1)
#
# photos_images1 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
#   stringr::str_subset(., 'barrel|outlet|upstream') %>%
#   image_read()
#
# photos_images2 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
#   stringr::str_subset(., 'downstream|road|inlet') %>%
#   image_read()
# #
# photos_stack1 <-image_append(image_scale(photos_images1, "x420")) ##1/3 the width 373.33 and half the original height
# photos_stack2 <- image_append(image_scale(photos_images2, "x420"))
# # photos_stack1 <-image_append(image_scale(photos_images1))
# # photos_stack2 <- image_append(image_scale(photos_images2))
#
# photos_stack <- c(photos_stack1, photos_stack2)
#
# # photos_stacked <- image_append(image_scale(photos_stack, "x200"), stack = T)
# photos_stacked <- image_append(image_scale(photos_stack), stack = T)
# photos_stacked
#
# image_write(photos_stacked, path = paste0(getwd(), '/data/photos/', site_id, '/crossing_all.JPG'), format = 'jpg')



########################---------------resize our generic image
# im <- image_read(path = paste0(getwd(), '/data/photos/3139/no_photo_road.JPG'))
# im2 <- image_scale(im, "1120x840!")
# image_write(im2, path = paste0(getwd(), '/data/photos/3139/no_photo_road.JPG'), format = 'jpg')
