##lets use magick to:
## back up original photos ont he d drive
##resize teh photos in our main file
##convert our pngs to jpg
##make a composite image of all the most relevant culvert shots for easy display in leaflet yo
##get rid of the AAE files from the iphone.



library(tidyverse)
library(magick)

##back up your photos onto the D drive.

##create a folder to copy the photos to


##get teh name of the folder we are in
bname <- basename(dirname(dirname(getwd())))

##input the name of the file we want to copy.  We should get a list of files next time and then purrr::map the procedure
filename = "kyle"

##here we back everything up to the D drive
targetdir = paste0("D:/backups/photos/", bname, "/")
dir.create(targetdir)

targetdir = paste0("D:/backups/photos/", bname, "/", filename)
dir.create(targetdir)


##path to the photos
path <- paste0("C:/Users/allan/OneDrive/New_Graph/Current/", bname, '/photos/', filename)

filestocopy <- list.files(path = path,
                          full.names = T)

#copy over the photos in the al folder -- this is done already
file.copy(from=filestocopy, to=targetdir,
          overwrite = F, recursive = FALSE,
          copy.mode = TRUE)


##this scales everything and converts everything to jpg
img_resize_convert <- function(img){
  image <- image_read(img)
  image_scaled <- image_scale(image,"1440x1080!")
image_write(image_scaled, path = paste0(path, '/', tools::file_path_sans_ext(basename(img)), '.JPG'), format = 'jpg')
}

filestocopy %>%
  purrr::map(img_resize_convert)


############ remove the png files that are now converted to jpg
##identify all the png files in the folder
filesremove <- grep('.PNG', filestocopy, value=TRUE)

file.remove(filesremove)

##use the pscis spreadsheet to make the folders to copy the photos to
df <- import_pscis(workbook_name = 'pscis_phase1.xlsm')

##create the data folder
dir.create(paste0(getwd(), '/data'))

##create the photos folder
dir.create(paste0(getwd(), '/data/photos'))

folderstocreate <- df$my_crossing_reference %>% as.character()

make_photo_folders <- function(xing){
  dir.create(paste0(getwd(), '/data/photos/', xing))
}

folderstocreate %>%
  purrr::map(make_photo_folders)

##rename 20200923010!!!
