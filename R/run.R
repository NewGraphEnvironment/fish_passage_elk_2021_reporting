##this is for as we work through
preview_chapter('0100-intro.Rmd')
preview_chapter('0200-background.Rmd')
preview_chapter('0300-method.Rmd')
preview_chapter('0400-results.Rmd')
preview_chapter('0600-appendix.Rmd')
preview_chapter('0800-appendix-197793.Rmd')
preview_chapter('index.Rmd')

#######################################################################################
##change your VErsion #
#######################################################################################

#######################################################################################
##if you have changed your bcfishpass model outputs by saving to sqlite with 0282-extract-bcfishpass2...
##you also need to make new html tables to link to in the leaflet map  use 0355-tables-reporting-html.R
########################################################################################



##this is how we clean up our bib file.  We need to find a way to add together the packages.bib file with the book.bib file first though.
# citr::tidy_bib_file(
#   rmd_file = "Elk-River-Fish-Passage-2020.Rmd",
#   messy_bibliography = 'book.bib',
#   file = 'book_tidy.bib')

##we also need to change all the date header to year in the bib file so that it can be run by our pdf maker
##i did this by hand last time but would be good to automate!!!

##  make the site
# source('R/photos-extract-metadata.R') ##if you added new photos
# source('R/0355-tables-reporting-html.R')  #if you changed the modelling outputs


#################################################################################################
##go to the index.Rmd and change gitbook_on <- TRUE
#################################################################################################
{
  # These files are included in the gitbook version already so we move them out of the build
files_to_move <- list.files(pattern = ".Rmd$") %>%
  stringr::str_subset(., '2200|2300|2400', negate = F) #move the attachments out
files_destination <- paste0('hold/', files_to_move)

##move the files
mapply(file.rename, from = files_to_move, to = files_destination)

rmarkdown::render_site(output_format = 'bookdown::gitbook',
                       encoding = 'UTF-8')

##move the files from the hold file back to the main file
mapply(file.rename, from = files_destination, to = files_to_move)
}

# pdf version

#################################################################################################
##go to the index.Rmd and change gitbook_on <- FALSE
#################################################################################################
##move the phase 1 appendix out of the main directory to a backup file or else the file is too big
# make sure the pdf is not open in your viewer!!!
filename_html <- 'Elk2021'


{
  file.rename('0600-appendix.Rmd', 'hold/0600-appendix.Rmd')

  ##   then make our printable pdf
  rmarkdown::render_site(output_format = 'pagedown::html_paged', encoding = 'UTF-8')

  ##move the phase 1 appendix back to main directory
  file.rename('hold/0600-appendix.Rmd', '0600-appendix.Rmd')

  # print to pdf
  pagedown::chrome_print(
    paste0(getwd(),'/Elk2021.html'),
    output = paste0(getwd(),'/docs/Elk2021.pdf')
  )

  tools::compactPDF(paste0(getwd(), "/docs/", filename_html, ".pdf"),
                    gs_quality = 'printer',
                    gs_cmd = "C:/Program Files/gs/gs9.56.1/bin/gswin64.exe")

  # get rid of the html as its too big and not needed
  file.remove(paste0(getwd(), "/Elk2021.html"))
}





##########################################make Phase 1 appendix seperately only when updated
#################################################################################################
##we need a workflow to print the Phase 1 attachment
files_to_move <- list.files(pattern = ".Rmd$") %>%
  stringr::str_subset(., 'index|Elk2021|0600', negate = T)
files_destination <- paste0('hold/', files_to_move)

##move the files
mapply(file.rename, from = files_to_move, to = files_destination)


##   then make our printable pdf
rmarkdown::render_site(output_format = 'pagedown::html_paged', encoding = 'UTF-8')

##  move it to the docs folder so that it can be in the same place as the report
# file.rename('Elk2021.html', 'docs/Attachment_3_Phase_1_Data_and_Photos.html')

##move the files from the hold file back to the main file
mapply(file.rename, from = files_destination, to = files_to_move)

#print the attachment to pdf with chrome print
# openHTML('docs/Attachment_3_Phase_1_Data_and_Photos_prep.html')

pagedown::chrome_print(
  paste0(getwd(),'/Elk2021.html'),
  output = paste0(getwd(),'/docs/Attachment_2_Phase_1_Data_and_Photos_prep.pdf')
)

##now get rid of the first 10 pages
length <- pdftools::pdf_length(paste0(getwd(), "/docs/Attachment_2_Phase_1_Data_and_Photos_prep.pdf"))

pdftools::pdf_subset(paste0(getwd(), "/docs/Attachment_2_Phase_1_Data_and_Photos_prep.pdf"),
           pages = 11:length, output = paste0(getwd(), "/docs/Attachment_2_Phase_1_Data_and_Photos.pdf"))

##clean out the old file
file.remove(paste0(getwd(), "/docs/Attachment_3_Phase_1_Data_and_Photos_prep.pdf"))
file.remove(paste0(getwd(), "/docs/Attachment_3_Phase_1_Data_and_Photos.html"))

