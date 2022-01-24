


##funciton ot find a string in your directory from https://stackoverflow.com/questions/45502010/is-there-an-r-version-of-rstudios-find-in-files

fif <- function(what, where=".", in_files="\\.[Rr]$", recursive = TRUE,
                ignore.case = TRUE) {
  fils <- list.files(path = where, pattern = in_files, recursive = recursive)
  found <- FALSE
  file_cmd <- Sys.which("file")
  for (fil in fils) {

    if (nchar(file_cmd) > 0) {
      ftype <- system2(file_cmd, fil, TRUE)
      if (!grepl("text", ftype)[1]) next
    }
    contents <- readLines(fil)
    res <- grepl(what, contents, ignore.case = ignore.case)
    res <- which(res)
    if (length(res) > 0) {
      found <-  TRUE
      cat(sprintf("%s\n", fil), sep="")
      cat(sprintf(" % 4s: %s\n", res, contents[res]), sep="")
    }
  }
  if (!found) message("(No results found)")
}




fit_to_page <- function(ft, pgwidth = 6.75){

  ft_out <- ft %>% flextable::autofit()

  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}

fit_to_page_landscape <- function(ft, pgwidth = 12){

  ft_out <- ft %>% flextable::autofit()

  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}


my_flextable <- function(df,  ...){ ##left_just_col = 2 was an option
  flextable::autofit(flextable::flextable(
    df,
    defaults = list(fontname = 'tahoma'))) %>%
    flextable::theme_booktabs(fontsize = 8) %>% ##changed from flextable::my_theme_booktabs(fontsize = 9) %>%
    fit_to_page()
}

##function to trim up sheet and get names (was previously source from altools package)
at_trim_xlsheet2 <- function(df, column_last = ncol(df)) {
  df %>%
    dplyr::select(1:column_last) %>% ##get rid of the extra columns.  should be more abstract
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}



####------my_kable-------------------------------
my_kable_scroll <- function(dat, caption_text = '', font = font_set){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed", "responsive"),
                              full_width = T,
                              font_size = font) %>%
    kableExtra::scroll_box(width = "100%", height = "500px")
}

my_tab_overview <- function(dat, caption_text = '', font = font_set){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed", "responsive"), full_width = T, font_size = font) %>%
    kableExtra::column_spec(column = c(9), width_min = '1.5in') %>%
    kableExtra::column_spec(column = c(5), width_min = '1.0in', width_max = '1.0in')
}

my_tab_overview_scroll <- function(dat, caption_text = '', font = font_set){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"),
                              full_width = T,
                              font_size = font) %>%
    kableExtra::column_spec(column = c(9), width_min = '1.5in') %>%
    kableExtra::column_spec(column = c(5), width_max = '1in') %>%
    kableExtra::scroll_box(width = "100%", height = "500px")
}


my_kable_scroll_no_height <- function(dat, caption_text = ''){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::scroll_box(width = "100%")
}

my_kable <- function(dat, caption_text = '', font = font_set){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed", "responsive"),
                              full_width = T,
                              font_size = font)
    # kableExtra::scroll_box(width = "100%", height = "500px")
}

get_img <- function(site = my_site, photo = my_photo){
  jpeg::readJPEG(paste0('data/photos/', site, '/', photo))
}

get_img_path_abs <- function(site = my_site, photo = my_photo){
  stub <- 'https://github.com/NewGraphEnvironment/fish_passage_elk_2020_reporting/blob/master/'
  paste0(stub, 'data/photos/', site, '/', photo)
}

get_img_path <- function(site = my_site, photo = my_photo){
  paste0('data/photos/', site, '/', photo)
}


print_tab_cost_mult <- function(dat = tab_cost_rd_mult_report, ...){
  tab_cost_rd_mult_report %>%
  my_kable()
}

##here is a shot at a function to pull a photo based on a string subset
pull_photo_by_str <- function(site_id = my_site, str_to_pull = 'barrel'){
  list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., str_to_pull) %>%
    basename()
}

appendix_title <- function(site = my_site){
  paste0('# Appendix - ', site, ' - ', my_overview_info() %>% pull(stream_name), ' {-}')
}


##when we have 2 crosings
appendix_title2 <- function(site = my_site, site2 = my_site2){
  paste0('# Appendix - ', site, ' & ', site2, ' - ', my_overview_info() %>% pull(stream_name), ' {-}')
}

##when we have 3 crosings
appendix_title3 <- function(site = my_site, site2 = my_site2, site3 = my_site3){
  paste0('# Appendix - ', site, ' & ', site2, ' & ', site3, ' - ', my_overview_info() %>% pull(stream_name), ' {-}')
}


appendix_subtitle <- function(){
  paste0('**', my_overview_info() %>% pull(road_name), ' - ', my_overview_info() %>% pull(stream_name), '**')
}


##############this is for making kmls
make_kml_col <- function(df){
  df %>%
    mutate(`PSCIS ID` = as.integer(`PSCIS ID`),
           `Modelled ID` = as.integer(`Modelled ID`),
           color = case_when(Priority == 'high' ~ 'red',
                             Priority == 'no fix' ~ 'green',
                             Priority == 'moderate' ~ 'yellow',
                             T ~ 'grey'),
           # shape = case_when(Priority == 'high' ~ 'http://maps.google.com/mapfiles/kml/pushpin/red-pushpin.png',
           #                   Priority == 'no fix' ~ 'http://maps.google.com/mapfiles/kml/pushpin/grn-pushpin.png',
           #                   Priority == 'moderate' ~ 'http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png',
           #                   T ~ 'http://maps.google.com/mapfiles/kml/pushpin/wht-pushpin.png'),
           shape = case_when(Priority == 'high' ~ 'http://maps.google.com/mapfiles/kml/paddle/red-blank.png',
                             Priority == 'no fix' ~ 'http://maps.google.com/mapfiles/kml/paddle/grn-blank.png',
                             Priority == 'moderate' ~ 'http://maps.google.com/mapfiles/kml/paddle/ylw-blank.png',
                             T ~ 'http://maps.google.com/mapfiles/kml/paddle/wht-blank.png'),
           color = plotKML::col2kml(color),
           site_id = case_when(!is.na(`PSCIS ID`) ~ paste('PSCIS ', `PSCIS ID`),
                               is.na(`PSCIS ID`) ~ paste0('Modelled ', `Modelled ID`)),
           label = paste0(site_id, '-', Priority),
           `Image link` = case_when(!is.na(`Image link`) ~ cell_spec('crossing', "html", link = `Image link`),
                                    T ~ `Image link`)) %>%
    select(site_id, Priority, label, color, shape, everything())
  # mutate(across(where(is.numeric), round(.,2)))

}

## add a line to the function to make the comments column wide enough
make_html_tbl <- function(df) {
  # df2 <- df %>%
  #   dplyr::mutate(`Image link` = cell_spec('crossing', "html", link = `Image link`))
  df2 <- select(df, -shape, -color, -label) %>% janitor::remove_empty()
  df %>%
    mutate(html_tbl = knitr::kable(df2, 'html', escape = F) %>%
             kableExtra::row_spec(0:nrow(df2), extra_css = "border: 1px solid black;") %>% # All cells get a border
             kableExtra::row_spec(0, background = "yellow") %>%
             kableExtra::column_spec(column = ncol(df2) - 1, width_min = '0.5in') %>%
             kableExtra::column_spec(column = ncol(df2), width_min = '4in')
    )
}


openHTML <- function(x) browseURL(paste0('file://', file.path(getwd(), x)))

##function to import pscis info
fpr_import_pscis <- function(workbook_name = 'pscis_phase1.xlsm'){ ##new template.  could change file back to .xls
  sig_fig0 <- c('length_or_width_meters')
  sig_fig1 <- c('culvert_slope_percent', 'stream_width_ratio')
  sig_fig2 <- c('outlet_drop_meters')
  readxl::read_excel(path = paste0(getwd(),"/data/", workbook_name),
                     sheet = 'PSCIS Assessment Worksheet') %>%
    # purrr::set_names(janitor::make_clean_names(names(.))) %>%
    at_trim_xlsheet2() %>% ##recently added function above and pulled the altools package as it was a week link
    rename(date = date_of_assessment_yyyy_mm_dd) %>%
    mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>%
    filter(!is.na(date)) %>%
    readr::type_convert() %>%  ##guess the type!!
    mutate(source = workbook_name) %>%
    mutate(across(all_of(sig_fig0), round, 0)) %>%
    mutate(across(all_of(sig_fig1), round, 1)) %>%
    mutate(across(all_of(sig_fig2), round, 2)) %>%
    tibble::rowid_to_column() %>%
    mutate(rowid = rowid + 4,
           pscis_crossing_id = as.numeric(pscis_crossing_id),
           my_crossing_reference = as.numeric(my_crossing_reference)
           ) %>%
    mutate(
      aggregated_crossings_id = case_when(!is.na(pscis_crossing_id) ~ pscis_crossing_id,
                                          my_crossing_reference > 200000000 ~ my_crossing_reference,  ##date based id's are greater than this number
                                          T ~ my_crossing_reference + 1000000000)
    )
}


# import_pscis_all <- function(){
#   dat1 <- import_pscis(workbook_name = 'pscis_phase1.xlsm')
#   # filter(!my_crossing_reference %in% dups)
#   dat2 <- import_pscis(workbook_name = 'pscis_phase2.xlsm')
#   dat3 <- import_pscis(workbook_name = 'pscis_reassessments.xlsm')
#   pscis <- bind_rows(
#     dat1,
#     dat2,
#     dat3
#   )
#   all <- list(dat1, dat2, dat3, pscis) %>%
#     purrr::set_names(c('pscis_phase1', 'pscis_phase2', 'pscis_reassessments', 'pscis_all'))
#   return(all)
# }



##back photos to another place.  Going to split into two functions
fpr_photos_backup <- function(filename = 'al'){
  ##get teh name of the folder we are in
  project_name <- basename(dirname(dirname(getwd())))
  ##here we back everything up to the D drive
  dir_backup_prj = paste0("D:/New_Graph/backups/photos/", project_name, "/")
  dir.create(dir_backup_prj)

  dir_backup_photos = paste0("D:/New_Graph/backups/photos/", project_name, "/", filename)
  dir.create(dir_backup_photos)


  ##path to the photos
  path_photos <- paste0("C:/Users/allan/OneDrive/New_Graph/Current/", project_name, '/data/photos/', filename)

  filestocopy <- list.files(path = path_photos,
                            full.names = T)

  #copy over the photos in the al folder -- this is done already
  file.copy(from=filestocopy, to=dir_backup_photos,
            overwrite = F, recursive = FALSE,
            copy.mode = TRUE)
}

## we  want to convert our png to jpeg in case we want them for something
fpr_img_resize_convert <- function(img){
  image <- image_read(img)
  image_scaled <- image_scale(image,"1440x1080!")
  image_write(image_scaled, path = paste0(path, '/', tools::file_path_sans_ext(basename(img)), '.JPG'), format = 'jpg')
}

##function that builds the folders
fpr_make_photo_folders <- function(xing){
  dir.create(paste0(getwd(), '/data/photos/', xing))
}


fpr_time_interval_idx <- function(date_input, intervs){
  which(date_input %within% intervs)
}


##get the photo sorting specific metadata from the photos in the file
fpr_photo_sort_metadat <- function(input_file){
  exifr::read_exif(input_file,recursive=T) %>%
    purrr::set_names(., nm = tolower(names(.))) %>%
    select(sourcefile, datetimeoriginal) %>%
    mutate(datetimeoriginal = lubridate::ymd_hms(datetimeoriginal))
}

##get the names of your pscis files
fpr_pscis_wkb_paths <- function(){
  list.files(path = 'data', pattern = "pscis", all.files = F) %>%
    grep(pattern = '~', invert = T, value = T)
}

fpr_import_pscis_all <- function(){
  wkbs_paths <- fpr_pscis_wkb_paths()

  pscis_list <- wkbs_paths %>%
    map(fpr_import_pscis) %>%
    purrr::set_names(nm = tools::file_path_sans_ext(wkbs_paths))
}

fpr_photo_qa <- function(site_id){
  list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet') %>%
    as_tibble() %>%
    mutate(x = case_when(
      value %ilike% 'road' ~ 'road',
      value %ilike% 'inlet' ~ 'inlet',
      value %ilike% 'upstream' ~ 'upstream',
      value %ilike% 'barrel' ~ 'barrel',
      value %ilike% 'outlet' ~ 'outlet',
      value %ilike% 'downstream' ~ 'downstream'
    )) %>%
    select(-value)
}

##here we stack up and down then side to side for reporting - this works!
fpr_photo_amalg_cv <- function(site_id){
  photos_images1 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., 'upstream|road|inlet') %>%
    as_tibble() %>%
    mutate(sort = case_when(
      value %ilike% 'road' ~ 1,
      value %ilike% 'inlet' ~ 2,
      value %ilike% 'upstream' ~ 3,
      # value %ilike% 'barrel' ~ 4,
      # value %ilike% 'outlet' ~ 5,
      # value %ilike% 'downstream' ~ 6,
    )) %>%
    arrange(sort) %>%
    pull(value) %>%
    image_read()
  photos_images2 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., 'barrel|outlet|downstream') %>%
    as_tibble() %>%
    mutate(sort = case_when(
      # value %ilike% 'road' ~ 1,
      # value %ilike% 'inlet' ~ 2,
      # value %ilike% 'upstream' ~ 3,
      value %ilike% 'barrel' ~ 4,
      value %ilike% 'outlet' ~ 5,
      value %ilike% 'downstream' ~ 6,
    )) %>%
    arrange(sort) %>%
    pull(value) %>%
    image_read()
  photos_stack1 <-image_append(image_scale(photos_images1, "x420"), stack = T) ##1/3 the width 373.33 and half the original height
  photos_stack2 <- image_append(image_scale(photos_images2, "x420"), stack = T)
  photos_stack <- c(photos_stack1, photos_stack2)
  photos_stacked <- image_append(image_scale(photos_stack), stack = F)
  image_write(photos_stacked, path = paste0(getwd(), '/data/photos/', site_id, '/crossing_all.JPG'), format = 'jpg')
}
