##now that we have loaded our phase 1 data to pscis we need to pull out the new PSCIS crossing id's

source('R/packages.R')
source('R/functions.R')

pscis_list <- fpr_import_pscis_all()

# xref_my_crossing_ref <- bind_rows(pscis_list) %>%
#   janitor::get_dupes(pscis_crossing_id, my_crossing_reference) %>%
#   filter(source == 'pscis_phase2.xlsm') %>%
#   pull(my_crossing_reference)


get_this <- bcdc_tidy_resources('pscis-assessments') %>%
  filter(bcdata_available == T)  %>%
  pull(package_id)

dat <- bcdata::bcdc_get_data(get_this)

##grab the pscis id xreferenced
xref_pscis_my_crossing_modelled <- dat %>%
  purrr::set_names(nm = tolower(names(.))) %>%
  dplyr::filter(funding_project_number == "ENV07_Elk") %>% ##we don't need these - funding_project_number == "Bulkley_6-288_Reassessments"
  select(external_crossing_reference, stream_crossing_id) %>%
  sf::st_drop_geometry() %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/xref_pscis_my_crossing_modelled.csv'))

####--------------bring in the habitat data and build csv to copy and paste corrected PSCIS ids in fisheries submission spreadsheet------------------
habitat_confirmations <- fpr_import_hab_con()

hab_loc <- habitat_confirmations %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  dplyr::filter(!is.na(site_number))%>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date))) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location', 'fish'), remove = F) %>%
  select(site:fish) %>%
  mutate(site = as.numeric(site))

hab_site_corrected <- left_join(
  hab_loc,
  xref_pscis_my_crossing_modelled,
  by = c('site' = 'external_crossing_reference')
) %>%
  mutate(stream_crossing_id = as.numeric(stream_crossing_id),
         stream_crossing_id = case_when(
    is.na(stream_crossing_id) ~ site,
    T ~ stream_crossing_id
  )) %>%
  mutate(site_corrected = paste(stream_crossing_id, location, fish, sep = '_')) %>%
  mutate(site_corrected = stringr::str_replace_all(site_corrected, '_NA', '')) %>%
  tibble::rownames_to_column() %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/hab_site_corrected.csv'))

##build csv to copy and paste new PSCIS stream_crossing_ids into pscis_phase2 spreadsheet
pscis_all <- bind_rows(pscis_list)

xref_pscis_my_crossing_phase2 <- left_join(
  pscis_all,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) %>%
  mutate(pscis_crossing_id = case_when(
    is.na(pscis_crossing_id) ~ stream_crossing_id,
    T ~ as.integer(pscis_crossing_id)
  )) %>%
  filter(source %ilike% 'phase2') %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/xref_pscis_my_crossing_phase2.csv'))

