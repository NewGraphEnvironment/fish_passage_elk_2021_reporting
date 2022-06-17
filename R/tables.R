# this file imports our data and builds the tables we need for our reporting

source('R/packages.R')
source('R/functions.R')



pscis_list <- fpr_import_pscis_all()
pscis_phase1 <- pscis_list %>% pluck('pscis_phase1')
pscis_phase2 <- pscis_list %>% pluck('pscis_phase2') %>%
  arrange(pscis_crossing_id)
pscis_reassessments <- pscis_list %>% pluck('pscis_reassessments')
pscis_all_prep <- pscis_list %>%
  bind_rows()

# this is a workaround for the wshd area since it is now dropped from the crossings table
wshds <- sf::read_sf('data/fishpass_mapping/fishpass_mapping.gpkg', layer = 'hab_wshds')

##lets add in the xref pscis id info - this is made from 01_prep_data/0140-extract-crossings-xref.R
xref_pscis_my_crossing_modelled <- readr::read_csv(
  file = paste0(getwd(),
                '/data/inputs_extracted/xref_pscis_my_crossing_modelled.csv'))
  # mutate(external_crossing_reference = as.numeric(external_crossing_reference)) %>%
  # rename(my_crossing_reference = external_crossing_reference)

# velocity data from Coal
coal_velocity <- readr::read_csv(file = paste0(getwd(), '/data/coal_velocity.csv')) %>%
  filter(!is.na(depth_perc)) %>%
  select(Distance = distance, Velocity = velocity)

pscis_all <- left_join(
  pscis_all_prep,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) %>%
  mutate(pscis_crossing_id = case_when(
    is.na(pscis_crossing_id) ~ stream_crossing_id,
    T ~ pscis_crossing_id
  )) %>%
  mutate(amalgamated_crossing_id = case_when(
    !is.na(my_crossing_reference) ~ my_crossing_reference,
    T ~ pscis_crossing_id
  )) %>%
  select(-stream_crossing_id) %>%
  arrange(pscis_crossing_id)


pscis_all_sf <- pscis_all %>%
  # distinct(.keep_all = T) %>%
  sf::st_as_sf(coords = c("easting", "northing"),
               crs = 26911, remove = F) %>% ##don't forget to put it in the right crs buds
  sf::st_transform(crs = 3005) ##convert to match the bcfishpass format


pscis_all_sf <- poisspatial::ps_elevation_google(pscis_all_sf,
                                        key = Sys.getenv('GOOG_API_KEY'),
                                        Z = 'elev') %>%
  mutate(elev = round(elev, 0))

##this is our new db made from 0282-extract-bcfishpass2-crossing-corrections.R and 0290
conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)
bcfishpass_phase2 <- readwritesqlite::rws_read_table("bcfishpass", conn = conn) %>%
  filter(stream_crossing_id %in% (pscis_phase2 %>% pull(pscis_crossing_id))) %>%
  mutate(downstream_route_measure = as.integer(downstream_route_measure))
bcfishpass <- readwritesqlite::rws_read_table("bcfishpass", conn = conn) %>%
  mutate(downstream_route_measure = as.integer(downstream_route_measure)) %>%
  mutate(wct_network_km = round(wct_network_km,2))
# bcfishpass_archive <- readwritesqlite::rws_read_table("bcfishpass_archive_2022-03-02-1403", conn = conn)
bcfishpass_column_comments <- readwritesqlite::rws_read_table("bcfishpass_column_comments", conn = conn)
# bcfishpass_archived <- readwritesqlite::rws_read_table("bcfishpass_morr_bulk_archive", conn = conn) %>%
#   mutate(downstream_route_measure = as.integer(downstream_route_measure))
# pscis_historic_phase1 <- readwritesqlite::rws_read_table("pscis_historic_phase1", conn = conn)
bcfishpass_spawn_rear_model <- readwritesqlite::rws_read_table("bcfishpass_spawn_rear_model", conn = conn)
wshds <- readwritesqlite::rws_read_table("wshds", conn = conn)
photo_metadata <- readwritesqlite::rws_read_table("photo_metadata", conn = conn)
# fiss_sum <- readwritesqlite::rws_read_table("fiss_sum", conn = conn)
rws_disconnect(conn)

##build the dams table
tab_dams_raw <- bcfishpass %>%
  filter(aggregated_crossings_id == 1100000129 |
           aggregated_crossings_id == 1100002016 |
           aggregated_crossings_id == 197542) %>%
  select(id = aggregated_crossings_id, stream = gnis_stream_name,utm_zone, utm_easting, utm_northing, dbm_mof_50k_grid) %>%
  mutate(barrier_ind = case_when(
    id == 1100000129 ~ 'F',
    T ~ 'T'),
    Notes = case_when(
      id == 1100000129 ~ 'Remnant dam not located in main channel.',
      id == 1100002016 ~ 'Large dam (15m  high at 55% grade) located in main channel. No fish ladder.',
      id == 197542 ~ 'Two small dams (30cm and 40cm high) located just upstream (7m and 20m) of Dicken Road. Likely easily passable by adult WCT but barrier to fry and small juveniles. If culvert replaced these could potentially be fixed at the same time.'
    )
  )


##this is not working or needed yet
# bcfishpass_rd <- bcfishpass %>%
#   select(pscis_crossing_id = stream_crossing_id, my_crossing_reference, crossing_id, distance, road_name_full,
#          road_class, road_name_full, road_surface, file_type_description, forest_file_id,
#          client_name, client_name_abb, map_label, owner_name, admin_area_abbreviation,
#          steelhead_network_km, steelhead_belowupstrbarriers_network_km, distance) %>%
#   # filter(distance < 100) %>% ## we need to screen out the crossings that are not matched well
#   select(pscis_crossing_id, my_crossing_reference:admin_area_abbreviation, steelhead_network_km, steelhead_belowupstrbarriers_network_km)

####----tab cost multipliers for road surface-----
tab_cost_rd_mult <- readr::read_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_cost_mult.csv'))


####-----------report table--------------------
tab_cost_rd_mult_report <- tab_cost_rd_mult %>%
  mutate(cost_m_1000s_bridge = cost_m_1000s_bridge * 10) %>%
  rename(
    Class = my_road_class,
    Surface = my_road_surface,
    `Class Multiplier` = road_class_mult,
    `Surface Multiplier` = road_surface_mult,
    `Bridge $K/10m` = cost_m_1000s_bridge,
    `Streambed Simulation $K` = cost_embed_cv
  ) %>%
  filter(!is.na(Class)) %>%
  mutate(Class = stringr::str_to_title(Class),
         Surface = stringr::str_to_title(Surface)
  )


#could run a source('R/01_prep_data/0150-extract-rd-cost-mult.R)
###note that some of the rd info is not likely correct if the distance is >100m
pscis_rd_raw <- readr::read_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_class_surface.csv'))


pscis_rd <- left_join(
  pscis_rd_raw,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) %>%
  mutate(pscis_crossing_id = case_when(!is.na(stream_crossing_id) ~ stream_crossing_id,
                                       T ~ pscis_crossing_id)) %>%
  select(-stream_crossing_id)
  # filter(distance < 100)


#load priorities
habitat_confirmations_priorities <- readr::read_csv(
  file = "./data/habitat_confirmations_priorities.csv") %>%
  filter(!local_name %like% 'ef') %>% ##ditch the ef sites
  tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>%
  mutate(site = as.numeric(site),
         upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1))

####--------------bring in the habitat and fish data------------------
habitat_confirmations <- fpr_import_hab_con()



#------------------make the tables for the methods----------
tab_habvalue <- tibble::tibble(`Habitat Value` = c('High', 'Medium', 'Low'),
                                `Fish Habitat Criteria` = c(
                                  'The presence of high value spawning or rearing habitat (e.g., locations with abundance of suitably sized gravels, deep pools, undercut banks, or stable debris) which are critical to the fish population.',
                                  'Important migration corridor. Presence of suitable spawning habitat. Habitat with moderate rearing potential for the fish species present.', 'No suitable spawning habitat, and habitat with low rearing potential (e.g., locations without deep pools, undercut banks, or stable debris, and with little or no suitably sized spawning gravels for the fish species present).'
                                )
)




tab_barrier_scoring <- dplyr::tribble(
  ~Risk,   ~Embedded,                                 ~Value,  ~`Outlet Drop (cm)`, ~Value, ~SWR,    ~Value, ~`Slope (%)`, ~Value, ~`Length (m)`, ~Value,
  "LOW",  ">30cm or >20% of diameter and continuous",  "0",       "<15",              '0',  '<1.0',    '0',      '<1',      '0',     '<15',         '0',
  "MOD",  "<30cm or 20% of diameter but continuous",   "5",       "15-30",            '5',  '1.0-1.3', '3',      '1-3',     '5',     '15-30',       '3',
  "HIGH", "No embedment or discontinuous",             "10",      ">30",             '10',  '>1.3',    '6',       '>3',     '10',    '>30',         '6',
)
  # kable() %>%
  # kable_styling(latex_options = c("striped", "scale_down")) %>%
  # kableExtra::save_kable("fig/tab_barrier_scoring.png")

tab_barrier_result <- dplyr::tribble(
  ~`Cumlative Score`, ~Result,
  '0-14',             'passable',
  '15-19',            'potential barrier',
  '>20',              'barrier'
)

##workflows to create these tables can be found at https://github.com/NewGraphEnvironment/fish_passage_elk_2020_reporting_cwf/blob/master/R/tables.R


####---------make a table to cross reference column names for ---------------
xref_names <- tibble::tribble(
                          ~bcdata,                               ~spdsht,                 ~report, ~id_join, ~id_side,
                             "id",                                    NA,                      NA,       NA,       NA,
         "funding_project_number",                                    NA,                      NA,       NA,       NA,
                "funding_project",                                    NA,                      NA,       NA,       NA,
                     "project_id",                                    NA,                      NA,       NA,       NA,
                 "funding_source",                                    NA,                      NA,       NA,       NA,
         "responsible_party_name",                                    NA,                      NA,       NA,       NA,
                "consultant_name",                                    NA,                      NA,       NA,       NA,
                "assessment_date",                                "date",                  "Date",       1L,       1L,
             "stream_crossing_id",                   "pscis_crossing_id",              "PSCIS ID",       2L,       1L,
                  "assessment_id",                                    NA,                      NA,       NA,       NA,
    "external_crossing_reference",               "my_crossing_reference",           "External ID",       3L,       1L,
                   "crew_members",                        "crew_members",                  "Crew",       5L,       1L,
                       "utm_zone",                            "utm_zone",              "UTM Zone",       6L,       1L,
                    "utm_easting",                             "easting",               "Easting",       7L,       1L,
                   "utm_northing",                            "northing",              "Northing",       8L,       1L,
        "location_confidence_ind",                                    NA,                      NA,       NA,       NA,
                    "stream_name",                         "stream_name",                "Stream",       9L,       1L,
                      "road_name",                           "road_name",                  "Road",      10L,       1L,
                   "road_km_mark",                        "road_km_mark",                      NA,       NA,       NA,
                    "road_tenure",                         "road_tenure",           "Road Tenure",      11L,       1L,
             "crossing_type_code",                       "crossing_type",         "Crossing Type",       NA,       NA,
             "crossing_type_desc",                                    NA,                      NA,       NA,       NA,
          "crossing_subtype_code",                    "crossing_subtype",     "Crossing Sub Type",       1L,       2L,
          "crossing_subtype_desc",                                    NA,                      NA,       NA,       NA,
               "diameter_or_span",             "diameter_or_span_meters",          "Diameter (m)",       2L,       2L,
                "length_or_width",              "length_or_width_meters",            "Length (m)",       3L,       2L,
    "continuous_embeddedment_ind",      "continuous_embeddedment_yes_no",              "Embedded",       5L,       2L,
     "average_depth_embededdment",   "average_depth_embededdment_meters",    "Depth Embedded (m)",       6L,       2L,
           "resemble_channel_ind",             "resemble_channel_yes_no",      "Resemble Channel",       7L,       2L,
                "backwatered_ind",                  "backwatered_yes_no",           "Backwatered",       8L,       2L,
         "percentage_backwatered",              "percentage_backwatered",   "Percent Backwatered",       9L,       2L,
                     "fill_depth",                   "fill_depth_meters",        "Fill Depth (m)",      10L,       2L,
                    "outlet_drop",                  "outlet_drop_meters",       "Outlet Drop (m)",      11L,       2L,
              "outlet_pool_depth",             "outlet_pool_depth_0_01m", "Outlet Pool Depth (m)",      12L,       2L,
                 "inlet_drop_ind",                   "inlet_drop_yes_no",            "Inlet Drop",      13L,       2L,
                  "culvert_slope",               "culvert_slope_percent",             "Slope (%)",      14L,       2L,
       "downstream_channel_width",     "downstream_channel_width_meters",     "Channel Width (m)",      12L,       1L,
                   "stream_slope",                        "stream_slope",      "Stream Slope (%)",      13L,       1L,
            "beaver_activity_ind",              "beaver_activity_yes_no",       "Beaver Activity",      14L,       1L,
              "fish_observed_ind",                "fish_observed_yes_no",          "Fish Sighted",       NA,       NA,
               "valley_fill_code",                         "valley_fill",           "Valley Fill",      15L,       2L,
          "valley_fill_code_desc",                                    NA,                      NA,       NA,       NA,
             "habitat_value_code",                       "habitat_value",         "Habitat Value",      15L,       1L,
             "habitat_value_desc",                                    NA,                      NA,       NA,       NA,
             "stream_width_ratio",                  "stream_width_ratio",                   "SWR",       NA,       NA,
       "stream_width_ratio_score",                                    NA,                 "Score",       NA,       NA,
           "culvert_length_score",                "culvert_length_score",                 "Score",       NA,       NA,
                    "embed_score",                         "embed_score",                 "Score",       NA,       NA,
              "outlet_drop_score",                   "outlet_drop_score",                 "Score",       NA,       NA,
            "culvert_slope_score",                 "culvert_slope_score",                 "Score",       NA,       NA,
                    "final_score",                         "final_score",           "Final score",      16L,       1L,
            "barrier_result_code",                      "barrier_result",        "Barrier Result",      16L,       2L,
     "barrier_result_description",                                    NA,                      NA,       NA,       NA,
              "crossing_fix_code",                                    NA,                      NA,       NA,       NA,
              "crossing_fix_desc",                        "crossing_fix",              "Fix type",      17L,       1L,
   "recommended_diameter_or_span", "recommended_diameter_or_span_meters",   "Fix Span / Diameter",      17L,       2L,
             "assessment_comment",                  "assessment_comment",               "Comment",       NA,       NA,
                     "ecocat_url",                                    NA,                      NA,       NA,       NA,
                 "image_view_url",                                    NA,                      NA,       NA,       NA,
           "current_pscis_status",                                    NA,                      NA,       NA,       NA,
     "current_crossing_type_code",                                    NA,                      NA,       NA,       NA,
     "current_crossing_type_desc",                                    NA,                      NA,       NA,       NA,
  "current_crossing_subtype_code",                                    NA,                      NA,       NA,       NA,
  "current_crossing_subtype_desc",                                    NA,                      NA,       NA,       NA,
    "current_barrier_result_code",                                    NA,                      NA,       NA,       NA,
    "current_barrier_description",                                    NA,                      NA,       NA,       NA,
                   "feature_code",                                    NA,                      NA,       NA,       NA,
                       "objectid",                                    NA,                      NA,       NA,       NA,
               "se_anno_cad_data",                                    NA,                      NA,       NA,       NA,
                       "geometry",                                    NA,                      NA,       NA,       NA
  )


xref_structure_fix <- tibble::tribble(
                        ~crossing_fix_code,                                ~crossing_fix_desc,                                     ~crossing_fix,
                                      "RM",                    "Remove / Deactivate Crossing",                      "Remove/Deactivate Crossing",
                                     "OBS",          "Replace with new open bottom structure",          "Replace with New Open Bottom Structure",
                                  "SS-CBS", "Replace structure with streambed simulation CBS", "Replace Structure with Streambed Simulation CBS",
                                      "EM",          "Add substrate to further imbed the CBS",          "Add Substrate to Further embed the CBS",
                                      "BW",     "Install downstream weir(s) to backwater CBS",     "Install Downstream Weir(s) to Backwater CBS"
                        )


##this is made from load-obstacles-xref.R
xref_obstacle_names <- tibble::tribble(
                                                    ~fiss_obstacle_name, ~spreadsheet_feature_type,
                                                          "Beaver Dams",                        NA,
                                                     "Velocity barrier",                        NA,
                                                                "Wedge",                        NA,
                                                               "Bridge",                        NA,
                                                            "Hydro Dam",                        NA,
                                         "Water Management Storage Dam",                        NA,
                                                        "Not Specified",                        NA,
                                                                 "Bars",                        NA,
                                                              "LWD Jam",                 "LWD Jam",
                                                                "OTHER",                        NA,
                                              "Irrigation District Dam",                        NA,
                                                 "Dam - Unknown Origin",                        NA,
                                                              "Cascade",                        NA,
                                                                 "Pump",                        NA,
                                                              "Log jam",                        NA,
                                                     "Cascade or Chute",                        NA,
                                                    "Persistent Debris",                        NA,
                                                            "Hydro dam",                        NA,
                                                                "Rocks",                        NA,
                         "Persistent debris; present for several years",                        NA,
                                                                 "Weir",                        NA,
                                                                "Falls",                   "falls",
                                                                 "Logs",                        NA,
                                                              "Log Jam",                        NA,
                                                              "Culvert",                        NA,
                                                                 "Rock",                        NA,
                                                               "Canyon",                        NA,
                                                           "Beaver Dam",                        NA,
                                                "Regional District Dam",                        NA,
                                                          "Underground",                        NA,
                                                         "Woody Debris",                        NA,
                                                        "Cascade/Chute",        "cascade or Chute",
                                                          "Private Dam",                        NA,
                                                             "Gradient",                        NA,
                                             "Fisheries Management Dam",                        NA,
                                                           "BEAVER DAM",                        NA,
                                          "Landslide or bank sloughing",                        NA,
                                                     "Velocity Barrier",                        NA,
                                                                  "Dam",                        NA
                         )


####------------make a table to summarize priorization of phase 1 sites
##uses habitat value to initially screen but then refines based on what are likely not barriers to most most the time
phase1_priorities <- pscis_all %>%
  filter(!source %ilike% 'phase2') %>% ##we don't want the phase 1 action
  select(aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, utm_zone:northing, habitat_value, barrier_result, source) %>%
  mutate(priority_phase1 = case_when(habitat_value == 'High' & barrier_result != 'Passable' ~ 'high',
                                     habitat_value == 'Medium' & barrier_result != 'Passable' ~ 'mod',
                                     habitat_value == 'Low' & barrier_result != 'Passable' ~ 'low',
                                     T ~ NA_character_)) %>%
  mutate(priority_phase1 = case_when(habitat_value == 'High' & barrier_result == 'Potential' ~ 'mod',
                                     T ~ priority_phase1)) %>%
  mutate(priority_phase1 = case_when(habitat_value == 'Medium' & barrier_result == 'Potential' ~ 'low',
                                     T ~ priority_phase1)) %>%
  # mutate(priority_phase1 = case_when(my_crossing_reference == 99999999999 ~ 'high', ##this is where we can make changes to the defaults
  #                                    T ~ priority_phase1)) %>%
  dplyr::rename(utm_easting = easting, utm_northing = northing)


##turn spreadsheet into list of data frames - why was this pscis_crossing_id?
pscis_phase1_for_tables <- pscis_all %>%
  filter(source %ilike% 'phase1') %>%
  arrange(pscis_crossing_id)

pscis_split <- pscis_phase1_for_tables  %>% #pscis_phase1_reassessments
  # sf::st_drop_geometry() %>%
  # mutate_if(is.numeric, as.character) %>% ##added this to try to get the outlet drop to not disapear
  # tibble::rownames_to_column() %>%
  dplyr::group_split(pscis_crossing_id) %>%
  purrr::set_names(pscis_phase1_for_tables$pscis_crossing_id)  ##changed to my_crossing_id

##make result summary tables for each of the crossings
tab_summary <- pscis_split %>%
  purrr::map(fpr_make_tab_summary)

tab_summary_comments <- pscis_split %>%
  purrr::map(fpr_make_tab_summary_comments)

##had a hickup where R cannot handle the default size of the integers we used for numbers so we had to change site names!!
tab_photo_url <- list.files(path = paste0(getwd(), '/data/photos/'), full.names = T) %>%
  basename() %>%
  as_tibble() %>%
  mutate(value = as.integer(value)) %>%  ##need this to sort
  dplyr::arrange(value)  %>%
  mutate(photo = paste0('![](data/photos/', value, '/crossing_all.JPG)')) %>%
  filter(value %in% pscis_phase1_for_tables$my_crossing_reference)  %>% ##we don't want all the photos - just the phase 1 photos for this use case!!!
  left_join(., xref_pscis_my_crossing_modelled, by = c('value' = 'external_crossing_reference'))  %>% ##we need to add the pscis id so that we can sort the same
  arrange(stream_crossing_id) %>%
  select(-value) %>%
  # pull(photo)
  dplyr::group_split(stream_crossing_id)
  # purrr::set_names(nm = . %>% bind_rows() %>% arrange(value) %>% pull(stream_crossing_id)) %>%
  # bind_rows()
  # arrange(stream_crossing_id) %>%
  # dplyr::group_split(value)


##these are the reassessments!!!!!
##built from funciton in functions.R file
tabs_phase1 <- mapply(fpr_print_tab_summary_all, tab_sum = tab_summary, comments = tab_summary_comments, photos = tab_photo_url)

##built from funciton in functions.R file
tabs_phase1_pdf <- mapply(fpr_print_tab_summary_all_pdf, tab_sum = tab_summary, comments = tab_summary_comments, photos = tab_photo_url)


##not needed this round
# tab_plan_raw <- readr::read_csv(file = 'data/extracted_inputs/planning_results.csv', guess_max = 1500)
#
# tab_plan_sf <- tab_plan_raw %>%
#   filter(!is.na(my_text)) %>%
#   arrange(stream_crossing_id, modelled_crossing_id) %>%
#   st_as_sf(crs = 3005, coords = c("long", "lat")) %>%
#   st_transform(crs = 4326) %>%
#   mutate(my_priority = case_when(my_priority == 'mod' ~ 'moderate',
#                                  T ~ my_priority)) %>%
#   # dplyr::mutate(image_view_url = case_when(is.na(image_view_url) ~ NA_character_,
#   #                                          T ~ paste0('<a href =', image_view_url,'>', 'PSCIS Image link', '</a>'))) %>%
#   dplyr::mutate(image_view_url = case_when(is.na(image_view_url) ~ NA_character_,
#                                            T ~ paste0('<a href =', image_view_url,' target="_blank">PSCIS Image</a>'))) %>%
#   dplyr::mutate(model_link = paste0('<a href =', 'sum/bcfp/', aggregated_crossings_id, '.html ', 'target="_blank">Model Data</a>')) %>%
#   select(
#          Priority = my_priority,
#          `PSCIS ID` = stream_crossing_id,
#          `Modelled ID` = modelled_crossing_id,
#          `Species` = observedspp_upstr,
#          `Order` = stream_order,
#          `Upstream habitat (km)` = salmon_network_km,
#          `Channel width` = downstream_channel_width,
#          `Habitat value` = habitat_value_code,
#          `Image link` = image_view_url,
#          `Model link` = model_link,
#          Comments = my_text)

hab_site_prep <-  habitat_confirmations %>%
  purrr::pluck("step_4_stream_site_data") %>%
  # tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>%
  mutate(average_gradient_percent = round(average_gradient_percent * 100, 1)) %>%
  mutate_if(is.numeric, round, 1) %>%
  select(-gazetted_names:-site_number, -feature_type:-utm_method) %>%   ##remove the feature utms so they don't conflict with the site utms
  distinct(reference_number, .keep_all = T) ##since we have features we need to filter them out


hab_loc <- habitat_confirmations %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  dplyr::filter(!is.na(site_number))%>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))

hab_site <- left_join(
  hab_loc,
  hab_site_prep,
  by = 'reference_number'
) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location'), remove = F) %>%
  mutate(site = as.numeric(site)) %>%
  dplyr::filter(!alias_local_name %like% '_ef') ##get rid of the ef sites

hab_fish_collect_prep <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  distinct(local_name, species, .keep_all = T) %>% ##changed this to make it work as a feed for the extract-fish.R file
  mutate(across(c(date_in,date_out), janitor::excel_numeric_to_date)) %>%
  mutate(across(c(time_in,time_out), chron::times))


##prep the location info so it is ready to join to the fish data
hab_loc2 <- hab_loc %>%
  tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  filter(alias_local_name %like% 'ef') ##changed this from ef1

##join the tables together
hab_fish_collect_prep2 <- left_join(
  select(hab_loc2, reference_number, site_id, utm_zone:utm_northing),
  select(hab_fish_collect_prep %>% distinct(site_id, species, .keep_all = T), site_id, species),
  by = 'site_id'
)


##add the species code
hab_fish_codes <- habitat_confirmations %>%
  purrr::pluck("species_by_group") %>% ##changed from specie_by_common_name because BB burbot was wrong!!
  select(-step)


hab_fish_collect <- left_join(
  hab_fish_collect_prep2 %>% mutate(species = as.factor(species)),  ##just needed to do this b/c there actually are no fish.
  select(hab_fish_codes, common_name, species_code),
  by = c('species' = 'common_name')
)


hab_fish_collect_prep1 <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  select(-gazetted_name:-site_number)

hab_features <- left_join(
  habitat_confirmations %>%
    purrr::pluck("step_4_stream_site_data") %>%
    select(reference_number,local_name, feature_type:utm_northing) %>%
    filter(!is.na(feature_type)),

  xref_obstacle_names,

  by = c('feature_type' = 'spreadsheet_feature_type')
)

##add the priorities to the site data
hab_site_priorities <- left_join(
  select(habitat_confirmations_priorities, reference_number, local_name, priority),
  select(hab_site, reference_number, alias_local_name, site, utm_zone:utm_northing),
  by = 'reference_number'
) %>%
  filter(!local_name %like% '_ds') %>%
  select(-local_name)
# filter(!is.na(priority))  ##this is how we did it before.  changed it to get a start on it



# When we need to update our column names according to the new output from bcfishpass.crossings...
bcfishpass_names_updated_prep <- names(bcfishpass) %>%
  tibble::as_tibble() %>%
  rename(bcfishpass = value)

# join to the comments
bcfishpass_names_updated <- left_join(
  bcfishpass_names_updated_prep,
  bcfishpass_column_comments,
  by = c('bcfishpass' = 'column_name')
)

# -----------bcfishpass modelling table setup for reporting
#this is how we line up our new column names and put things in order for reporting on the fish habitat modeling
# we need to update this sometimes.  When we do we update 02_prep_reporting/0160-load-bcfishpass-data.R,
# get the data from  rename the xref_bcfishpass_names tribble to xref_bcfishpass_names_old  and go through the following procedure
# xref_bcfishpass_names_old <- xref_bcfishpass_names


# ## join the new with the old so you can kable(xref_bcfishpass_names_prep) then run in Rmd chunk and copy paste tribble yo
# xref_bcfishpass_names_prep <- left_join(
#   bcfishpass_names_updated,
#   select(xref_bcfishpass_names_old, -column_comment),
#   by = c('bcfishpass')
# ) %>%
#     mutate(report = stringr::str_replace_all(bcfishpass, '_', ' ') %>%
#              stringr::str_to_title() %>%
#              stringr::str_replace_all('Km', '(km)') %>%
#              stringr::str_replace_all('Ha', '(ha)') %>%
#              stringr::str_replace_all('Lakereservoir', 'Lake Reservoir') %>%
#              stringr::str_replace_all('Co ', 'CO ') %>%
#              stringr::str_replace_all('Ch', 'CH ') %>%
#              stringr::str_replace_all('St ', 'ST ') %>%
#              stringr::str_replace_all('Sk ', 'SK ') %>%
#              stringr::str_replace_all('Wct ', 'WCT ') %>%
#              stringr::str_replace_all('Pscis', 'PSCIS') %>%
#              stringr::str_replace_all('Spawningrearing', 'Spawning Rearing') %>%
#              stringr::str_replace_all('Betweenbarriers', 'Between Barriers') %>%
#              stringr::str_replace_all('Belowupstrbarriers', 'Below Barriers')) %>%
#   select(bcfishpass, report, id_join, id_side, column_comment)
    # select(bcfishpass, report, id_join, id_side)

xref_bcfishpass_names <- tibble::tribble(
                                                                         ~bcfishpass,                                                 ~report, ~id_join, ~id_side,                                                                                                                                                                                                                                          ~column_comment,
                                                           "aggregated_crossings_id",                               "Aggregated Crossings Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                "stream_crossing_id",                                    "Stream Crossing Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                            "dam_id",                                                "Dam Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                     "user_barrier_anthropogenic_id",                         "User Barrier Anthropogenic Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                              "modelled_crossing_id",                                  "Modelled Crossing Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "crossing_source",                                       "Crossing Source",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                             "crossing_feature_type",                                 "Crossing Feature Type",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                      "pscis_status",                                          "PSCIS Status",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                "crossing_type_code",                                    "Crossing Type Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                             "crossing_subtype_code",                                 "Crossing Subtype Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                     "modelled_crossing_type_source",                         "Modelled Crossing Type Source",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                    "barrier_status",                                        "Barrier Status",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "pscis_road_name",                                       "PSCIS Road Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                 "pscis_stream_name",                                     "PSCIS Stream Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                          "pscis_assessment_comment",                              "PSCIS Assessment Comment",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                             "pscis_assessment_date",                                 "PSCIS Assessment Date",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                 "pscis_final_score",                                     "PSCIS Final Score",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                  "transport_line_structured_name_1",                      "Transport Line Structured Name 1",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                   "transport_line_type_description",                       "Transport Line Type Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                "transport_line_surface_description",                    "Transport Line Surface Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                               "ften_forest_file_id",                                   "Ften Forest File Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                        "ften_file_type_description",                            "Ften File Type Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                "ften_client_number",                                    "Ften Client Number",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                  "ften_client_name",                                      "Ften Client Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                       "ften_life_cycle_status_code",                           "Ften Life Cycle Status Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "rail_track_name",                                       "Rail Track Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "rail_owner_name",                                       "Rail Owner Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                        "rail_operator_english_name",                            "Rail Operator English Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                     "ogc_proponent",                                         "Ogc Proponent",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                          "dam_name",                                              "Dam Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                         "dam_owner",                                             "Dam Owner",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                          "utm_zone",                                              "Utm Zone",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                       "utm_easting",                                           "Utm Easting",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                      "utm_northing",                                          "Utm Northing",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                  "dbm_mof_50k_grid",                                      "Dbm Mof 50k Grid",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                 "linear_feature_id",                                     "Linear Feature Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                     "blue_line_key",                                         "Blue Line Key",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                     "watershed_key",                                         "Watershed Key",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                          "downstream_route_measure",                              "Downstream Route Measure",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                      "wscode_ltree",                                          "Wscode Ltree",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "localcode_ltree",                                       "Localcode Ltree",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                              "watershed_group_code",                                  "Watershed Group Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                  "gnis_stream_name",                                      "Gnis Stream Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "crossings_dnstr",                                       "Crossings Dnstr",       NA,       NA,                                                                                                                                          "List of the aggregated_crossings_id values of crossings downstream of the given crossing, in order downstream",
                                                      "barriers_anthropogenic_dnstr",                          "Barriers Anthropogenic Dnstr",       NA,       NA,                                                                                                                                  "List of the aggregated_crossings_id values of barrier crossings downstream of the given crossing, in order downstream",
                                                      "barriers_anthropogenic_upstr",                          "Barriers Anthropogenic Upstr",       NA,       NA,                                                                                                                                                         "List of the aggregated_crossings_id values of barrier crossings upstream of the given crossing",
                                                "barriers_anthropogenic_dnstr_count",                    "Barriers Anthropogenic Dnstr Count",       NA,       NA,                                                                                                                                                                                      "A count of the barrier crossings downstream of the given crossing",
                                                "barriers_anthropogenic_upstr_count",                    "Barriers Anthropogenic Upstr Count",       NA,       NA,                                                                                                                                                                                        "A count of the barrier crossings upstream of the given crossing",
                                                                      "stream_order",                                          "Stream Order",       NA,       NA,                                                                                                                                                                                                                           "Order of FWA stream at point",
                                                                  "stream_magnitude",                                      "Stream Magnitude",       NA,       NA,                                                                                                                                                                                                                       "Magnitude of FWA stream at point",
                                                                          "gradient",                                              "Gradient",       NA,       NA,                                                                                                                                                                                                                                  "Stream slope at point",
                                                             "access_model_ch_co_sk",                                 "Access Model CH CO Sk",       NA,       NA,                                                                                                                                                                                                             "Modelled accessibility to Salmon (15% max)",
                                                                   "access_model_st",                                       "Access Model St",       NA,       NA,                                                                                                                                                                                                          "Modelled accessibility to Steelhead (20% max)",
                                                                  "access_model_wct",                                      "Access Model Wct",       NA,       NA,                                                                                                                                                  "Modelled accessibility to West Slope Cutthroat Trout (20% max or downstream of known WCT observation)",
                                                                 "observedspp_dnstr",                                     "Observedspp Dnstr",       NA,       NA,                                                                                                                                                                           "Fish species observed downstream of point (on the same stream/blue_line_key)",
                                                                 "observedspp_upstr",                                     "Observedspp Upstr",       NA,       NA,                                                                                                                                                                                                       "Fish species observed anywhere upstream of point",
                                                                "watershed_upstr_ha",                                  "Watershed Upstr (ha)",       NA,       NA,                                                                                                                       "Total watershed area upstream of point (approximate, does not include area of the fundamental watershed in which the point lies)",
                                                                  "total_network_km",                                    "Total Network (km)",       NA,       NA,                                                                                                                                                                                                       "Total length of stream network upstream of point",
                                                                   "total_stream_km",                                     "Total Stream (km)",       NA,       NA,                                                                                                                                                "Total length of streams and rivers upstream of point (does not include network connectors in lakes etc)",
                                                            "total_lakereservoir_ha",                             "Total Lake Reservoir (ha)",       NA,       NA,                                                                                                                                                                                                      "Total area lakes and reservoirs upstream of point",
                                                                  "total_wetland_ha",                                    "Total Wetland (ha)",       NA,       NA,                                                                                                                                                                                                                  "Total area wetlands upstream of point",
                                                 "total_slopeclass03_waterbodies_km",                   "Total Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                                                            "Total length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                             "total_slopeclass03_km",                               "Total Slopeclass03 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 0-3%",
                                                             "total_slopeclass05_km",                               "Total Slopeclass05 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 3-5%",
                                                             "total_slopeclass08_km",                               "Total Slopeclass08 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 5-8%",
                                                             "total_slopeclass15_km",                               "Total Slopeclass15 (km)",       NA,       NA,                                                                                                                                                                       "Total length of stream potentially accessible upstream of point with slope 8-15%",
                                                             "total_slopeclass22_km",                               "Total Slopeclass22 (km)",       NA,       NA,                                                                                                                                                                      "Total length of stream potentially accessible upstream of point with slope 15-22%",
                                                             "total_slopeclass30_km",                               "Total Slopeclass30 (km)",       NA,       NA,                                                                                                                                                                      "Total length of stream potentially accessible upstream of point with slope 22-30%",
                                               "total_belowupstrbarriers_network_km",                     "Total Below Barriers Network (km)",       NA,       NA,                                                                                                                                     "Total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                                "total_belowupstrbarriers_stream_km",                      "Total Below Barriers Stream (km)",       NA,       NA,                                                                              "Total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                         "total_belowupstrbarriers_lakereservoir_ha",              "Total Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                                                    "Total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                               "total_belowupstrbarriers_wetland_ha",                     "Total Below Barriers Wetland (ha)",       NA,       NA,                                                                                                                                                "Total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                              "total_belowupstrbarriers_slopeclass03_waterbodies_km",    "Total Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                "Total length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                          "total_belowupstrbarriers_slopeclass03_km",                "Total Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                          "total_belowupstrbarriers_slopeclass05_km",                "Total Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                          "total_belowupstrbarriers_slopeclass08_km",                "Total Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                          "total_belowupstrbarriers_slopeclass15_km",                "Total Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                                                           "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                          "total_belowupstrbarriers_slopeclass22_km",                "Total Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                                                          "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                          "total_belowupstrbarriers_slopeclass30_km",                "Total Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                                                          "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                               "ch_co_sk_network_km",                                 "CH CO SK Network (km)",       NA,       NA,                                                                                                                                             "Chinook/Coho/Sockeye salmon model, total length of stream network potentially accessible upstream of point",
                                                                "ch_co_sk_stream_km",                                  "CH CO SK Stream (km)",       NA,       NA,                                                                                      "Chinook/Coho/Sockeye salmon model, total length of streams and rivers potentially accessible upstream of point (does not include network connectors in lakes etc)",
                                                         "ch_co_sk_lakereservoir_ha",                          "CH CO SK Lake Reservoir (ha)",       NA,       NA,                                                                                                                                            "Chinook/Coho/Sockeye salmon model, total area lakes and reservoirs potentially accessible upstream of point",
                                                               "ch_co_sk_wetland_ha",                                 "CH CO SK Wetland (ha)",       NA,       NA,                                                                                                                                                        "Chinook/Coho/Sockeye salmon model, total area wetlands potentially accessible upstream of point",
                                              "ch_co_sk_slopeclass03_waterbodies_km",                "CH CO SK Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                          "ch_co_sk_slopeclass03_km",                            "CH CO SK Slopeclass03 (km)",       NA,       NA,                                                                                                                                           "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 0-3%",
                                                          "ch_co_sk_slopeclass05_km",                            "CH CO SK Slopeclass05 (km)",       NA,       NA,                                                                                                                                           "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 3-5%",
                                                          "ch_co_sk_slopeclass08_km",                            "CH CO SK Slopeclass08 (km)",       NA,       NA,                                                                                                                                           "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 5-8%",
                                                          "ch_co_sk_slopeclass15_km",                            "CH CO SK Slopeclass15 (km)",       NA,       NA,                                                                                                                                          "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 8-15%",
                                                          "ch_co_sk_slopeclass22_km",                            "CH CO SK Slopeclass22 (km)",       NA,       NA,                                                                                                                                         "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 15-22%",
                                                          "ch_co_sk_slopeclass30_km",                            "CH CO SK Slopeclass30 (km)",       NA,       NA,                                                                                                                                         "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 22-30%",
                                            "ch_co_sk_belowupstrbarriers_network_km",                  "CH CO SK Below Barriers Network (km)",       NA,       NA,                                                                                                  "Chinook/Coho/Sockeye salmon model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                             "ch_co_sk_belowupstrbarriers_stream_km",                   "CH CO SK Below Barriers Stream (km)",       NA,       NA,                                           "Chinook/Coho/Sockeye salmon model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                      "ch_co_sk_belowupstrbarriers_lakereservoir_ha",           "CH CO SK Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                 "Chinook/Coho/Sockeye salmon model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                            "ch_co_sk_belowupstrbarriers_wetland_ha",                  "CH CO SK Below Barriers Wetland (ha)",       NA,       NA,                                                                                                             "Chinook/Coho/Sockeye salmon model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                           "ch_co_sk_belowupstrbarriers_slopeclass03_waterbodies_km", "CH CO SK Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                   "Chinook/Coho/Sockeye salmon model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass03_km",             "CH CO SK Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass05_km",             "CH CO SK Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass08_km",             "CH CO SK Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass15_km",             "CH CO SK Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                              "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass22_km",             "CH CO SK Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                             "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass30_km",             "CH CO SK Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                             "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                                     "st_network_km",                                       "ST Network (km)",       NA,       NA,                                                                                                                                                               "Steelhead model, total length of stream network potentially accessible upstream of point",
                                                                      "st_stream_km",                                        "ST Stream (km)",       NA,       NA,                                                                                                        "Steelhead model, total length of streams and rivers potentially accessible upstream of point (does not include network connectors in lakes etc)",
                                                               "st_lakereservoir_ha",                                "ST Lake Reservoir (ha)",       NA,       NA,                                                                                                                                                              "Steelhead model, total area lakes and reservoirs potentially accessible upstream of point",
                                                                     "st_wetland_ha",                                       "ST Wetland (ha)",       NA,       NA,                                                                                                                                                                          "Steelhead model, total area wetlands potentially accessible upstream of point",
                                                    "st_slopeclass03_waterbodies_km",                      "ST Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                                                 "Steelhead model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                                "st_slopeclass03_km",                                  "ST Slopeclass03 (km)",       NA,       NA,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 0-3%",
                                                                "st_slopeclass05_km",                                  "ST Slopeclass05 (km)",       NA,       NA,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 3-5%",
                                                                "st_slopeclass08_km",                                  "ST Slopeclass08 (km)",       NA,       NA,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 5-8%",
                                                                "st_slopeclass15_km",                                  "ST Slopeclass15 (km)",       NA,       NA,                                                                                                                                                            "Steelhead model, length of stream potentially accessible upstream of point with slope 8-15%",
                                                                "st_slopeclass22_km",                                  "ST Slopeclass22 (km)",       NA,       NA,                                                                                                                                                           "Steelhead model, length of stream potentially accessible upstream of point with slope 15-22%",
                                                                "st_slopeclass30_km",                                  "ST Slopeclass30 (km)",       NA,       NA,                                                                                                                                                           "Steelhead model, length of stream potentially accessible upstream of point with slope 22-30%",
                                                  "st_belowupstrbarriers_network_km",                        "ST Below Barriers Network (km)",       NA,       NA,                                                                                                                    "Steelhead model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                                   "st_belowupstrbarriers_stream_km",                         "ST Below Barriers Stream (km)",       NA,       NA,                                                             "Steelhead model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                            "st_belowupstrbarriers_lakereservoir_ha",                 "ST Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                                   "Steelhead model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                                  "st_belowupstrbarriers_wetland_ha",                        "ST Below Barriers Wetland (ha)",       NA,       NA,                                                                                                                               "Steelhead model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                                 "st_belowupstrbarriers_slopeclass03_waterbodies_km",       "ST Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                     "Steelhead model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                             "st_belowupstrbarriers_slopeclass03_km",                   "ST Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                             "st_belowupstrbarriers_slopeclass05_km",                   "ST Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                             "st_belowupstrbarriers_slopeclass08_km",                   "ST Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                             "st_belowupstrbarriers_slopeclass15_km",                   "ST Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                                                "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                             "st_belowupstrbarriers_slopeclass22_km",                   "ST Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                                               "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                             "st_belowupstrbarriers_slopeclass30_km",                   "ST Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                                               "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                                    "wct_network_km",                                      "WCT Network (km)",     117L,       1L,                                                                                                                                                "Westslope Cuthroat Trout model, total length of stream network potentially accessible upstream of point",
                                                                     "wct_stream_km",                                       "WCT Stream (km)",     115L,       1L,                                                                                         "Westslope Cuthroat Trout model, total length of streams and rivers potentially accessible upstream of point (does not include network connectors in lakes etc)",
                                                              "wct_lakereservoir_ha",                               "WCT Lake Reservoir (ha)",     120L,       1L,                                                                                                                                               "Westslope Cuthroat Trout model, total area lakes and reservoirs potentially accessible upstream of point",
                                                                    "wct_wetland_ha",                                      "WCT Wetland (ha)",     125L,       1L,                                                                                                                                                           "Westslope Cuthroat Trout model, total area wetlands potentially accessible upstream of point",
                                                   "wct_slopeclass03_waterbodies_km",                     "WCT Slopeclass03 Waterbodies (km)",     130L,       1L,                                                                                                                 "Westslope Cutthroat Trout model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                               "wct_slopeclass03_km",                                 "WCT Slopeclass03 (km)",     140L,       1L,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 0-3%",
                                                               "wct_slopeclass05_km",                                 "WCT Slopeclass05 (km)",     150L,       1L,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 3-5%",
                                                               "wct_slopeclass08_km",                                 "WCT Slopeclass08 (km)",     160L,       1L,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 5-8%",
                                                               "wct_slopeclass15_km",                                 "WCT Slopeclass15 (km)",     170L,       1L,                                                                                                                                            "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 8-15%",
                                                               "wct_slopeclass22_km",                                 "WCT Slopeclass22 (km)",     180L,       1L,                                                                                                                                           "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 15-22%",
                                                               "wct_slopeclass30_km",                                 "WCT Slopeclass30 (km)",       NA,       NA,                                                                                                                                           "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 22-30%",
                                                 "wct_belowupstrbarriers_network_km",                       "WCT Below Barriers Network (km)",     117L,       2L,                                                                                                    "Westslope Cutthroat Trout model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                                  "wct_belowupstrbarriers_stream_km",                        "WCT Below Barriers Stream (km)",     115L,       2L,                                              "Westslope Cuthroat Trout model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                           "wct_belowupstrbarriers_lakereservoir_ha",                "WCT Below Barriers Lake Reservoir (ha)",     120L,       2L,                                                                                                   "Westslope Cutthroat Trout model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                                 "wct_belowupstrbarriers_wetland_ha",                       "WCT Below Barriers Wetland (ha)",     125L,       2L,                                                                                                               "Westslope Cutthroat Trout model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                                "wct_belowupstrbarriers_slopeclass03_waterbodies_km",      "WCT Below Barriers Slopeclass03 Waterbodies (km)",     130L,       2L,                                                                     "Westslope Cutthroat Trout model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                            "wct_belowupstrbarriers_slopeclass03_km",                  "WCT Below Barriers Slopeclass03 (km)",     140L,       2L,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                            "wct_belowupstrbarriers_slopeclass05_km",                  "WCT Below Barriers Slopeclass05 (km)",     150L,       2L,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                            "wct_belowupstrbarriers_slopeclass08_km",                  "WCT Below Barriers Slopeclass08 (km)",     160L,       2L,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                            "wct_belowupstrbarriers_slopeclass15_km",                  "WCT Below Barriers Slopeclass15 (km)",     170L,       2L,                                                                                                "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                            "wct_belowupstrbarriers_slopeclass22_km",                  "WCT Below Barriers Slopeclass22 (km)",     180L,       2L,                                                                                               "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                            "wct_belowupstrbarriers_slopeclass30_km",                  "WCT Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                               "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                                    "ch_spawning_km",                                      "CH Spawning (km)",       NA,       NA,                                                                                                                                                                      "Length of stream upstream of point modelled as potential Chinook spawning habitat",
                                                                     "ch_rearing_km",                                       "CH Rearing (km)",       NA,       NA,                                                                                                                                                                       "Length of stream upstream of point modelled as potential Chinook rearing habitat",
                                                 "ch_spawning_belowupstrbarriers_km",                       "CH Spawning Below Barriers (km)",       NA,       NA,                                                                                                                          "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Chinook spawning habitat",
                                                  "ch_rearing_belowupstrbarriers_km",                        "CH Rearing Below Barriers (km)",       NA,       NA,                                                                                                                           "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Chinook rearing habitat",
                                                                    "co_spawning_km",                                      "CO Spawning (km)",       NA,       NA,                                                                                                                                                                         "Length of stream upstream of point modelled as potential Coho spawning habitat",
                                                                     "co_rearing_km",                                       "CO Rearing (km)",       NA,       NA,                                                                                                                                                                          "Length of stream upstream of point modelled as potential Coho rearing habitat",
                                                                     "co_rearing_ha",                                       "CO Rearing (ha)",       NA,       NA,                                                                                                                                                                          "Area of wetlands upstream of point modelled as potential Coho rearing habitat",
                                                 "co_spawning_belowupstrbarriers_km",                       "CO Spawning Below Barriers (km)",       NA,       NA,                                                                                                                             "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Coho spawning habitat",
                                                  "co_rearing_belowupstrbarriers_km",                        "CO Rearing Below Barriers (km)",       NA,       NA,                                                                                                                              "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Coho rearing habitat",
                                                  "co_rearing_belowupstrbarriers_ha",                        "CO Rearing Below Barriers (ha)",       NA,       NA,                                                                                                                              "Area of wetlands upstream of point and below any additional upstream barriers, modelled as potential Coho rearing habitat",
                                                                    "sk_spawning_km",                                      "SK Spawning (km)",       NA,       NA,                                                                                                                                                                      "Length of stream upstream of point modelled as potential Sockeye spawning habitat",
                                                                     "sk_rearing_km",                                       "SK Rearing (km)",       NA,       NA,                                                                                                                                                                       "Length of stream upstream of point modelled as potential Sockeye rearing habitat",
                                                                     "sk_rearing_ha",                                       "SK Rearing (ha)",       NA,       NA,                                                                                                                                                                          "Area of lakes upstream of point modelled as potential Sockeye rearing habitat",
                                                 "sk_spawning_belowupstrbarriers_km",                       "SK Spawning Below Barriers (km)",       NA,       NA,                                                                                                                          "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Sockeye spawning habitat",
                                                  "sk_rearing_belowupstrbarriers_km",                        "SK Rearing Below Barriers (km)",       NA,       NA,                                                                                                                           "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Sockeye rearing habitat",
                                                  "sk_rearing_belowupstrbarriers_ha",                        "SK Rearing Below Barriers (ha)",       NA,       NA,                                                                                                                              "Area of lakes upstream of point and below any additional upstream barriers, modelled as potential Sockeye rearing habitat",
                                                                    "st_spawning_km",                                      "ST Spawning (km)",       NA,       NA,                                                                                                                                                                    "Length of stream upstream of point modelled as potential Steelhead spawning habitat",
                                                                     "st_rearing_km",                                       "ST Rearing (km)",       NA,       NA,                                                                                                                                                                     "Length of stream upstream of point modelled as potential Steelhead rearing habitat",
                                                 "st_spawning_belowupstrbarriers_km",                       "ST Spawning Below Barriers (km)",       NA,       NA,                                                                                                                        "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Steelhead spawning habitat",
                                                  "st_rearing_belowupstrbarriers_km",                        "ST Rearing Below Barriers (km)",       NA,       NA,                                                                                                                         "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Steelhead rearing habitat",
                                                                   "wct_spawning_km",                                     "WCT Spawning (km)",     090L,       1L,                                                                                                                                                          "Length of stream upstream of point modelled as potential Westslope Cutthroat spawning habitat",
                                                                    "wct_rearing_km",                                      "WCT Rearing (km)",     100L,       1L,                                                                                                                                                           "Length of stream upstream of point modelled as potential Westslope Cutthroat rearing habitat",
                                                "wct_spawning_belowupstrbarriers_km",                      "WCT Spawning Below Barriers (km)",     090L,       2L,                                                                                                              "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Westslope Cutthroat spawning habitat",
                                                 "wct_rearing_belowupstrbarriers_km",                       "WCT Rearing Below Barriers (km)",     100L,       2L,                                                                                                               "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Westslope Cutthroat rearing habitat",
                                                                   "all_spawning_km",                                     "All Spawning (km)",       NA,       NA,                                                                                                                                                        "Length of stream upstream of point modelled as potential spawning habitat (all CH,CO,SK,ST,WCT)",
                                                "all_spawning_belowupstrbarriers_km",                      "All Spawning Below Barriers (km)",       NA,       NA,                                                                                                                                                         "Length of stream upstream of point modelled as potential rearing habitat (all CH,CO,SK,ST,WCT)",
                                                                    "all_rearing_km",                                      "All Rearing (km)",       NA,       NA,                                                                                                            "Length of stream upstream of point and below any additional upstream barriers, modelled as potential spawning habitat (all CH,CO,SK,ST,WCT)",
                                                 "all_rearing_belowupstrbarriers_km",                       "All Rearing Below Barriers (km)",       NA,       NA,                                                                                                             "Length of stream upstream of point and below any additional upstream barriers, modelled as potential rearing habitat (all CH,CO,SK,ST,WCT)",
                                                            "all_spawningrearing_km",                             "All Spawning Rearing (km)",       NA,       NA,                                                                                                                                                                                           "Length of all spawning and rearing habitat upstream of point",
                                         "all_spawningrearing_belowupstrbarriers_km",              "All Spawning Rearing Below Barriers (km)",       NA,       NA,                                                                                                                                                                                           "Length of all spawning and rearing habitat upstream of point",
                                                    "wct_betweenbarriers_network_km",                     "WCT Between Barriers Network (km)",       NA,       NA,                                                                                                            "Westslope Cutthroat Trout model, total length of potentially accessible stream network between crossing and all in-stream adjacent barriers",
                                                   "wct_spawning_betweenbarriers_km",                    "WCT Spawning Between Barriers (km)",       NA,       NA,                                                                                                                                 "Westslope Cutthroat Trout model, total length of spawning habitat between crossing and all in-stream adjacent barriers",
                                                    "wct_rearing_betweenbarriers_km",                     "WCT Rearing Between Barriers (km)",       NA,       NA,                                                                                                                                  "Westslope Cutthroat Trout model, total length of rearing habitat between crossing and all in-stream adjacent barriers",
                                            "wct_spawningrearing_betweenbarriers_km",            "WCT Spawning Rearing Between Barriers (km)",       NA,       NA,                                                                                                                     "Westslope Cutthroat Trout model, total length of spawning and rearing habitat between crossing and all in-stream adjacent barriers",
                                                   "all_spawningrearing_per_barrier",                      "All Spawning Rearing Per Barrier",       NA,       NA, "If the given barrier and all barriers downstream were remediated, the amount of connected spawning/rearing habitat that would be added, per barrier. (ie the sum of all_spawningrearing_belowupstrbarriers_km for all barriers, divided by n barriers)"
                           )

####-----------overview table------------

tab_overview_prep1 <- pscis_phase2 %>%
  select(pscis_crossing_id, stream_name, road_name, road_tenure, easting, northing, habitat_value)

tab_overview_prep2 <- habitat_confirmations_priorities %>%
  filter(location == 'us') %>%
  select(site, species_codes, upstream_habitat_length_m, priority, comments) %>%
  mutate(upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1))

tab_overview <- left_join(
  tab_overview_prep1,
  tab_overview_prep2,
  by = c('pscis_crossing_id' = 'site')
) %>%
  mutate(utm = paste0(round(easting,0), ' ', round(northing,0))) %>%
  select(`PSCIS ID` = pscis_crossing_id,
         Stream = stream_name,
         Road = road_name,
         Tenure = road_tenure,
         `UTM (11U)` = utm,
         `Fish Species` = species_codes,
         `Habitat Gain (km)` = upstream_habitat_length_km,
         `Habitat Value` = habitat_value,
         Priority = priority,
         Comments = comments )
# mutate(test = paste0('[', Site, ']', '(Appendix 1 - Site Assessment Data and Photos)'))##hmm.. thought this worked
# %>%
#   replace(., is.na(.), "-")


rm(tab_overview_prep1, tab_overview_prep2)

####---------habitat summary--------------------------------

tab_hab_summary <- left_join(
  hab_site %>%
    select(site, location, avg_channel_width_m, avg_wetted_width_m,
           average_residual_pool_depth_m, average_gradient_percent, total_cover),

  habitat_confirmations_priorities %>%
    select(site, location, survey_length_m, hab_value),

  by = c('site', 'location')
) %>%
  mutate(location = case_when(
    location == 'us' ~ 'Upstream',
    T ~ 'Downstream'
  )) %>%
  arrange(site) %>%
  select(Site = site,
         Location = location,
         `Length Surveyed (m)` = survey_length_m,
         `Channel Width (m)` = avg_channel_width_m,
         `Wetted Width (m)` = avg_wetted_width_m,
         `Pool Depth (m)` = average_residual_pool_depth_m,
         `Gradient (%)` = average_gradient_percent,
         `Total Cover` = total_cover,
         `Habitat Value` = hab_value)
  # replace(., is.na(.), "--") #this shouldn't be necessary


####--------------------cost estimates phase1
##make the cost estimates
tab_cost_est_prep <- left_join(
  pscis_rd %>%  arrange(aggregated_crossings_id),
  select(tab_cost_rd_mult, my_road_class, my_road_surface, cost_m_1000s_bridge, cost_embed_cv),
  by = c('my_road_class','my_road_surface')
)


tab_cost_est_prep2 <- left_join(
  tab_cost_est_prep,
  select(xref_structure_fix, crossing_fix, crossing_fix_code),
  by = c('crossing_fix')
) %>%
  mutate(cost_est_1000s = case_when(
    crossing_fix_code == 'SS-CBS' ~ cost_embed_cv,
    crossing_fix_code == 'OBS' ~ cost_m_1000s_bridge * recommended_diameter_or_span_meters)
  ) %>%
  mutate(cost_est_1000s = round(cost_est_1000s, 0))



##add in the model data.  This is a good reason for the data to be input first so that we can use the net distance!!
tab_cost_est_prep3 <- left_join(
  tab_cost_est_prep2,
  bcfishpass %>%
    select(aggregated_crossings_id,
           wct_network_km,
           wct_belowupstrbarriers_network_km),
  by = c('aggregated_crossings_id') #not sure we need to add my_crossing_reference
) %>%
  mutate(cost_net = round(wct_belowupstrbarriers_network_km * 1000/cost_est_1000s, 1),
         cost_gross = round(wct_network_km * 1000/cost_est_1000s, 1),
         cost_area_net = round((wct_belowupstrbarriers_network_km * 1000 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1), ##this is a triangle area!
         cost_area_gross = round((wct_network_km * 1000 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1)) ##this is a triangle area!


# # ##add the xref stream_crossing_id
# tab_cost_est_prep4 <- left_join(
#   tab_cost_est_prep3,
#   xref_pscis_my_crossing_modelled,
#   by = c('my_crossing_reference' = 'external_crossing_reference')
# ) %>%
#   mutate(stream_crossing_id = case_when(
#     is.na(stream_crossing_id) ~ pscis_crossing_id,
#     T ~ stream_crossing_id
#   ))

##add the priority info
tab_cost_est_phase1 <- left_join(
  phase1_priorities %>% select(aggregated_crossings_id,
         priority_phase1),
  tab_cost_est_prep3,
  by = 'aggregated_crossings_id'
) %>%
  arrange(aggregated_crossings_id) %>%
  select(pscis_crossing_id, my_crossing_reference, my_crossing_reference, stream_name, road_name,
         barrier_result, habitat_value, downstream_channel_width_meters,
         priority_phase1,
         crossing_fix_code, cost_est_1000s, wct_network_km,
         cost_gross, cost_area_gross, source) %>%
  filter(barrier_result != 'Unknown' & barrier_result != 'Passable')

# too_far_away <- tab_cost_est %>% filter(distance > 100) %>% ##after review all crossing match!!!!! Baren rail is the hwy but that is fine. added source, distance, crossing_id above
#   filter(source %like% 'phase2')

tab_cost_est_phase1_prep <- tab_cost_est_phase1 %>%
  rename(
    `PSCIS ID` = pscis_crossing_id,
    `External ID` = my_crossing_reference,
    Priority = priority_phase1,
    Stream = stream_name,
    Road = road_name,
    Result = barrier_result,
    `Habitat value` = habitat_value,
    `Stream Width (m)` = downstream_channel_width_meters,
    Fix = crossing_fix_code,
    `Cost Est ( $K)` =  cost_est_1000s,
    `Habitat Upstream (km)` = wct_network_km,
    `Cost Benefit (m / $K)` = cost_gross,
    `Cost Benefit (m2 / $K)` = cost_area_gross) %>%
  filter(!is.na(Priority))


tab_cost_est_phase1 <- tab_cost_est_phase1_prep %>%
  filter(!source %like% 'phase2') %>%
  select(-source)

# -----------------------------------------phase 2 cost estimate
##phase 2 specific
tab_cost_est_prep4 <- left_join(
  tab_cost_est_prep2,
  select(
    filter(habitat_confirmations_priorities, location == 'us'),
    site, upstream_habitat_length_m),
  by = c('pscis_crossing_id' = 'site')
)

# tab_cost_est_prep4 %>% filter(stream_name %ilike% 'parker')


tab_cost_est_prep5 <- left_join(
  tab_cost_est_prep4,
  select(hab_site %>% filter(!alias_local_name %like% 'ds' & !alias_local_name %like% 'ef'), site, avg_channel_width_m),
  by = c('pscis_crossing_id' = 'site')
) %>%
  ##intervention for Parker to reflect that there are 2 bridges to build.
  mutate(cost_est_1000s = case_when(pscis_crossing_id == 50067 ~ 500, T ~ cost_est_1000s)) %>%
  mutate(cost_net = round(upstream_habitat_length_m/cost_est_1000s, 1),
         cost_area_net = round((upstream_habitat_length_m * avg_channel_width_m)/cost_est_1000s, 1)) #downstream_channel_width_meters


##add the priority info
tab_cost_est_phase2 <- tab_cost_est_prep5 %>%
  select(pscis_crossing_id, stream_name, road_name, barrier_result, habitat_value, avg_channel_width_m,
         crossing_fix_code, cost_est_1000s, upstream_habitat_length_m,
         cost_net, cost_area_net, source) %>%
  mutate(upstream_habitat_length_m = round(upstream_habitat_length_m,0)) %>%
  ##we have a hickup in the dry creek estimate.  Lets just fix it here
  mutate(cost_est_1000s = case_when(pscis_crossing_id == 62182 ~ 7200,
                                    T ~ cost_est_1000s))

tab_cost_est_phase2_report <- tab_cost_est_phase2 %>%
  filter(source %like% 'phase2') %>%
  rename(`PSCIS ID` = pscis_crossing_id,
         Stream = stream_name,
         Road = road_name,
         Result = barrier_result,
         `Habitat value` = habitat_value,
         `Stream Width (m)` = avg_channel_width_m,
         Fix = crossing_fix_code,
         `Cost Est (in $K)` =  cost_est_1000s,
         `Habitat Upstream (m)` = upstream_habitat_length_m,
         `Cost Benefit (m / $K)` = cost_net,
         `Cost Benefit (m2 / $K)` = cost_area_net) %>%
  select(-source)


rm(tab_cost_est_prep, tab_cost_est_prep2,
   tab_cost_est_prep3, tab_cost_est_prep4, tab_cost_est_prep5)


# -------------------------map tables

##we need an sf object with details for the interactive map
##prep the location data
hab_loc_prep <- left_join(
  hab_loc %>%
    tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
    filter(!alias_local_name %ilike% 'ef' &
             alias_local_name %ilike% 'us') %>%
    mutate(site = as.integer(site)),
  select(filter(habitat_confirmations_priorities, location == 'us'),
         site, priority, comments),
  by = 'site'
)


##need to populate the coordinates before this will work
####please note that the photos are only in those files ecause they are referenced in other parts
##of the document
tab_hab_map <- left_join(
  tab_cost_est_phase2 %>% filter(source %like% 'phase2'),
  hab_loc_prep %>% select(site, priority, utm_easting, utm_northing, comments),
  by = c('pscis_crossing_id' = 'site')
)   %>%
  sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
               crs = 26911, remove = F) %>%
  sf::st_transform(crs = 4326) %>%
  ##changed this to docs .html from fig .png
  # mutate(data_link = paste0('<a href =',
  #                           'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/docs/sum/', pscis_crossing_id,
  #                           '.html', '>', 'data link', '</a>')) %>%
  mutate(data_link = paste0('<a href =', 'sum/cv/', pscis_crossing_id, '.html ', 'target="_blank">Culvert Data</a>')) %>%
  mutate(photo_link = paste0('<a href =', 'https://raw.githubusercontent.com/NewGraphEnvironment/fish_passage_elk_2021_reporting/master/data/photos/', pscis_crossing_id, '/crossing_all.JPG ',
                             'target="_blank">Culvert Photos</a>')) %>%
  mutate(model_link = paste0('<a href =', 'sum/bcfp/', pscis_crossing_id, '.html ', 'target="_blank">Model Data</a>'))
# mutate(photo_link = paste0('<a href =', 'data/photos/', pscis_crossing_id,
#                           '/crossing_all.JPG', '>', 'Photos', '>New Tab</a>'))
# mutate(data_link = paste0('[data](fig/sum/', pscis_crossing_id, '.png)')) %>%
# mutate(photo_link = paste0('<a href =',
#                            'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/data/photos/', pscis_crossing_id,
#                            '/crossing_all.JPG', '>', 'photo link', '</a>'))


#--------------need to review if this is necessary
tab_map_prep <- left_join(
  pscis_all %>%
    sf::st_as_sf(coords = c("easting", "northing"),
                 crs = 26911, remove = F) %>% ##don't forget to put it in the right crs buds
    sf::st_transform(crs = 4326), ##convert to match the bcfishpass format,
  phase1_priorities %>% select(-utm_zone:utm_northing, -my_crossing_reference, priority_phase1, -habitat_value, -barrier_result), # %>% st_drop_geometry()
  by = 'pscis_crossing_id'
)

# mutate(data_link = paste0('<a href =', 'sum/', pscis_crossing_id,
#                           '.html', '>', 'Data', '>New Tab</a>'))

tab_map <- tab_map_prep %>%
  # mutate(pscis_crossing_id = as.character(pscis_crossing_id),
  #        my_crossing_reference = as.character(my_crossing_reference)) %>%
  # mutate(ID = case_when(
  #   !is.na(pscis_crossing_id) ~ pscis_crossing_id,
  #   T ~ paste0('*', my_crossing_reference
  #   ))) %>%
  # sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
  #              crs = 26911, remove = F) %>%
  # sf::st_transform(crs = 4326) %>%
  mutate(priority_phase1 = case_when(priority_phase1 == 'mod' ~ 'moderate',
                                     T ~ priority_phase1)) %>%
  mutate(data_link = paste0('<a href =', 'sum/cv/', pscis_crossing_id, '.html ', 'target="_blank">Culvert Data</a>')) %>%
  mutate(photo_link = paste0('<a href =', 'data/photos/', amalgamated_crossing_id, '/crossing_all.JPG ',
                             'target="_blank">Culvert Photos</a>')) %>%
  mutate(model_link = paste0('<a href =', 'sum/bcfp/', pscis_crossing_id, '.html ', 'target="_blank">Model Data</a>'))
# mutate(data_link = paste0('<a href =',
#                           'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/fig/sum/', pscis_crossing_id,
#                           '.png', '>', 'data link', '</a>')) %>%
# dplyr::mutate(photo_link = paste0('<a href =',
#                                   'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/data/photos/', amalgamated_crossing_id,
#                                   '/crossing_all.JPG', '>', 'photo link', '</a>'))





##clean up the objects
rm(hab_site_prep,
   # hab_fish_indiv_prep,
   # hab_fish_indiv_prep2,
   hab_fish_collect_prep2,
   hab_loc2)




