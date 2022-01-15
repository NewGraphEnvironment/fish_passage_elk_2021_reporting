##figure out how big of a structure to use.
##cost estimates
source('R/packages.R')
source('R/functions.R')


# pscis <- import_pscis() %>%
#   tibble::rownames_to_column() %>%
#   mutate(rowname = as.numeric(rowname)) %>%
#   mutate(column_num = rowname + 4)

pscis <- import_pscis(workbook_name = 'pscis_reassessments.xlsm') %>% ##was pscis_phase1.xlsm then pscis_phase2.xlsm first
  tibble::rownames_to_column() %>%
  mutate(rowname = as.numeric(rowname)) %>%
  mutate(column_num = rowname + 4)


##assign a value that we want to call standard fill
fill_dpth <- 3

##assign a multiplier to determine the length of a bridge above the standard
##that you get when you go deeper


##standard bridge width - we go with 10m
brdg_wdth <- 10

##% bigger than the channel that the bridge should be
chn_wdth_max <- 6 ##changed this from 5



##fill depth multiplier
##for every 1 m deeper than 3m, we need a 1.5:1 slope so there is 3m more bridge required
fill_dpth_mult <- 3

####----------backwater candidates------------------
##backwatering required od<30 and slope <2, swr <1.2 see if there are options
tab_backwater <- pscis %>%  ##changed this to pscis2!
  filter(barrier_result != 'Passable' &
           barrier_result != 'Unknown' &
           outlet_drop_meters < 0.3 &
           stream_width_ratio_score < 1.2 &
           culvert_slope_percent <= 2 )

##need to tweak the span so that we add 4 to the width if it is over 6m...
str_type <- pscis %>%
  select(column_num, pscis_crossing_id, my_crossing_reference, barrier_result, downstream_channel_width_meters, fill_depth_meters) %>%
  mutate(fill_dpth_over = fill_depth_meters - fill_dpth_mult) %>%
  mutate(crossing_fix = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                & downstream_channel_width_meters >= 2 ~ 'Replace with New Open Bottom Structure',
                                barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_character_,
                                T ~ 'Replace Structure with Streambed Simulation CBS'))  %>%
  mutate(span_input = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                & downstream_channel_width_meters >= 2 ~ brdg_wdth,
                                barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_real_,
                                T ~ 3))  %>%
  mutate(span_input = case_when(fill_dpth_over > 0 ~
                                  (brdg_wdth + fill_dpth_mult * fill_dpth_over),  ##1m more fill = 3 m more bridge
                                T ~ span_input)) %>%
  mutate(span_input = case_when(span_input < (downstream_channel_width_meters + 4) & ##span not need be extended if already 4m bigger than channel width
                                  downstream_channel_width_meters > chn_wdth_max ~
                                  (downstream_channel_width_meters - chn_wdth_max) + span_input,  ##for every m bigger than a 5 m channel add that much to each side in terms of span
                                T ~ span_input)) %>%
  mutate(span_input = round(span_input, 1)) %>%
  arrange(column_num)

##burn to a csv so you can copy and paste into spreadsheet

# str_type %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis1_str_type.csv'))
# str_type %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis2_str_type.csv'))
str_type %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis_reassessments_str_type.csv'))
