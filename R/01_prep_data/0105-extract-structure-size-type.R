##figure out how big of a structure to use.
##cost estimates
source('R/packages.R')
source('R/functions.R')


pscis_list <- fpr_import_pscis_all()

pscis_all <- bind_rows(pscis_list)


##assign a value that we want to call standard fill
fill_dpth <- 3

##assign a multiplier to determine the length of a bridge above the standard
##that you get when you go deeper
##standard bridge width - we go with 12 b/c not seeing 10s in the field
brdg_wdth <- 12

##% bigger than the channel that the bridge should be
chn_wdth_max <- 6 ##changed this from 5

##fill depth multiplier
##for every 1 m deeper than 3m, we need a 1.5:1 slope so there is 3m more bridge required
fill_dpth_mult <- 3

####----------backwater candidates------------------
##backwatering required od<30 and slope <2, swr <1.2 see if there are options
tab_backwater <- pscis_all %>%  ##changed this to pscis2!
  filter(barrier_result != 'Passable' &
           barrier_result != 'Unknown' &
           outlet_drop_meters < 0.3 &
           stream_width_ratio_score < 1.2 &
           culvert_slope_percent <= 2 )

##we actually have 1 but it is not going to be worth it because it is very unlikely to be fish habitat

##need to tweak the span so that we add 4 to the width if it is over 6m...


##this will require some ore work for cost estimates too.
str_type <- pscis_all %>%
  select(rowid, aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, source, barrier_result, downstream_channel_width_meters, fill_depth_meters) %>%
  mutate(fill_dpth_over = fill_depth_meters - fill_dpth_mult) %>%
  mutate(crossing_fix = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                & downstream_channel_width_meters >= 2 ~ 'Replace with New Open Bottom Structure',
                                barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_character_,
                                T ~ 'Replace Structure with Streambed Simulation CBS'))  %>%
  mutate(span_input = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                & downstream_channel_width_meters >= 2 ~ brdg_wdth,
                                barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_real_,
                                T ~ 3))  %>%
  mutate(span_input = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential') & fill_dpth_over > 0 & !crossing_fix %ilike% 'Simulation' ~
                                  (brdg_wdth + fill_dpth_mult * fill_dpth_over),  ##1m more fill = 3 m more bridge
                                T ~ span_input)) %>%
  mutate(span_input = case_when(span_input < (downstream_channel_width_meters + 4) & ##span not need be extended if already 4m bigger than channel width
                                  downstream_channel_width_meters > chn_wdth_max ~
                                  (downstream_channel_width_meters - chn_wdth_max) + span_input,  ##for every m bigger than a 5 m channel add that much to each side in terms of span
                                T ~ span_input)) %>%
  ##let's add an option that if the stream is under 3.5m wide and under more than 5m of fill we do a streambed simulation with a 4.5m embedded multiplate like 4607464 on Flathead fsr
  mutate(crossing_fix = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                  & downstream_channel_width_meters > 2 &
                                    downstream_channel_width_meters <= 3.5 &
                                    fill_depth_meters > 5 ~ 'Replace Structure with Streambed Simulation CBS',
                                  T ~ crossing_fix),
         span_input = case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                  & downstream_channel_width_meters > 2 &
                                  downstream_channel_width_meters <= 3.5 &
                                  fill_depth_meters > 5 ~ 4.5,
                                  T ~ span_input)) %>%
  mutate(span_input = round(span_input, 1))

##burn to a csvs so we can copy and paste into spreadsheet (could make a function to do this all at once....)
str_type %>%
  filter(source %ilike% 'phase1') %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/pscis1_str_type.csv'))
str_type %>%
  filter(source %ilike% 'phase2') %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/pscis2_str_type.csv'))
str_type %>%
  filter(source %ilike% 'reasses') %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/pscis_reassessments_str_type.csv'))

