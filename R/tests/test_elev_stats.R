# test to try to get elevation stats per watershed
# https://github.com/jhollist/elevatr/blob/main/contributions/mulhearn_rayshader_elevatr_cheatsheet.pdf

wshd <- wshds %>% filter(stream_crossing_id == my_site)
elev <- elevatr::get_elev_raster(wshd, 14) #wont go higher than 14
nh_elmat <- rayshader::raster_to_matrix(elev)


nh_elmat %>% quantile(.,.4)
nh_elmat %>% mean()

test <- wshds %>% slice(1:2)

sites <- test %>% pull(stream_crossing_id)

fpr_elev_stats_ind <- function(site_ids){
  wshd <- wshds %>%
    filter(stream_crossing_id == site_ids)

  nh_elmat <- wshd %>%
    elevatr::get_elev_raster(., 14) %>%
    rayshader::raster_to_matrix()

  nh_elmat[nh_elmat < 100] = NA #ditch values <100m bc must be errors -something is wrong at the mine

  wshd %>%
    mutate(elev_mean = mean(nh_elmat, na.rm = T),
           elev_max = max(nh_elmat, na.rm = T),
           elev_min = min(nh_elmat, na.rm = T),
           elev_median = median(nh_elmat, na.rm = T),
           elev_p60 = quantile(nh_elmat, probs = .4, na.rm = T))
}

fpr_elev_stats <- function(){
  wshds %>%
    filter(stream_crossing_id == 50067) %>%
    pull(stream_crossing_id) %>%
    map(fpr_elev_stats_ind) %>%
    bind_rows()
}


fpr_elev_stats <- function(){
  wshds %>%
    pull(stream_crossing_id) %>%
    map(
      function(site_ids){
        wshd <- wshds %>%
          filter(stream_crossing_id == site_ids)

        nh_elmat <- wshd %>%
          elevatr::get_elev_raster(., 14) %>%
          rayshader::raster_to_matrix()

        nh_elmat[nh_elmat < 100] = NA #ditch values <100m bc must be errors -something is wrong at the mine

        wshd %>%
          mutate(elev_mean = mean(nh_elmat, na.rm = T),
                 elev_max = max(nh_elmat, na.rm = T),
                 elev_min = min(nh_elmat, na.rm = T),
                 elev_median = median(nh_elmat, na.rm = T),
                 elev_p60 = quantile(nh_elmat, probs = .4, na.rm = T))
      }
    ) %>%
    bind_rows()
}




# maybe a strange place to do it but lets get some elev for our phase2 points for reporting
# these numbers are way off so not going to use them
pscis_phase2_elev_prep <- pscis_phase2 %>%
  sf::st_as_sf(coords = c("easting", "northing"),
               crs = 26911, remove = F) %>% ##don't forget to put it in the right crs buds
  as_Spatial() %>%
  # as.data.frame() %>%
  elevatr::get_elev_point(., src = 'aws',
                          prj = "EPSG:26911",
                          zoom = 14)

pscis_phase2_elev <- pscis_phase2_elev_prep@data %>%
  select(-elev_units)




