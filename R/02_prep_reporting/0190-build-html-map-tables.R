# lets just make the html tables when we need to


unlink('docs/sum/bcfp', recursive = T) #erase the file and start over every time
dir.create('docs/sum/bcfp')

##for now we just print out the phase 2 tables.
# There are issues with the phase 1s even now we generalized the input column so we can use modelled_crossing_id
pscis_all %>%
  # distinct(pscis_crossing_id) %>%
  filter(source %ilike% 'phase2') %>%
  pull(pscis_crossing_id) %>%
  map(fpr_print_tab_bcfp_html)


unlink('docs/sum/cv', recursive = T) #erase the file and start over every time
dir.create('docs/sum/cv')

# build all the cv tables for interactive map
pscis_all %>%
  distinct(pscis_crossing_id) %>%
  # filter(source %ilike% 'phase2') %>%
  pull(pscis_crossing_id) %>%
  map(fpr_print_tab_cv_html)
