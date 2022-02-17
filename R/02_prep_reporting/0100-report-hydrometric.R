source('R/packages.R')

##08NK002 is elk river at Fernie  - 08NK016 is near sparwood
##08NK016

# download_hydat()

##C:\Users\allan\AppData\Local\tidyhydat\tidyhydat
# C:\Users\al\AppData\Local\tidyhydat\tidyhydat

hydatr::hydat_load(source = "C://Users//allan//AppData//Local//tidyhydat//tidyhydat") # loads the database (you'll need to call this one each time you load the package)
# hydatr::hydat_load(source = "C://Users//al//AppData//Local//tidyhydat//tidyhydat") # loads the database (you'll need to call this one each time you load the package)


tidyhat_info <- search_stn_name("Elk")



#elk river at Fernie
station <- '08NK002'
start_year = 1970

flow_raw <- tidyhydat::hy_daily_flows(station)

tidyhat_info <- search_stn_number(station) #08EE003 is near houston
hy_stn_data_coll('08NK002')

##build caption for the figure
caption_info <- mutate(tidyhat_info, title_stats = paste0(stringr::str_to_title(STATION_NAME),
                                                    " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                    " Lon ",round(LONGITUDE,6), "). Available daily discharge data from ", start_year,
                                                    # FIRST_YEAR, ##removed the default here
                                                    " to ",end_year, "."))

hydrograph1_stats_caption <- caption_info$title_stats


##fasstr::plot_data_screening2 is a custom version of plot_data_screening
hydrograph_stats_print <- fasstr::plot_data_screening(station_number = station, start_year = start_year)[["Data_Screening"]] + ggdark::dark_theme_bw() ##first version is not dark
hydrograph_stats_print

ggsave(plot = hydrograph_stats_print, file=paste0("fig/hydrology_stats_", station, ".png"),
       h=3.4, w=5.11, units="in", dpi=300)

##another way to make the graph
flow <- flow_raw %>%
  mutate(day_of_year = yday(Date)) %>%
  group_by(day_of_year) %>%
  summarise(daily_ave = mean(Value, na.rm=TRUE),
            daily_sd = sd(Value, na.rm = TRUE),
            max = max(Value, na.rm = TRUE),
            min = min(Value, na.rm = TRUE)) %>%
            # q025 = quantile(Value, probs = 0.025),
            # q975 = quantile(Value, probs = 0.975)) %>%
  mutate(Date = as.Date(day_of_year, origin = "2015-12-31"))

plot <- ggplot()+
  geom_ribbon(data = flow, aes(x = Date, ymax = max,
                               ymin = min),
              alpha = 0.3, linetype = 1)+
  # geom_ribbon(data = flow_UDR, aes(x = Date, ymax = daily_ave + 2 * daily_sd,
  #                                  ymin = daily_ave - 2 * daily_sd),
  #             alpha = 0.4, linetype = 1)+
  # geom_ribbon(data = flow_UDR, aes(x = Date, ymax = q975,
  #                                  ymin = q025),
  #             alpha = 0.3, linetype = 1)+

  scale_x_date(date_labels = "%b", date_breaks = "2 month") +
  labs(x = NULL, y = expression(paste("Mean Daily Discharge (", m^3, "/s)", sep="")))+
  ggdark::dark_theme_bw() +
  # ylim(0,600) +
  # theme(axis.text.y=element_blank())+
  # scale_y_continuous() +
  geom_line(data = flow, aes(x = Date, y = daily_ave),
            linetype = 1, size = 0.7) +
  scale_colour_manual(values = c("grey10", "red"))
# coord_cartesian(ylim = c(0, 600))
plot

ggsave(plot = plot, file=paste0("./fig/hydrograph_", station, ".png"),
       h=3.4, w=5.11, units="in", dpi=300)

########----------------------------------------------------------------------------------
#08NK016 is near sparwood
station <- '08NK016'
tidyhat_info <- search_stn_number("08NK016")


flow_raw <- tidyhydat::hy_daily_flows(station)

start_year <- flow_raw$Date %>% min() %>% lubridate::year()
end_year <- flow_raw$Date %>% max() %>% lubridate::year()

tidyhat_info <- search_stn_number(station) #

##build caption for the figure
caption_info <- mutate(tidyhat_info, title_stats = paste0(stringr::str_to_title(STATION_NAME),
                                                  " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                  " Lon ",round(LONGITUDE,6), "). Available daily discharge data from ", start_year,
                                                  # FIRST_YEAR, ##removed the default here
                                                  " to ",end_year, "."))

hydrograph2_stats_caption <- caption_info$title_stats

#Bulkley River Near Houston (Station #08EE003 - Lat 54.40 Lon -126.72). Available daily discharge data from 1980 to 2018.


hydrograph_stats_print <- fasstr::plot_data_screening(station_number = station)[["Data_Screening"]] + ggdark::dark_theme_bw()
hydrograph_stats_print

ggsave(plot = hydrograph_stats_print, file=paste0("./fig/hydrology_stats_", station, ".png"),
       h=3.4, w=5.11, units="in", dpi=300)

# flow_raw <- tidyhydat::hy_daily_flows(station, start_date = '1980-01-01')

flow <- flow_raw %>%
  mutate(day_of_year = yday(Date)) %>%
  group_by(day_of_year) %>%
  summarise(daily_ave = mean(Value, na.rm=TRUE),
            daily_sd = sd(Value, na.rm = TRUE),
            max = max(Value, na.rm = TRUE),
            min = min(Value, na.rm = TRUE)) %>%
            # q025 = quantile(Value, probs = 0.025),
            # q975 = quantile(Value, probs = 0.975)) %>%
  mutate(Date = as.Date(day_of_year, origin = "2015-12-31"))


plot <- ggplot()+
  geom_ribbon(data = flow, aes(x = Date, ymax = max,
                               ymin = min),
              alpha = 0.3, linetype = 1)+
  # geom_ribbon(data = flow_UDR, aes(x = Date, ymax = daily_ave + 2 * daily_sd,
  #                                  ymin = daily_ave - 2 * daily_sd),
  #             alpha = 0.4, linetype = 1)+
  # geom_ribbon(data = flow_UDR, aes(x = Date, ymax = q975,
  #                                  ymin = q025),
  #             alpha = 0.3, linetype = 1)+

  scale_x_date(date_labels = "%b", date_breaks = "2 month") +
  labs(x = NULL, y = expression(paste("Mean Daily Discharge (", m^3, "/s)", sep="")))+
  ggdark::dark_theme_bw() +
  # ylim(0,600) +
  # theme(axis.text.y=element_blank())+
  # scale_y_continuous() +
  geom_line(data = flow, aes(x = Date, y = daily_ave),
            linetype = 1, size = 0.7) +
  scale_colour_manual(values = c("grey10", "red"))
# coord_cartesian(ylim = c(0, 600))
plot

ggsave(plot = plot, file=paste0("./fig/hydrograph_", station, ".png"),
       h=3.4, w=5.11, units="in", dpi=300)
