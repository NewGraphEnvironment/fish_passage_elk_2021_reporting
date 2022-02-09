source('R/packages.R')

##08NK002 is elk river at Fernie  - 08NK016 is near sparwood

library(tidyhydat)
library(fasstr)
library(hydatr)

# download_hydat()

##C:\Users\allan\AppData\Local\tidyhydat\tidyhydat
# C:\Users\al\AppData\Local\tidyhydat\tidyhydat

hydatr::hydat_load(source = "C://Users//allan//AppData//Local//tidyhydat//tidyhydat") # loads the database (you'll need to call this one each time you load the package)
# hydatr::hydat_load(source = "C://Users//al//AppData//Local//tidyhydat//tidyhydat") # loads the database (you'll need to call this one each time you load the package)


tidyhat_info <- search_stn_name("Morice")
hydatr_info <- as.data.frame(hydat_station_info(tidyhat_info$STATION_NUMBER))

tidyhat_info <- search_stn_number("08EE005") #08EE003 is near houston
hydatr_info <- as.data.frame(hydat_station_info(tidyhat_info$STATION_NUMBER))


#BULKLEY RIVER near QUICK
station <- '08EE004'

tidyhat_info <- search_stn_number(station) #08EE003 is near houston
hydatr_info <- as.data.frame(hydat_station_info(tidyhat_info$STATION_NUMBER))

##standard faster plot to view but not for the report
# hydrograph <- fasstr::plot_daily_stats(station_number = station,
#                                        # start_year = 1970,
#                                        end_year = 9999,
#                                        log_discharge = TRUE,
#                                        ignore_missing = TRUE)
# hydrograph


# hydrograph_print <- hydrograph[["Daily_Statistics"]]
# hydrograph_print
# ggsave(plot = hydrograph_print, file="./fig/hydrology1.png",
#        h=3.4, w=5.11, units="in", dpi=300)

hydatr_info <- mutate(hydatr_info, title = paste0(stringr::str_to_title(STATION_NAME),
                                                  " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                  " Lon ",round(LONGITUDE,6), "). Available daily discharge data from ",
                                                  FIRST_YEAR, ##removed the default here
                                                  " to ",LAST_YEAR, "."))
hydrograph1_caption <- hydatr_info$title
##Bulkley River At Quick (Station #08EE004 - Lat 54.62 Lon -126.90). Available daily discharge data from 1930 to 2018.

##fasstr::plot_data_screening2 is a custom version of plot_data_screening
hydrograph_stats_print <- fasstr::plot_data_screening3(station_number = station)[["Data_Screening"]] + ggdark::dark_theme_bw() ##first version is not dark
hydrograph_stats_print

ggsave(plot = hydrograph_stats_print, file=paste0("fig/hydrology_stats_", station, ".png"),
       h=3.4, w=5.11, units="in", dpi=300)

##another way to make the graph
flow_raw <- tidyhydat::hy_daily_flows(station)


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

#BULKLEY RIVER AT houston
station <- '08EE003'

tidyhat_info <- search_stn_number(station) #08EE003 is near houston
hydatr_info <- mutate(hydatr_info, title = paste0(stringr::str_to_title(STATION_NAME),
                                                  " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                  " Lon ",round(LONGITUDE,6), "). Available daily discharge data from 1980",
                                                  # FIRST_YEAR, ##removed the default here
                                                  " to ",LAST_YEAR, "."))
hydrograph1_caption <- hydatr_info$title

#Bulkley River Near Houston (Station #08EE003 - Lat 54.40 Lon -126.72). Available daily discharge data from 1980 to 2018.


hydrograph_stats_print <- fasstr::plot_data_screening3(station_number = station, start_year = 1980)[["Data_Screening"]] + ggdark::dark_theme_bw()
hydrograph_stats_print

ggsave(plot = hydrograph_stats_print, file=paste0("./fig/hydrology_stats_", station, ".png"),
       h=3.4, w=5.11, units="in", dpi=300)

flow_raw <- tidyhydat::hy_daily_flows(station, start_date = '1980-01-01')

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

############################################################################################################
############################################################################################################
#########################################         Morice    ################################################
############################################################################################################
############################################################################################################

tidyhat_info <- search_stn_name("Morice")
hydatr_info <- as.data.frame(hydat_station_info(tidyhat_info$STATION_NUMBER))

##we need to query which active stations are within the watershed group to get some context upstream
##b/c they don't have Morice in

##MORICE RIVER at outlet of Morice lake
station <- '08ED002'

tidyhat_info <- search_stn_number(station)
hydatr_info <- as.data.frame(hydat_station_info(tidyhat_info$STATION_NUMBER))
hydatr_info <- mutate(hydatr_info, title = paste0(stringr::str_to_title(STATION_NAME),
                                                  " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                  " Lon ",round(LONGITUDE,6), "). Available daily discharge data from ",
                                                  FIRST_YEAR, ##removed the default here
                                                  " to ",LAST_YEAR, "."))
hydrograph1_caption <- hydatr_info$title
hydrograph1_caption
#Bulkley River Near Houston (Station #08EE003 - Lat 54.40 Lon -126.72). Available daily discharge data from 1980 to 2018.


hydrograph_stats_print <- fasstr::plot_data_screening3(station_number = station)[["Data_Screening"]] + ggdark::dark_theme_bw() #, start_year = 1980
hydrograph_stats_print

ggsave(plot = hydrograph_stats_print, file=paste0("./fig/hydrology_stats_", station, ".png"),
       h=3.4, w=5.11, units="in", dpi=300)

flow_raw <- tidyhydat::hy_daily_flows(station) #, start_date = '1980-01-01'

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

##Morice at the mouth
##kid price outlet 08ED001

# station <- '08ED001'
station <- '08ED003'

tidyhat_info <- search_stn_number(station)
hydatr_info <- as.data.frame(hydat_station_info(tidyhat_info$STATION_NUMBER))
hydatr_info <- mutate(hydatr_info, title = paste0(stringr::str_to_title(STATION_NAME),
                                                  " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                  " Lon ",round(LONGITUDE,6), "). Available daily discharge data from ",
                                                  FIRST_YEAR, ##removed the default here
                                                  " to ",LAST_YEAR, "."))
hydrograph1_caption <- hydatr_info$title
hydrograph1_caption
#Bulkley River Near Houston (Station #08EE003 - Lat 54.40 Lon -126.72). Available daily discharge data from 1980 to 2018.


hydrograph_stats_print <- fasstr::plot_data_screening3(station_number = station)[["Data_Screening"]] + ggdark::dark_theme_bw() #, start_year = 1980
hydrograph_stats_print

ggsave(plot = hydrograph_stats_print, file=paste0("./fig/hydrology_stats_", station, ".png"),
       h=3.4, w=5.11, units="in", dpi=300)

flow_raw <- tidyhydat::hy_daily_flows(station) #, start_date = '1980-01-01'

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
