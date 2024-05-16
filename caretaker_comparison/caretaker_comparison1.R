################################################################################
##' @title Compare caretaker data to envloggers
##' @author Robin Elahi
##' @date 2024-05-16
##' @log 
################################################################################

#### File paths ####
here::i_am("caretaker_comparison/caretaker_comparison1.R")
library(here)
folder <- "caretaker_comparison"
file_name <- "caretaker_comparison1"

#### PACKAGES, DATA ####
library(tidyverse)
library(rtide)
library(patchwork)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank()))

## Load compiled intertidal envlogger data (from compile_intertidal_hms_env_raw.R)
d <- read_csv("data_output/intertidal_hms_env_raw.csv")
glimpse(d)
d$time[1]

# Convert time zone, get rounded hours for joining with tidal height
d <- d %>% 
  mutate(time = with_tz(time, tzone = "PST8PDT"), 
         time_hr = round_date(time, unit = "hour")) %>% 
  mutate(date_w_time = ymd_hms(paste(date, "10:00:00", sep = ""), tz = "PST8PDT"))

d$time[1]
d$time_hr[1]
d$date_w_time[1]

# Plot raw data - 2nd set of loggers
d %>% 
  filter(date > "2022-12-01") %>% 
  ggplot(aes(time, temp, color = serial)) + 
  geom_line(alpha = 1) +
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Date") +
  theme(legend.position = "bottom") +
  facet_wrap(~ name) 

## Load caretake sst data and filter to > 2023
d_sst <- read_csv("data/PacificGrove_TEMP_1919-202312.csv", skip = 42)
glimpse(d_sst)
d_sst <- d_sst %>% 
  filter(YEAR > 2022) %>%
  mutate(date = as_date(paste(YEAR, MONTH, DAY, sep = "-")), 
         date_w_time = ymd_hms(paste(date, "10:00:00", sep = ""), tz = "PST8PDT")) %>%
  rename(temp = SURF_TEMP_C) %>% 
  select(date, date_w_time, temp)

d_sst %>% 
  ggplot(aes(date, temp)) + 
  geom_line() + 
  labs(x = "Time", y = "Temperature (C)")


#### WRANGLE INTERTIDAL DATA ####
unique(d$site)

d_ag <- d %>% 
  filter(site == "Agassiz") %>% 
  filter(date >= "2023-01-01") %>% 
  filter(date < "2024-01-01")

d_ag_daily <- d_ag %>% 
  group_by(date, date_w_time) %>% 
  summarize(median = median(temp)) %>% 
  ungroup()

d_ag_daily <- d_sst %>% 
  select(-date) %>% 
  left_join(d_ag_daily, ., by = "date_w_time") %>% 
  rename(median_c = median, caretaker_c = temp)

d_ag_daily_long <- d_ag_daily %>% 
  pivot_longer(names_to = "source", values_to = "temp", median_c:caretaker_c)

p1 <- d_ag %>% 
  ggplot(aes(time, temp)) + 
  geom_line(alpha = 0.5) +
  geom_line(data = d_ag_daily, aes(date_w_time, median_c), 
            inherit.aes = FALSE, color = "red") +
  geom_line(data = d_ag_daily, aes(date_w_time, caretaker_c), 
            inherit.aes = FALSE, color = "blue") +
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Date") 
p1

p2 <- d_ag_daily_long %>% 
  ggplot(aes(date_w_time, temp, color = source)) + 
  geom_line(alpha = 1) +
  scale_color_manual(values = c("blue", "red")) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Date") 
p2

p3 <- d_ag_daily %>% 
  ggplot(aes(caretaker_c, median_c)) + 
  geom_point() + 
  geom_abline(aes(slope = 1, intercept = 0))
p3

# Zoom in
date_zoom_start <- "2023-04-09"
date_zoom_end <- "2023-04-23"

d_ag_daily_sub <- d_ag_daily %>% 
  filter(date >= date_zoom_start) %>% 
  filter(date < date_zoom_end) 

d_ag %>% 
  filter(date >= date_zoom_start) %>% 
  filter(date < date_zoom_end) %>% 
  ggplot(aes(time, temp)) + 
  geom_line(alpha = 0.5) +
  geom_point(data = d_ag_daily_sub, aes(date_w_time, median_c), 
            inherit.aes = FALSE, color = "red") +
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Date") 

#### JOIN TIDAL DATA TO ENVLOGGER DATA ####
min_date <- min(d_ag_daily$date)
max_date <- max(d_ag_daily$date)

## Load rtide data
d_tide <- tide_height('Monterey Harbor',from = min_date, 
                      to = max_date, minutes = 10, tz ='PST8PDT')

range(d_tide$DateTime)

ggplot(data = d_tide, aes(x = DateTime, y = TideHeight)) + 
  geom_line(alpha = 0.2) + 
  scale_y_continuous(name = "Tide Height (m)") +
  ggtitle("Monterey Harbor")

## Wrangle d_tide and d to match
d_tide2 <- d_tide %>% 
  rename(time = DateTime, tide_height = TideHeight) %>% 
  select(-Station) %>% 
  mutate(time_hr = round_date(time, unit = "hour"))

head(d_tide2$time_hr)
head(d_ag$time_hr)
tail(d_tide2$time_hr)
tail(d_ag$time_hr)

# Choose the median tide for each rounded hour
d_tide2_sub <- d_tide2 %>% 
  group_by(time_hr) %>% 
  summarize(tide_height = median(tide_height))

# Join with d_ag
d_ag <- left_join(d_ag, d_tide2_sub, by = "time_hr")
summary(d_ag$tide_height)

# Tide threshold
tide_threshold <- quantile(d_ag$tide_height, probs = 0.25, na.rm = TRUE)
tide_threshold <- 1

d_ag %>% 
  filter(tide_height > tide_threshold) %>% 
  summary()
  
d_ag %>% 
  filter(tide_height > tide_threshold) %>% 
  ggplot(aes(time, temp)) + 
  geom_point(alpha = 0.5, size = 0.2)

d_ag_daily <- d_ag %>% 
  filter(tide_height > tide_threshold) %>% 
  group_by(date, date_w_time) %>% 
  summarize(median = median(temp)) %>% 
  ungroup()

d_ag_daily <- d_sst %>% 
  select(-date) %>% 
  left_join(d_ag_daily, ., by = "date_w_time") %>% 
  rename(median_c = median, caretaker_c = temp)

d_ag_daily_long <- d_ag_daily %>% 
  pivot_longer(names_to = "source", values_to = "temp", median_c:caretaker_c)


d_ag %>% 
  filter(tide_height > tide_threshold) %>% 
  ggplot(aes(time, temp)) + 
  geom_line(alpha = 0.5) +
  geom_line(data = d_ag_daily, aes(date_w_time, median_c), 
            inherit.aes = FALSE, color = "red") +
  geom_line(data = d_ag_daily, aes(date_w_time, caretaker_c), 
            inherit.aes = FALSE, color = "blue") +
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Date") 

p2 <- d_ag_daily_long %>% 
  ggplot(aes(date_w_time, temp, color = source)) + 
  geom_line(alpha = 1) +
  scale_color_manual(values = c("blue", "red")) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Date") +
  theme(legend.position = c(0.25, 0.85), 
        legend.title = element_blank())
p2

p3 <- d_ag_daily %>% 
  ggplot(aes(caretaker_c, median_c)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(aes(slope = 1, intercept = 0))
p3

p3 + p2

ggsave(paste(folder, "/figs/", file_name, "_a.pdf", sep = ""), height = 3.5, width = 7)



#### PLOT ####
# 
# # Filter to immersed temperatures
# 
# time_min <- as_datetime('2024-05-03 16:35:00')
# time_max <- as_datetime('2024-05-03 17:15:00')
# 
# d2 %>% 
#   filter(time > time_min & time < time_max) %>% 
#   ggplot(aes(time, temp, color = depth_m, group = file_i)) + 
#   geom_line() + 
#   scale_color_continuous(name = "Depth (m)") + 
#   labs(x = "Time", y = "Temperature (C)", 
#        title = "MacAbee vertical tow, 3 May 2024")
# 
# ggsave(here("figs", paste(my_folder, "_time_series", ".pdf", sep = "")), 
#        height = 3, width = 4)
# 
# ##### WRITE PROCESSED FILE ####
# write_csv(d2, here("data_output", paste("envlogger_", my_folder, ".csv", sep = "")))
