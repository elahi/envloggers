################################################################################
##' @title Analyze Hewatt temperature data
##' @author Robin Elahi
##' @date 2022-12-14
##' @log 2024-11-15
################################################################################

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank()))

# Hewatt 1.0m
d1 <- read_csv("data_output/envlogger_04924E000C5503.csv") %>% 
  mutate(zone = "1.0m")
d1 %>% 
  head(n = 1000) %>% 
  ggplot(aes(time, temp)) + 
  geom_line()

# Hewatt mid zone
d2 <- read_csv("data_output/envlogger_04EB240037540B.csv") %>% 
  mutate(zone = "0.5m")
d2 %>% 
  head(n = 1000) %>% 
  ggplot(aes(time, temp)) + 
  geom_line()

# Combine
d <- rbind(d1, d2)

# Lubridate
d <- d %>% 
  mutate(date = date(time), 
         year = year(time), 
         month = month(time), 
         week = week(time), 
         jday = yday(time))

# Plot raw data
d %>% 
  tail() %>% 
  ggplot(aes(time, temp, color = year)) + 
  facet_wrap(~ zone) +
  geom_line()

#### QUALITY CONTROL ####
# Need to remove days without a near-complete set of observations
# Loggers recorded every 10 minutes, only use days with 95% of n_max
n_max <- 24 * 60 / 10
n_threshold <- 0.95 * n_max
n_threshold

d %>% count(date)

d <- d %>% 
  group_by(date, zone) %>%
  mutate(n_obs = n()) %>% 
  ungroup()

unique(d$n_obs)

d <- d %>% 
  filter(n_obs > n_threshold)

#### SUMMARIZE DAILY TEMPERATURE ####

d_summary <- d %>%
  group_by(year, month, week, jday, date, zone) %>% 
  summarize(mean = mean(temp), 
            max = max(temp), 
            min = min(temp)) 

ds_long <- d_summary %>% 
  pivot_longer(cols = mean:min, names_to = "metric", values_to = "tempC")

ds_long %>% 
  ggplot(aes(date, tempC, color = metric)) + 
  facet_wrap(~ zone) + 
  geom_vline(xintercept = date("2022-01-01"), color = "gray", linetype = "dashed") + 
  geom_line() + 
  theme(legend.position = "top", 
        legend.title = element_blank()) +
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Date") 

ggsave(here("figs", "hewatt_env_time_series.pdf"), 
       height = 4, width = 7)

ds_long %>% 
  ggplot(aes(zone, tempC)) + 
  facet_wrap(~ metric, scales = "free_y") + 
  geom_violin(color = "gray") + 
  geom_boxplot(width = 0.2, outlier.size = -1) + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Zone")

ggsave(here("figs", "hewatt_env_boxplot.pdf"), 
       height = 4, width = 7)

#### SAVE DATA SUMMARY FOR CLASS ANALYSIS ####

# Save long file as is
ds_long
write.csv(ds_long, "data_output/hewatt_env_time_series_2022.csv")

# Save wide files by tidal height
d_high <- d_summary %>% filter(zone == "high")
d_mid <- d_summary %>% filter(zone == "mid")  

write.csv(d_high, "data_output/hewatt_env_time_series_wide_high_2022.csv")
write.csv(d_mid, "data_output/hewatt_env_time_series_wide_mid_2022.csv")



