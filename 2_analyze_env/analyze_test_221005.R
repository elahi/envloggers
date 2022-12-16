################################################################################
##' @title Analyze envlogger data
##' @author Robin Elahi
##' @date 2022-10-06
##' @log 
################################################################################

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid.minor = element_blank(), 
                  strip.background = element_blank()))

d <- read_csv(here("data_env_processed/env_test_data_221005.csv"))
d

# Check for duplicates
d %>% count(file_i)

d %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line() + 
  facet_wrap(~ file_i)

d %>% 
  filter(!is.na(temp)) %>% 
  filter(temp < 17) %>% 
  group_by(serial) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n)) %>% 
  as.data.frame()

#### Filter data ####

d2 <- d %>% 
  filter(time > "2022-10-04 18:19:00" & time < "2022-10-05 15:40:00")

# Seawater only
# Start: 11:04 AM, 4 Oct
# End: 1:08 PM, 5 oct
d2 <- d %>% 
  filter(time > "2022-10-04 11:20:00" & time < "2022-10-05 13:00:00")

d2 %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line()

serial_summary <- d2 %>% 
  filter(!is.na(temp)) %>% 
  group_by(serial) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 2 * se) 

d_summary <- d2 %>% 
  filter(!is.na(temp)) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 2 * se) 

serial_summary %>% 
  ggplot(aes(serial, mean)) + 
  geom_hline(data = d_summary, aes(yintercept = mean), color = "red", 
             linetype = "solid", lwd = 1.1) +
  geom_hline(data = d_summary, aes(yintercept = mean + CI), color = "red", 
             linetype = "dashed", lwd = 1.1) + 
  geom_hline(data = d_summary, aes(yintercept = mean - CI), color = "red", 
             linetype = "dashed", lwd = 1.1) +
  geom_point() + 
  geom_errorbar(aes(ymin = mean - CI, ymax = mean + CI), width = 0.2) + 
  labs(x = "Serial number", y = "Temperature (C)",
       caption = "mean +- 95% CI") + 
  coord_flip() + 
  ggtitle("Tank test, n = 154, frequency = 10min, 4-5 Oct 2022") 

ggsave(here("figs", "env_test_221005.pdf"), height = 3.5, width = 6)

#### Minidot ####

# Need to skip the 2nd line
col_names <- names(read_csv(here("data_minidot_processed/176547.csv"), n_max = 0))
md <- read_csv(here("data_minidot_processed/176547.csv"), col_names = col_names, skip = 2) %>%
  rename(time = Pacific.Standard.Time, 
         temp = Temperature)

md %>% 
  ggplot(aes(time, temp)) + 
  geom_line()
md %>% 
  ggplot(aes(time, Dissolved.Oxygen)) + 
  geom_line()

# Filter to match env loggers
# Did this by trial and error b/c the times don't match ugh
md2 <- md %>% 
  filter(time > "2022-10-04 04:20:00" & time < "2022-10-05 06:00:00")

d2 %>% 
  ggplot(aes(time, temp, color = serial)) + 
  geom_line() + 
  geom_line(data = md2, aes(time + hours(7)), color = "black", lwd = 1.2) +
  labs(x = "Date", y = "Temperature (C)") +
  ggtitle("Tank test, n = 154, frequency = 10min, 4-5 Oct 2022") 
ggsave(here("figs", "env_minidot_test_time_series_221005.pdf"), height = 3.5, width = 6)
