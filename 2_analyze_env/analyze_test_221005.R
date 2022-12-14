################################################################################
##' @title Analyze envlogger data
##' @author Robin Elahi
##' @date 2022-12-16
##' @log 
################################################################################

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank()))

d <- read_csv(here("data_output", "envlogger_env_test_221005.csv"))
d

d %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line() + 
  facet_wrap(~ file_i)

#### Filter data ####

d <- d %>% 
  filter(time > "2022-10-04 18:19:00" & time < "2022-10-05 15:40:00")

# Seawater only
# Start: 11:04 AM, 4 Oct
# End: 1:08 PM, 5 oct
d <- d %>% 
  filter(time > "2022-10-04 11:20:00" & time < "2022-10-05 13:00:00")

d %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line()

#### Summarize data ####

serial_summary <- d %>% 
  filter(!is.na(temp)) %>% 
  group_by(serial) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 2 * se) 

d_summary <- d %>% 
  filter(!is.na(temp)) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 2 * se) 

temp_max <- max(d$temp) 
temp_min <- min(d$temp) 

serial_summary %>% 
  ggplot(aes(serial, mean)) + 
  geom_hline(data = d_summary, aes(yintercept = mean), color = "red", 
             linetype = "solid", lwd = 1) +
  geom_hline(data = d_summary, aes(yintercept = mean + 0.1), color = "red", 
             linetype = "solid", lwd = 0.7) + 
  geom_hline(data = d_summary, aes(yintercept = mean - 0.1), color = "red", 
             linetype = "solid", lwd = 0.7) +
  geom_hline(data = d_summary, aes(yintercept = temp_max), 
             color = "gray", linetype = "dashed", lwd = 0.5) + 
  geom_hline(data = d_summary, aes(yintercept = temp_min), 
             color = "gray", linetype = "dashed", lwd = 0.5) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean - CI, ymax = mean + CI), width = 0.2) + 
  labs(x = "Serial number", y = "Temperature (C)",
       caption = "mean +- 95% CI") + 
  coord_flip() + 
  ggtitle("Sea table, n = 154, frequency = 10min, 4-5 Oct 2022") +
  scale_y_continuous(limits = c(temp_min, temp_max))

ggsave(here("figs", "env_test_221005.pdf"), height = 3.5, width = 6)

