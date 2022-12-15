################################################################################
##' @title Analyze Hewatt temperature data
##' @author Robin Elahi
##' @date 2022-12-14
##' @log 
################################################################################

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid.minor = element_blank(), 
                  strip.background = element_blank()))

# Hewatt high zone
d1 <- read_csv("data_output/envlogger_04924E000C5503.csv") %>% 
  mutate(zone = "high")

# Hewatt low zone
d2 <- read_csv("data_output/envlogger_04EB240037540B.csv") %>% 
  mutate(zone = "low")

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
  ggplot(aes(time, temp, color = year)) + 
  facet_wrap(~ zone) +
  geom_line()

d %>% 
  ggplot(aes(jday, temp, color = year)) + 
  facet_wrap(~ zone) +
  geom_line()

#### SUMMARIZE DAILY TEMPERATURE ####

