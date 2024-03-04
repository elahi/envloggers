################################################################################
##' @title Compile raw Hewatt temperature data
##' @author Robin Elahi
##' @date 2023-04-28
##' @log 
##' 2024-03-03: added new file
################################################################################

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 10) + 
            theme(#panel.grid = element_blank(), 
                  strip.background = element_blank()))

# Hewatt high zone - 2021 logger
d1 <- read_csv("data_output/envlogger_04924E000C5503.csv") %>% 
  mutate(site = "Hewatt", 
         elev_m = "1.04",
         name = paste(site, "_", elev_m, "m", sep = ""))
d1 %>% 
  head(n = 1000) %>% 
  ggplot(aes(time, temp)) + 
  geom_line()

# Hewatt mid zone - 2021 logger
d2 <- read_csv("data_output/envlogger_04EB240037540B.csv") %>% 
  mutate(site = "Hewatt", 
         elev_m = "0.51",
         name = paste(site, "_", elev_m, "m", sep = ""))
d2 %>% 
  head(n = 1000) %>% 
  ggplot(aes(time, temp)) + 
  geom_line()

# Hewatt high zone - 2022 logger
d3 <- read_csv("data_output/envlogger_043D6E009A5A04.csv") %>% 
  mutate(site = "Hewatt", 
         elev_m = "1.04",
         name = paste(site, "_", elev_m, "m", sep = ""))
d3 %>% 
  head(n = 1000) %>% 
  ggplot(aes(time, temp)) + 
  geom_line()

# Hewatt mid zone - 2022 logger
d4 <- read_csv("data_output/envlogger_04DF7800E60609.csv") %>% 
  mutate(site = "Hewatt", 
         elev_m = "0.51",
         name = paste(site, "_", elev_m, "m", sep = ""))
d4 %>% 
  head(n = 1000) %>% 
  ggplot(aes(time, temp)) + 
  geom_line()

# Agassiz beach - 2022 logger
d5 <- read_csv("data_output/envlogger_041CDE000B1A0D.csv") %>% 
  mutate(site = "Agassiz", 
         elev_m = "tbd",
         name = paste(site, "_", elev_m, "m", sep = ""))
d5 %>% 
  head(n = 1000) %>% 
  ggplot(aes(time, temp)) + 
  geom_line()

# Combine
d <- rbind(d1, d3, d2, d4, d5)

# Lubridate
d <- d %>% 
  mutate(date = date(time), 
         year = year(time), 
         month = month(time), 
         week = week(time), 
         jday = yday(time))

write.csv(d, "data_output/intertidal_hms_env_raw.csv")

#### Time-series plot of raw data ####

# Plot raw data - Hewatt
d %>% 
  filter(site != "Agassiz") %>% 
  ggplot(aes(time, temp, color = serial)) + 
  geom_line(alpha = 1) +
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Date") +
  theme(legend.position = "bottom") +
  facet_wrap(~ name) 

ggsave(here("figs", "hewatt_env_raw.pdf"), 
       height = 3.5, width = 7)


# Plot raw data - 2nd set of loggers
d %>% 
  filter(date > "2022-12-01") %>% 
  ggplot(aes(time, temp, color = serial)) + 
  geom_line(alpha = 1) +
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  xlab("Date") +
  theme(legend.position = "bottom") +
  facet_wrap(~ name) 

ggsave(here("figs", "intertidal_hms_env_raw.pdf"), 
       height = 3.5, width = 7)

