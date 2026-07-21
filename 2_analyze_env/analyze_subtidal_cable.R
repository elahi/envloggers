################################################################################
##' @title Analyze subtidal cable temperature data
##' @author Robin Elahi
##' @date 2026-07-20
##' @log 
################################################################################

#### File paths ####
here::i_am("2_analyze_env/analyze_subtidal_cable.R")
library(here)
file_name <- "analyze_subtidal_cable"


#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank()))

# Cable 000m
d000 <- read_csv("data_output/envlogger_cable000.csv") %>% 
  mutate(year = year(time), 
         month = month(time), 
         cable = "000m")

d000 %>% 
  ggplot(aes(time, temp)) + 
  geom_line()

# Cable 100m
d090 <- read_csv("data_output/envlogger_cable090.csv") %>% 
  mutate(year = year(time), 
         month = month(time), 
         cable = "090m")

d090 %>% 
  ggplot(aes(time, temp)) + 
  geom_line()

# Cable 170m
d170 <- read_csv("data_output/envlogger_cable170.csv") %>% 
  mutate(year = year(time), 
         month = month(time), 
         cable = "170m")

d170 %>% 
  ggplot(aes(time, temp)) + 
  geom_line()

range(d000$time)
range(d090$time)
range(d170$time)

min_date <- range(d170$time)[1]

##### COMBINE #####

d_all <- rbind(d000, d090, d170) |> 
  filter(time > min_date) |> 
  mutate(month_year = paste(month, year, sep = "_"))

d_all |> 
  ggplot(aes(time, temp, color = cable)) + 
  facet_wrap(~ month_year) +
  geom_line()

d_all |> 
  ggplot(aes(cable, temp)) + 
  facet_wrap(~ month_year) +
  geom_boxplot(notch = TRUE)

d_all |> 
  ggplot(aes(temp, fill = cable)) + 
  facet_wrap(~ month_year, scales = "free_y") +
  geom_density(alpha = 0.5)

ggsave(paste("figs/", file_name, "_a.pdf", sep = ""), height = 8, width = 10.5)


##### SUMMER #####

# Pick one month
my_month <- "8"

d0 <- d000 |> 
  filter(year == "2025" & month == my_month)

d1 <- d090 |> 
  filter(year == "2025" & month == my_month)

d2 <- d170 |> 
  filter(year == "2025" & month == my_month)

# Combine
d <- rbind(d0, d1, d2)

d |> 
  ggplot(aes(time, temp, color = cable)) + 
  geom_line()

d |> 
  ggplot(aes(cable, temp)) + 
  geom_boxplot(notch = TRUE)

d |> 
  ggplot(aes(temp, fill = cable)) + 
  geom_density(alpha = 0.5)


##### WINTER #####

# Pick one month
d0 <- d000 |> 
  filter(year == "2025" & month == "1")

d1 <- d090 |> 
  filter(year == "2025" & month == "1")

d2 <- d170 |> 
  filter(year == "2025" & month == "1")

# Combine
d <- rbind(d0, d1, d2)

d |> 
  ggplot(aes(time, temp, color = cable)) + 
  geom_line()

d |> 
  ggplot(aes(cable, temp)) + 
  geom_boxplot(notch = TRUE)

d |> 
  ggplot(aes(temp, fill = cable)) + 
  geom_density(alpha = 0.5)

