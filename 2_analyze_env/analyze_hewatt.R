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
d1 <- read_csv("data_output/envlogger_04924E000C5503.csv")

# Hewatt low zone
d2 <- read_csv("data_output/envlogger_04EB240037540B.csv")

# Combine
d <- rbind(d1, d2)

# Plot raw data
d %>% 
  ggplot(aes(time, temp, color = serial)) + 
  facet_wrap(~ serial) +
  geom_line()

#### SUMMARIZE DAILY TEMPERATURE ####

