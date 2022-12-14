################################################################################
##' @title EnvLogger processing
##' @author Robin Elahi
##' @date 2022-12-13
##' @log 
################################################################################

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid.minor = element_blank(), 
                  strip.background = element_blank()))

source("R/envlogger_functions.R")

#### COMPILE FILES FROM A SINGLE FOLDER ####
# Get file names
files <- list.files(path = "data/0492 4E00 0C55 03")
files
n_files <- length(files)
i <- 1

# Folder with files
serial <- env_file_parse(files[i])
serial2 <- gsub(" ", "", serial, fixed = TRUE)

# Compile files
d <- env_file_compile()

##### QUALITY CONTROL ####
# Check for duplicates
d %>% count(file_i)

d %>% 
  filter(!is.na(temp)) %>% 
  group_by(file_i) %>% 
  summarise(mean = mean(temp)) %>% 
  as.data.frame()

d %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line() + 
  facet_wrap(~ file_i)

# Remove duplicates
d <- d %>% filter(file_i != "3")

##### WRITE PROCESSED FILE ####
write_csv(d, here("data_output", paste("envlogger_", serial2, ".csv", sep = "")))
