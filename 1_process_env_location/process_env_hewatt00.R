################################################################################
##' @title EnvLogger processing - by location
##' @author Robin Elahi
##' @date 2026-06-05
##' @log 
################################################################################

##' Location: Hewatt transect, 0.0m
##' Logger serial: 041C DE00 0B1A 0D
##' Note that before 2025, # of lines to skip is 20
##' After 2025, # of lines to skip is 21
##' [1] "0460 1700 1840 09-20230217 181134.csv" (subtidal, do not use!)
##' [2] "0460 1700 1840 09-20251204 160009.csv" (hewatt_00)
##' [3] "0460 1700 1840 09-20260519 083014.csv" (hewatt_00)

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
source("R/envlogger_functions.R")

#### COMPILE FILES FROM A SINGLE FOLDER ####
# Get file names
files <- list.files(path = "data/0460 1700 1840 09", pattern = "\\.csv")
files

# Remove first file
files[-1]
files <- files[-1]
n_files <- length(files)
i <- 1

# Folder with files
serial <- env_file_parse(files[i])
serial2 <- gsub(" ", "", serial, fixed = TRUE)
my_location <- "hewatt00"

# Post-2025; skipping 21 lines
d <- env_file_compile(my_files = files, my_skip = 21)

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

d %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line()

##### WRITE PROCESSED FILE ####
write_csv(d, here("data_output", paste("envlogger_", my_location, ".csv", sep = "")))
