################################################################################
##' @title EnvLogger processing - by location
##' @author Robin Elahi
##' @date 2026-06-05
##' @log 
################################################################################

##' Location: Hewatt transect, 1.5m
##' Logger serial: 04A5 3700 374B 0F
##' Note that before 2025, # of lines to skip is 20
##' After 2025, # of lines to skip is 21
##' [1] "04A5 3700 374B 0F-20230407 104413.csv" (skip; subtidal)
##' [2] "04A5 3700 374B 0F-20250502 100524.csv"
##' [3] "04A5 3700 374B 0F-20251204 153600.csv"
##' [4] "04A5 3700 374B 0F-20260519 084701.csv"

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
source("R/envlogger_functions.R")

#### COMPILE FILES FROM A SINGLE FOLDER ####
# Get file names
files <- list.files(path = "data/04A5 3700 374B 0F", pattern = "\\.csv")
files

# Remove first file
files[-1]
files <- files[-1]
n_files <- length(files)
i <- 1

# Folder with files
serial <- env_file_parse(files[i])
serial2 <- gsub(" ", "", serial, fixed = TRUE)
my_location <- "hewatt15"

# Pre-2025; skipping 20 lines
d1 <- env_file_compile(my_files = files[1], my_skip = 20)
# Post-2025; skipping 21 lines
d2 <- env_file_compile(my_files = files[-1], my_skip = 21)
d2 <- d2 |> 
  mutate(file_i = as.character(as.numeric(file_i) + 1))

# Compile files
d <- rbind(d1, d2)

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
