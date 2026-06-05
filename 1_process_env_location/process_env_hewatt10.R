################################################################################
##' @title EnvLogger processing - by location
##' @author Robin Elahi
##' @date 2026-06-05
##' @log 
################################################################################

##' Location: Hewatt transect, 1.0m
##' Logger serial: 043D 6E00 9A5A 04
##' Note that before 2025, # of lines to skip is 20
##' After 2025, # of lines to skip is 21

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
source("R/envlogger_functions.R")

#### COMPILE FILES FROM A SINGLE FOLDER ####
# Get file names
files <- list.files(path = "data/043D 6E00 9A5A 04", pattern = "\\.csv")
files
n_files <- length(files)
i <- 1

# Folder with files
serial <- env_file_parse(files[i])
serial2 <- gsub(" ", "", serial, fixed = TRUE)
my_location <- "hewatt10"

# Pre-2025; skipping 20 lines
d1 <- env_file_compile(my_files = files[1:5], my_skip = 20)
# Post-2025; skipping 21 lines
d2 <- env_file_compile(my_files = files[6:7], my_skip = 21)
d2 <- d2 |> 
  mutate(file_i = as.character(as.numeric(file_i) + 5))

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

##### LOAD PREVIOUS ENVLOGGER DATA ####
# Already compiled from env_big (process_env_04924E000C5503.R)
d0 <- read_csv(here("data_output", "envlogger_04924E000C5503.csv"))
d0
d |> distinct(file_i)

d0 <- d0 |> 
  mutate(file_i = 0)

d2 <- rbind(d0, d)
d2 %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line()

##### WRITE PROCESSED FILE ####
write_csv(d, here("data_output", paste("envlogger_", my_location, ".csv", sep = "")))
