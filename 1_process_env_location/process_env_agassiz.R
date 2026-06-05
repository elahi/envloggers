################################################################################
##' @title EnvLogger processing - by location
##' @author Robin Elahi
##' @date 2026-06-05
##' @log 
################################################################################

##' Location: Agassiz intertidal
##' Logger serial: 041C DE00 0B1A 0D
##' Note that before 2025, # of lines to skip is 20
##' After 2025, # of lines to skip is 21

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
source("R/envlogger_functions.R")

#### COMPILE FILES FROM A SINGLE FOLDER ####
# Get file names
files <- list.files(path = "data/041C DE00 0B1A 0D")
files
n_files <- length(files)
i <- 1

# Folder with files
serial <- env_file_parse(files[i])
serial2 <- gsub(" ", "", serial, fixed = TRUE)
my_location <- "agassiz"

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

##### WRITE PROCESSED FILE ####
write_csv(d, here("data_output", paste("envlogger_", my_location, ".csv", sep = "")))
