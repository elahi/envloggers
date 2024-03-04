################################################################################
##' @title EnvLogger processing
##' @author Robin Elahi
##' @date 2023-02-21
##' @log 
##' 2024-03-03: added new files
################################################################################

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
source("R/envlogger_functions.R")

#### COMPILE FILES FROM A SINGLE FOLDER ####
# Get file names
files <- list.files(path = "data/04DF 7800 E606 09")
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

##### WRITE PROCESSED FILE ####
write_csv(d, here("data_output", paste("envlogger_", serial2, ".csv", sep = "")))
