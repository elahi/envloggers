################################################################################
##' @title EnvLogger processing 
##' @author Robin Elahi
##' @date 2022-12-16
##' @log 
################################################################################

#### PACKAGES, DATA ####
library(here)
library(readxl)
library(tidyverse)
source("R/envlogger_functions.R")

env_names <- read_excel(path = here("data_output", "envlogger_names_221216.xlsx"))

#### COMPILE FILES FROM A SINGLE FOLDER ####
# Folder with files
my_folder <- "env_test_221210"

# Get file names
files <- list.files(path = here("data", my_folder))
files
n_files <- length(files)
i <- 1

# Join with envlogger names
d_serial <- tibble(
  serial = sapply(files, env_file_parse))
d_serial <- left_join(d_serial, env_names)

# Compile files
d <- env_file_compile2(folder = my_folder)

##### QUALITY CONTROL ####
# Check for duplicates
d %>% count(file_i)

summary(d)

d %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line() + 
  facet_wrap(~ serial, scales = "free_x")

d %>% 
  filter(!is.na(temp)) %>% 
  group_by(serial) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n), 
            ci = 2 * se) %>% 
  as.data.frame()

##### WRITE PROCESSED FILE ####
write_csv(d, here("data_output", paste("envlogger_", my_folder, ".csv", sep = "")))
