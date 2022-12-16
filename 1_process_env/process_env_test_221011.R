################################################################################
##' @title EnvLogger processing 
##' @author Robin Elahi
##' @date 2022-12-16
##' @log 
################################################################################

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
source("R/envlogger_functions.R")

#### COMPILE FILES FROM A SINGLE FOLDER ####
# Folder with files
my_folder <- "env_test_221011"

# Get file names
files <- list.files(path = here("data", my_folder))
files
n_files <- length(files)
i <- 1

# Save envlogger names
d_serial <- tibble(
  serial = sapply(files, env_file_parse))
d_serial <- d_serial %>%
  mutate(letter = c("F", "C", "B", "E", "D", "A"))
d_serial

# Compile files
d <- env_file_compile2(folder = my_folder)

##### QUALITY CONTROL ####
# Check for duplicates
d %>% count(file_i)

d %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line() + 
  facet_wrap(~ file_i)

d <- d %>% 
  filter(time > "2022-10-11 11:35:00.00" & time < "2022-10-11 14:40:00.00")

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
