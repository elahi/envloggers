################################################################################
##' @title EnvLogger processing 
##' @author Robin Elahi
##' @date 2024-05-04
##' @log 
################################################################################

#### PACKAGES, DATA ####
library(here)
library(readxl)
library(tidyverse)
source("R/envlogger_functions.R")


theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid.minor = element_blank(), 
                  strip.background = element_blank()))

#### LOGGER METADATA ####

env_names <- read_excel(path = here("data", "env_plankton_240503/env_plankton_240503.xlsx"))

env_names <- env_names %>% 
  mutate(serial = sapply(x, extract_serial))

env_names

#### COMPILE FILES FROM A SINGLE FOLDER ####
# Folder with files
my_folder <- "env_plankton_240503"

# Get file names
files <- list.files(path = here("data", my_folder), pattern = "\\.csv$")
files

# Get length of csv files
n_files <- length(files) 
i <- 1

# Join with envlogger names
d_serial <- tibble(
  serial = sapply(files, env_file_parse))
d_serial

d_serial <- left_join(env_names, d_serial, by = "serial")
d_serial

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

##### JOIN TO METADATA #####

d2 <- d_serial %>% 
  select(depth_m, serial) %>% 
  left_join(d, d_serial, by = "serial") 

d2
glimpse(d2)

d2 %>% 
  ggplot(aes(time, temp, color = depth_m, group = file_i)) + 
  geom_line() + 
  theme_bw()

# Filter to immersed temperatures

time_min <- as_datetime('2024-05-03 16:35:00')
time_max <- as_datetime('2024-05-03 17:15:00')

d2 %>% 
  filter(time > time_min & time < time_max) %>% 
  ggplot(aes(time, temp, color = depth_m, group = file_i)) + 
  geom_line() + 
  scale_color_continuous(name = "Depth (m)") + 
  labs(x = "Time", y = "Temperature (C)", 
       title = "MacAbee vertical tow, 3 May 2024")

ggsave(here("figs", paste(my_folder, "_time_series", ".pdf", sep = "")), 
       height = 3, width = 4)

##### WRITE PROCESSED FILE ####
write_csv(d2, here("data_output", paste("envlogger_", my_folder, ".csv", sep = "")))
