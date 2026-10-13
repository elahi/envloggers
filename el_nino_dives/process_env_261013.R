################################################################################
##' @title EnvLogger processing - by location and date
##' @author Robin Elahi
##' @date 2026-10-13
##' @log 
################################################################################

##' Location: 170m and 170s
##' Date: 261013
##' Logger serial: env_11 049B 7D00 000D 0A-20260807 115918 - REFERENCE
##' Logger serial: env_12 0441 F100 3817 08-20260807 120045 - MOBILE
##' Note that before 2025, # of lines to skip is 20
##' After 2025, # of lines to skip is 21

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(hms)
source("R/envlogger_functions.R")

#### COMPILE FILES FROM A SINGLE FOLDER ####
# Get file names
my_location <- "170m"
my_date <- "261013"
folder <- "data/collected_261013"
files <- list.files(path = folder)
files
n_files <- length(files)

# File 1
i <- 1
serial <- env_file_parse(files[i])
serial2 <- gsub(" ", "", serial, fixed = TRUE)

d1 <- read_csv(here(folder, files[i]), skip = 21) %>% 
  mutate(serial = serial) |> 
  mutate(logger = "Mobile") # CHECK THIS!

# File 2
i <- 2
serial <- env_file_parse(files[i])
serial2 <- gsub(" ", "", serial, fixed = TRUE)

d2 <- read_csv(here(folder, files[i]), skip = 21) %>% 
  mutate(serial = serial) |> 
  mutate(logger = "Reference") # CHECK THIS!

# Compile and change timezone
# Daylight savings (-7); PST (-8)
time_change <- -7
d <- rbind(d1, d2) |> 
  mutate(date_time_pst = time + hours(time_change), 
         time_pst = as_hms(date_time_pst))

# Plot all data
d |> 
  ggplot(aes(time_pst, temp, color = logger)) + 
  geom_line()

# In seawater
time1 <- as_hms("09:20:00")
time2 <- as_hms("10:55:00")

# On bottom
# time1 <- as_hms("09:38:00")
# time2 <- as_hms("10:25:00")

d |> 
  filter(time_pst > time1 & time_pst < time2) |> 
  ggplot(aes(time_pst, temp, color = logger)) + 
  geom_line()

ggsave(paste("figs/", my_location, "_comparison_", my_date, "_a.jpg", sep = ""), height = 3, width = 5)
       
# Convert to wide
d_wide <- d |> 
  select(time_pst, logger, temp) |> 
  pivot_wider(names_from = logger, values_from = temp)

d_wide

##### SUBSET DATA ####

# 2026-10-13
# Timer: RE's dive computer (i200c) 
# 11 seconds slower than iphone

# env_11 049B 7D00 000D 0A-20260807 115918 - REFERENCE
# env_12 0441 F100 3817 08-20260807 120045 - MOBILE

# Dive start: 09:33 AM (REs dive computer synced with iPhone)

# 170m - cable
# placed reference env_logger (11) and mobile (12) at 5'
# start and end MLLW depths (ft): 24 (why so shallow?) and 31
# 170 duration: 7'-24'
# Mobile logger transit: 26-29

# 170m SHALLOW (30m on perpendicular transect, 240deg)
# Moved mobile logger (12) at 25'
# 170s duration: 30' to 48'
# start and end MLLW depths (ft): 30 (?) and 25

# collected both loggers at 51' 

# Dive end: 10:35 (61')

# Placed in rinse bin, then placed on table while showering (was in the sun, then I put the slate on it for shade)

# Ref - 040m Shallow
time1 <- as_hms("09:35:00")
time2 <- as_hms("10:30:00")
d_shallow <- d |>
  filter(time_pst > time1 & time_pst < time2)
d_shallow |> 
  ggplot(aes(time_pst, temp, color = logger)) + 
  geom_line()

d_shallow |> 
  ggplot(aes(time_pst, temp, color = logger)) + 
  geom_point() + 
  geom_smooth()
