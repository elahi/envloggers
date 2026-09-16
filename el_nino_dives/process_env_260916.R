################################################################################
##' @title EnvLogger processing - by location and date
##' @author Robin Elahi
##' @date 2026-09-16
##' @log 
################################################################################

##' Location: 040m and 040s
##' Date: 260916
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
my_location <- "040m"
my_date <- "260916"
folder <- "data/collected_260916"
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
time1 <- as_hms("09:30:00")
time2 <- as_hms("10:40:00")

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

# 2026-09-16
# Timer: RE's dive computer (i200c) 
# 11 seconds slower than iphone
# Also Julia's computer

# env_11 049B 7D00 000D 0A-20260807 115918 - REFERENCE
# env_12 0441 F100 3817 08-20260807 120045 - MOBILE

# Dive start: 09:35 AM (REs dive computer synced with iPhone)

# 040m - cable
# placed reference env_logger (11) and mobile (12) at 9' (and minidot!)
# start and end MLLW depths (ft): 24 and 23
# Also, I collected 90m minidot, and placed next to ref logger (11) for the duration of both transect video surveys. 
# 9'-21'
# 09:42-09:57
# Mobile logger transit: 22-26

# 040m SHALLOW (30m on perpendicular transect, 260deg)
# Moved mobile logger (12) at 22'
# 040s duration: 26' to 39' (10:02-10:14)
# start and end MLLW depths (ft): 13 and 17

# collected both loggers at 42'. 

# Surfaced to check kelp chain at ~50', then descended for transit to Agassiz Beach
# Dive end: 10:38 (63')

# Placed in rinse bin together while showering

# Ref - 040m Shallow
time1 <- as_hms("10:05:00")
time2 <- as_hms("10:16:00")
d_shallow <- d |>
  filter(time_pst > time1 & time_pst < time2)
d_shallow |> 
  ggplot(aes(time_pst, temp, color = logger)) + 
  geom_line()
