################################################################################
##' @title EnvLogger processing - by location and date
##' @author Robin Elahi
##' @date 2026-08-07
##' @log 
################################################################################

##' Location: 100m
##' Date: 260807
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
my_location <- "100m"
my_date <- "260807"
folder <- "data/collected_260807"
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

# At depth (70 minute dive)
time1 <- as_hms("09:50:00")
time2 <- as_hms("11:10:00")
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

# Ref - Deep
# started 100m deep at 10:19:00, finished transect at 10:31:50
time1 <- as_hms("10:19:00")
time2 <- as_hms("10:31:50")
d_deep <- d |>
  filter(time_pst > time1 & time_pst < time2)
d_deep |> 
  ggplot(aes(time_pst, temp, color = logger)) + 
  geom_line()

# Ref - Shallow
# started 100m shallow at 10:40:00, finished transect at 10:53:00
time1 <- as_hms("10:40:00")
time2 <- as_hms("10:53:00")
d_shallow <- d |>
  filter(time_pst > time1 & time_pst < time2)
d_shallow |> 
  ggplot(aes(time_pst, temp, color = logger)) + 
  geom_line()

# Calibration 1 (during 100m transect)
# 09:58-10:14
time1 <- as_hms("09:58:00")
time2 <- as_hms("10:10:00")
d_cal1 <- d |>
  filter(time_pst > time1 & time_pst < time2)
d_cal1 |> 
  ggplot(aes(time_pst, temp, color = logger)) + 
  geom_line()

# Calibration 2 (during return swim)
# 09:58-10:14
time1 <- as_hms("10:56:50")
time2 <- as_hms("11:12:00")
d_cal2 <- d |>
  filter(time_pst > time1 & time_pst < time2)
d_cal2 |> 
  ggplot(aes(time_pst, temp, color = logger)) + 
  geom_line()

# Calibration 3 
# 11:24:00-11:47:00 (rinse bin)
time1 <- as_hms("11:24:00")
time2 <- as_hms("11:47:00")
d_cal3 <- d |>
  filter(time_pst > time1 & time_pst < time2)
d_cal3 |> 
  ggplot(aes(time_pst, temp, color = logger)) + 
  geom_line()

# Combine the cals
d_cal <- rbind(d_cal1, d_cal2, d_cal3)

# Convert to wide
d_cal_wide <- d_cal |> 
  select(time_pst, logger, temp) |> 
  pivot_wider(names_from = logger, values_from = temp)

d_cal_wide |> 
  ggplot(aes(Reference, Mobile)) + 
  geom_abline(aes(slope = 1, intercept = 0)) + 
  geom_point() + 
  geom_smooth()

# Get calibration model
m_cal <- lm(Mobile ~ Reference, data = d_cal_wide)
summary(m_cal) 
# intercept is not sig different from 0 and within sensitivity of device (0.1C)
# don't need to adjust

##### MODEL REF-DEEP ####
# Convert to wide
d_sub <- d_deep
d_sub |> 
  ggplot(aes(logger, temp)) + 
  geom_jitter(height = 0.02, width = 0.05, alpha = 0.5)

d_wide <- d_sub |> 
  select(time_pst, logger, temp) |> 
  pivot_wider(names_from = logger, values_from = temp)

d_wide |> 
  ggplot(aes(Reference, Mobile)) + 
  geom_abline(aes(slope = 1, intercept = 0)) + 
  geom_jitter(width = 0.01, height = 0.01) + 
  geom_smooth(method = "lm")

# Get calibration model
m_deep <- lm(Mobile ~ Reference, data = d_wide)
summary(m_deep) 
t.test(d_wide$Reference, d_wide$Mobile, paired = TRUE)

##### MODEL REF-SHALLOW ####
# Convert to wide
d_sub <- d_shallow
d_sub |> 
  ggplot(aes(logger, temp)) + 
  geom_jitter(height = 0.02, width = 0.05, alpha = 0.5)

d_wide <- d_sub |> 
  select(time_pst, logger, temp) |> 
  pivot_wider(names_from = logger, values_from = temp)

d_wide |> 
  ggplot(aes(Reference, Mobile)) + 
  geom_abline(aes(slope = 1, intercept = 0)) + 
  geom_jitter(width = 0.01, height = 0.01) + 
  geom_smooth(method = "lm")

d_wide |> 
  ggplot(aes(Reference, Mobile)) + 
  geom_abline(aes(slope = 1, intercept = 0)) + 
  geom_jitter(width = 0.01, height = 0.01) + 
  geom_smooth(method = "lm")

# Get calibration model
m_shallow <- lm(Mobile ~ Reference, data = d_wide)
summary(m_shallow) 
t.test(d_wide$Reference, d_wide$Mobile, paired = TRUE)


##### WRITE PROCESSED FILE ####
write_csv(d, here("data_output", paste("envlogger_", my_location, ".csv", sep = "")))
