################################################################################
##' @title EnvLogger processing - by location
##' @author Robin Elahi
##' @date 2026-06-08
##' @log 
################################################################################

my_location <- "cable140"
##' Location: HMS cable, 000m
##' Logger serial: 04A5 3700 374B 0F (n = 1)
##' Logger serial: 0419 6F00 263B 02 (n = 1)
##' Note that before 2025, # of lines to skip is 20
##' After 2025, # of lines to skip is 21
##' Filter after 12pm of deployment date (check that this is ok!)
##' Filter before 9am of collection date (check that this is ok!)

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
source("R/envlogger_functions.R")

#### DEPLOYMENT 1 ####

# Get file names
files <- list.files(path = "data/04A5 3700 374B 0F", pattern = "\\.csv")
files
i <- 2

# Folder with files
serial <- env_file_parse(files[i])
serial2 <- gsub(" ", "", serial, fixed = TRUE)

d <- env_file_load(i = i, my_files = files, my_skip = 20)
d |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

head(d)
d |> 
  slice_head(n = 500) |> 
  filter(time > as.POSIXct("2023-05-18 12:00:00")) |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

tail(d)
d |> 
  slice_tail(n = 500) |> 
  filter(time < as.POSIXct("2024-08-15 09:00:00")) |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

d_trimmed <- d |>
  filter(time > as.POSIXct("2023-05-18 12:00:00")) |> 
  filter(time < as.POSIXct("2024-08-15 09:00:00"))
  
d_trimmed |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

# Rename
d1 <- d_trimmed |> 
  mutate(file_i = as.character(1), 
         serial = serial)

#### DEPLOYMENT 2 ####
# Get file names
files <- list.files(path = "data/0481 A700 CA1E 05", pattern = "\\.csv")
files
i <- 1

# Folder with files
serial <- env_file_parse(files[i])
serial2 <- gsub(" ", "", serial, fixed = TRUE)

d <- env_file_load(i = 1, my_files = files[1], my_skip = 20)
d |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

head(d)
d |> 
  slice_head(n = 500) |> 
  filter(time > as.POSIXct("2024-07-24 12:00:00")) |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

tail(d)
d |> 
  slice_tail(n = 500) |> 
  filter(time < as.POSIXct("2025-09-12 09:00:00")) |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

d_trimmed <- d |>
  filter(time > as.POSIXct("2024-07-24 12:00:00")) |> 
  filter(time < as.POSIXct("2025-09-12 09:00:00"))
  
d_trimmed |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

# Rename
d2 <- d_trimmed |> 
  mutate(file_i = as.character(2), 
         serial = serial)

##### QUALITY CONTROL ####
d <- rbind(d1, d2)

# Check for duplicates
d %>% count(file_i, serial)

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
