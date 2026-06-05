################################################################################
##' @title EnvLogger processing - by location
##' @author Robin Elahi
##' @date 2026-06-05
##' @log 
################################################################################

my_location <- "cable000"
##' Location: HMS cable, 000m
##' Logger serial: 0412 4400 DD3A 05 (n = 2)
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
files <- list.files(path = "data/0412 4400 DD3A 05", pattern = "\\.csv")
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
  filter(time > as.POSIXct("2022-12-15 12:00:00")) |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

tail(d)
d |> 
  slice_tail(n = 500) |> 
  filter(time < as.POSIXct("2023-04-07 09:00:00")) |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

d_trimmed <- d |>
  filter(time > as.POSIXct("2022-12-15 12:00:00")) |> 
  filter(time < as.POSIXct("2023-04-07 09:00:00"))

d_trimmed |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

# Rename
d1 <- d_trimmed |> 
  mutate(file_i = as.character(1), 
         serial = serial)

#### DEPLOYMENT 2 ####
d <- env_file_load(i = 1, my_files = files[2], my_skip = 20)
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
d2 <- d_trimmed |> 
  mutate(file_i = as.character(2), 
         serial = serial)

#### DEPLOYMENT 3 ####
# Get file names
files <- list.files(path = "data/0419 6F00 263B 02", pattern = "\\.csv")
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
  filter(time > as.POSIXct("2024-07-09 12:00:00")) |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

tail(d)
d |> 
  slice_tail(n = 500) |> 
  filter(time < as.POSIXct("2025-09-12 09:00:00")) |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

d_trimmed <- d |>
  filter(time > as.POSIXct("2024-07-09 12:00:00")) |> 
  filter(time < as.POSIXct("2025-09-12 09:00:00"))
  
d_trimmed |> 
  ggplot(aes(time, temp)) + geom_point(alpha = 0.5)

# Rename
d3 <- d_trimmed |> 
  mutate(file_i = as.character(3), 
         serial = serial)

##### QUALITY CONTROL ####
d <- rbind(d1, d2, d3)

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
