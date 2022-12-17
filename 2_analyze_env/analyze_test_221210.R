################################################################################
##' @title Analyze envlogger data
##' @author Robin Elahi
##' @date 2022-12-16
##' @log 
################################################################################

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank()))

d <- read_csv(here("data_output", "envlogger_env_test_221210.csv"))
d

d %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line() + 
  facet_wrap(~ file_i, scales = "free_x")

d %>% count(file_i)

# These times for serial 0412 4400 DD3A 05 are f'd
d2 <- d %>% filter(!is.na(temp))
d2 %>% count(file_i)
d2 %>% filter(file_i == 4)
d2 %>% filter(file_i == 1)

# Label by sequence
d <- d %>% 
  filter(!is.na(temp)) %>% 
  group_by(serial) %>% 
  mutate(id = 1:n()) %>% 
  ungroup()

d %>% 
  filter(id > 20) %>% 
  ggplot(aes(id, temp, color = serial)) + 
  geom_line() 

#### Filter data ####
d <- d %>% 
  filter(id > 20)
d %>% count(serial)

#### Summarize data ####
time_summary <- d %>% 
  group_by(id) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 2 * se)  

d %>% 
  ggplot(aes(id, temp, color = serial)) + 
  geom_line(data = time_summary, aes(id, mean), inherit.aes = FALSE, 
            color = "grey70") + 
  geom_ribbon(data = time_summary, 
              aes(id, ymin = mean - 0.1, ymax = mean + 0.1), 
              inherit.aes = FALSE, fill = "grey70", alpha = 0.5) + 
  geom_line() + 
  theme(legend.position = "top")

serial_summary <- d %>% 
  filter(!is.na(temp)) %>% 
  group_by(serial) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 2 * se) 

d_summary <- d %>% 
  filter(!is.na(temp)) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 2 * se) 

temp_max <- max(d$temp) 
temp_min <- min(d$temp) 

serial_summary %>% 
  ggplot(aes(serial, mean)) + 
  geom_hline(data = d_summary, aes(yintercept = mean), color = "red", 
             linetype = "solid", lwd = 1) +
  geom_hline(data = d_summary, aes(yintercept = mean + 0.1), color = "red", 
             linetype = "solid", lwd = 0.7) + 
  geom_hline(data = d_summary, aes(yintercept = mean - 0.1), color = "red", 
             linetype = "solid", lwd = 0.7) +
  geom_hline(aes(yintercept = temp_max), 
             color = "gray", linetype = "dashed", lwd = 0.5) + 
  geom_hline(aes(yintercept = temp_min), 
             color = "gray", linetype = "dashed", lwd = 0.5) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean - CI, ymax = mean + CI), width = 0.2) + 
  labs(x = "Serial number", y = "Temperature (C)",
       caption = "mean +- 95% CI") + 
  coord_flip() + 
  ggtitle("Fridge bath, n = 125, frequency = 10min, 10 Dec 2022") +
  scale_y_continuous(limits = c(temp_min, temp_max))

ggsave(here("figs", "env_test_221210.pdf"), height = 3.5, width = 6)

