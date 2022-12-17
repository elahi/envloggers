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

d <- read_csv(here("data_output", "envlogger_env_test_221011.csv"))
d

d %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line() + 
  facet_wrap(~ file_i)

#### Summarize data ####

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

temp_max <- max(d$temp) * 1.1
temp_min <- min(d$temp) * 0.9

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
  ggtitle("Ice bath, n = 18, frequency = 10min, 11 Oct 2022") +
  scale_y_continuous(limits = c(temp_min, temp_max))

ggsave(here("figs", "env_test_221011.pdf"), height = 3.5, width = 6)

