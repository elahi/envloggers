################################################################################
##' @title Analyze envlogger data
##' @author Robin Elahi
##' @date 2023-02-21
##' @log 
################################################################################

## 0460 1700 1840 09 (Logger E)
# Deployed
# 15 Dec 2022 - deployed to 1m below the surface buoy at 100m on the HMLR cable. 
# I dove on 26 Jan 2023 with Kersten, and the buoy was still attached. But sometime between then, and 10 (?) Feb 2023, the surface buoy was gone - pulled out by winter storms. 
# Recovered
# 17 Feb 2023 - logger was recovered by Jennifer Yin; buoy was tangled in MacAbee kelp. 
# I received it around 1pm, and placed it in freshwater rinse with minidot till 5:30pm. Then I took them both home to download. 

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 10) + 
            theme(strip.background = element_blank()))

d <- read_csv(here("data_output", "envlogger_04601700184009.csv"))
d <- d %>% 
  mutate(date = date(time))

d %>% 
  filter(date > "2022-12-15") %>% 
  filter(date < "2023-02-17") %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line() + 
  scale_y_continuous(limits = c(10, 15)) + 
  geom_vline(xintercept = as.numeric(ymd_hms("2023-01-26 12:00:00"))) # saw buoy on a dive


#### Filter data ####
d <- d %>% 
  filter(date > "2022-12-15") %>% 
  filter(date < "2023-02-17")
d %>% count(serial)

#### Summarize data ####
time_summary <- d %>% 
  group_by(date) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 2 * se, 
            CV = sd / mean) %>% 
  mutate(time = as_datetime(paste(date, "12:00:00", sep = " ")))

d %>% 
  ggplot(aes(time, temp)) + 
  geom_line(color = "pink", alpha = 1, size = 0.5) + 
  geom_line(data = time_summary, aes(time, mean), inherit.aes = FALSE) + 
  geom_ribbon(data = time_summary, 
              aes(time, ymin = mean - sd, ymax = mean + sd), 
              inherit.aes = FALSE, alpha = 0.5) + 
  theme(legend.position = "top") + 
  ylab(expression(paste("Temperature (", degree, "C)"))) + 
  scale_y_continuous(limits = c(11, 14)) + 
  labs(x = "Date", caption = "black line: daily mean with 1 SD uncertainty", 
       title = "1m below surface at 100m buoy, then MacAbee")
  
ggsave(here("figs", "hms_surface_221215.pdf"), height = 3.5, width = 5)

time_summary %>% 
  ggplot(aes(time, CV)) + 
  geom_line(color = "pink")
  

