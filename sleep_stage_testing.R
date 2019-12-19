devtools::load_all()

library(tidyverse)
library(scales)
library(ggthemes)
library(grid)
library(lubridate)


FITBIT_KEY    <- ""
FITBIT_SECRET <- ""

token <- fitbitr::oauth_token()

date <- "2019-12-06"

### SLEEP STAGE TESTING ###

# from Eamon's sleep.r process
url_sleep <- paste0("https://api.fitbit.com/1.2/", "user/-/", "sleep/")

#' Get Sleep Logs
#'
#' Returns a summary and list of a user's sleep log entries as well as minute by minute sleep entry data for a given day in the format requested.
#' The response includes summary for all sleep log entries for the given day (including naps.)
#' If you need to fetch data only for the user's main sleep event, you can send the request with isMainSleep=true or use a Time Series call.
#'
#' The relationship between sleep log entry properties is expressed with the following equation:
#' timeInBed = minutesToFallAsleep + minutesAsleep + minutesAwake + minutesAfterWakeup
#'
#' Also, values for minuteData can be 1 ("asleep"), 2 ("awake"), or 3 ("really awake").
#'
#' @param token An OAuth 2.0 token
#' @param date	The date of records to be returned in the format "yyyy-MM-dd" or "today" as a string or Date object.
#' @return a list of "sleep" and "summary" data.frame. "sleep" data.frame contains time series information of sleep. "summary" data.frame contains the summary of each "sleep" information.
#' @export
#' 
#' NOTE: removed all but minimal sleep data capture/processing for GET request
get_sleep_logs <- function(token, date)
{
  date <- format_date(date)
  url <- paste0(url_sleep, sprintf("date/%s.json", date))
  data <- get(url, token)
}

# just get the raw sleep response
sleep_stages <- get_sleep_logs(token, date)


# save "long" data as df
sleep_stages_long <- sleep_stages$sleep$levels$data[[1]] %>% 
    mutate(dateTime = ymd_hms(dateTime))

# save short wakes as df
# process short wakes to get "long" df with all 30sec intervals for short wakes
sleep_stages_short <- sleep_stages$sleep$levels$shortData[[1]]  %>% 
  mutate(dateTime = ymd_hms(dateTime)) %>% 
  rename(short_level = level) %>% 
  mutate(endTime = dateTime + (seconds-30)) %>% 
  group_by(dateTime) %>% 
  mutate(date_time_seq = list(seq(dateTime, endTime, by='30 sec'))) %>%
  unnest() %>% 
  ungroup() %>% 
  select(short_level, date_time_seq, seconds)

# get start and end time for sleep period
sleep_start <- ymd_hms(sleep_stages$sleep$startTime)
sleep_end <- ymd_hms(sleep_stages$sleep$endTime)

# create "long" df with all 30sec intervals for sleep period with appropriate stage assignment
sleep_30s <- tibble(dateTime = seq(sleep_start, sleep_end, by = '30 sec')) %>% 
  left_join(., sleep_stages_long) %>% 
  fill(level) %>% 
  left_join(., sleep_stages_short, by = c("dateTime" = "date_time_seq")) %>% 
  mutate(sleep_stage = case_when(is.na(short_level) ~ level,
                                 short_level == "wake" ~ "wake"),
         sleep_stage = factor(sleep_stage, levels= c("deep", "light", "rem", "wake"), ordered = TRUE)
  )
  
# check to see stage sums match summary values  

sleep_30s %>% 
  group_by(sleep_stage) %>% 
  summarise(total = n()/2)

# quick plot
ggplot(sleep_30s, aes(dateTime, sleep_stage, color = factor(sleep_stage)))+
  geom_point() +
  theme_clean() +
  theme(legend.position = "none") +
  labs(title = "Fitbit Sleep Stages",
       subtitle = "Data generated via Fitbit intraday API",
       y = "Sleep Stage",
       x = "Time"
       )

# create numeric representation for short wakes
sleep_30s <- sleep_30s %>% 
  mutate(level = factor(level, levels= c("deep", "light", "rem", "wake"), ordered = TRUE),
         short_level_num = case_when(short_level == "wake" ~ 4)) 

# hypnogram attempt
ggplot(sleep_30s, aes(dateTime, as.numeric(level))) +
    geom_line(alpha = .75, linetype = "dashed") +
    geom_point(aes(color = level)) +
    scale_y_continuous(labels=c("1" = "Deep", "2" = "Light",
                             "3" = "REM", "4" = "Awake")) +
    geom_point(aes(dateTime, short_level_num), color = "yellow") +
    theme_clean() +
    theme(legend.position = "none") +
    labs(title = "Fitbit Sleep Stages",
         subtitle = "Data generated via Fitbit intraday API",
         y = "Sleep Stage",
         x = "Time")

sleep_30s %>% 
  mutate(level_num = as.numeric(sleep_stage))
