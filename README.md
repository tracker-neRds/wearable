
<!-- README.md is generated from README.Rmd. Please edit that file -->
wearable
========

<!-- ![](https://travis-ci.org/tracker-neRds/wearable.svg?branch=master) -->
<!-- [![CRAN Version](http://www.r-pkg.org/badges/version/wearable)](http://cran.rstudio.com/web/packages/wearable)  -->
<!-- ![](http://cranlogs.r-pkg.org/badges/grand-total/wearable) -->
The `wearable` package allows users to interact with activity tracker data in R using device API.

Currently this package allows for read/write functionality for interacting with personal Fitbit data.

Installation
------------

wearable isn't available from CRAN yet, but you can get it from github with:

``` r
# install.packages("devtools")
devtools::install_github("tracker-neRds/wearable")
```

Preparation
-----------

### API key

To get your own token (API key), you have to register your own application in [here](https://dev.fitbit.com/apps/new). For your reference, we share our setting:

![](man/figures/register_app.png)

After registration, you can get your own `FITBIT_KEY` and `FITBIT_SECRET` (referred to as **OAuth 2.0 Client ID** and **Client Secret** in the next figure).

![](man/figures/manage_my_apps.png)

If you set the following variables as a global variable, this package will use these values for API key.

``` r
# As a global variable
FITBIT_KEY    <- "<your-fitbit-key>"
FITBIT_SECRET <- "<your-firbit-secret>"
# If you want, Default: "http://localhost:1410/"
# FITBIT_CALLBACK <- "<your-fitbit-callback>" 
```

Or, you can set these values as a environment variable

``` r
Sys.setenv(FITBIT_KEY = "<your-fitbit-key>", FITBIT_SECRET = "<your-firbit-secret>")
```

<!-- ### Load libraries -->
<!-- ```{r} -->
<!-- #library("wearable") -->
<!-- #library("tidyvserse") # for visualization in this document -->
<!-- ``` -->
<!-- ### Get Fitbit API token -->
<!-- You can get your Fitbit toekn using `wearable::oauth_token()`: -->
<!-- ```{r, eval=FALSE} -->
<!-- # Get token -->
<!-- token <- wearable::oauth_token() -->
<!-- # Get token -->
<!-- token <- wearable::oauth_token(language="en_US") -->
<!-- # Get token -->
<!-- token <- wearable::oauth_token(language="en_GB") -->
<!-- ``` -->
<!-- This function open a web browser autmatically and return Fitbit token. -->
<!-- ```{r, echo=FALSE, cache=FALSE} -->
<!-- #Adhoc to build this document on my PC... -->
<!-- #saveRDS(token, "temp_token") -->
<!-- token <- readRDS("temp_token") -->
<!-- ``` -->
<!-- ## How to use -->
<!-- ### Activity -->
<!-- ```{r} -->
<!-- # Example date -->
<!-- date <- "2017-12-29" -->
<!-- # Get daily activity summary -->
<!-- str(get_activity_summary(token, date)) -->
<!-- # Get Activity Time Series -->
<!-- get_activity_time_series(token, "distance", date=date, period="7d") -->
<!-- # Get activity intraday time series -->
<!-- # You have to use a **personal** key and secret. -->
<!-- df <- get_activity_intraday_time_series(token, "steps", date, detail_level="15min") -->
<!-- df$time <- as.POSIXct(strptime(paste0(df$dateTime, " ", df$dataset_time), "%Y-%m-%d %H:%M:%S")) -->
<!-- ggplot2::ggplot(df, aes(x=time, y=dataset_value)) + geom_line() -->
<!-- # Get Activity Types (complicated nested list) -->
<!-- length(get_activity_types(token)) -->
<!-- # Get Activity Type (Yoga=52001) -->
<!-- get_activity_type(token, 52001) -->
<!-- # Get Frequent Activities -->
<!-- get_frequent_activities(token) -->
<!-- # Get Recent Activities -->
<!-- get_recent_activity_types(token) -->
<!-- # Add, get and delete favorite activities -->
<!-- add_favorite_activity(token, 52001) -->
<!-- get_favorite_activities(token) -->
<!-- delete_favorite_activity(token, 52001) -->
<!-- get_favorite_activities(token) -->
<!-- get_activity_goals(token, period="daily") -->
<!-- update_activity_goals(token, period="daily", distance=11.3) -->
<!-- get_activity_goals(token, period="daily") -->
<!-- get_activity_goals(token, period="weekly") -->
<!-- update_activity_goals(token, period="weekly", steps="100000") -->
<!-- get_activity_goals(token, period="weekly") -->
<!-- # Get Lifetime Stats -->
<!-- get_lifetime_stats(token) -->
<!-- ``` -->
<!-- You can find more details in [here](https://dev.fitbit.com/docs/activity/) -->
<!-- ### Heart Rate -->
<!-- ```{r} -->
<!-- # Set a date for example -->
<!-- date <- "2017-12-25" -->
<!-- # Get heart rate time series -->
<!-- df <- get_heart_rate_time_series(token, date=date, period="7d") -->
<!-- ggplot(df, aes(x=date, y=peak_max)) + geom_line() -->
<!-- # Get intraday heart rate time series -->
<!-- df <- get_heart_rate_intraday_time_series(token, date=date, detail_level="15min") -->
<!-- df$time <- as.POSIXct(paste0(date, " ", df$time)) -->
<!-- ggplot(df, aes(x=time, y=value)) + geom_line() -->
<!-- ``` -->
<!-- You can find more details [here](https://dev.fitbit.com/docs/heart-rate/). -->
<!-- ### Sleep -->
<!-- ```{r} -->
<!-- # Get Sleep Logs(date is character or Date) -->
<!-- x <- get_sleep_logs(token, "2016-03-30") -->
<!-- print(head(x$sleep)) -->
<!-- x$summary -->
<!-- #Get the current sleep goal. -->
<!-- get_sleep_goal(token) -->
<!-- #Update sleep goal -->
<!-- update_sleep_goal(token, 380) -->
<!-- #Get Sleep Time Series -->
<!-- get_sleep_time_series(token, "timeInBed", date="2016-04-02", period="7d") -->
<!-- get_sleep_time_series(token, "efficiency", base_date="2016-03-30", end_date="2016-03-30") -->
<!-- #Log sleep -->
<!-- log <- log_sleep(token, "22:00", 180, date="2010-04-18") -->
<!-- print(head(log)) -->
<!-- #Delete sleep log -->
<!-- delete_sleep_log(token, log$logId) -->
<!-- ``` -->
<!-- You can find more details [here](https://dev.fitbit.com/docs/sleep/). -->
<!-- ### Devices  -->
<!-- ```{r} -->
<!-- # Get deice information you registerd -->
<!-- get_devices(token) -->
<!-- ``` -->
<!-- ```{r} -->
<!-- # Add alarms -->
<!-- tracker_id <- get_devices(token)$id[1] -->
<!-- add_alarm(token, tracker_id, "07:15-08:00", "MONDAY") -->
<!-- alarm <- get_alarms(token, tracker_id) -->
<!-- alarm -->
<!-- ``` -->
<!-- ```{r} -->
<!-- # Update the content alarm -->
<!-- alarm_id <- tail(alarm, 1)$alarmId -->
<!-- update_alarm(token, tracker_id, alarm_id, "02:15-03:00", "FRIDAY") -->
<!-- get_alarms(token, tracker_id) -->
<!-- ``` -->
<!-- ```{r} -->
<!-- # Delete alarm you registered here -->
<!-- delete_alarm(token, tracker_id, alarm_id) -->
<!-- get_alarms(token, tracker_id) -->
<!-- ``` -->
<!-- You can find more details [here](https://dev.fitbit.com/docs/devices/). -->
<!-- ## Contributing -->
<!-- - Fork it ( https://github.com/teramonagi/wearable/fork ) -->
<!-- - Create your feature branch (git checkout -b my-new-feature) -->
<!-- - Commit your changes (git commit -am 'Add some feature') -->
<!-- - Push to the branch (git push origin my-new-feature) -->
<!-- - Create a new Pull Request -->
<!-- ## Acknowledgements -->
<!-- This package has is based on extending and improving the [fitbitr](https://github.com/teramonagi/fitbitr/) package from [Nagi Teramo](https://github.com/teramonagi). -->
<!-- Additional thanks to dichika as the fitbitr package is based on the [myFitbit package](https://github.com/dichika/myFitbit). -->
<!-- <!-- -->
<!-- Future implementation -->
<!-- ### Food Logging -->
<!-- You can find more details [here](https://dev.fitbit.com/docs/food-logging/). -->
<!-- ### Friends -->
<!-- You can find more details [here](https://dev.fitbit.com/docs/friends/). -->
<!-- ### Subscriptions -->
<!-- - https://dev.fitbit.com/docs/subscriptions/ -->
<!-- ### User -->
<!-- - https://dev.fitbit.com/docs/user/ -->
<!-- ### Body & Weight -->
<!-- You can find more details in [here](https://dev.fitbit.com/docs/body/) -->
<!-- -->
--&gt;
