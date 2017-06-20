Calculation of total daily precipitation using ASOS data: Example
================
Robert McDonald
June 19, 2017

What is this?
-------------

The purpose of this document is to illustrate the computation of daily precipitation totals using Automated Surface Observation System (ASOS) [data from Iowa State](https://mesonet.agron.iastate.edu/ASOS/).

You should be aware that the concept of a "daily precipitation total" is fraught. If you are interested, there is a presentation on the topic by Daryl Herzmann from Iowa State. I think the phrase "down the rabbit hole" occurs to everyone who looks into this topic.

History
-------

Official historical precipitation data, by weather station, is available from the [NOAA](https://www.ncdc.noaa.gov/cdo-web/). The system requires you to submit a request after which they email you a link. It is clunky but fairly quick.

Computing totals from ASOS data
-------------------------------

The ASOS data was obtained from [Iowa State](https://mesonet.agron.iastate.edu/request/download.phtml?network=NY_ASOS) by selecting the New York ASOS and then LGA.

<!-- https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=LGA&data=all&year1=2013&month1=5&day1=30&year2=2013&month2=8&day2=1&tz=Etc%2FUTC&format=comma&latlon=no&direct=no&report_type=1&report_type=2 -->
There are two important things to realize when using the ASOS data, which is hourly.

1.  The reported precipitation number is cumulative, up to a reset time which is typically between 51 and 57 minutes (thank you Daryl!) So the hourly total is this final value before the reset.

2.  The daily total is the sum of the hourly values, where a day is midnight to midnight, local *standard* time. Note that DST is not observed for this purpose. This makes sense, as the days on which DST is adopted and removed would otherwise create either an hour hole or a double-counted hour.

One complication is that the reset minute can vary by site. Although I knew that for LGA the reset occurs at 51 minutes, this is not generally true. So in the code below I compute the `modalminute`, which is the most frequently occurring minute in the data. I assume that this is the reset time. Because R has no function for computing the mode, I use the `table` function to find the most common value for minute.

Note also that I specify the time zone as "America/New\_York" and then undo DST by using the `dst` function to see if daylight savings time is in effect, and then if so, subtracting 3600 seconds from the time.

A final note: on two days, the offical precipitation total is off by 0.01", and this is corrected the next day. I am not sure why this happens.

``` r
## The following is based in part on Hadley Wickham's 
## weather.r in the R package nycflights13
## 
x = read_csv('data/asos_lga_2013-06-01_2013-08-01.csv')
tmp = x %>% 
  select(station, valid, p01i) %>% 
  mutate(date = with_tz(as.POSIXct(valid, tz='UCT'),
                       "America/New_York"),
         date = date-dst(date)*3600,
         year = year(date),
         month = month(date),
         day = day(date),
         hour = hour(date),
         minute = minute(date)) %T>% 
         {modalminute <<- 
           as.numeric(names(sort(table(.$minute), 
                                 decreasing=TRUE))[1])} %>% 
  group_by(year, month, day, hour) %>% 
  filter(minute <= modalminute) %>% 
  summarize(hourlyprecip = max(p01i)) %>% 
  group_by(year, month, day) %>% 
  summarize('Daily Precipitation (ASOS)' = sum(hourlyprecip)) %>% 
  filter(year==2013, month==6) %>% 
  left_join(lga %>% select(Historical:day))
kable(tmp)
```

|  year|  month|  day|  Daily Precipitation (ASOS)|  Historical|
|-----:|------:|----:|---------------------------:|-----------:|
|  2013|      6|    1|                        0.00|        0.00|
|  2013|      6|    2|                        0.11|        0.11|
|  2013|      6|    3|                        0.96|        0.96|
|  2013|      6|    4|                        0.00|        0.00|
|  2013|      6|    5|                        0.00|        0.00|
|  2013|      6|    6|                        0.10|        0.10|
|  2013|      6|    7|                        3.32|        3.33|
|  2013|      6|    8|                        0.80|        0.79|
|  2013|      6|    9|                        0.00|        0.00|
|  2013|      6|   10|                        1.25|        1.25|
|  2013|      6|   11|                        0.14|        0.14|
|  2013|      6|   12|                        0.00|        0.00|
|  2013|      6|   13|                        0.87|        0.88|
|  2013|      6|   14|                        0.26|        0.25|
|  2013|      6|   15|                        0.00|        0.00|
|  2013|      6|   16|                        0.00|        0.00|
|  2013|      6|   17|                        0.00|        0.00|
|  2013|      6|   18|                        0.20|        0.20|
|  2013|      6|   19|                        0.00|        0.00|
|  2013|      6|   20|                        0.00|        0.00|
|  2013|      6|   21|                        0.00|        0.00|
|  2013|      6|   22|                        0.00|        0.00|
|  2013|      6|   23|                        0.00|        0.00|
|  2013|      6|   24|                        0.00|        0.00|
|  2013|      6|   25|                        0.01|        0.01|
|  2013|      6|   26|                        0.01|        0.01|
|  2013|      6|   27|                        0.13|        0.13|
|  2013|      6|   28|                        0.00|        0.00|
|  2013|      6|   29|                        0.00|        0.00|
|  2013|      6|   30|                        0.00|        0.00|
