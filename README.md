Calculation of total daily precipitation using ASOS data: Example
================
Robert McDonald
July 2, 2017

Introduction
------------

The purpose of this document is to illustrate the computation of daily precipitation totals using Automated Surface Observation System (ASOS) [data from Iowa State](https://mesonet.agron.iastate.edu/ASOS/). (This data is the source for the `weather` dataframe in the `nycflights13` R package.)

You should be aware that the concept of a "daily precipitation total" is fraught. If you are interested, there is a [presentation on the topic](https://mesonet.agron.iastate.edu/present/130903_isu/isumet_fall2013_web.pdf) by Daryl Herzmann from Iowa State. I think the phrase "down the rabbit hole" occurs to everyone who looks into this topic.

Official NOAA Daily Precipitation Totals
----------------------------------------

Official historical daily precipitation data, by weather station, is available from the National Oceanic and Atmospheric Administration ([NOAA](https://www.ncdc.noaa.gov/cdo-web/)). The system requires you to submit a request after which they email you a link. It is clunky but fairly quick.

Computing LGA precipitation totals from ASOS data
-------------------------------------------------

The ASOS data was obtained from [Iowa State](https://mesonet.agron.iastate.edu/request/download.phtml?network=NY_ASOS) by selecting the New York ASOS and then LGA.

<!-- https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=LGA&data=all&year1=2013&month1=5&day1=30&year2=2013&month2=8&day2=1&tz=Etc%2FUTC&format=comma&latlon=no&direct=no&report_type=1&report_type=2 -->
There are a number of issues that must be considered when interpreting the ASOS data.

1.  The reported precipitation number is cumulative for an hour, up to a reset minute which is typically between 51 and 57 minutes (thank you Daryl!) So the hourly total is the final value before the reset.

2.  The reset minute can vary by site. It is obvious by inspection that the LGA reset occurs at 51 minutes, but this is not true for all ASOS.

3.  The reported daily total (available from the NOAA site above and widely reported as the official number) is the sum of the hourly values, where a day is defined as midnight to midnight, local *standard* time. Daylight savings time (DST) is not observed for this purpose. Ignoring DST makes sense, as the days on which DST is adopted and removed would otherwise create either an hour hole or a double-counted hour.

4.  Beginning in 2017, there is typically, but not always, a record every 5 minutes, plus a record for the precipitation reset minute. Prior to 2017, there was most commonly only a record for the reset minute.

5.  There is sometimes no record for the reset minute. For example, the last record in the hour for LGA occurs at 2013-06-08T01:49:00Z. Precipitation for that hour was 0.24".

The algorithm to compute daily precipitation has to take into account all of these issues. In the code below, for each hour I compute `maxminute`, which is the minute at which maximum precipitation for the hour is reported. I then compute `modalminute`, which is the most frequent `maxminute` in the data. I assume that `modalminute` is the reset minute.

I use the `which.max` function to find `maxminute`, but there is a subtlety. The `which.max` function will pick out the row with the maximum precipitation if there is a unique maximum, otherwise it will pick out the first row reporting the maximum value. At those times when there are reports for both 50 and 51 minutes, 50 minutes will almost always be selected. However, there are also times when there are no reports for 50 minutes. The result is that unless the data is sorted in descending order by minute, 50 rather than 51 minutes will be selected as the most common minute at which maximum precipitation is recorded, and hours without records at 50 minutes can miss significant amounts of precipitation. There are also times when precipitation at 51 minutes is greater than that at 50 minutes.

Note also that we can't simply use the precipitation amount reported at `maxminute`, because there could be precipitation reported at 55 minutes and not at 51 minutes. Since precipitation at 55 minutes will be included in the next hour's total, we would double-count that amount.

Once `maxminute` is computed for each hour, the mode of `maxminute` should be the reset minute. R has no function for computing the mode, so I use the `tabulate` function together with `which.max` to find the most common value for minute. This is `modalminute`.

I specify the time zone for LGA as "America/New\_York" and then undo DST by using the `dst` function to see if daylight savings time is in effect, and then if so, subtracting 3600 seconds from the time.

If you are interested in the issues that arise when measuring precipitation, [be sure to take a look at this presentation.](https://mesonet.agron.iastate.edu/present/130903_isu/isumet_fall2013_web.pdf)

``` r
## This was inspired by Hadley Wickham's 
## weather.r in the R package nycflights13
#x = read_csv('data/asos_lga_2013-06-01_2013-08-01.csv'
x = read_csv('data/asos_lga_2017-03-31_2017-06-02.csv',
             skip=5, na='M')
precip = x %>% 
  filter(p01i != 'M') %>% 
  select(station, valid, p01i) %>% 
  mutate(date = with_tz(as.POSIXct(valid, tz='UCT'),
                       "America/New_York"),
         date = date-dst(date)*3600,
         year = year(date),
         month = month(date),
         day = day(date),
         hour = hour(date),
         minute = minute(date)) %>% 
  group_by(year, month, day, hour) 

modalminute = precip %>%
  arrange(desc(minute), .by_group=TRUE) %>% 
  mutate(maxminute = minute[which.max(p01i)]) %>% 
  {which.max(tabulate(.$maxminute))}

precip = precip %>% 
  filter(minute <= modalminute) %>% 
  summarize(hourlyprecip = max(p01i, na.rm=TRUE)) %>% 
  group_by(year, month, day) %>% 
  summarize('Daily Precipitation (ASOS)' = 
              sum(hourlyprecip, na.rm=TRUE)) %>% 
  filter(year==yr, month==mth) %>% 
  left_join(lga %>% select(Historical:day))
```

Comparison of NOAA and ASOS daily precipitation totals
------------------------------------------------------

You can see in the table below that the ASOS totals, computed above, exactly match the NOAA daily precipitation totals. At times, though not in May 2017, the ASOS total differs from the NOAA total by 0.01", and this difference is corrected the next day. I am not sure why this happens. This suggests that NOAA is using more precisely timed precipitation measures.

|  year|  month|  day|  Daily Precipitation (ASOS)|  Historical|
|-----:|------:|----:|---------------------------:|-----------:|
|  2017|      5|    1|                        0.00|        0.00|
|  2017|      5|    2|                        0.00|        0.00|
|  2017|      5|    3|                        0.00|        0.00|
|  2017|      5|    4|                        0.00|        0.00|
|  2017|      5|    5|                        2.27|        2.27|
|  2017|      5|    6|                        0.06|        0.06|
|  2017|      5|    7|                        0.00|        0.00|
|  2017|      5|    8|                        0.00|        0.00|
|  2017|      5|    9|                        0.00|        0.00|
|  2017|      5|   10|                        0.00|        0.00|
|  2017|      5|   11|                        0.00|        0.00|
|  2017|      5|   12|                        0.00|        0.00|
|  2017|      5|   13|                        1.74|        1.74|
|  2017|      5|   14|                        0.01|        0.01|
|  2017|      5|   15|                        0.00|        0.00|
|  2017|      5|   16|                        0.00|        0.00|
|  2017|      5|   17|                        0.00|        0.00|
|  2017|      5|   18|                        0.00|        0.00|
|  2017|      5|   19|                        0.00|        0.00|
|  2017|      5|   20|                        0.01|        0.01|
|  2017|      5|   21|                        0.00|        0.00|
|  2017|      5|   22|                        0.64|        0.64|
|  2017|      5|   23|                        0.00|        0.00|
|  2017|      5|   24|                        0.03|        0.03|
|  2017|      5|   25|                        0.87|        0.87|
|  2017|      5|   26|                        0.12|        0.12|
|  2017|      5|   27|                        0.00|        0.00|
|  2017|      5|   28|                        0.00|        0.00|
|  2017|      5|   29|                        0.16|        0.16|
|  2017|      5|   30|                        0.03|        0.03|
|  2017|      5|   31|                        0.05|        0.05|
