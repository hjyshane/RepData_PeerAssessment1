---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

``` r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 4.5.1
```

```
## Warning: package 'ggplot2' was built under R version 4.5.1
```

```
## Warning: package 'tibble' was built under R version 4.5.1
```

```
## Warning: package 'tidyr' was built under R version 4.5.1
```

```
## Warning: package 'readr' was built under R version 4.5.1
```

```
## Warning: package 'purrr' was built under R version 4.5.1
```

```
## Warning: package 'dplyr' was built under R version 4.5.1
```

```
## Warning: package 'stringr' was built under R version 4.5.1
```

```
## Warning: package 'forcats' was built under R version 4.5.1
```

```
## Warning: package 'lubridate' was built under R version 4.5.1
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.1.0     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```



## Loading and preprocessing the data

``` r
activity <- read.csv("./activity.csv", header = T)
```



## What is mean total number of steps taken per day?


``` r
daily <- activity %>%
  group_by(date) %>%
  summarise(total = sum(steps, na.rm = T))
mean_daily <- mean(daily$total)
hist(daily$total)
```

![](PA1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
mean(daily$total)
```

```
## [1] 9354.23
```

``` r
median(daily$total)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

``` r
interval <- activity %>%
  group_by(interval) %>%
  summarise(avg_step = mean(steps, na.rm = T))

plot(interval$interval, interval$avg_step, type = 'l')
```

![](PA1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
interval_max <- activity %>%
  group_by(interval) %>%
  summarise(Max = max(steps, na.rm = T))

interval_max[interval_max[['Max']] == max(interval_max$Max),]
```

```
## # A tibble: 1 × 2
##   interval   Max
##      <int> <int>
## 1      615   806
```




## Imputing missing values

``` r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

``` r
activity_nafill <- activity %>%
  group_by(date) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = T), steps)) %>%
  ungroup() %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = T), steps))

daily_nafill <- activity_nafill %>%
  group_by(date) %>%
  summarise(total = sum(steps, na.rm = T))
mean_daily_nafill <- mean(daily_nafill$total)
hist(daily_nafill$total)
```

![](PA1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

``` r
mean(daily_nafill$total)
```

```
## [1] 10766.19
```

``` r
median(daily_nafill$total)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

``` r
activity_wk <- activity_nafill %>%
  mutate(Weekdays = 
           ifelse(
             weekdays((as.Date(date))) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), yes = "weekday", no = "weekend")) %>%
  group_by(interval, Weekdays) %>%
  summarise(avg_steps = mean(steps, na.rm = T))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

``` r
ggplot(activity_wk, aes(x = interval, y = avg_steps)) +
  geom_line() +
  facet_wrap(~Weekdays, ncol = 1)
```

![](PA1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


