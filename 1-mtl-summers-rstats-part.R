### IN R - NOT RSTUDIO

install.packages("tidyverse")
install.packages("C:/Users/moukm/Documents/R/win-library/4.0/weathercan_0.3.4.tar.gz")

library(tidyverse) # As if you ever did something in R without it
library(weathercan)

stations_search("Montreal", interval = "day")

## Seems that the data I am interested in is split in two "stations", one from the 40s to 2013 and one from 2013 to today.
## stations ids 5415, 51157

## Although there's one from 2002 to 2020.
## stations id 30165

## 30165 - 2002 to 2020
## 5415 - 1940s to 2013
## 51157 - 2013 to today

## So after a gruesome experience I figured that the weathercan package, which was removed from CRAN, couldn't work completely in either RStudio 3.5 or the new one 4.0.
## So what follows was done in R, not RStudio. Anytime something's off in the latter and you tried a lot of stuff just try it in R, saved me a couple of times.

mtlsum30165 <- weather_dl(station_ids = "30165", start = "2002-12-23", end = "2020-08-08", interval = "day")

mtlsum5415 <- weather_dl(station_ids = "5415", start = "1980-01-01", end = "2013-02-13", interval = "day")

mtlsum51157 <- weather_dl(station_ids = "51157", start = "2013-02-18", end = "2020-08-08", interval = "day")

all_three_stations <- rbind(mtlsum30165, mtlsum51157, mtlsum30165)

saveRDS(all_three_stations, file = "mtlsummers3stations")
