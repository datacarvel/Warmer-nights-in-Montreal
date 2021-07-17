library(tidyverse)
library(devtools)
library(ggcal)

## I have data now, but omg it's such a hassle to look at it in good old regular R.
## So obviously we'll bring it all here in RStudio.

mtlsummers <- readRDS(file = "C:/Program Files/R/R-4.0.2/bin/mtlsummers3stations.rds")

mtlsummers_selected <- select(mtlsummers, station_id, date, year, month, day, min_temp, mean_temp, max_temp)

## devtools::install_github("jayjacobs/ggcal") # Select 3 (None) if asked to update other packages - jeez, this project really is starting on an awkward foot

str(mtlsummers_selected) # Making sure my dates are into a proper date format - they are here !

# For now I am interested in the months of May through October only

mtlsummers_clean <- filter(mtlsummers_selected, month %in% c("05", "06", "07", "08", "09", "10"))

mtlsummers_clean30165 <- filter(mtlsummers_clean, station_id == 30165)
mtlsummers_clean5415 <- filter(mtlsummers_clean, station_id == 5415)
mtlsummers_clean51157 <- filter(mtlsummers_clean, station_id == 51157)

## 30165 - 2002 to 2020
## 5415 - 1940s to 2013
## 51157 - 2013 to today

library(ggcal)

### Issues : there are a few major gaps in data, I am guessing those 3 stations complete each other. 
# I'd like to know first which stations has the most gaps.

sum(is.na(mtlsummers_clean30165$max_temp)) / nrow(mtlsummers_clean30165) # 22 % of NA
sum(is.na(mtlsummers_clean5415$max_temp)) / nrow(mtlsummers_clean5415) # Less than 0.005 % NA
sum(is.na(mtlsummers_clean51157$max_temp)) / nrow(mtlsummers_clean51157) # 1.4 % NA

### So to fill the gaps - remember it's all at the Montreal International Airport - I'd go in that order :
# 5415 - 1940s to 2013
# 51157 - 2013 to today
# 30165 - 2002 to 2020
# The first two can be merged with no overlapping dates, while the third one will. Of course, we'll make sure of that before doing anything

head(mtlsummers_clean51157) # First date is May 1st, 2013
tail(mtlsummers_clean5415) # Last date is October 31, 2012

mtlsum_first_two <- bind_rows(mtlsummers_clean51157, mtlsummers_clean5415)

mtlsum_all_three <- left_join(mtlsum_first_two, mtlsummers_clean30165, by = "date", suffix = c("", "30165"))

head(mtlsum_all_three) # Looking good

### So now I want my NA of my first 2 datasets to be filled with the non-NA temps found in the third dataset

sum(is.na(mtlsum_all_three$max_temp)) # 49 NAs after joining datasets

mtlsum_all_three_filled <- mtlsum_all_three %>%
  mutate(all_max_temp = coalesce(max_temp, max_temp30165)) %>%
  mutate(all_min_temp = coalesce(min_temp, min_temp30165)) 

test <- coalesce(mtlsum_all_three$max_temp, mtlsum_all_three$max_temp30165)
  # 29 after taking values for the 30165 station into the 2 first non-overlapping, joined datasets

# Just making sure here I understood what I did in the piped operation
# Coalesce is useful for completing a dataset from missing pieces. All NA will be replaced with the non-NA if available, and it works both way.
# If there is a conflict between two values that aren't NA, the first dataset's value will be preserved (x in coalesce(x, y)), which is exactly what I want !

Wario1 <- c(0, 1, 2, 3, NA) # I like Wario
Wario2 <- c(NA, 1, 2, 4, 4)
coalesce(Wario1, Wario2)
# result : 0 1 2 3 4

# This below is only me checking whether everything went as desired since I am using functions I never used before, but it's a bit hard to follow, I infliged significant self-doubt to myself a few times.
# In other words, you may skip that

z <- coalesce(mtlsum_all_three$max_temp, mtlsum_all_three$max_temp30165)
sum(is.na(z)) # Same result
sum(is.na(mtlsum_all_three_filled$all_max_temp)) # Same result
IsAllGood <- mtlsum_all_three_filled$max_temp == mtlsum_all_three_filled$all_max_temp # There should only be TRUE and NA here, no FALSE, which means the first values we want to keep were preserved
sum(is.na(mtlsum_all_three_filled$all_max_temp)) # 29 NA
sum(is.na(mtlsum_all_three_filled$max_temp)) # 49 NA
sum(is.na(IsAllGood)) 
unique(IsAllGood) # Only TRUE and NA, no FALSE at all !

# We're all good to go here! So yes, all this for only 20 more days of data. 
# The 29 missing days are all before 2001, so the third dataset isn't helping here - found that using which() and then viewing the highest row number like so x[3819,]

#############

### Here is the ggcal function as written in the ggcal package but with a few slight edits of my own so my plot can show up the way I want it

ggcal <- function(dates, fills) {
  # get ordered vector of month names
  months <- format(seq(as.Date("2016-05-01"), as.Date("2016-10-01"), by="1 month"), "%B")
  
  # get lower and upper bound to fill in missing values
  mindate <- as.Date(format(min(dates), "%Y-%m-01"))
  maxdate <- (seq(as.Date(format(max(dates), "%Y-%m-01")), length.out = 2, by="1 month")-1)[2]
  # set up tibble with all the dates.
  filler <- tibble(date = seq(mindate, maxdate, by="1 day"))
  
  t1 <- tibble(date = dates, fill=fills) %>%
    #right_join(filler, by="date") %>% # fill in missing dates with NA [SC] # This was giving me an extra NA month - gotcha!
    mutate(dow = as.numeric(format(date, "%w"))) %>%
    mutate(month = format(date, "%B")) %>%
    mutate(woy = as.numeric(format(date, "%U"))) %>%
    mutate(year = as.numeric(format(date, "%Y"))) %>%
    mutate(month = factor(month, levels=months, ordered=TRUE)) %>%
    arrange(year, month) %>%
    mutate(monlabel=month)
  
  if (length(unique(t1$year))>1) { # multi-year data set
    t1$monlabel <- paste(t1$month, t1$year)
  }
  
  t2 <- t1 %>%
    mutate(monlabel = factor(monlabel, ordered=TRUE)) %>%
    mutate(monlabel = fct_inorder(monlabel)) %>%
    mutate(monthweek = woy-min(woy),
           y=max(monthweek)-monthweek+1)
  
  weekdays <- c("S", "M", "T", "W", "T", "F", "S")
  ggplot(t2, aes(dow, y, fill=fill)) +
    geom_tile(color=NA) +
    facet_wrap(~monlabel, ncol=3, scales="free") +
    scale_x_continuous(expand=c(0,0), position="top",
                       breaks=seq(0,6), labels=weekdays) +
    scale_y_continuous(expand=c(0,0)) +
    theme_void() +
    theme(panel.background=element_rect(fill=NA, color=NA),
          strip.background = element_rect(fill=NA, color=NA),
          strip.text.x = element_blank(),
          legend.title = element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          axis.text.y = element_blank(),
          strip.placement = "outside")
}

#############

library(extrafont)
library(lubridate)

mtlsum_all_good <- arrange(mtlsum_all_three_filled, year, month, day) ## Had to sort the data sets by year, because it wasn't

# So with all this I can have the 6 months of a specific year, which I kinda need to specify and type manually at 4 places

ggcal(mtlsum_all_good$date[year(mtlsum_all_good$date) == 2019], mtlsum_all_good$min_temp[year(mtlsum_all_good$date) == 2019]) +
  scale_fill_gradient2(low = "blue", mid = "green", high = "red", midpoint = 10) +
  geom_tile(color = NA) +
  geom_text(aes(label = mtlsum_all_good$min_temp[year(mtlsum_all_good$date) == 2019]), family = "Open Sans Semibold", size = 3) +
  facet_wrap(~monlabel, ncol = 6, scales="free") +
  labs(
    title = paste("The highest temperature in degrees Celsius (°C) observed", "(1986)"), 
    subtitle = "As recorded at MONTREAL/PIERRE ELLIOTT TRUDEAU INTL A", 
    caption = "Source : Environment and Climate Change Canada through the weathercan R package"
  ) +
  theme(text = element_text(family = "Open Sans Semibold", size = 18))

# So that's ok-ish... I'd like a video or gif where each year appears a few second and then goes on to the next one.
# Apparently I need a software called ImageMagick - https://www.imagemagick.org/script/index.php but I'm not sure but I think only the recent magick package can do now

# I only need to make 2020 with the same months as all others, even though as of this writing it's August 10th, 2020 so we'll lack some data, but otherwise the last chart looks weird

str(mtlsum_all_good$date)

extra_2020 <- seq(as.Date("2020-08-09"), by = "day", length = 84)

extra_2020df <- data.frame(station_id = "None", date = extra_2020, year = as.character(year(extra_2020)), month = as.character(month(extra_2020)), day = as.character(day(extra_2020)), min_temp = NA, mean_temp = NA, max_temp = NA, station_id30165 = NA, year30165 = NA, month30165 = NA, day30165 = NA, min_temp30165 = NA, mean_temp30165 = NA, max_temp30165 = NA, all_max_temp = NA, all_min_temp = NA)

mtlsum_all_good <- bind_rows(mtlsum_all_good, extra_2020df)

### OK NOW FOR THE NIGHTS (minimum)

img <- image_graph(1500, 350, res = 96)
for (i in c(1980:2020)) {
    print(ggcal(mtlsum_all_good$date[year(mtlsum_all_good$date) == i], mtlsum_all_good$all_min_temp[year(mtlsum_all_good$date) == i]) +
      scale_fill_gradient2(low = "light blue", high = "red", midpoint = 15, rescale(0:20)) +
      geom_tile(color = NA) +
      geom_text(aes(label = mtlsum_all_good$all_min_temp[year(mtlsum_all_good$date) == i]), family = "Open Sans Semibold", size = 3) +
      facet_wrap(~monlabel, ncol = 6, scales="free") +
      labs(
        title = paste("The lowest temperature in degrees Celsius (°C) observed", "(", i, ")"), 
        subtitle = "As recorded at MONTREAL/PIERRE ELLIOTT TRUDEAU INTL A", 
        caption = "Source : Environment and Climate Change Canada through the weathercan R package"
      ) +
      theme(text = element_text(family = "Open Sans Semibold", size = 18)))
}
dev.off()

animation <- image_animate(img, fps = 4, optimize = TRUE)
print(animation)

### AND NOW FOR THE DAY (maximum)

img <- image_graph(1500, 350, res = 96)
for (i in c(1980:2020)) {
  print(ggcal(mtlsum_all_good$date[year(mtlsum_all_good$date) == i], mtlsum_all_good$all_max_temp[year(mtlsum_all_good$date) == i]) +
          scale_fill_gradient2(low = "grey99", mid = "yellow", high = "red", midpoint = 20, rescale(0:30)) +
          geom_tile(color = NA) +
          geom_text(aes(label = mtlsum_all_good$all_max_temp[year(mtlsum_all_good$date) == i]), family = "Open Sans Semibold", size = 3) +
          facet_wrap(~monlabel, ncol = 6, scales="free") +
          labs(
            title = paste("The highest temperature in degrees Celsius (°C) observed", "(", i, ")"), 
            subtitle = "As recorded at MONTREAL/PIERRE ELLIOTT TRUDEAU INTL A", 
            caption = "Source : Environment and Climate Change Canada through the weathercan R package"
          ) +
          theme(text = element_text(family = "Open Sans Semibold", size = 18)))
}
dev.off()

animation <- image_animate(img, fps = 4, optimize = TRUE)
print(animation)

### Now A STATIC VERSION OF EACH 

# DAY

ggcal(mtlsum_all_good$date, mtlsum_all_good$all_max_temp) +
  scale_fill_gradient2(low = "grey99", mid = "yellow", high = "red", midpoint = 20, rescale(0:30)) +
  geom_tile(color = NA) +
  #geom_text(aes(label = mtlsum_all_good$all_max_temp, family = "Open Sans Semibold", size = 0.001)) +
  facet_wrap(~monlabel, ncol = 6, scales="free") +
  labs(
    title = "The highest temperature in degrees Celsius (°C) observed", 
    subtitle = "As recorded at MONTREAL/PIERRE ELLIOTT TRUDEAU INTL A", 
    caption = "Source : Environment and Climate Change Canada through the weathercan R package"
  ) +
  theme(text = element_text(family = "Open Sans Semibold", size = 18), legend.position = "none")

# NIGHT

ggcal(mtlsum_all_good$date, mtlsum_all_good$all_min_temp) +
  scale_fill_gradient2(low = "light blue", high = "red", midpoint = 15, rescale(0:20)) +
  geom_tile(color = NA) +
  #geom_text(aes(label = mtlsum_all_good$all_max_temp, family = "Open Sans Semibold", size = 0.001)) +
  facet_wrap(~monlabel, ncol = 6, scales="free") +
  labs(
    title = "The highest temperature in degrees Celsius (°C) observed", 
    subtitle = "As recorded at MONTREAL/PIERRE ELLIOTT TRUDEAU INTL A", 
    caption = "Source : Environment and Climate Change Canada through the weathercan R package"
  ) +
  theme(text = element_text(family = "Open Sans Semibold", size = 18), legend.position = "none")
