# Warmer-nights-in-Montreal

The lowest temperature in degrees Celsius observed each day in Montreal from 1980 through early/mid-August 2020. An increasing number of nights over 20 !

Starting at 20 °C, nights are considered uncomfortable for sleeping in MTL. Yes I know, It's Canada, have a laugh, but this is partly how heatwaves are classified as such here.
In Northern Quebec it's even lower, obviously in Florida this will be higher, and so on.

Anyway, boxes in white are at 15 °C, the darker red boxes are around 20 °C, while everything in between (lighter reds are days with mins between 15 and 20.

I imported data from the Montreal International Airport since 1980 through the weathercan R package (which only worked in the R console, not RStudio), cleaned it with the usual tidyverse packages such as dplyr, stringr but also the lubridate package, for dealing with dates and time. 

I then used ggplot2 and ggcal, which uses ggplot2's features to make a calendar visualization possible in a easier and faster way.

Finally, I finished everything up in Photoshop for both vizzes below (adding extra notes, credits, creating the gif).

Here's a gif of the data visualization showing 5-year periods :

![](https://scarufel.com/wp-content/uploads/2020/08/Gifsum.gif)

And here is the full visualization :

![](https://scarufel.com/wp-content/uploads/2020/08/mtl-sum-nights-long2.png)

Two script files can be found in this repo, a short one for the R console part, a second much longer one for the RStudio part.

Enjoy! On my side, I always enjoy feedback!

I'm on Twitter if there is anything @stevecarufel_
