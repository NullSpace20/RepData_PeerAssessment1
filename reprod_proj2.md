---
title: "Reproducible Research 2nd Project"
author: "Mahmoud Samy"
date: "December 7, 2018"
output:
  html_document:
    keep_md: yes
    toc: yes
---
### Data loading and processing

 download and  load the data.

```r
loadData <- function()
{
        URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        destfile <- "./data/dataset.csv.bz2"
        
        if (!file.exists(destfile))  # Check if the file already exists
        {
                download.file(URL, destfile,"curl") #Download data
        }
        data <- read.csv(destfile)   # load the data
}
```

Calling the previous  function to load data:

```r
data <- loadData()
```

#####Some information about the loaded data:
Cols names:

```r
names(data)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

Number of rows:

```r
nrow(data)
```

```
## [1] 902297
```

#### Some packages that might be needed to reproduce this analysis:

```r
if (!("ggplot2" %in% rownames(installed.packages())))
        install.packages("ggplot2")
library(ggplot2)
library(grid)
if (!("gridExtra" %in% rownames(installed.packages())))
        install.packages("gridExtra")
library(gridExtra)
```

#### 1) Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

To answer this question there were considered the variables 1) number of fatalities and 2) number of injuries for each type of event in the data set.
The first step was: 

* Aggregating the total of fatalities and injuries by type of event:


```r
fatalities <- aggregate(FATALITIES ~ EVTYPE, data = data, FUN = sum)
injuries   <- aggregate(INJURIES   ~ EVTYPE, data = data, FUN = sum)
```


* Secondly, it was selected from the aggregated data the top 10 events that cause most fatalities and the top 10 that cause most injuries:


```r
# Sorting by number of fatalities and removing the top 10
top10Fatal <- head(fatalities[ order(-fatalities[,2]), ],10)
# Sorting by number of injuries and removing the top 10
top10Injur <- head(injuries[ order(-injuries[,2]), ],10)
```


* Thirdly, relevel the FATALITIES by EVTYPE, so that the plot starts with the lower fatalities and the same applyied to the INJURIES aggregated data.


```r
# bars and ends with the higher ones.
top10Fatal$EVTYPE <- factor(top10Fatal$EVTYPE, levels=top10Fatal[order(top10Fatal$FATALITIES),"EVTYPE"])
top10Injur$EVTYPE <- factor(top10Injur$EVTYPE, levels=top10Injur[order(top10Injur$INJURIES),"EVTYPE"])
```


* Finally, the plots were drawn:

```r
# Draw the plots
fatalPlot <- ggplot(data=top10Fatal, aes(x=EVTYPE, y=FATALITIES, fill=EVTYPE)) + 
             geom_bar(colour="black", fill="yellow",stat="identity") + 
             xlab("Types of events") + ylab("Total of fatalities") +
             ggtitle("Top 10 : Most Fatal events to population health") 

injurPlot <- ggplot(data=top10Injur, aes(x=EVTYPE, y=INJURIES, fill=EVTYPE)) + 
                geom_bar(colour="black", fill="orange",stat="identity") + 
                xlab("Types of events") + ylab("Total of Injurie") +
                ggtitle("Top 10 : Most Injure events to population health") 

grid.arrange(fatalPlot, injurPlot, nrow=2)
```

![](reprod_proj2_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


The plots shows the 10 most fatal and injure events to population's health. As we can see in the plots tornadoes are the main event that causes the majory of fatalities and injuries.

#### 2) Across the United States, which types of events have the greatest economic consequences?

from the Weather Service Storm Data Documentation:

* PROPDMG Damage > Property damage estimates should be entered as actual dollar amounts

* CROPDMG > Crop Damage Data, Crop damage information may be obtained from reliable sources

Besides those two variables (**PROPDMG, CROPDMG**), **PROPDMGEXP** and **CROPDMGEXP** are also related with economic consequences. **PROPDMGEXP** and **CROPDMGEXP** are orders of magnitude,  which can be understanded as 10^EXP.

Therefore, the original data was subseted into a data with four columns one for each economic consequence variable:


```r
economic <- data[c("EVTYPE","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
```

By analyising the types  used for the Property damage

```r
unique(economic$PROPDMGEXP)
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

and for the Crop damage


```r
unique(economic$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```


it was indicate that :
* H or h = 2 (hundred = 10^2)
* K or k = 3 (thousand = 10^3)
* M or m = 6 (million = 10^6)
* B or b = 9 (billion = 10^9)

and symbols '+', '-' and '?' as meaningless information (0 will be the indicator)

The below function will transform the rows with "Hh","Kk","Mm","Bb","+-?" and "" symbols into "2", "3","6","9", "0" and "0" values, respectively.
Thus, this new values will be used to calculate the total economical consequences of a given event.


```r
transfData <- function(col)
{
        replace <- c("2","3","6","9","0")
        regex   <- c("[Hh]","[Kk]","[Mm]","[Bb]","[+-/?]")
        size <- length(replace)
        for (i in 1:size)
        {
                col <- sub(regex[i], replace[i],col)
        }
        col[col == ""] <- "0" # Removing empty space by zero instead
        col <- as.numeric(col) # Converting col values into numeric ones
        col
}
```

Changing the old symbols of the colunm **PROPDMGEXP**:

```r
#Removing simbols for values
economic$PROPDMGEXP = transfData(economic$PROPDMGEXP)
# New values
unique(economic$PROPDMGEXP) # "3" "6" "0" "9" "5" "4" "2" "7" "1" "8""
```

```
##  [1] 3 6 0 9 5 4 2 7 1 8
```

Changing the old symbols of the colunm **CROPDMGEXP**:

```r
#Removing simbols for values
economic$CROPDMGEXP= transfData(economic$CROPDMGEXP)
# New values
unique(economic$CROPDMGEXP) #[1] "0" "6" "3" "9" "2"
```

```
## [1] 0 6 3 9 2
```

In order to calculate the total economical consequence of a given event the following formula was used: 
**PROPDMG * 10 ^ PROPDMGEXP + CROPDMG * 10 ^ CROPDMGEXP**


```r
economic$EconomicLoss <- economic$PROPDMG * 10 ^ economic$PROPDMGEXP + economic$CROPDMG * 10 ^ economic$CROPDMGEXP
```


After calculating the economical consequence for each event, stored in a new colunm, the new column was aggregated along with the types of events:

```r
economicLoss <- aggregate(EconomicLoss ~ EVTYPE, data = economic, FUN = sum)
```

It was selected from the aggregated data the top 10 events responsible for the most economical damage:

```r
top10EconomicLoss <- head(economicLoss[ order(-economicLoss[,2]), ],10)
top10EconomicLoss$EVTYPE <- factor(top10EconomicLoss$EVTYPE, levels=top10EconomicLoss[order(top10EconomicLoss$EconomicLoss),"EVTYPE"])
```


Finally, the plot was drawn with the top 10 types of events:


```r
ggplot(data=top10EconomicLoss, aes(x=EVTYPE, y=EconomicLoss, fill=EVTYPE)) + 
        geom_bar(colour="black", fill="yellow", width=.7,stat="identity") + 
        xlab("Types of events") + ylab("Dollars lost") +
        ggtitle("Top 10 : Events with more losses to the economy")+ coord_flip()
```

![](reprod_proj2_files/figure-html/unnamed-chunk-19-1.png)<!-- -->



```r
sum(top10EconomicLoss$EconomicLoss) / sum(economicLoss$EconomicLoss)
```

```
## [1] 0.8561574
```

85.6% of the dollars spend with all types of events.

Considering all the events floods are responsible for:

```r
max(economicLoss$EconomicLoss) / sum(economicLoss$EconomicLoss)
```

```
## [1] 0.3149183
```

31.49\% of the damages.
