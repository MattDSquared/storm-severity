# Reproducible Research: Storm Severity Analysis

## Questions
1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

## Project Setup

The libraries used in this analysis are:

```r
library(data.table)
library(plyr); library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:data.table':
## 
##     between, last
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

## Loading and preprocessing

The data comes from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. An intermediate download location for this class is hosted on cloudfront:


```r
setwd("~/../datascience/storm-severity")
dir.create("data")
```

```
## Warning in dir.create("data"): 'data' already exists
```

```r
dlURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
filepath <- "data/StormData.csv.bz2"

# set browser to download file through IE protocol (for https)
setInternet2(use = TRUE)
download.file(dlURL, destfile = filepath, mode="wb")
```


```r
stormdata <- read.csv(filepath, stringsAsFactors=FALSE);
```

To answer the two questions posed earlier we only need to care about the columns for `EVTYPE`, `FATALITIES`, `INJURIES`, `PROPDMG`, `PROPDMGEXP`, `CROPDMG`, `CROPDMGEXP`, and `REMARKS`. 

```r
stormdat <- data.table(select(stormdata, EVTYPE, FATALITIES:CROPDMGEXP, REMARKS))
stormdat <- mutate(stormdat, EVTYPE=factor(EVTYPE), 
                   PROPDMGEXP=factor(PROPDMGEXP), 
                   CROPDMGEXP=factor(CROPDMGEXP))
dim(stormdat)
```

```
## [1] 902297      8
```

```r
sapply(stormdat[1,], class)
```

```
##      EVTYPE  FATALITIES    INJURIES     PROPDMG  PROPDMGEXP     CROPDMG 
##    "factor"   "numeric"   "numeric"   "numeric"    "factor"   "numeric" 
##  CROPDMGEXP     REMARKS 
##    "factor" "character"
```

```r
head(stormdat)
```

```
##     EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1: TORNADO          0       15    25.0          K       0           
## 2: TORNADO          0        0     2.5          K       0           
## 3: TORNADO          0        2    25.0          K       0           
## 4: TORNADO          0        2     2.5          K       0           
## 5: TORNADO          0        2     2.5          K       0           
## 6: TORNADO          0        6     2.5          K       0           
##    REMARKS
## 1:        
## 2:        
## 3:        
## 4:        
## 5:        
## 6:
```

## Any missing Values? 

```r
NA.count <- !complete.cases(select(stormdat, FATALITIES:PROPDMG, CROPDMG))
```
There are 0 rows with NA values.

## Managing EVTYPE variable
There are 985 different event types in `EVTYPE`, which is much larger than the defined list in the [NOAA instruction sheet](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf). Let's see if this can be cleaned up. 

First we exclude any entry which contains no adverse effects.

```r
# filter for adverse effects > 0
stormdat <- filter(stormdat, (FATALITIES+INJURIES+PROPDMG+CROPDMG) > 0)
dim(stormdat)
```

```
## [1] 254633      8
```

```r
length(levels(stormdat$EVTYPE))
```

```
## [1] 985
```
That didn't seem to have much effect, but at least we can work with fewer data.

Taking a look at a few values of `EVTYPE`:

```r
head(unique(stormdat$EVTYPE), 15)
```

```
##  [1] TORNADO                   TSTM WIND                
##  [3] HAIL                      ICE STORM/FLASH FLOOD    
##  [5] WINTER STORM              HURRICANE OPAL/HIGH WINDS
##  [7] THUNDERSTORM WINDS        HURRICANE ERIN           
##  [9] HURRICANE OPAL            HEAVY RAIN               
## [11] LIGHTNING                 THUNDERSTORM WIND        
## [13] DENSE FOG                 RIP CURRENT              
## [15] THUNDERSTORM WINS        
## 985 Levels:    HIGH SURF ADVISORY  COASTAL FLOOD ... WND
```
shows several spelling variations for the entry `THUNDERSTORM WINDS`. Also various hurricanes are named and labeled with variations such as `Hurricane Opal`, `Hurricane Erin`, `Hurricane Opal/High Winds`. 

Custom-defined catagories for disaster were used to categorize the `EVTYPE` column. 


## Storm effects on health

### Definition of storm severity

### Worst event type


## Storm effects on economy

### Definition of storm severity

### Worst event type
