# Reproducible Research: Storm Severity Analysis

## Questions
1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

## Project Setup

The libraries used in this analysis are:

```r
library(reshape2)
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
library(stringr)
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

## Reducing data set for ease of manipulation
This is a fairly large data set, especially considering the need to do some fairly manual cleaning of the EVTYPE data, as discussed below. The below plots show the range of values present in the adverse effects. 


```r
histlog <- function(x, base) pmax(log(x,base),rep(0, length(x)))
ggplot(filter(stormdat, FATALITIES > 0))+
    geom_boxplot(aes(1,log(FATALITIES,10)))
```

![](StormSeverityAnalysis_files/figure-html/data.range-1.png) 


```r
# filter for adverse effects > 0
stormdat <- filter(stormdat, (FATALITIES+INJURIES+PROPDMG+CROPDMG) > 0)
dim(stormdat)
```

```
## [1] 254633      8
```

## Managing EVTYPE variable
There are 488 different event types in `EVTYPE`, which is much larger than the defined list in the [NOAA instruction sheet](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf). Let's see if this can be cleaned up. 


```r
# clean up variations
stormdat <- mutate(stormdat, 
                   EVTYPE=str_trim(EVTYPE),
                   EVTYPE=tolower(EVTYPE),
                   EVTYPE=gsub("\\.$", "", EVTYPE),
                   EVTYPE=gsub("/ ", "/", EVTYPE))

evtypes <- unique(stormdat$EVTYPE)
length(evtypes)
```

```
## [1] 439
```
That helped a little, but there's still too much data.

Taking a look at values of `EVTYPE`:

```r
head(evtypes, 15)
```

```
##  [1] "tornado"                   "tstm wind"                
##  [3] "hail"                      "ice storm/flash flood"    
##  [5] "winter storm"              "hurricane opal/high winds"
##  [7] "thunderstorm winds"        "hurricane erin"           
##  [9] "hurricane opal"            "heavy rain"               
## [11] "lightning"                 "thunderstorm wind"        
## [13] "dense fog"                 "rip current"              
## [15] "thunderstorm wins"
```
shows several spelling variations for the entry `THUNDERSTORM WINDS`. Also various hurricanes are named and labeled with variations such as `Hurricane Opal`, `Hurricane Erin`, `Hurricane Opal/High Winds`. 

The "real" event types are extracted from the table of contents of the NOAA instruction sheet.

```r
# get event types from event types text file
evtypes.raw <- readLines("data/event types.txt", warn=F)

# extract 1st-level event types, excluding "...", page numbers and other data
#   "7.2 Words, (with-stuff/other stuff) "
evtypes.def <- str_extract_all(evtypes.raw, 
                               "^[0-9]+\\.[0-9]+\\s[A-Za-z\\s,/()-]{2,}")

# remove empty rows
evtypes.def <- unlist(evtypes.def[sapply(evtypes.def, length) > 0])

# remove leading numbers and trailing (<char>)
evtypes.def <- str_sub(evtypes.def, 
                       str_locate(evtypes.def, "[0-9] ")[,2]+1, 
                       str_locate(evtypes.def, " \\([A-Z]\\)")[,1]-1)
```


```r
# organize event type data to match evtype.def
evtypes <- evtypes[order(evtypes)]
file.create("data/event mapping.txt")
```

```
## [1] TRUE
```

```r
writeLines(evtypes.def, "data/event mapping.txt")
con <- file("data/event mapping.txt", open="a")
writeLines(as.character(evtypes), con)
close(con)
```

Custom-defined catagories for disaster will be used to categorize the `EVTYPE` column. 

## Storm effects on health

### Definition of storm severity

### Worst event type


## Storm effects on economy

### Definition of storm severity

### Worst event type
