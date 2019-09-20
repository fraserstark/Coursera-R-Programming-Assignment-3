R Programming Assignment Three
================
Fraser Stark
20/09/2019

### Coursera R Programming - Hospital Care Outcomes

### Introduction

The data for this assignment come from the Hospital Compare web site
(<http://hospitalcompare.hhs.gov>) run by the U.S. Department of Health
and Human Services. The purpose of the web site is to provide data and
information about the quality of care at over 4,000 Medicare-certified
hospitals in the U.S. This dataset essentially covers all major U.S.
hospitals. This dataset is used for a variety of purposes, including
determining whether hospitals should be fined for not providing high
quality care to patients (see <http://goo.gl/jAXFX> for some background
on this particular
topic).

## Read the data

``` r
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
```

## 1\. Plot the 30-day mortality rates for heart attack

``` r
outcome[, 11] <- as.numeric(outcome[, 11]) # Column 11 for heart attack rates

hist(outcome[, 11]
    ,xlab='Deaths'
    ,main='Hospital 30-Day Death (Mortality) Rates from Heart Attack'
    ,col="lightblue")
```

![](Assignment3-Markdown_files/figure-gfm/Plot%20outcomes-1.png)<!-- -->

## 2\. Finding the best hospital in a state

Write a function called best that take two arguments: the 2-character
abbreviated name of a state and an outcome name. The function reads the
outcome-of-care-measures.csv file and returns a character vector with
the name of the hospital that has the best (i.e. lowest) 30-day
mortality for the specified outcome in that state. The hospital name is
the name provided in the Hospital.Name variable. The outcomes can be one
of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do
not have data on a particular outcome should be excluded from the set of
hospitals when deciding the rankings.

``` r
best <- function(state, outcome) {
    ## Read outcome data
    
    outcomes <- read.csv("outcome-of-care-measures.csv", 
                          colClasses = "character",
                          header = TRUE)
    
    ## Get data we're interested in
    
    rates <- as.data.frame(cbind(outcomes[, 2],  # hospital 
                                outcomes[, 7],   # state
                                outcomes[, 11],  # heart attack 
                                outcomes[, 17],  # heart failure 
                                outcomes[, 23]), # pneumonia
                           stringsAsFactors = FALSE)
    
    ## Rename columns
    
    colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    
    if(!state %in% rates[,"state"]){
        stop('invalid state')
    }
    
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    }
    
    
    ## Return hospital name in that state with lowest 30-day death rate
    
    ## Get only the hospitals in chosen state
    hRates <- rates[(rates[, "state"] == state), ]
    
    ## Convert outcome rate to numberic
    hRates[, outcome] <- as.numeric(hRates[, outcome])
    
    ## Remove NA values
    hRates <- hRates[!is.na(hRates[, outcome]), ]
    
    ## Order by outcome rate
    hRates <- hRates[order(hRates[, outcome]), ]
    
    ## Get names of hosptial with the lowest rate
    hNames <- hRates[hRates[, outcome] == min(hRates[,outcome]),1]

    ## Sort by hospital name if tie
    sort(hNames)[1]
}
```

Some sample outputs

``` r
best("TX", "heart attack")
```

    ## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"

``` r
best("MD", "pneumonia")
```

    ## [1] "GREATER BALTIMORE MEDICAL CENTER"

## 3\. Ranking hospitals by outcome in a state

Write a function called rankhospital that takes three arguments: the
2-character abbreviated name of a state (state), an outcome (outcome),
and the ranking of a hospital in that state for that outcome (num). The
function reads the outcome-of-care-measures.csv file and returns a
character vector with the name of the hospital that has the ranking
specified by the num argument. For example, the call rankhospital(“MD”,
“heart failure”, 5) would return a character vector containing the
name of the hospital with the 5th lowest 30-day death rate for heart
failure. The num argument can take values “best”, “worst”, or an integer
indicating the ranking (smaller numbers are better). If the number given
by num is larger than the number of hospitals in that state, then the
function should return NA. Hospitals that do not have data on a
particular outcome should be excluded from the set of hospitals when
deciding the rankings.

``` r
rankhospital <- function(state, outcome, num = 'best') {
    
    ## Read outcome data
    
    outcomes <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character",
                         header = TRUE)
    
    ## Get data we're interested in
    
    rates <- as.data.frame(cbind(outcomes[, 2],   # hospital
                                 outcomes[, 7],   # state
                                 outcomes[, 11],  # heart attack
                                 outcomes[, 17],  # heart failure
                                 outcomes[, 23]), # pneumonia
                           stringsAsFactors = FALSE)
    
    ## Rename columns
    
    colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    
    if(!state %in% rates[,"state"]){
        stop('invalid state')
    }
    
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    }
    
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    ## Get only the hospitals in chosen state
    hRates <- rates[(rates[, "state"] == state), ]
    
    ## Convert outcome rate to numberic, gets a warning
    hRates[, outcome] <- as.numeric(hRates[, outcome])
    
    ## Remove NA values
    hRates <- hRates[!is.na(hRates[, outcome]), ]
    
    ## convert num argument to valid rank
    
    if(num == "best") {
        num <- 1 
    }
    
    if (num == "worst") {
        num <- nrow(hRates) 
    }
    
    ## Order by outcome rate
    hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
    
    ## Get names of hospital 
    
    hRates[num,1]

}
```

Sample outputs

``` r
rankhospital("TX", "heart failure", 4)
```

    ## [1] "DETAR HOSPITAL NAVARRO"

``` r
rankhospital("MD", "heart attack", "worst")
```

    ## [1] "HARFORD MEMORIAL HOSPITAL"

## 4\. Ranking hospitals in all states

Write a function called rankall that takes two arguments: an outcome
name (outcome) and a hospital ranking (num). The function reads the
outcome-of-care-measures.csv file and returns a 2-column data frame
containing the hospital in each state that has the ranking specified in
num. For example the function call rankall(“heart attack”, “best”) would
return a data frame containing the names of the hospitals that are the
best in their respective states for 30-day heart attack death rates. The
function should return a value for every state (some may be NA). The
first column in the data frame is named hospital, which contains the
hospital name, and the second column is named state, which contains the
2-character abbreviation for the state name. Hospitals that do not have
data on a particular outcome should be excluded from the set of
hospitals when deciding the rankings.

``` r
rankall <- function(outcome, num = 'best') {
    ## Read outcome data
    
    outcomes <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character",
                         header = TRUE)
    
    ## Get data we're interested in
    
    rates <- as.data.frame(cbind(outcomes[, 2],   # hospital
                                 outcomes[, 7],   # state
                                 outcomes[, 11],  # heart attack
                                 outcomes[, 17],  # heart failure
                                 outcomes[, 23]), # pneumonia
                           stringsAsFactors = FALSE)
    
    ## Rename columns
    
    colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    ## Check outcome is valid
   
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    hRank <- data.frame()
    
    for(state in sort(unique(rates[,"state"]))){
        
        ## Get only the hospitals in this state
        hRates <- rates[(rates[, "state"] == state), ]
        
        ## Convert outcome rate to numberic, gets a warning
        hRates[, outcome] <- as.numeric(hRates[, outcome])
        
        ## Remove NA values
        hRates <- hRates[!is.na(hRates[, outcome]), ]
        
        ## convert num argument to valid rank
        
        if(num == "best") {
            rnum <- 1 
        } else if (num == "worst") {
            rnum <- nrow(hRates) 
        }
        else {rnum = num}
        
        
        ## Order by outcome rate & hospital name
        hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
        
        hName <- hRates[rnum,1]
        
        hRank <- rbind(hRank,
                       data.frame(hospital = hName,
                                  state = state))
    }

    ## Return dataframe
    hRank
    
}
```

Sample outputs

``` r
 head(rankall("heart attack", 20), 10)
```

    ##                               hospital state
    ## 1                                 <NA>    AK
    ## 2       D W MCMILLAN MEMORIAL HOSPITAL    AL
    ## 3    ARKANSAS METHODIST MEDICAL CENTER    AR
    ## 4  JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
    ## 5                SHERMAN OAKS HOSPITAL    CA
    ## 6             SKY RIDGE MEDICAL CENTER    CO
    ## 7              MIDSTATE MEDICAL CENTER    CT
    ## 8                                 <NA>    DC
    ## 9                                 <NA>    DE
    ## 10      SOUTH FLORIDA BAPTIST HOSPITAL    FL

``` r
tail(rankall("pneumonia", "worst"), 3)
```

    ##                                      hospital state
    ## 52 MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
    ## 53                     PLATEAU MEDICAL CENTER    WV
    ## 54           NORTH BIG HORN HOSPITAL DISTRICT    WY
