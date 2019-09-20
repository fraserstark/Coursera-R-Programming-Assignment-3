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
