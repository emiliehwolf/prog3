##rankall.R
##Author: Emilie H. Wolf
##February 12, 2017

rankall <- function(outcome, num="best") {
        
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character", 
                       na.strings = "Not Available")
        
        
        ## Slim down the csv object and fix classes
        ## Col 1: Hospital.Name
        ## Col 2: State
        ## Col 3: Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        ## Col 4: Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        ## Col 5: Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        o <- data.frame(df[,2],df[,7],as.numeric(df[,11]),
                        as.numeric(df[,17]),as.numeric(df[,23]))
        
        ## Give column names to our slim data frame
        names(o) <- c("hospital","state","ha","hf","pn")
        
        ## Check that outcome is valid and assign x to the column index
        if(outcome=="heart attack") x <- 3
        else if(outcome=="heart failure") x <- 4
        else if(outcome=="pneumonia") x <- 5
        else stop("invalid outcome")
        
        ## If we had a state argument, this would check if valid, except this datafile contains 54 state abbreviations, not 50
        ## if((state %in% state.abb) == FALSE) stop("invalid state")
        
        d <- data.frame("hospital"=character(54),"state"=character(54),stringsAsFactors=FALSE)
        
        ## Create a vector of available states in the datafile
        states <- levels(o$state)
        
        ## For each state, find the hospital of the given rank
        for(i in 1:54) {
                
                ## Extract the observations for current state in loop
                ostate <- o[o$state==states[i],]
                
                ## Reorder the rows by outcome, then hospital name
                sorted <- ostate[ order(ostate[,x],ostate$hospital, na.last=NA),]
                sorted$hospital <- as.character(sorted$hospital)
                sorted$state <- as.character(sorted$state)
                
                if(num=="best") y <- 1
                else if(num=="worst") y <- nrow(sorted)
                else y <- num
                
                d[i,] <- sorted[y,1:2]
        }  
        
        ## Return a data frame with the hospital names and the state names
        d
}