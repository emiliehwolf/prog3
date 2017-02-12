##best.R
##Author: Emilie H Wolf
##February 11, 2017

best <- function(state, outcome) {
        
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv",
                colClasses = "character", na.strings =
                "Not Available")

        ## Slim down the csv object and fix classes
        ## Col 1: Hospital.Name
        ## Col 2: State
        ## Col 3: Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        ## Col 4: Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        ## Col 5: Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        o <- data.frame(df[,2],df[,7],as.numeric(df[,11]),
                as.numeric(df[,17]),as.numeric(df[,23]))
        
        ## Check that state is valid
        ## If the state argument doesn't match any of the
        ## values in the state column, the program stops
        if(any(o[,2]==state)==FALSE) stop("invalid state")
        
        ## Check that outcome is valid and assign x to column number
        if(outcome=="heart attack") x <- 3
        else if(outcome=="heart failure") x <- 4
        else if(outcome=="pneumonia") x <- 5
        else stop("invalid outcome")
        
        ## Subset the outcomes for the state that was called
        ostate <- o[o[,2]==state,]
        
        ## Return hospital name in the state with lowest
        ## 30-day death rate
        as.character(ostate[which.min(ostate[,x]),1])
}