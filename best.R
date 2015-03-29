best <- function(state, outcome) {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    dfslim <- data.frame(df[,2],df[,7],df[,11],df[,17],df[,23])

    ##colnames(df) <- c("hosp","state","ha","hf","pn") ##unneccessary

    if(outcome=="heart attack") x<-3
    if(outcome=="heart failure") x<-4
    if(outcome=="pneumonia") x<-5
   
    ##else {
    ##    stop() ###
    ##}
    ##if(any(df[,2]==state)==FALSE) {       
    ##    stop() ###
    ##}

    dfstate <- dfslim[dfslim[,2]==state,]  ##subsetting just the states in question
    minamount <- min(as.numeric(dfstate[,x]), na.rm=TRUE)

    ##    lowest <- df[which.min(dfstate[,x]),2]
    
    besthospitals <- vector(mode="character", length=0)
    
    for (i in nrow(dfstate)) {
        if(dfstate[i,x]==minamount) {
            besthospitals <- c(besthospitals,dfstate[i,1])
        }
    }
    
    lowest <- min(besthospitals)
    lowest
}
