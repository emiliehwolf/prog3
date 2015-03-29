best <- function(state, outcome) {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    df <- data.frame(df[,2],df[,7],df[,11],df[,17],df[,23])

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

    lowest <- min(as.numeric(df[,x]), na.rm=TRUE)
    lowest
    
}
