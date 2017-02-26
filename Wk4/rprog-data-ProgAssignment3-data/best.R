best <- function(state, outcome) {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv",
                   colClasses = "character",
                   na.strings = c("", "Not Available"))

    ## Check that state and outcome are valid
    if (!(state %in% levels(as.factor(df$State)))) {
        stop("invalid state")
    }
    if (outcome == "heart attack") {
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        ## set this column to be numbers
        df[, 11] <- as.numeric(df[, 11])
    } else if (outcome == "heart failure") {
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        ## set this column to be numbers
        df[, 17] <- as.numeric(df[, 17])
    } else if (outcome == "pneumonia") {
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        ## set this column to be numbers
        df[, 23] <- as.numeric(df[, 23])
    } else {
        stop("invalid outcome")
    }

    ## remove NAs
    data <- df[df$State == state,c("Hospital.Name", "State", outcome)]
    good <- complete.cases(data)
    data <- data[good,]

    ## sort the data, first by outcome rate, and then by hospital name
    ## both ascending
    data <- data[order(data[3], data[1]),]

    ## Return hospital name in that state with lowest 30-day death
    ## rate
    data[1,1]
}
