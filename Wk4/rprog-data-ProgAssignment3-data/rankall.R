rankall <- function(outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv",
                   colClasses = "character",
                   na.strings = c("", "Not Available"))

    ## Check that outcome is valid
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
    data <- df[,c("Hospital.Name", "State", outcome)]
    good <- complete.cases(data)
    data <- data[good,]

    ## create empty data frame to be returned
    df <- data.frame(hospital=character(), state=character())

    ## For each state, find the hospital of the given rank
    for (state in levels(as.factor(data$State))) {
        df_state <- data[data$State == state, c(1,3)]

        ## sort the data, first by outcome rate,
        ## and then by hospital name, all ascending
        df_state <- df_state[order(df_state[2], df_state[1]),]

        ## Gather hospital name in that state with the given rank
        ## 30-day death rate
        if (num == "best") {
            hosp <- df_state[1, 1]
        } else if (num == "worst") {
            hosp <- df_state[nrow(df_state), 1]
        } else {
            hosp <- df_state[num, 1]
        }
        #colnames(df) <- c("hospital", "state")
        df <- rbind(df, data.frame(hospital=hosp, state=state))
    }

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    df
}
