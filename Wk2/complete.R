build_df <- function(directory, id) {
  if (dir.exists(directory)) {
    fn <- sprintf("%03d.csv", id)
    csv <- paste(directory, fn, sep="/")
    #print(paste("Reading ", csv))
    df <- read.csv(csv)
  }
  df
}

complete <- function(directory, id = 1:332){
  #create empty data frame
  nobs <- data.frame(row.names(c("id", "nobs")))
  #get complete cases for each data frame
  for(i in seq_along(id)) {
    df <- build_df(directory, id[i])
    good <- complete.cases(df)
    nob <- data.frame("id"=id[i], "nobs"=length(good[good==T]))
    nobs <- rbind(nobs, nob)
  }
  nobs
}
