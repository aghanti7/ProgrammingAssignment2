build_df <- function(directory, id) {
  if (dir.exists(directory)) {
    fn <- sprintf("%03d.csv", id)
    csv <- paste(directory, fn, sep="/")
    #print(paste("Reading ", csv))
    df <- read.csv(csv)
  }
  df
}

pollutantmean <- function(directory, pollutant, id = 1:332){
  #create empty data frame
  df <- data.frame(row.names(c("Date", "sulfate", "nitrate", "ID")))
  #concat all data frames
  for(i in seq_along(id)) {
    df <- rbind(df, build_df(directory, id[i]))
  }
  bad <- is.na(df[pollutant])
  mean(df[pollutant][!bad])
}
