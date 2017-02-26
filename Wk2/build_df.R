build_df <- function(directory, id) {
  if (dir.exists(directory)) {
    fn <- sprintf("%03d.csv", id)
    csv <- paste(directory, fn, sep="/")
    #print(paste("Reading ", csv))
    df <- read.csv(csv)
  }
  df
}
