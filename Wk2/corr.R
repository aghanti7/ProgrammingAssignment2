build_df <- function(directory, id) {
  if (dir.exists(directory)) {
    fn <- sprintf("%03d.csv", id)
    csv <- paste(directory, fn, sep="/")
    #print(paste("Reading ", csv))
    df <- read.csv(csv)
  }
  df
}

corr <- function(directory, threshold = 0) {
  nobs <- complete(directory)
  dat <- nobs[nobs$nobs>threshold, ]
  corr <- vector("numeric", length = length(dat$id))
  
  for(i in seq_along(dat$id)) {
    df <- build_df(directory, dat$id[i])
    good <- complete.cases(df)
    df <- df[good,][]
    corr[i] <- cor(df$sulfate, df$nitrate)
  }
  corr
}