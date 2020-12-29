## Misc helpers

seq_date <- function(x) {
    ## Takes a vector of Date objects and returns a sequence from 
    ## highest to lowest.
    x <- as.Date(x)
    seq(min(x), max(x), by = "day")
}