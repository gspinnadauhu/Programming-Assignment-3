rankall <- function(outcome, num = "best") {
        ## Read outcome data
        datafile <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
        ## Check that outcome is valid
        types <- c("heart attack","heart failure","pneumonia")
        if(!outcome %in% types){
                stop('invalid outcome')
        }else{}
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}
