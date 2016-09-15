rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        datafile <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
        ## Check that state and outcome are valid
        if(!state %in% datafile[,7]){
                stop('invalid state')
        }else if(!outcome %in% types){
                stop('invalid outcome')
        }else{}
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}
