#Programming Assignment3
##2 Finding the best hospital in a state
best <- function(state,outcome){
        ###Read outcome data
        datafile <- read.csv("outcome-of-care-measures.csv")
        ###Check that state and outcome are valid
        types <- c("heart attack","heart failure","pneumonia")
        if(!state %in% datafile[,7]){
                stop('invalid state')
        }else if(!outcome %in% types){
                stop('invalid outcome')
        }else{
        ###Return hispital name in that state with lowerst 30day death rate
        ####heartattack-col 11, heartfail-col 17, pneumonia-col 23
                if(outcome=="heart attack"){
                        rates <- subset.data.frame(datafile,State=state,select=datafile[,c(2,11)])   
                }else if(outcome=="heart failure"){
                        rates <- subset.data.frame(datafile,State=state,select=datafile[,c(2,17)])
                }else if(outcome=="pneumonia"){
                rates <- subset.data.frame(datafile,State=state,select=datafile[,c(2,23)])
                }else{}
                
        }
}