#Programming Assignment3
##2 Finding the best hospital in a state
best <- function(state,outcome){
        ###Read outcome data
        datafile <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
        ###Check that state and outcome are valid
        types <- c("heart attack","heart failure","pneumonia")
        if(!state %in% datafile[,7]){
                stop('invalid state')
        }else if(!outcome %in% types){
                stop('invalid outcome')
        }else{}
        ###Return hispital name in that state with lowerst 30day death rate
        ####heartattack-col 11, heartfail-col 17, pneumonia-col 23
        shortset<-datafile[,c(2,7,11,17,23)]
        names(shortset)<-c("hospital","state","heartattack","heartfailure","pneumonia")
        shortset$heartattack<-suppressWarnings(as.numeric(shortset$heartattack))
        shortset$heartfailure<-suppressWarnings(as.numeric(shortset$heartfailure))
        shortset$pneumonia<-suppressWarnings(as.numeric(shortset$pneumonia))
        if(outcome=="heart attack"){
                result<-shortset[which.min(!is.na(shortset$heartattack))]
        }else if(outcome=="heart failure"){
                result<-shortset[which.min(!is.na(shortset$heartfailure))]
        }else if(outcome=="pneumonia"){
                result<-shortset[which.min(!is.na(shortset$pneumonia))]
        }else{}
        return(result[1,1])

}