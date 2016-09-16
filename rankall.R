rankall <- function(outcome, num = "best") {
        ## Read outcome data
        datafile <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
        ## Check that outcome is valid
        types <- c("heart attack","heart failure","pneumonia")
        if(!outcome %in% types){
                stop('invalid outcome')
        }else{}
        ## creating a subset dataframe 
        shortset<-datafile[,c(2,7,11,17,23)]
        names(shortset)<-c("Hospital","State","heartattack","heartfailure","pneumonia")
        shortset$heartattack<-suppressWarnings(as.numeric(shortset$heartattack))
        shortset$heartfailure<-suppressWarnings(as.numeric(shortset$heartfailure))
        shortset$pneumonia<-suppressWarnings(as.numeric(shortset$pneumonia))
        if(outcome=="heart attack"){
                colnum<-c(3)
                shortset<-shortset[with(shortset,order(shortset$State,
                                                       shortset$heartattack,
                                                       shortset$Hospital)),c(1,2,colnum)]  
        }else if(outcome=="heart failure"){
                colnum<-c(4)
                shortset<-shortset[with(shortset,order(shortset$State,
                                                       shortset$heartfailure,
                                                       shortset$Hospital)),c(1,2,colnum)] 
        }else{
                colnum<-c(5)
                shortset<-shortset[with(shortset,order(shortset$State,
                                                       shortset$pneumonia,
                                                       shortset$Hospital)),c(1,2,colnum)] 
        }
        ## For each state, find the hospital of the given rank
        names(shortset)<-c("Hospital","State","Rate")
        shortset<-na.omit(shortset)
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        if(num=="best"){
                shortset<-do.call(rbind,by(shortset,shortset$State,function(x) x[1,]))    
        }else if(num=="worst"){
                shortset<-do.call(rbind,by(shortset,shortset$State,function(x) tail(x,n=1)))
        }else if(num>length(shortset$Hospital) | num<1){
                return(NA)
        }else{
                shortset<-do.call(rbind,by(shortset,shortset$State,function(x) x[num,]))
        }
        shortset[,1:2]
}
