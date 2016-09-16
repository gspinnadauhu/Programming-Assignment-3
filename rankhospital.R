rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        datafile <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
        ## Check that state and outcome are valid
        types <- c("heart attack","heart failure","pneumonia")
        if(!state %in% datafile[,7]){
                stop('invalid state')
        }else if(!outcome %in% types){
                stop('invalid outcome')
        }else{}
        ## Return hospital name in that state with the given rank
        ## creating a subset dataframe 
        shortset<-datafile[,c(2,7,11,17,23)]
        names(shortset)<-c("Hospital","State","heartattack","heartfailure","pneumonia")
        shortset$heartattack<-suppressWarnings(as.numeric(shortset$heartattack))
        shortset$heartfailure<-suppressWarnings(as.numeric(shortset$heartfailure))
        shortset$pneumonia<-suppressWarnings(as.numeric(shortset$pneumonia))
        shortset<-subset(shortset,State==state,select=c("Hospital","State","heartattack","heartfailure","pneumonia"))
        ##ordering by outcome in ascending order, removing non-selected outcomes
        if(outcome=="heart attack"){
                colnum<-c(3)
                shortset<-shortset[with(shortset,order(shortset$heartattack,shortset$Hospital)),c(1,colnum)]  
        }else if(outcome=="heart failure"){
                colnum<-c(4)
                shortset<-shortset[with(shortset,order(shortset$heartfailure,shortset$Hospital)),c(1,colnum)]
        }else{
                colnum<-c(5)
                shortset<-shortset[with(shortset,order(shortset$pneumonia,shortset$Hospital)),c(1,colnum)] 
        }
        shortset<-na.omit(shortset)
        ranking<-c(1:length(shortset$Hospital))
        shortset<-cbind.data.frame(shortset,ranking)
        names(shortset)<-c("Hospital","Rate","Rank")
        ## 30-day death rate - setting up selection based on num=
        if(num=="best"){
                shortset[1,]    
        }else if(num=="worst"){
                shortset[length(shortset$Hospital),]
        }else if(num>length(shortset$Hospital) | num<1){
                return(NA)
        }else{
                shortset[num,1]
        }
}
