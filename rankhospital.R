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
        ##ordering by outcome in ascending order
        attach(shortset)
        if(outcome=="heart attack"){
                colnum<-c(3)
                shortset<-shortset[with(shortset,order(heartattack,Hospital)),c(1,colnum)]  
        }else if(outcome=="heart failure"){
                colnum<-c(4)
                shortset<-shortset[with(shortset,order(heartfailure,Hospital)),c(1,colnum)]
        }else{
                colnum<-c(5)
                shortset<-shortset[with(shortset,order(pneumonia,Hospital)),c(1,colnum)] 
        }
        shortset<-na.omit(shortset)
        ranking<-c(1:length(shortset$Hospital))
        shortset<-cbind.data.frame(shortset,ranking)
        names(shortset)<-c("Hospital","Rate","Rank")
        ## 30-day death rate
        #if(num=="best"){
                
                #shortset[c(1:5),c(1,2,colnum)]    
        #}else if(num=="worst"){
        #        length()
        #        shortset[c(1:5),]        
        #}
        shortset[c(1:5),]
        #shortset
}
