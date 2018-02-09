#a-HW2 513
#1. Read 2 csv file into 2 variables.
 Goo <- read.csv(file = "googl.csv", header = T)     
 Appl <- read.csv(file = "aapl.csv", header = T) 
 
 #2. Check the column name and sample data for these 2 data frames
 head(Appl)
 head(Goo)                                          
 class(Goo)
 class(Appl)
 
 #3. Use loop to match whether the dates of 2 stocks are all the same, If not, return the different date.
 n <- dim(Goo)[1]                                   
 m <- dim(Appl)[1]                                  
 uniqueGoo <- list(date = c(), close = c())
 uniqueAppl <- list(date = c(), close = c())
 for(i in 1 : 248){
     s <- 0
     for(j in 1 : 248 ){
         if(as.vector(Goo$Date[i]) != as.vector(Appl$Date[j]) )
             s <- s + 1
    }
     if (s == 248){
         uniqueGoo$date <- c(uniqueGoo$date , as.vector(Goo$Date[i]))
         uniqueGoo$close <- c(uniqueGoo$close, Goo$Close[i])
     }
 }
for(j in 1 : 248){
    s <- 0
    for(i in 1 : 248 ){
        if(as.vector(Goo$Date[i]) != as.vector(Appl$Date[j]))
             s <- s + 1
    }
    if (s == 248){
        uniqueAppl$date <- c(uniqueAppl$date , as.vector(Appl$Date[j]))
        uniqueAppl$close <- c(uniqueAppl$close, Appl$Close[j])
    }
}
 
#4. Besides of loop, is there any other method you can use to matching the date?         
 as.vector(Appl$Date[is.na(match(as.vector(Goo$Date),as.vector(Appl$Date)))])    #in Appl not in Goo
 as.vector(Goo$Date[is.na(match(as.vector(Appl$Date),as.vector(Goo$Date)))])     #in Goo not in Appl

#5. Bind 2 stock close price together with date. If there is difference between 2 dates, 
combine <- merge(Goo, Appl, by = c("Date"), all = TRUE)   
 
#6. Calculate average daily return for these 2 stocks
head(combine) 
library(quantmod)

norGoo <- Delt((combine$Close.x))           #get normal return
norAppl <- Delt(combine$Close.y)

logGoo <- diff(combine$Close.x)             #get log return
logAppl <- diff(combine$Close.y) 
                    
xna <- is.na(norGoo)                        # get mean without NA,  take normal return for example
mean(norGoo[!xna])                          # mean of Goo return = 0.008044594
yna <- is.na(norAppl)  
mean(norAppl[!yna])                         # mean of Appl return = 0.005568926

