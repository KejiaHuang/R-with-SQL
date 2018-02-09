library(RMySQL)
library(quantmod)

# Connect to database 
conn = dbConnect(RMySQL::MySQL(),host = "fscwinsrv04.fsc.stevens.edu" ,
                 user = "a513_Kejia",password = "Hh626491",
                 dbname = "a513_kejia",port = 3306)

# Downloads ten stocks data
 MSFT <- data.frame(getSymbols ( "MSFT", from = "2013-01-01", 
                            to = "2014-12-31", auto.assign = FALSE  ))
 NKE <- data.frame(getSymbols ( "NKE", from = "2013-01-01", 
                            to = "2014-12-31", auto.assign = FALSE  ))
 PFE <- data.frame(getSymbols ( "PFE", from = "2013-01-01", 
                            to = "2014-12-31", auto.assign = FALSE  ))
 PG <- data.frame(getSymbols ( "PG", from = "2013-01-01", 
                            to = "2014-12-31", auto.assign = FALSE  ))
 TRV <- data.frame(getSymbols ( "TRV", from = "2013-01-01", 
                            to = "2014-12-31", auto.assign = FALSE  ))
 UTX <- data.frame(getSymbols ( "UTX", from = "2013-01-01", 
                            to = "2014-12-31", auto.assign = FALSE  ))
 UNH <- data.frame(getSymbols ( "UNH", from = "2013-01-01", 
                            to = "2014-12-31", auto.assign = FALSE  ))
 VZ <- data.frame(getSymbols ( "VZ", from = "2013-01-01",
                            to = "2014-12-31", auto.assign = FALSE  ))
 V <- data.frame(getSymbols ( "V", from = "2013-01-01",
                            to = "2014-12-31", auto.assign = FALSE  ))
 WMT <- data.frame(getSymbols ( "WMT", from = "2013-01-01", 
                            to = "2014-12-31", auto.assign = FALSE  ))



 #  Save 10 stock data into your own database as 10 tables
dbWriteTable(conn, "MSFT", MSFT)
dbWriteTable(conn, "NKE", NKE)
dbWriteTable(conn, "PFE", PFE)
dbWriteTable(conn, "PG", PG)
dbWriteTable(conn, "TRV", TRV)
dbWriteTable(conn, "UTX", UTX)
dbWriteTable(conn, "UNH", UNH)
dbWriteTable(conn, "VZ", VZ)
dbWriteTable(conn, "V", V)
dbWriteTable(conn, "WMT", WMT)

# Show table
x <- as.vector(dbGetQuery(conn, "SHOW tables"))
#   Tables_in_a513_kejia
#1                  msft
#2                   nke
#3                   pfe
#4                    pg
#5                   trv
#6                   unh
#7                   utx
#8                     v
#9                    vz
#10                  wmt

# Disconnect from database
dbDisconnect(conn)

# Build stock matrix
date <- read.csv("date.csv", stringsAsFactors = F)
stocks10 <- cbind(date, MSFT[,4], NKE[,4], PFE[,4], PG[,4], TRV[,4], UNH[,4], UTX[,4], V[,4], VZ[,4], WMT[,4])
colnames(stocks10) <- c("date", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "V", "VZ")
head(stocks10)
# Plot
par(mar=c(1,1,1,1))
colors <- rainbow(10)
plot( MSFT[,4], type="l",ylim = c(0, 300), xlab = "date " , ylab = "price")
lines( NKE[,4], type="l", lwd=1.5, col=colors[1] ) 
lines( PFE[,4], type="l", lwd=1.5, col=colors[2] ) 
lines( PG[,4], type="l", lwd=1.5, col=colors[3] ) 
lines( TRV[,4], type="l", lwd=1.5, col=colors[4] ) 
lines( UNH[,4], type="l", lwd=1.5, col=colors[5] ) 
lines( UTX[,4], type="l", lwd=1.5, col=colors[6] ) 
lines( V[,4], type="l", lwd=1.5, col=colors[7] ) 
lines( VZ[,4], type="l", lwd=1.5, col=colors[8] ) 
lines( WMT[,4], type="l", lwd=1.5, col=colors[9] ) 
title("stock price")
#ggplot2
library(ggplot2)
xx <- rownames(MSFT)[]
xx <- as.Date(xx)
data <- data.frame(x = xx, y = stocks10[,2:11])
ggplot(data, aes(date)) +
    geom_line(aes(x = xx, y = data[,2], colour = colors[1])) +
    geom_line(aes(x = xx, y = data[,3], colour = colors[2])) +
    geom_line(aes(x = xx, y = data[,4], colour = colors[3])) +
    geom_line(aes(x = xx, y = data[,5], colour = colors[4])) +
    geom_line(aes(x = xx, y = data[,6], colour = colors[5])) +
    geom_line(aes(x = xx, y = data[,7], colour = colors[6])) +
    geom_line(aes(x = xx, y = data[,8], colour = colors[7])) +
    geom_line(aes(x = xx, y = data[,9], colour = colors[8])) +
    geom_line(aes(x = xx, y = data[,10], colour = colors[9])) +
    geom_line(aes(x = xx, y = data[,11], colour = colors[10])) +
    xlab("Date") +
    ylab("Stock Price") +
    ggtitle("ggplot stock price")

# histogram
timez <- read.csv("twitter.csv", stringsAsFactors = F)
tiz <- table(timez)
head(tiz)
t<- data.frame(tiz)

x = t$timez
y = t$Freq
data = data.frame( c(1:140), y )
ggplot(data, aes(c(1:140),y)) + geom_line( color = "blue") + geom_point()+
    ggtitle( " Frequency")+labs(x = "timezone", y = "Frequency")



# Connect to database 
conn = dbConnect(RMySQL::MySQL(),host = "fscwinsrv04.fsc.stevens.edu" ,
                 user = "a513_Kejia",password = "Hh626491",
                 dbname = "fe513_twitter",port = 3306)

sentense <- read.csv("sentence.csv", stringsAsFactors = F)
head(sentense)
x <- c()
for( i in 1: length(sentense)){
    x <- c(x,sentense[i])
}

library(tm)
step1 <- Corpus(VectorSource(x))
step2 <- tm_map(step1, removePunctuation)

# Change all to lower case
step2 <- tm_map(step2, content_transformer(tolower))

# Delete "stop words" from the document
# Stop word can be read from existed list or customized list
stopw <- c(stopwords("en"), "legends")
step3 <- tm_map(step2, removeWords, stopw)

# Get the result of word & its frequency
step4 <- TermDocumentMatrix(step3)
step4 <- as.matrix(step4)


final <- data.frame(word = rownames(step4),freq1=step4[,1])
head(final, 10)

# Plot word cloud
library(wordcloud)
par(mar=c(1,1,1,1))
par(mfrow = c(1, 2))
wordcloud(words = final$word, freq = final$freq1, min.freq = 2, colors=brewer.pal(8, "Dark2"))
ggplot(final, aes(c(1:5714),final$freq1)) + geom_line( color = "blue") + geom_point()+
    ggtitle( " Frequency")+labs(x = "word", y = "Frequency")
