library(RMySQL)
library(ggplot2)
library(colorspace)
# Get data from fe513_twitter twitter_message
conn = dbConnect( RMySQL::MySQL() , host = "fscwinsrv04.fsc.stevens.edu" , user = "a513_Kejia", password = "Hh626491",
                  dbname = "fe513_twitter", port = 3306)
a = dbReadTable( conn , "twitter_message" )
dbDisconnect ( conn )


# Plot the histogram in default R functions 
hist( a$user_id , col = "blue" , main ="Histogram with user id", xlab = "user id " , ylab = "frequency")

# Plot message frequency with  default R functions

table_id <- data.frame( table ( a$user_id ) )

plot( as.numeric(levels(table_id$Var1)) , table_id$Freq ,type = "o", 
      col = "blue", main = "Message frequency" , xlab = "user id " , ylab = "frequency" )

# ggplot histogram
class(a)
head(a)

 ggplot(a, aes(user_id)) + geom_histogram( color = "black", fill = "blue" ) +
 ggtitle( "Histogram")+labs(x = "User id", y = "Frequency")


# ggplot message frequency
x = as.numeric(levels(table_id$Var1))
y = table_id$Freq
data = data.frame( x, y )
ggplot(data, aes(x, y)) + geom_line( color = "blue") + geom_point()+
ggtitle( "Message Frequency")+labs(x = "User id", y = "Frequency")



