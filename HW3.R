 #1. RMySQL Connector
# Connect to HFSL database server using R program
library(RMySQL)
conn = dbConnect(RMySQL::MySQL(),host = "fscwinsrv04.fsc.stevens.edu" ,user = "a513_Kejia",password = "Hh626491",
                dbname = "fe513_healthcare",port = 3306)
# Pick up one table in one database
x <- as.vector(dbGetQuery(conn, "SHOW tables"))
x
# Disconnect from the database server
dbDisconnect(conn)

#2. R function return name
name <- function(x) {
    conn = dbConnect(RMySQL::MySQL(),host = "fscwinsrv04.fsc.stevens.edu" ,user = "a513_Kejia",password = "Hh626491",
                     dbname = x,port = 3306)
    v <- (dbGetQuery(conn, "SHOW tables"))
    dbDisconnect(conn)
    return(v)
}

y <- "fe513_twitter"
name(y)
#print 5 sample records
library(RODBCext)
print5 <- function(x,y) {#This function need package RODBCext
    con = dbConnect(RMySQL::MySQL(),host = "fscwinsrv04.fsc.stevens.edu" ,user = "a513_Kejia",password = "Hh626491",
                     dbname = y, port = 3306)
    query <- paste0("select * from ",x," limit 10")
    library(RODBCext)
    res <- dbSendQuery(con, query)
    dbFetch(res, n = 5)
}
k="p_backup"
y <- "fe513_twitter"
print5(k,y)














