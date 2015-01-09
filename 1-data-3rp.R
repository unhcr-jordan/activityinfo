#load required packages

source("0-packages.R")

#####################################################
#  ActivityInfo Monitoring analysis script          #
#####################################################

# authenticate
activityInfoLogin()


### JOR-#RP Plan Database Jordan db 1662

data.1662 <- getIndicatorValueTable(1662)

## get all locations
locations <- as.data.frame(getLocations(50512))

# put the DB schema  in a  R list
schema1662 <- getDatabaseSchema(1662)


# list all activities and put them in a data frame
activity.table <-asActivityDataFrame(schema1662)

act1 <- activity.table[1,2]

#sites <- getSitesDataFrame(act1)

acti <- schema1662$activities[[1]]
df <- getSitesDataFrame(acti)


uid <- as.numeric("1")
disnumber <- length(district)
for (i in 1:disnumber)
{
  districti <- district[i,]
  assign(paste("gg",i,sep=""), gIntersection(districti,vorototal,byid=TRUE))
  temp.data <- gIntersection(districti,vorototal,byid=TRUE)
  n <- length(slot(temp.data, "polygons"))
  temp.data <- spChFIDs(temp.data, as.character(uid:(uid+n-1)))
  uid <- as.numeric( uid + n)
  poly.data <- spRbind(poly.data,temp.data)
  i <- i + 1;
}


