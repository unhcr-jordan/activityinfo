library(RJSONIO)

dataviz.agg <- aggregate(cbind(Total) ~ RegionCODE + Area2 + Partner + Sector , data = dataviz2, FUN = sum, na.rm = TRUE)

makeList<-function(x){
  if(ncol(x)>2){
    listSplit<-split(x[-1],x[1],drop=T)
    lapply(names(listSplit),function(y){list(name=y,children=makeList(listSplit[[y]]))})
  }else{
    lapply(seq(nrow(x[1])),function(y){list(name=x[,1][y],Percentage=x[,2][y])})
  }
}


jsonOut<-toJSON(list(name="dataviz.agg",children=makeList(dataviz.agg[-1])))
cat(jsonOut)

###########################################
# option 2

list1<-split(subset(dataviz.agg,select=c(-Sector)),dataviz.agg$Sector)
list2<-lapply(list1,function(x){split(subset(x,select=c(-Partner)),x$Partner,drop=TRUE)})
list3<-lapply(list2,function(x){lapply(x,function(y){split(subset(y,select=c(-RegionCODE,-Area2)),y$Total,drop=TRUE)})})
jsonOut<-toJSON(list(MyData=list3))
jsonOut1<-gsub('([^\n]*?): \\{\n "Percentage"','\\{"name":\\1,"Percentage"',jsonOut)
jsonOut2<-gsub('"([^"]*?)": \\{','"name":"\\1","children":\\{',jsonOut1)


#Write geojson
#====

#Load libraries
library(rgdal)

#dataMap is a dataframe with coordinates on cols 11 (LATITUDE) and 12 (LONGITUDE)
#Transfor coordinates to numeric
dataMap$LATITUDE <- as.numeric(dataMap$LATITUDE)
dataMap$LONGITUDE <- as.numeric(dataMap$LONGITUDE)
dataMap.SP <- SpatialPointsDataFrame(dataMap[,c(12,11)],dataMap[,-c(12,11)])
str(dataMap.SP) # Now is class SpatialPointsDataFrame

#Write as geojson
writeOGR(dataMap.SP, 'dataMap.geojson','dataMap', driver='GeoJSON') 