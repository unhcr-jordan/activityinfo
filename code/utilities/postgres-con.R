library(RPostgreSQL)

## loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## Open a connection
con <- dbConnect(drv, dbname="jordan",host="localhost",port=5432,
                 user=" ",password=" ")


#Read table from PostgreSQL into R data frame:
refcalc <- dbReadTable(con, c("progres","level_1_calc"))
campcalc <- dbReadTable(con, c("progres","maplevel_camp2"))
dataviz.melt <- melt(dataviz, id=c(13:18), measure=c(9))
names(dataviz.melt)
#"Objective"  "Sector"     "Category"   "Output"     "RegionCODE" "Area2"      "variable"   "value"

dataviz.melt.ref <- subset( dataviz.melt, dataviz$Category=="Refugee")

dataviz.melt.ref.ag <- aggregate(cbind(value) ~ RegionCODE + Area2 + Sector + Objective + Category + Output , data = dataviz.melt.ref, FUN = sum, na.rm = TRUE)
dataviz.melt.ref.ag$id <- as.integer(rownames(dataviz.melt.ref.ag))



#Write results back to PostgreSQL:
dbWriteTable(con, c("budget","budg3rp"), value=dataviz.melt.ref.ag,overwrite=TRUE,row.names=FALSE)

dataviz.melt.ref.agg <- aggregate(cbind(value) ~ RegionCODE + Area2  , data = dataviz.melt.ref, FUN = sum, na.rm = TRUE)
dataviz.melt.ref.agg$id <- as.integer(rownames(dataviz.melt.ref.agg))

ctr <- dataviz.melt.ref.agg[ which(dataviz.melt.ref.agg$Area2=="Country Wide Intervention"), c("value")] 
refcalc$budcont <- refcalc$refpc * ctr 

ctrcamp <- dataviz.melt.ref.agg[ which(dataviz.melt.ref.agg$Area2=="ALL CAMPS IN JORDAN"), c("value")] 
campcalc$budcontcamp <- campcalc$pcref * ctrcamp 

dataviz.melt.ref.final <- merge (x=dataviz.melt.ref.agg, y=refcalc, by.x="RegionCODE", by.y="adm1_code", all.x=TRUE )
dataviz.melt.ref.final <- merge (x=dataviz.melt.ref.final , y=campcalc, by.x="Area2", by.y="alt_name", all.x=TRUE )

dataviz.melt.ref.final$value1 <- dataviz.melt.ref.final$value
dataviz.melt.ref.final$value <- psum(dataviz.melt.ref.final$value1 , dataviz.melt.ref.final$budcont)
dataviz.melt.ref.final$value <- psum(dataviz.melt.ref.final$value , dataviz.melt.ref.final$budcontcamp)
#Write results back to PostgreSQL:
dbWriteTable(con, c("budget","budg3rpall"), value=dataviz.melt.ref.agg,overwrite=TRUE,row.names=FALSE)


dataviz.cast <- dcast(dataviz.melt, Objective + Sector + Category+ Output ~ Area2 , sum) 
names(dataviz.cast)
#dataviz.cast <- rename( "Country Wide Intervention"= "countrywide")




## Closes the connection
dbDisconnect(con)

## Frees all the resources on the driver
#dbUnloadDriver(drv)
#rm(con)
#rm(drv)