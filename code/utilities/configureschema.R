############ Script to generate Schema for Activity Info - 2015 Monitoring

database.id <- 1662
schema.rrrp <- getDatabaseSchema(database.id)
activities.rrrp <-asActivityDataFrame(schema.rrrp)
indicators.rrrp <- asIndicatorDataFrame(schema.rrrp)
rrrpindicator <- merge(x=indicators.rrrp, y=activities.rrrp, by="activityId", all=TRUE)

## Subset variables linked to budget
rrrpindicator1 <-  subset(rrrpindicator, rrrpindicator$indicatorCategory != "Budget")
names(rrrpindicator1)

rrrpindicator1$activityCategory <- as.factor(rrrpindicator1$activityCategory)
rrrpindicator1$activityName <- as.factor(rrrpindicator1$activityName)
rrrpindicator1$indicatorCategory <- as.factor(rrrpindicator1$indicatorCategory)
rrrpindicator1$indicatorName <- as.factor(rrrpindicator1$indicatorName)

## Order the activity
rrrpindicator1 <- rrrpindicator1[order(rrrpindicator1$activityCategory, rrrpindicator1$activityName, rrrpindicator1$indicatorCategory, rrrpindicator1$indicatorName),]


############################3
### Reformating the new indicator table

## "ActivityCategory", "ActivityName", "FormFieldType", "Category"  "Name" ,  "Description" , "Units",  "AttributeValue"
rrrpindicator2 <- rename(rrrpindicator1 , c("activityCategory"= "ActivityCategory",
                                            "activityName"="ActivityName",
                                            "indicatorName"="Name",
                                            "indicatorCategory"="Category",
                                            "locationTypeName"="LocationType",
                                            "indicatorCode" =  "Code" ,
                                            "aggregation" =   "Aggregation" ,
                                           # "Description",
                                            "units"="Units",
                                            "reportingFrequency"="ReportingFrequency" ))

indicmin <- rrrpindicator2[ , c( "ActivityCategory", "ActivityName","LocationType", "Name", "Category", "Code", "Aggregation",  "Units","ReportingFrequency","mandatory" )]

indicmin$FormFieldType <- "Indicator"

#indicmin$Category <- "Indicator"

indicmin$AttributeValue  <- ""
#indicmin$ReportingFrequency   <- ""
#indicmin$LocationType	 <- ""
indicmin$multipleAllowed	 <- ""
indicmin$Description   <- ""
#indicmin$mandatory	 <- ""
indicmin$sort <- ""

activity <- indicmin[,c("ActivityCategory","ActivityName")]
activity <- as.data.frame(unique(activity))

##############################################################################################
## Getting attribute from monitoring DB
#database.id <- 1064
#schema <- getDatabaseSchema(database.id)
#include.multiple.selection <- TRUE
#attributes <-
#  do.call(rbind, lapply(schema$activities,
#                        function(activity) {
#                          extractAttributes(activity, include.multiple.selection)
#                        }))
#attributes <- unique(attributes[,c( "name", "multipleAllowed", "group" )])
## Order the attributes
#attributes.or <- attributes[order(attributes$group, attributes$name),]

## A few correction by hand
#write.csv(attributes.or, file = "data/config/attribmonit2015.csv",na="")

attributes.or2 <- read.csv("data/config/attribmonit2015.csv")

#attributes.or2 <- rename(attributes.or2, c("name"="AttributeValue",  "group"="Name"))

attributes.or2$FormFieldType <- "AttributeGroup"

attributes.or2$Category<- ""
#attributes.or2$Name,

attributes.or2$Code <- ""
attributes.or2$Description <- ""
attributes.or2$Units <- ""
#attributes.or2$AttributeValue,
attributes.or2$ReportingFrequency <- ""
attributes.or2$LocationType <- ""
attributes.or2$multipleAllowed <- ""
attributes.or2$mandatory <- ""
attributes.or2$sort <- ""
attributes.or2$Aggregation <- ""

activity_att <- merge(x=activity, y=attributes.or2)
                          
#activity_att  <- activity_att[c(-3)]


###############################################################
#### Now we can merge
                          
### check the lables of the 2 df to append
act <- as.data.frame(names(activity_att))
act <- rename(act,c("names(activity_att)"="nm"))
act <- as.data.frame(act[order(act$nm),])
ind <- as.data.frame(names(indicmin))
ind <- rename(ind,c("names(indicmin)"="nm"))
ind <- as.data.frame(ind[order(ind$nm),])

plant <- merge(act,ind)

plan <- rbind(activity_att, indicmin)

plan <-replace(plan, is.na(plan), "")

plan <- plan[order(plan$ActivityCategory, plan$ActivityName, plan$FormFieldType, plan$Category, plan$Name, plan$AttributeValue ),]

plan$ReportingFrequency <-'Monthly'
plan$LocationType <- 'SyrRefRespRRP6'

write.csv(plan, file = "out/monitor2015.csv",na="")

#
