

## step 1 import list of activities & indicators
rm(activity)
activity <- read.csv("data/config/2016.csv")


## Step 2: import attributes
attribute3rp <- read.csv("data/config/attribute3rp.csv", comment.char="#")


## step3: unique list of activities
rm(activity.unique)
activity.unique <- activity[,c("ActivityCategory","ActivityName")]
activity.unique <- as.data.frame(unique(activity.unique))
write.csv(activity.unique, file = "out/plan2016unique.csv",na="")


## step4: merge unique list of activities with attributes
rm(activity.unique.attr)
activity.unique.attr <- merge (activity.unique, attribute3rp )

names(activity.unique.attr)
names(activity)

activity$FormFieldType <- "Indicator"
activity$Category <- ""
activity$AttributeValue <- ""
activity$ReportingFrequency <- "Once"
activity$LocationType <- "SyrRefRespRRP6"
activity$multipleAllowed <- "Once"
activity$mandatory <- "Once"
activity$multipleAllowed <- ""
activity$sort <- ""
#activity$Aggregation <- ""

rm(plan2016)
plan2016 <- rbind(activity,activity.unique.attr )

plan2016 <- plan2016[order(plan2016$ActivityCategory, plan2016$ActivityName, plan2016$FormFieldType, plan2016$Category, plan2016$Name, plan2016$sort ),]


write.csv(plan2016, file = "out/plan2016.csv",na="")
