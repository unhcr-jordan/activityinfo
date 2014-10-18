############ Script to generate Schema for Activity Info - IRAQ Refugee Response

rm(activity)
rm(att_monit)
rm(att_plan)
rm(indic)
rm(activity_att_monit)
rm(activity_att_plan)
rm(activity_indic)
rm(plan)
rm(monit)
rm(drop)
rm(drop2)
rm(names_monit)
rm(names_plan)


attribute <- read.csv("~/unhcr_r_project/activityinfojordan/config/attribute3rp.csv")
indic <- read.csv("~/unhcr_r_project/activityinfojordan/config/Protection-3rp.csv")

activity <- as.data.frame(unique(indic[,c("ActivityCategory",
                 "ActivityName")]))

activity_att <- merge(x=activity, y=attribute)

indicmin <- indic[ , c( "ActivityCategory", "ActivityName", "Name", "Description",  "Units" )]

indicmin$FormFieldType <- "Indicator"

indicmin$Category <- "Indicator"

indicmin$AttributeValue  <- ""
indicmin$ReportingFrequency	 <- ""
indicmin$LocationType	 <- ""
indicmin$multipleAllowed	 <- ""
indicmin$mandatory	 <- ""
indicmin$sort <- ""

plan <- rbind(activity_att, indicmin)

replace(plan, is.na(plan), "")

plan <- plan[order(plan$ActivityCategory, plan$ActivityName, plan$FormFieldType, plan$Category, plan$Name, plan$AttributeValue ),]

plan$ReportingFrequency <-'Once'
plan$LocationType <- 'SyrRefRespRRP6'

write.csv(plan, file = "~/unhcr_r_project/activityinfojordan/config/plan.csv",na="")

