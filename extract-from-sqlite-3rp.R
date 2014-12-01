#########################################
#  ActivityInfo extraction script       #
#########################################


#install.packages("reshape2", "RSQLite")
library(reshape2)
library(RSQLite)
library(plyr)
# Connect to local SQLite replica installed when offline usage is enabled
# replace 9 with the name of the database used in your chrome/Default/databases/https_www.syrianrefugeeresponse.org_0 folder 
## -- copy the database in your rstudio project

# C:\Users\Kaleem\AppData\Local\Google\Chrome\User Data\Default\databases\https_www.syrianrefugeeresponse.org_0
con <- dbConnect(RSQLite::SQLite(), "data/2")


#con <- dbConnect(RSQLite::SQLite(), "/home/edouard/.config/google-chrome/Default/databases/https_www.activityinfo.org_0/7")

#######################################################
##### Exaction for RRP6 Planning Database  DB - 1662

# get sites
sitesplan <- dbGetQuery(con, paste("select s.siteid siteid, a.category sector, a.name activity, p.name partner, loc.name location, ",
                                   "s.comments from site s",
                                   "left join activity a on (s.activityid = a.activityid)",
                                   "left join location loc on (s.locationid= loc.locationid)",
                                   "left join partner p on (s.partnerid = p.partnerid)",
                                   "where a.databaseid=1662 and s.datedeleted is null and a.datedeleted is null"));



# get attributes
attrsplan <- dbGetQuery(con, paste("select s.siteid siteid, ag.name agroup, ag.multipleallowed multiple, at.name attribute from site s",
                                   "left join activity a on (s.activityid = a.activityid)",
                                   "left join attributevalue av on (av.siteid=s.siteid)",
                                   "left join attribute at on (at.attributeid=av.attributeid)",
                                   "left join attributegroup ag on (ag.attributegroupid=at.attributegroupid)",
                                   "where a.databaseid=1662 and s.datedeleted is null and a.datedeleted is null and at.datedeleted is null and ag.datedeleted is null and av.value = 1"));

# reformat attributes
attrplan.single <- subset(attrsplan, multiple == 0)
attrplan.single.wide <- dcast(attrplan.single, siteid ~ agroup, value.var="attribute")

#attrplan.multiple <- subset(attrsplan, multiple == 1)
#attrplan.multiple.wide <- dcast(attrplan.multiple, siteid ~ agroup + attribute, value.var="multiple", fill=0)

# get units
unitsplan <- dbGetQuery(con, paste("select s.siteid siteid, level.name level, entity.name name, loc.name location from site s",
                                   "left join activity a on (s.activityid = a.activityid)",
                                   "left join location loc on (s.locationid= loc.locationid)",
                                   "left join locationadminlink link on (link.locationid = s.locationid)",
                                   "left join adminentity entity on (link.adminentityid=entity.adminentityid)",
                                   "left join adminlevel level on (entity.adminlevelid=level.adminlevelid)",
                                   "where a.databaseid=1662 and s.datedeleted is null and a.datedeleted is null"));
# reformat units
unitsplanblank <- subset(unitsplan, is.na(name))

#unitsplan <- subset(unitsplan, ! is.na(name))
unitsplan.wide <- dcast(unitsplan, siteid+location ~ level, value.var="name")

# start merging sites with attributes
#sitesplan <- merge(sitesplan, attrplan.multiple.wide)
#sitesplan <- merge(sitesplan, attrplan.single.wide)

# Write in files
#write.csv(sitesplan, file="RRP6plan_Sites.csv", row.names=F, na="")

# Try to merge sites, attributes and indicators
ivplan <- dbGetQuery(con, paste("select s.siteid siteid, rp.reportingperiodid, rp.date1, rp.date2, i.indicatorid, i.category, i.name, iv.value, i.units from indicatorvalue iv",
                                "left join reportingperiod rp on(rp.reportingperiodid=iv.reportingperiodid)", 
                                "left join site s on (rp.siteid = s.siteid)",
                                "left join activity a on (s.activityid = a.activityid)",
                                "left join indicator i on (iv.indicatorid = i.indicatorid)",
                                "where a.databaseid=1662 and s.datedeleted is null and a.datedeleted is null and i.dateDeleted is null"));




indicatorsplan <- merge(sitesplan, ivplan)
write.csv(indicatorsplan, file="out3rp/JOR-3RP-Plan_Indicatorsall.csv",row.names=F, na="")

# Add long narrative
#rrp6long <- read.csv("~/unhcr_r_project/activityinfo/rrp6long.csv")
#sitesplanlong <- merge(sitesplan, rrp6long)
#indicatorsplanlong <- merge(sitesplanlong, ivplan2)
#write.csv(indicatorsplan, file="RRP6Plan_Indicatorsalllong.csv",row.names=F, na="")

####################################################
### Check submission without budget
ivplancheck <- ivplan [ which(ivplan$Name=='Budgetary Requirement for 2015'), ]
rm(indicatorsplancheck)
indicatorsplancheck <- merge(x=sitesplan, y=ivplancheck, by="siteid", all=TRUE)

indicatorsplancheck2 <- subset(indicatorsplancheck, is.na(Name))
write.csv(indicatorsplancheck2, file="out3rp/submissionwihtoutbudget.csv",row.names=F, na="")
sitesplanunique <- unique(sitesplan)
ivplancheck2 <- unique(ivplancheck)


###########################


##############
# Check wrong extract
wrong <- indicatorsplan[ which(indicatorsplan$siteid=='683969144'), ]

### merge with Location
## Check blank location
indicatorsplanblank <- merge(x=indicatorsplan, y=unitsplanblank, by="siteid")
indicatorsplanblank <- indicatorsplanblank[ which(indicatorsplanblank$Name=='Budgetary Requirement for 2015'), ]
indicatorsplanblank <- reorder(indicatorsplanblank, indicatorsplanblank$sector)




###################################################
# Generate a reformated extract for the dataviz
########################################################
### merge with Full geographic three
indicatorsplan2 <- merge(x=indicatorsplan, y=unitsplan.wide, by="siteid")

## Extract target pop
## clean first labels from the config

indicatorsplan2$Name[indicatorsplan2$Name=="Urban/Rural Syrian Men (Age 18 and Above)"] <- "Urban/Rural Syrian Men (Age 18 and above)"
indicatorsplan2$Name[indicatorsplan2$Name=="Urban/Rural Syrian Men (Age 18 and Abocve)"] <- "Urban/Rural Syrian Men (Age 18 and above)"
indicatorsplan2$Name[indicatorsplan2$Name=="Camps Syrian  Boys (Age 0-17)"] <- "Camps Syrian Boys (Age 0-17)"
indicatorsplan2$Name[indicatorsplan2$Name=="Camps Syrian  Girls (Age 0-17)"] <- "Camps Syrian Girls (Age 0-17)" 
indicatorsplan2$Name[indicatorsplan2$Name=="Camps Syrian Men (Age 18 and above)-"] <- "Camps Syrian Men (Age 18 and above)" 
indicatorsplan2$Name[indicatorsplan2$Name=="Camps Syrian Men (Age 18 and above)-"] <- "Camps Syrian Men (Age 18 and above)" 
indicatorsplan2$Name[indicatorsplan2$Name=="Camps Syrian  Women (Age 18 and above)"] <-  "Camps Syrian Women (Age 18 and above)"

#levels(indicatorsplan2$Name)

#indicatorsplan2$Name <- as.factor(indicatorsplan2$Name)
levels(indicatorsplan2$Name)


target <- indicatorsplan2[ which(indicatorsplan2$Name=="Camps Syrian Boys (Age 0-17)"|indicatorsplan2$Name=="Camps Syrian Girls (Age 0-17)"|
                                   indicatorsplan2$Name=="Camps Syrian Men (Age 18 and above)"|indicatorsplan2$Name=="Camps Syrian Women (Age 18 and above)"|
                                   indicatorsplan2$Name=="Host Community Boys (Age 0-17)"|indicatorsplan2$Name=="Host Community Girls (Age 0-17)"|
                                   indicatorsplan2$Name=="Host Community Men (Age 18 and above)"|indicatorsplan2$Name=="Host Community Women (Age 18 and above)"|
                                   indicatorsplan2$Name=="Urban/Rural Syrian Boys (Age 0-17)"|indicatorsplan2$Name=="Urban/Rural Syrian Girls (Age 0-17)"|
                                   indicatorsplan2$Name=="Urban/Rural Syrian Men (Age 18 and above)"|indicatorsplan2$Name=="Urban/Rural Syrian Women (Age 18 and above)"), ]

target.melt <- melt(target, id=c(1,12), measure=c(13))
target.cast <- dcast(target.melt, siteid ~ Name, sum)

# Function that will sum values even if we have NA
psum <- function(..., na.rm=FALSE) {
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
}

target.cast <- rename(target.cast, c("Camps Syrian Boys (Age 0-17)" = "campboy", "Camps Syrian Girls (Age 0-17)"="campgirl", 
                                  "Camps Syrian Men (Age 18 and above)"="campmen",	"Camps Syrian Women (Age 18 and above)"= "campwomen",
                                  "Host Community Boys (Age 0-17)"="hostboy", "Host Community Girls (Age 0-17)"="hostgirl",
                                  "Host Community Men (Age 18 and above)"="hostmen",	"Host Community Women (Age 18 and above)"="hostwomen",
                                  "Urban/Rural Syrian Boys (Age 0-17)"="urbboy", "Urban/Rural Syrian Girls (Age 0-17)"="urbgirl",
                                  "Urban/Rural Syrian Men (Age 18 and above)"="urbmen",	"Urban/Rural Syrian Women (Age 18 and above)" = "urbwomen"))

target.cast$camp <- psum(target.cast$campboy ,target.cast$campgirl, target.cast$campmen, target.cast$campwomen, na.rm = TRUE)
target.cast$host <- psum(target.cast$hostboy ,target.cast$hostgirl, target.cast$hostmen, target.cast$hostwomen, na.rm = TRUE)
target.cast$urban <- psum(target.cast$urbboy ,target.cast$urbgirl, target.cast$urbmen, target.cast$urbwomen, na.rm = TRUE)

target.cast$boy <- psum(target.cast$campboy ,target.cast$hostboy, target.cast$urbboy, na.rm = TRUE)
target.cast$girl <- psum(target.cast$campgirl ,target.cast$hostgirl, target.cast$urbgirl, na.rm = TRUE)
target.cast$men <- psum(target.cast$campmen ,target.cast$hostmen, target.cast$urbmen, na.rm = TRUE)
target.cast$women <- psum(target.cast$campwomen ,target.cast$hostwomen, target.cast$urbwomen, na.rm = TRUE)

target.cast$gender <- paste( with(target.cast, ifelse(target.cast$boy>0, paste0("boy"),"")),
                             with(target.cast, ifelse(target.cast$girl>0, paste0("girl"),"")) ,
                             with(target.cast, ifelse(target.cast$men>0, paste0("men"),"")) ,
                             with(target.cast, ifelse(target.cast$women>0, paste0("women"),"")) ,
                             sep= "-")

target.cast$gender[target.cast$gender==""] <- "2-No reported target"
target.cast$gender[target.cast$gender=="---"] <- "2-No reported target"
target.cast$gender[target.cast$gender=="---women"] <- "8-Women only"
target.cast$gender[target.cast$gender=="--men-"] <- "9-Men only"
target.cast$gender[target.cast$gender=="--men-women"] <- "7-Adult"
target.cast$gender[target.cast$gender=="-girl--"] <- "4-Girl Only"
target.cast$gender[target.cast$gender=="-girl--women"] <- "3-Mix"
target.cast$gender[target.cast$gender=="-girl-men-"] <- "3-Mix"
target.cast$gender[target.cast$gender=="-girl-men-women"] <- "3-Mix"
target.cast$gender[target.cast$gender=="boy---"] <- "5-Boy only"
target.cast$gender[target.cast$gender=="boy--men-"] <- "3-Mix"
target.cast$gender[target.cast$gender=="boy--men-women"] <- "3-Mix"
target.cast$gender[target.cast$gender=="boy-girl--"] <- "6-Minor only"
target.cast$gender[target.cast$gender=="boy-girl--women"] <- "3-Mix"
target.cast$gender[target.cast$gender=="boy-girl-men-"] <- "3-Mix"
target.cast$gender[target.cast$gender=="boy-girl-men-women"] <- "1-All"

target.cast$gender <- as.factor(target.cast$gender)
levels(target.cast$gender)

target.cast$target <- paste( with(target.cast, ifelse(target.cast$camp>0, paste0("camp"),"")),
                             with(target.cast, ifelse(target.cast$host>0, paste0("host"),"")) ,
                             with(target.cast, ifelse(target.cast$urban>0, paste0("urban-rural"),"")) ,
                             sep= "-")
target.cast$target[target.cast$target==""] <- "1-No reported target"
target.cast$target[target.cast$target=="--"] <- "1-No reported target"

target.cast$target[target.cast$target=="camp-host-urban-rural"] <- "2-All"

target.cast$target[target.cast$target=="-host-"] <- "3-Host only"
target.cast$target[target.cast$target=="-host-"] <- "3-Host only"
target.cast$target[target.cast$target=="camp--"] <- "4-Camp only"

target.cast$target[target.cast$target=="--urban-rural"] <- "5-Urban & Rural only"
target.cast$target[target.cast$target=="camp--urban-rural"] <- "6-Camp, Urban & Rural"
target.cast$target[target.cast$target=="-host-urban-rural"] <- "7-Host, Urban & Rural"
target.cast$target[target.cast$target=="camp-host-"] <- "8-Camp & Host"

target.cast$target <- as.factor(target.cast$target)
levels(target.cast$target)

target.cast$total <- psum (target.cast$camp ,target.cast$host, target.cast$urban, na.rm = TRUE)




rm(dataviz)
dataviz <- indicatorsplan2[ which(indicatorsplan2$Name=='Budgetary Requirement for 2015'), ]

## Check on units
indicatorsplan2$Units <- as.factor(indicatorsplan2$Units)
levels(indicatorsplan2$Units)
indicatorsplan2$Units[indicatorsplan2$Units=="USD$"] <-  "USD $"
datavizunit <- indicatorsplan2[ which(indicatorsplan2$Units=='USD $'), ]

####### merge with target benef
dataviz <- merge(x=dataviz, y=target.cast, by="siteid", all.x=TRUE)

dataviz$target[is.na(dataviz$target)] <- "1-No reported target"
dataviz$target[dataviz$target==""] <- "1-No reported target"
dataviz$gender[is.na(dataviz$gender)] <- "2-No reported target"

levels(dataviz$target)
summary(dataviz$target)
levels(dataviz$gender)

#Classify unit cost

dataviz$unitcost <- dataviz$Value / dataviz$total

datatest1 <- dataviz[is.na(dataviz$unitcost),]
datatest2 <- dataviz[dataviz$unitcost==Inf,]

dataviz$unitcost[is.na(dataviz$unitcost)] <- 0
dataviz$unitcost[dataviz$unitcost==Inf] <- 0

library(classInt) ## Classififcation
dataviz$unitcost.class <- as.factor(findCols(classIntervals(dataviz$unitcost, n = 7, style = "fixed", fixedBreaks = c(0, 0.0001, 50, 100, 250, 500, 2000, 10000000))))
dataviz$costunit <- revalue(dataviz$unitcost.class, c(`1` = "No Unit cost", `2` = "a- 0-49$", `3` = "b- 50-99$", `4` = "c- 100-249$", `5` = "d- 250-499$", `6` = "e- 500-2000$", `7` = "f- >2000$"))

summary(dataviz)

# row.names  siteid	sector	activity	partner	location	comments	ReportingPeriodId	Date1	Date2	IndicatorId	Category	Name	Value	Units
# Sector  Objective	Output	Partner	Area	RegionCODE	Category	Total



dataviz$sector2 <- substr(dataviz$sector , (regexpr("]", dataviz$sector , ignore.case=FALSE, fixed=TRUE))+1,50)
dataviz$sector1 <- substr(dataviz$sector ,1, (regexpr("[", dataviz$sector , ignore.case=FALSE, fixed=TRUE))-1)



dataviz$Category <- substr(dataviz$activity ,(regexpr("[", dataviz$activity , ignore.case=FALSE, fixed=TRUE))+1, (regexpr("]", dataviz$activity , ignore.case=FALSE, fixed=TRUE))-4)
dataviz$Category[dataviz$Category=="RES "] <- "Resilience"
dataviz$Category[dataviz$Category=="RES"] <- "Resilience"
dataviz$Category[dataviz$Category=="REF"] <- "Refugee"
dataviz$Category <- as.factor(dataviz$Category)
levels(dataviz$Category)

dataviz$activity2 <- substr(dataviz$activity , (regexpr("]", dataviz$activity , ignore.case=FALSE, fixed=TRUE))+1,50)
## get shorten version of the column to decrease dataviz size
dataviz <-rename(dataviz, c("sector1"="Sector"  , "sector2"="Objective"  ,"activity2"="Output"  , "partner"="Partner"  , "location.x"="Area" , "Date1"="Start"  , "Date2"="End"  , "Value"="Total"))

#dataviz$Area <- as.factor(dataviz$Area)
#levels(dataviz$Area)
dataviz$Governorate <- as.factor(dataviz$Governorate)
levels(dataviz$Governorate)



regionactivityinfo <- read.csv("data/regionactivityinfo.csv")


dataviz <-rename(dataviz, c("Refugee Camps"="Refugee.Camps")) 

dataviz <- merge(x=dataviz, y=regionactivityinfo, by="Governorate", all.x=TRUE)

dataviz$RegionCODE <- as.character(dataviz$RegionCODE)
dataviz$Area2 <- as.character(dataviz$Area2)


test <- dataviz$Area[!is.na(dataviz$Refugee.Camps)]

dataviz$RegionCODE[!is.na(dataviz$Refugee.Camps)] <- "2"
#dataviz$Area2[!is.na(dataviz$Refugee.Camps)] <- dataviz$Refugee.Camps

dataviz <- transform(dataviz, Area2 = ifelse(is.na(dataviz$Refugee.Camps), Area2, dataviz$Refugee.Camps))

dataviz$RegionCODE[dataviz$Area=="Camp"] <- "2"
dataviz$Area2[dataviz$Area=="Camp"] <- "ALL CAMPS IN JORDAN"

dataviz$RegionCODE[dataviz$Area=="Country Wide"] <- "1"
dataviz$Area2[dataviz$Area=="Country Wide"] <- "Country Wide Intervention"

dataviz$RegionCODE[dataviz$Area=="Cyber City Refugee Center"] <- "2"
dataviz$Area2[dataviz$Area=="Cyber City Refugee Center"] <- "Cyber City Refugee Center"

dataviz$RegionCODE[dataviz$Area=="King Abdullah Park Refugee Center"] <- "2"
dataviz$Area2[dataviz$Area=="King Abdullah Park Refugee Center"] <- "King Abdullah Park Refugee Center"

dataviz$Area[dataviz$Area=="Azraq Camp Village 3"] <- "Azraq Camp"
dataviz$Area[dataviz$Area=="Zaatari Camp (all district)"] <- "Zaatari Camp"
dataviz$Area[dataviz$Area=="Zaatari District 10"] <- "Zaatari Camp"
dataviz$Area[dataviz$Area=="Zaatari District 12"] <- "Zaatari Camp"
dataviz$Area[dataviz$Area=="Zaatari District 3"] <- "Zaatari Camp"
dataviz$Area[dataviz$Area=="Zaatari District 4"] <- "Zaatari Camp"
dataviz$Area[dataviz$Area=="Zaatari District 5"] <- "Zaatari Camp"
dataviz$Area[dataviz$Area=="Zaatari District 6"] <- "Zaatari Camp"
dataviz$Area[dataviz$Area=="Zaatari District 7"] <- "Zaatari Camp"
dataviz$Area[dataviz$Area=="Zaatari District 8"] <- "Zaatari Camp"
dataviz$Area[dataviz$Area=="Zaatari District 9"] <- "Zaatari Camp"

## Recode sector names
#"BASIC NEEDS" "EDU"         "FOOD/LIV"    "HLTH"        "MUNICIPAL"   "PROT"        "SHLT"        "WASH"  

dataviz$Sector[dataviz$Sector=="EDU"] <-"EDUCATION"
dataviz$Sector[dataviz$Sector=="FOOD/LIV"] <-"FOOD/LIVELIHOOD"
dataviz$Sector[dataviz$Sector=="PROT"] <-"PROTECTION"
dataviz$Sector[dataviz$Sector=="SHLT"] <-"SHELTER"
dataviz$Sector[dataviz$Sector=="HLTH"] <-"HEALTH"

dataviz$Sector <- as.factor(dataviz$Sector)
levels(dataviz$Sector)

#dataviz <- reorder(dataviz, dataviz$Sector)

write.csv(dataviz, file="out3rp/all.csv",row.names=F, na="")

## select the column of interest for the dataviz
dataviz <-dataviz[ , c(
  "Governorate","sector","activity","Partner","Area","Refugee.Camps","Start","End","Total","costunit","gender","target","Objective","Sector","Category","Output","RegionCODE","Area2")]

write.csv(dataviz, file="out3rp/3rp.csv",row.names=F, na="")

## Subset on refugees only
dataviz2 <- subset( dataviz, dataviz$Category=="Refugee")
write.csv(dataviz2, file="out3rp/3rp2.csv",row.names=F, na="")


######Clean

rm(attrplan.single)
rm(attrplan.single.wide)
rm(attrsplan)
rm(datatest1)
rm(datatest2)
rm(datavizunit)
rm(indicatorsplan)
rm(indicatorsplan2)
rm(indicatorsplancheck)
rm(indicatorsplancheck2)
rm(ivplan)
rm(ivplancheck)
rm(ivplancheck2)
rm(regionactivityinfo)
rm(sitesplan)
rm(sitesplanunique)
rm(target)
#rm(target.cast)
#rm(target.melt)
rm(unitsplan)
rm(unitsplan.wide)
rm(unitsplanblank)
rm(wrong)
rm(con)
rm(indicatorsplanblank)
rm(test)
