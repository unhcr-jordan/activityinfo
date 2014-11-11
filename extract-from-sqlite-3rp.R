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

ivplancheck <- ivplan [ which(ivplan$Name=='Budgetary Requirement for 2015'), ]
rm(indicatorsplancheck)
indicatorsplancheck <- merge(x=sitesplan, y=ivplancheck, by="siteid", all=TRUE)

indicatorsplancheck2 <- subset(indicatorsplancheck, is.na(Name))

sitesplanunique <- unique(sitesplan)
ivplancheck2 <- unique(ivplancheck)

write.csv(ivplan, file="RRP6plan_Indics.csv", row.names=F, na="")
indicatorsplan <- merge(sitesplan, ivplan)
write.csv(indicatorsplan, file="out3rp/JOR-3RP-Plan_Indicatorsall.csv",row.names=F, na="")

# Add long narrative
#rrp6long <- read.csv("~/unhcr_r_project/activityinfo/rrp6long.csv")
#sitesplanlong <- merge(sitesplan, rrp6long)
#indicatorsplanlong <- merge(sitesplanlong, ivplan2)
#write.csv(indicatorsplan, file="RRP6Plan_Indicatorsalllong.csv",row.names=F, na="")


### merge with Location

## Check balnk location
indicatorsplanblank <- merge(x=indicatorsplan, y=unitsplanblank, by="siteid")
indicatorsplanblank <- indicatorsplanblank[ which(indicatorsplanblank$Name=='Budgetary Requirement for 2015'), ]
indicatorsplanblank <- reorder(indicatorsplanblank, indicatorsplanblank$sector)

### merge with Full geographic three
indicatorsplan2 <- merge(x=indicatorsplan, y=unitsplan.wide, by="siteid")

#dataviz <- subset (indicatorsplan)
rm(dataviz)
dataviz <- indicatorsplan2[ which(indicatorsplan2$Name=='Budgetary Requirement for 2015'), ]


# row.names  siteid	sector	activity	partner	location	comments	ReportingPeriodId	Date1	Date2	IndicatorId	Category	Name	Value	Units
# Sector  Objective	Output	Partner	Area	RegionCODE	Category	Total

## select the column of interest for the dataviz
dataviz <-dataviz[ , c("sector"  , "activity"  , "partner"  , "location.x", "Governorate", "Refugee Camps"  , #"comments"
                        "Date1"  , "Date2"  , "Value")]

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

#"Azraq Camp" 
#"Azraq Camp Village 3" 
#"Cyber City Refugee Center"
#"King Abdullah Park Refugee Center" 
#"Zaatari Camp (all district)"                    
#"Zaatari District 10"                             "Zaatari District 12"                             "Zaatari District 3"                             
# "Zaatari District 4"                              "Zaatari District 5"                              "Zaatari District 6"                             
# "Zaatari District 7"                              "Zaatari District 8"                              "Zaatari District 9"   

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


write.csv(dataviz, file="out3rp/3rp.csv",row.names=F, na="")
