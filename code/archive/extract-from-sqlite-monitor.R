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
con <- dbConnect(RSQLite::SQLite(), "data/6")

#######################################################
##### Exaction for RRP6 Monitoring Database  DB - 2300

# get sites
sitesmonitor <- dbGetQuery(con, paste("select s.siteid siteid, a.category sector, a.name activity, p.name partner, loc.name location, ",
                                      "s.comments from site s",
                                      "left join activity a on (s.activityid = a.activityid)",
                                      "left join location loc on (s.locationid= loc.locationid)",
                                      "left join partner p on (s.partnerid = p.partnerid)",
                                      "where a.databaseid=2300 and s.datedeleted is null and a.datedeleted is null"));
# get attributes
attrsmonitor <- dbGetQuery(con, paste("select s.siteid siteid, ag.name agroup, ag.multipleallowed multiple, at.name attribute from site s",
                                      "left join activity a on (s.activityid = a.activityid)",
                                      "left join attributevalue av on (av.siteid=s.siteid)",
                                      "left join attribute at on (at.attributeid=av.attributeid)",
                                      "left join attributegroup ag on (ag.attributegroupid=at.attributegroupid)",
                                      "where a.databaseid=2300 and s.datedeleted is null and a.datedeleted is null and at.datedeleted is null and ag.datedeleted is null and av.value = 1"));

# reformat attributes
attrmonitor.single <- subset(attrsmonitor, multiple == 0)
attrmonitor.single.wide <- dcast(attrmonitor.single, siteid ~ agroup, value.var="attribute")

attrmonitor.multiple <- subset(attrsmonitor, multiple == 1)
attrmonitor.multiple.wide <- dcast(attrmonitor.multiple, siteid ~ agroup + attribute, value.var="multiple", fill=0)

# get units
unitsmonitor <- dbGetQuery(con, paste("select s.siteid siteid, level.name level, entity.name name from site s",
                                      "left join activity a on (s.activityid = a.activityid)",
                                      "left join locationadminlink link on (link.locationid = s.locationid)",
                                      "left join adminentity entity on (link.adminentityid=entity.adminentityid)",
                                      "left join adminlevel level on (entity.adminlevelid=level.adminlevelid)",
                                      "where a.databaseid=2300 and s.datedeleted is null and a.datedeleted is null"));
# reformat admin units
unitsmonitor <- subset(unitsmonitor, ! is.na(name))
unitsmonitor.wide <- dcast(unitsmonitor, siteid ~ level, value.var="name")

# start merging sites with attributes
# sites <- merge(sites, units.wide)  # ends with duplicate when merging units...
sitesmonitor <- merge(sitesmonitor, attrmonitor.multiple.wide)
sitesmonitor <- merge(sitesmonitor, attrmonitor.single.wide)

# Write in files
#write.csv(sites, file="RRP6Monitor_Sites.csv", row.names=F, na="")

#sites$comments <- NULL
# write.csv(sites, file="RRP6Monitor_Sites_no_comments.csv", row.names=F, na="")

# Try to merge sites, attributes and indicators
ivmonitor <- dbGetQuery(con, paste("select s.siteid siteid, rp.reportingperiodid, rp.date1, rp.date2, i.indicatorid, i.category, i.name, iv.value, i.units from indicatorvalue iv",
                                   "left join reportingperiod rp on(rp.reportingperiodid=iv.reportingperiodid)", 
                                   "left join site s on (rp.siteid = s.siteid)",
                                   "left join activity a on (s.activityid = a.activityid)",
                                   "left join indicator i on (iv.indicatorid = i.indicatorid)",
                                   "where a.databaseid=2300 and s.datedeleted is null and a.datedeleted is null and i.dateDeleted is null"));


#write.csv(ivmonitor, file="RRP6Monitor_Indics.csv", row.names=F, na="")
indicatorsmonitor <- merge(sitesmonitor, ivmonitor)
#write.csv(indicatorsmonitor, file="out/monitor/3rpMonitor_Indicatorsall.csv",row.names=F, na="")

### merge with Full geographic three
indicatorsmonitor2 <- merge(x=indicatorsmonitor, y=unitsmonitor.wide, by="siteid",all.x=TRUE)


indicatorsmonitor2$objective <- substr(indicatorsmonitor2$sector , (regexpr("-", indicatorsmonitor2$sector , ignore.case=FALSE, fixed=TRUE))+1,50)
indicatorsmonitor2$sector <- substr(indicatorsmonitor2$sector ,1, (regexpr("-", indicatorsmonitor2$sector , ignore.case=FALSE, fixed=TRUE))-1)

write.csv(indicatorsmonitor2, file="out/monitor/3rpMonitor_Indicatorsall2.csv",row.names=F, na="")
# Add long narrative
#rrp6long <- read.csv("rrp6long.csv")
#siteslong <- merge(sites, rrp6long)
#indicatorslong <- merge(siteslong, ivplan2)
#write.csv(indicatorslong, file="monitor/RRP6Monitor_Indicatorsalllong.csv",row.names=F, na="")
