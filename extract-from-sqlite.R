#########################################
#  ActivityInfo extraction script       #
#########################################


# install.packages("reshape2", "RSQLite")
library(reshape2)
library(RSQLite)

# Connect to local SQLite replica installed when offline usage is enabled
# replace 9 with the name of the database used in your chrome/Default/databases/https_www.syrianrefugeeresponse.org_0 folder 
## -- copy the database in your rstudio project
con <- dbConnect("SQLite", "/home/rstudio/unhcr_r_project/activityinfo/9")

#######################################################
##### Exaction for RRP6 Planning Database  DB - 901

# get sites
sitesplan <- dbGetQuery(con, paste("select s.siteid siteid, a.category sector, a.name activity, p.name partner, loc.name location, ",
                                   "s.comments from site s",
                                   "left join activity a on (s.activityid = a.activityid)",
                                   "left join location loc on (s.locationid= loc.locationid)",
                                   "left join partner p on (s.partnerid = p.partnerid)",
                                   "where a.databaseid=901 and s.datedeleted is null and a.datedeleted is null"));
# get attributes
attrsplan <- dbGetQuery(con, paste("select s.siteid siteid, ag.name agroup, ag.multipleallowed multiple, at.name attribute from site s",
                                   "left join activity a on (s.activityid = a.activityid)",
                                   "left join attributevalue av on (av.siteid=s.siteid)",
                                   "left join attribute at on (at.attributeid=av.attributeid)",
                                   "left join attributegroup ag on (ag.attributegroupid=at.attributegroupid)",
                                   "where a.databaseid=901 and s.datedeleted is null and a.datedeleted is null and at.datedeleted is null and ag.datedeleted is null and av.value = 1"));

# reformat attributes
attrplan.single <- subset(attrsplan, multiple == 0)
attrplan.single.wide <- dcast(attrplan.single, siteid ~ agroup, value.var="attribute")

attrplan.multiple <- subset(attrsplan, multiple == 1)
attrplan.multiple.wide <- dcast(attrplan.multiple, siteid ~ agroup + attribute, value.var="multiple", fill=0)

# get units
unitsplan <- dbGetQuery(con, paste("select s.siteid siteid, level.name level, entity.name name from site s",
                                   "left join activity a on (s.activityid = a.activityid)",
                                   "left join locationadminlink link on (link.locationid = s.locationid)",
                                   "left join adminentity entity on (link.adminentityid=entity.adminentityid)",
                                   "left join adminlevel level on (entity.adminlevelid=level.adminlevelid)",
                                   "where a.databaseid=901 and s.datedeleted is null and a.datedeleted is null"));
# reformat units
unitsplan <- subset(unitsplan, ! is.na(name))
unitsplan.wide <- dcast(unitsplan, siteid ~ level, value.var="name")

# start merging sites with attributes
sitesplan <- merge(sitesplan, attrplan.multiple.wide)
sitesplan <- merge(sitesplan, attrplan.single.wide)

# Write in files
#write.csv(sitesplan, file="RRP6plan_Sites.csv", row.names=F, na="")

# Try to merge sites, attributes and indicators
ivplan <- dbGetQuery(con, paste("select s.siteid siteid, rp.reportingperiodid, rp.date1, rp.date2, i.indicatorid, i.category, i.name, iv.value, i.units from indicatorvalue iv",
                                "left join reportingperiod rp on(rp.reportingperiodid=iv.reportingperiodid)", 
                                "left join site s on (rp.siteid = s.siteid)",
                                "left join activity a on (s.activityid = a.activityid)",
                                "left join indicator i on (iv.indicatorid = i.indicatorid)",
                                "where a.databaseid=901 and s.datedeleted is null and a.datedeleted is null and i.dateDeleted is null"));

write.csv(ivplan, file="RRP6plan_Indics.csv", row.names=F, na="")
indicatorsplan <- merge(sitesplan, ivplan)
write.csv(indicatorsplan, file="RRP6Plan_Indicatorsall.csv",row.names=F, na="")

# Add long narrative
#rrp6long <- read.csv("~/unhcr_r_project/activityinfo/rrp6long.csv")
#sitesplanlong <- merge(sitesplan, rrp6long)
#indicatorsplanlong <- merge(sitesplanlong, ivplan2)
#write.csv(indicatorsplan, file="RRP6Plan_Indicatorsalllong.csv",row.names=F, na="")


#######################################################
##### Exaction for RRP6 Planning Review Database  DB - 1272

# get sites
sitesreview <- dbGetQuery(con, paste("select s.siteid siteid, a.category sector, a.name activity, p.name partner, loc.name location, ",
                                     "s.comments from site s",
                                     "left join activity a on (s.activityid = a.activityid)",
                                     "left join location loc on (s.locationid= loc.locationid)",
                                     "left join partner p on (s.partnerid = p.partnerid)",
                                     "where a.databaseid=1272 and s.datedeleted is null and a.datedeleted is null"));
# get attributes
attrsreview <- dbGetQuery(con, paste("select s.siteid siteid, ag.name agroup, ag.multipleallowed multiple, at.name attribute from site s",
                                     "left join activity a on (s.activityid = a.activityid)",
                                     "left join attributevalue av on (av.siteid=s.siteid)",
                                     "left join attribute at on (at.attributeid=av.attributeid)",
                                     "left join attributegroup ag on (ag.attributegroupid=at.attributegroupid)",
                                     "where a.databaseid=1272 and s.datedeleted is null and a.datedeleted is null and at.datedeleted is null and ag.datedeleted is null and av.value = 1"));

# reformat attributes
attrreview.single <- subset(attrsreview, multiple == 0)
attrreview.single.wide <- dcast(attrreview.single, siteid ~ agroup, value.var="attribute")

attrreview.multiple <- subset(attrsreview, multiple == 1)
attrreview.multiple.wide <- dcast(attrreview.multiple, siteid ~ agroup + attribute, value.var="multiple", fill=0)

# get units
unitsreview <- dbGetQuery(con, paste("select s.siteid siteid, level.name level, entity.name name from site s",
                                     "left join activity a on (s.activityid = a.activityid)",
                                     "left join locationadminlink link on (link.locationid = s.locationid)",
                                     "left join adminentity entity on (link.adminentityid=entity.adminentityid)",
                                     "left join adminlevel level on (entity.adminlevelid=level.adminlevelid)",
                                     "where a.databaseid=1272 and s.datedeleted is null and a.datedeleted is null"));
# reformat administrative units
unitsreview <- subset(unitsreview, ! is.na(name))
unitsreview.wide <- dcast(unitsreview, siteid ~ level, value.var="name")

# start merging sites with attributes
sitesreview <- merge(sitesreview, attrreview.multiple.wide)
sitesreview <- merge(sitesreview, attrreview.single.wide)

# Write in files
#write.csv(sitesreview, file="RRP6review_Sites.csv", row.names=F, na="")

# Try to merge sites, attributes and indicators
ivreview <- dbGetQuery(con, paste("select s.siteid siteid, rp.reportingperiodid, rp.date1, rp.date2, i.indicatorid, i.category, i.name, iv.value, i.units from indicatorvalue iv",
                                  "left join reportingperiod rp on(rp.reportingperiodid=iv.reportingperiodid)", 
                                  "left join site s on (rp.siteid = s.siteid)",
                                  "left join activity a on (s.activityid = a.activityid)",
                                  "left join indicator i on (iv.indicatorid = i.indicatorid)",
                                  "where a.databaseid=1272 and s.datedeleted is null and a.datedeleted is null and i.dateDeleted is null"));

#write.csv(ivreview, file="RRP6review_Indics.csv", row.names=F, na="")
summary(ivreview)

indicatorsreview <- merge(sitesreview, ivreview)
write.csv(indicatorsreview, file="RRP6review_Indicatorsall.csv",row.names=F, na="")

# Add long narrative
#rrp6long <- read.csv("~/unhcr_r_project/activityinfo/rrp6long.csv")
#sitesreviewlong <- merge(sitesreview, rrp6long)
#indicatorsreviewlong <- merge(sitesreviewlong, ivreview)
#write.csv(indicatorsreview, file="RRP6review_Indicatorsalllong.csv",row.names=F, na="")



#######################################################
##### Exaction for RRP6 Monitoring Database  DB - 1064

# get sites
sitesmonitor <- dbGetQuery(con, paste("select s.siteid siteid, a.category sector, a.name activity, p.name partner, loc.name location, ",
                                      "s.comments from site s",
                                      "left join activity a on (s.activityid = a.activityid)",
                                      "left join location loc on (s.locationid= loc.locationid)",
                                      "left join partner p on (s.partnerid = p.partnerid)",
                                      "where a.databaseid=1064 and s.datedeleted is null and a.datedeleted is null"));
# get attributes
attrsmonitor <- dbGetQuery(con, paste("select s.siteid siteid, ag.name agroup, ag.multipleallowed multiple, at.name attribute from site s",
                                      "left join activity a on (s.activityid = a.activityid)",
                                      "left join attributevalue av on (av.siteid=s.siteid)",
                                      "left join attribute at on (at.attributeid=av.attributeid)",
                                      "left join attributegroup ag on (ag.attributegroupid=at.attributegroupid)",
                                      "where a.databaseid=1064 and s.datedeleted is null and a.datedeleted is null and at.datedeleted is null and ag.datedeleted is null and av.value = 1"));

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
                                      "where a.databaseid=1064 and s.datedeleted is null and a.datedeleted is null"));
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
                                   "where a.databaseid=1064 and s.datedeleted is null and a.datedeleted is null and i.dateDeleted is null"));

#write.csv(ivmonitor, file="RRP6Monitor_Indics.csv", row.names=F, na="")
indicatorsmonitor <- merge(sitesmonitor, ivmonitor)
write.csv(indicatorsmonitor, file="RRP6Monitor_Indicatorsall.csv",row.names=F, na="")

# Add long narrative
#rrp6long <- read.csv("~/unhcr_r_project/activityinfo/rrp6long.csv")
#siteslong <- merge(sites, rrp6long)
#indicatorslong <- merge(siteslong, ivplan2)
#write.csv(indicatorslong, file="RRP6Monitor_Indicatorsalllong.csv",row.names=F, na="")


#######################################################
##### Exaction for services Mapping Database  DB - 1100

# get sites
sitesservice <- dbGetQuery(con, paste("select s.siteid siteid, a.category sector, a.name activity, p.name partner, loc.name location, ",
                                      "s.comments from site s",
                                      "left join activity a on (s.activityid = a.activityid)",
                                      "left join location loc on (s.locationid= loc.locationid)",
                                      "left join partner p on (s.partnerid = p.partnerid)",
                                      "where a.databaseid=1100 and s.datedeleted is null and a.datedeleted is null"));
# get attributes
attrsservice <- dbGetQuery(con, paste("select s.siteid siteid, ag.name agroup, ag.multipleallowed multiple, at.name attribute from site s",
                                      "left join activity a on (s.activityid = a.activityid)",
                                      "left join attributevalue av on (av.siteid=s.siteid)",
                                      "left join attribute at on (at.attributeid=av.attributeid)",
                                      "left join attributegroup ag on (ag.attributegroupid=at.attributegroupid)",
                                      "where a.databaseid=1100 and s.datedeleted is null and a.datedeleted is null and at.datedeleted is null and ag.datedeleted is null and av.value = 1"));

# reformat attributes
attrservice.single <- subset(attrsservice, multiple == 0)
attrservice.single.wide <- dcast(attrservice.single, siteid ~ agroup, value.var="attribute")

attrservice.multiple <- subset(attrsservice, multiple == 1)
attrservice.multiple.wide <- dcast(attrservice.multiple, siteid ~ agroup + attribute, value.var="multiple", fill=0)

# get units
unitsservice <- dbGetQuery(con, paste("select s.siteid siteid, level.name level, entity.name name from site s",
                                      "left join activity a on (s.activityid = a.activityid)",
                                      "left join locationadminlink link on (link.locationid = s.locationid)",
                                      "left join adminentity entity on (link.adminentityid=entity.adminentityid)",
                                      "left join adminlevel level on (entity.adminlevelid=level.adminlevelid)",
                                      "where a.databaseid=1100 and s.datedeleted is null and a.datedeleted is null"));
# reformat administrative units
unitsservice <- subset(unitsservice, ! is.na(name))
unitsservice.wide <- dcast(unitsservice, siteid ~ level, value.var="name")

# start merging sites with attributes
sitesservice <- merge(sitesservice, attrservice.multiple.wide)
sitesservice <- merge(sitesservice, attrservice.single.wide)

# Write in files
#write.csv(sitesservice, file="Services_Sites.csv", row.names=F, na="")

# Try to merge sites, attributes and indicators
ivservice <- dbGetQuery(con, paste("select s.siteid siteid, rp.reportingperiodid, rp.date1, rp.date2, i.indicatorid, i.category, i.name, iv.value, i.units from indicatorvalue iv",
                                   "left join reportingperiod rp on(rp.reportingperiodid=iv.reportingperiodid)", 
                                   "left join site s on (rp.siteid = s.siteid)",
                                   "left join activity a on (s.activityid = a.activityid)",
                                   "left join indicator i on (iv.indicatorid = i.indicatorid)",
                                   "where a.databaseid=1100 and s.datedeleted is null and a.datedeleted is null and i.dateDeleted is null"));

services <- merge(sitesservice, ivservice)

# Export to csv
write.csv(services, file="Refugee_Services.csv",row.names=F, na="")