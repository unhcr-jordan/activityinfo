#########################################
#  ActivityInfo extraction script       #
#########################################


#install.packages("reshape2", "RSQLite")
library(reshape2)
library(RSQLite)

# Connect to local SQLite replica installed when offline usage is enabled
# replace 9 with the name of the database used in your chrome/Default/databases/https_www.syrianrefugeeresponse.org_0 folder 
## -- copy the database in your rstudio project
con <- dbConnect(RSQLite::SQLite(), "/home/rstudio/unhcr_r_project/activityinfojordan/data/8")

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
unitsplan <- dbGetQuery(con, paste("select s.siteid siteid, level.name level, entity.name name from site s",
                                   "left join activity a on (s.activityid = a.activityid)",
                                   "left join locationadminlink link on (link.locationid = s.locationid)",
                                   "left join adminentity entity on (link.adminentityid=entity.adminentityid)",
                                   "left join adminlevel level on (entity.adminlevelid=level.adminlevelid)",
                                   "where a.databaseid=1662 and s.datedeleted is null and a.datedeleted is null"));
# reformat units
unitsplan <- subset(unitsplan, ! is.na(name))
unitsplan.wide <- dcast(unitsplan, siteid ~ level, value.var="name")

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

write.csv(ivplan, file="RRP6plan_Indics.csv", row.names=F, na="")
indicatorsplan <- merge(sitesplan, ivplan)
write.csv(indicatorsplan, file="out3rp/JOR-3RP-Plan_Indicatorsall.csv",row.names=F, na="")

# Add long narrative
#rrp6long <- read.csv("~/unhcr_r_project/activityinfo/rrp6long.csv")
#sitesplanlong <- merge(sitesplan, rrp6long)
#indicatorsplanlong <- merge(sitesplanlong, ivplan2)
#write.csv(indicatorsplan, file="RRP6Plan_Indicatorsalllong.csv",row.names=F, na="")


