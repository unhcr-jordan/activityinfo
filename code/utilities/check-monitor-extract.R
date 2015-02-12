###Check 2 extract for 2015 extract

##indicatorsmonitor2 
# values.unique.attribute  2161 obs. of 46 variables

saveRDS(indicatorsmonitor2, file = "out/monitor/indicatorsmonitor2.rds")
saveRDS(values.unique.attribute, file = "out/monitor/values.unique.attribute.rds") 


names(indicatorsmonitor2)
indicatorsmonitor2.unique <- unique (indicatorsmonitor2)

names(values.unique.attribute)
values.unique.attribute.unique <- unique (values.unique.attribute)

ii<-which(!indicatorsmonitor2$siteid %in% values.unique.attribute$siteId)

missing <- indicatorsmonitor2[ii,]
saveRDS(missing, file = "out/monitor/missing.rds")