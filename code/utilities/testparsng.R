values.unique.attribute <- values.unique.attribute.bcp[ , c("gender", "poptype", "indic", "indicatorName", "activityName") ]


values.unique.attribute <- values.unique.attribute[ values.unique.attribute$activityName == "[REF2.4]Psychosocial Support" , ]

# & values.unique.attribute$activityName == "[REF1.3]Cash,Vouchers for most vuln."

values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Urban/Rural Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Women"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Urban/Rural Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Urban"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Urban/Rural Syrian Women (Age 18 and above)", ignore.case = TRUE, #fixed = FALSE, #useBytes = FALSE,
                                                   values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Women (Age 18 and above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+44,250)
                                             ), values.unique.attribute$indic)
)

values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Urban/Rural Syrian Women (Age 18-24)", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Women (Age 18-24)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+37,250)
                                             ), values.unique.attribute$indic)
)
