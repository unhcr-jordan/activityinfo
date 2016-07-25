# Author: Maarten-Jan Kallen <mj@bedatadriven.com>
# Last revision: October 15, 2014
# Usage:
#  (1) uncomment and change the commands at the bottom of this script.
#  (2) do source("copy_partners.R")

library("activityinfo")

# Function to read partner names from a database with numeric identifier
# provided by 'databaseId'. Returns a character vector with one or more partner
# names.
getPartnerNames <- function(databaseId) {
  schema <- getDatabaseSchema(databaseId)
  partners <- schema$partners
  
  # pull partner names out of list into a vector:
  sapply(partners, function(p) { p$name })
}

# Function that copies the partners linked to 'sourceDatabase' to each database
# in the vector 'targetDatabase'. Databases are identified by their numeric
# identifier and you must, of course, have access to all databases.
copyPartners <- function(sourceDatabase, targetDatabase) {
  
  partner.names <- getPartnerNames(sourceDatabase)
  
  for (database in targetDatabase) {  
    database.partners <- getPartnerNames(database)
    for (partner in partner.names) {
      if (!(partner %in% database.partners)) {
        cat("Adding partner '", partner, "'' to database ", database,
            "\n", sep="")
        tryCatch(executeCommand("AddPartner",
                                databaseId=database,
                                partner=list(name=partner)),
                 error=function(e) {
                   cat("Failed to add partner '", partner, "' with error: ", e,
                       "\n", sep="")
                 }
        )
      }
    }
  }
  cat("Done.\n")
  invisible()
}

# authenticate
activityInfoLogin()

# CHANGE SETTINGS BELOW:
# databases <- c(1540, 1563, 1564)
# copyPartners(1539, databases)



#from database 
df <- getPartnerNames(5026)

# New Database Id

databases <- c(5576)
#########   (OLD , New )
copyPartners(5026, databases)


## Copy all users from 3rp plan and monitrong db 2014 to monitor 2015
# copyUsers(sourceDatabase, targetDatabase)

copyUsers(5026, 4470)


