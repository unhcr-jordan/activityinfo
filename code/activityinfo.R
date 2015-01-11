### ActivityInfo Login

# required packages:
require("activityinfo")
require("reshape2")

# authenticate
activityInfoLogin()

# a helper function to extract all single-selection attributes from an activity:
extractSingleAttributes <- function(activity) {
  
  extractAttributes <- function(attr) {
    do.call(rbind, lapply(attr, function (attr) {
      data.frame(id = attr$id, name = attr$name, stringsAsFactors = FALSE)
    }))
  }
  
  single.attributes <-
    do.call(rbind,
            lapply(activity$attributeGroups, function(group) {
              if (!group$multipleAllowed) {
                attributes <- extractAttributes(group$attributes)
                attributes$group <- rep(group$name, times = nrow(attributes))
                return(attributes)
              } else {
                NULL
              }
            }))
  
  single.attributes
}

sanitizeNames <- function(s) {
  # convert strings to a format that's suitable for use as name
  gsub("\\s|-|_", ".", tolower(s))
}


extractAdminLevelEntities <- function(loc) {
  # helper function to extract a admin entities from a location. The return 
  # value is a vector with entity names and the names of the elements are the
  # names of the columns in 'value' where these entity names need to be stored.
  entities <- sapply(loc$adminEntities, function(e) e$name)
  ii <- match(names(entities), admin.levels.table$id, nomatch = 0)
  names(entities) <- admin.levels.table$column[ii]
  entities
}
