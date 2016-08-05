#####################################################
#  ActivityInfo Monitoring analysis script          #
#####################################################

required.packages <- c("activityinfo", "reshape2")

for (pkg in required.packages) {
  if (!require(pkg, character.only = TRUE)) {
    stop("package '", pkg, "' is required by this script, but is not installed")
  }
}

# authenticate
activityInfoLogin()

### JOR 2016 Monitoring Database Jordan db 5026
database.id <- 5026

# Set the following to 'TRUE' if you also want to export the comment field of each site:
include.comments <- TRUE

#-------------------------------------------------------------------------------
# Function definitions
#-------------------------------------------------------------------------------

na.if.null <- function(x, mode) {
  if (is.null(x)) as.vector(NA, mode) else x
}

makeColumnName <- function(s, prefix = NULL) {
  
  stopifnot(is.character(s))
  
  s <- gsub("\\s+|\\.+|-+", "_", trimws(tolower(s)))
  s <- gsub("#", "nr", s)
  
  if (is.null(prefix)) {
    make.names(s)  
  } else {
    make.names(paste(prefix, s, sep = ""))
  }
}

is.formInstance <- function(formInstance) {
  is.list(formInstance) &&
    all(c("name",
          "id",
          "attributeGroups",
          "reportingFrequency",
          "indicators") %in% names(formInstance))
}

getSiteData <- function(formInstance, include.comments = FALSE) {
  
  # Ensure that we are dealing with a form instance from the database schema:
  stopifnot(is.formInstance(formInstance))
  
  # Fields which exist for all sites:
  query <- list(
    site.id = "_id",
    partner.id = "partner._id",
    partner.label = "partner.label",
    partner.description = "partner.[full%20name]",
    location.latitude = "location.latitude",
    location.longitude = "location.longitude",
    location.name = "location.name",
    location.alternate_name = "location.axe",
    project.label = "project.name",
    project.description = "project.description"
  )
  
  # Add comments if requested:
  if (include.comments) {
    query$site.comments <- "comments"
  }
  
  # Start and end date are site data if reporting frequency is "once":
  if (as.character(formInstance$reportingFrequency) == "0") {
    query[["start_date"]] <- "date1"
    query[["end_date"]] <- "date2"
  }
  
  # Fields which depend on the administrative levels in the country of the
  # database which contains the form:
  for (admin.level in adminlevel.names) {
    column.name <- makeColumnName(admin.level, prefix = "location.adminlevel.")
    
    if (column.name %in% names(query)) {
      stop("cannot create unique column name for administrative level '", admin.level, "'")
    }
    
    query[[column.name]] <- paste("location.[", URLencode(admin.level), "].name", sep = "")
  }
  
  # Translation table for the attributes (from identifier to full name):
  attributes <- data.frame(
    id = vapply(formInstance$attributeGroups, function(x) sprintf("Q%010d", x$id), character(1L)),
    name = vapply(formInstance$attributeGroups, function(x) x$name, character(1L)),
    stringsAsFactors = FALSE
  )
  
  for (id in attributes$id) {
    # Use indicator identifiers instead of names for greater robustness and
    # shorter URLs:
    query[[id]] <- id
  }
  
  # Execute the query through the ActivityInfo API:
  path <- sprintf("form/a%s/query/columns", formInstance$id)
  list(resource = getResource(path = path, queryParams = query),
       column.names = attributes)
}

getIndicatorData <- function(formInstance) {
  
  # Ensure that we are dealing with a form instance from the database schema:
  stopifnot(is.formInstance(formInstance))
  
  # Return an empty result if the form has no indicators in its design:
  if (length(formInstance$indicators) == 0L) {
    warning("form '", formInstance$name, "' has no indicators")
    return(list(rows = 0L, columns = list()))
  }
  
  query <- list(site.id = "site@id")
  
  # Start and end date are report data if reporting frequency is "monthly":
  if (as.character(formInstance$reportingFrequency) == "1") {
    query[["start_date"]] <- "date1"
    query[["end_date"]] <- "date2"
    query[["report.id"]] <- "_id"
    path <- sprintf("form/M%s/query/columns", formInstance$id)
  } else {
    path <- sprintf("form/a%s/query/columns", formInstance$id)
  }
  
  # Translation table for the indicators (from identifier to full name):
  indicators <- data.frame(
    id = vapply(formInstance$indicators, function(x) sprintf("i%010d", x$id), character(1L)),
    name = vapply(formInstance$indicators, function(x) x$name, character(1L)),
    units = vapply(formInstance$indicators, function(x) x$units, character(1L)),
    stringsAsFactors = FALSE
  )
  
  for (id in indicators$id) {
    # Use indicator identifiers instead of names for greater robustness and
    # shorter URLs:
    query[[id]] <- id
  }
  
  # Execute the query through the ActivityInfo API:
  list(resource = getResource(path = path, queryParams = query),
       column.names = indicators)
}

getFormData <- function(formInstance, include.comments = FALSE) {
  
  convertToTable <- function(x) {
    as.data.frame(
      lapply(x$resource$columns, function(column) {
        switch(column$storage,
               constant = {
                 if (is.null(column$values)) {
                   rep(switch(column$type,
                              STRING = NA_character_,
                              NUMBER = NA_real_),
                       x$resource$rows)
                 } else {
                   rep(column$values, x$resource$rows)
                 }
               },
               array = {
                 if (is.list(column$values)) {
                   # one or more of the values is 'NULL'
                   mode <- switch(column$type,
                                  STRING = "character",
                                  NUMBER = "double")
                   vapply(column$values, na.if.null, vector(mode, 1L), mode = mode)
                 } else {
                   column$values
                 }
               },
               empty = {
                 rep(switch(column$type,
                            STRING = NA_character_,
                            NUMBER = NA_real_),
                     x$resource$rows)
               },
               stop("unknown storage mode '", column$storage, "'")
        )
      }),
      stringsAsFactors = FALSE
    )
  }
  
  site <- getSiteData(formInstance, include.comments)
  
  if (site$resource$rows == 0) {
    cat("Form '", formInstance$name,
        "' has no reports. Skipping...\n", sep = "")
    return(invisible())
  } else {
    cat("Form '", formInstance$name,
        "' has ", site$resource$rows, " reports.\n", sep = "")
  }
  
  # Convert the site data from a list to a data frame:
  site.data <- convertToTable(site)
  
  indicators <- getIndicatorData(formInstance)
  
  if (indicators$resource$rows == 0) {
    cat("Form '", formInstance$name,
        "' has no indicator values in any report. Skipping...\n", sep = "")
    return(invisible())
  }
  
  # Convert reported indicator values from a list to a data frame:
  indicator.data <- convertToTable(indicators)
  # Reshape the table to create a separate row for each reported indicator value:
  indicator.columns <- grep("^i\\d+", names(indicator.data), value = TRUE)
  indicator.data <- reshape2::melt(indicator.data,
                                   measure.vars = indicator.columns,
                                   variable.name = "indicator.id",
                                   value.name = "indicator.value",
                                   na.rm = TRUE)
  
  indicator.metadata <- indicators$column.names
  names(indicator.metadata) <- paste("indicator", names(indicator.metadata), sep = ".")
  indicator.data <- merge(indicator.data, indicator.metadata, by = "indicator.id", all.x = TRUE)
  
  # Merge site (meta) data and reported values:
  form.data <- merge(indicator.data, site.data, by = "site.id", all.x = TRUE)
  # Add form name and category:
  form.data$form <- formInstance$name
  form.data$form.category <- na.if.null(formInstance$category)
  # Sort columns alphabetically to group related columns together:
  form.data <- form.data[, order(names(form.data))]
  # Rename the columns with attribute values:
  names(form.data) <- vapply(names(form.data), function(colname) {
    if (colname %in% site$column.names$id) {
      site$column.names$name[match(colname, site$column.names$id)]
    } else {
      colname
    }
  }, character(1L), USE.NAMES = FALSE)
  
  form.data
}

#-------------------------------------------------------------------------------
# Script body: data extraction from ActivityInfo
#-------------------------------------------------------------------------------

cat("Fetching database schema...\n")
db.schema <- getDatabaseSchema(database.id)

cat("Fetching administrative levels...\n")
adminlevel.names <- vapply(getAdminLevels(db.schema$country$id), function(x) {
  x$name
}, character(1L))

cat("Database contains ", length(db.schema$activities),
    " forms. Retrieving data per form...\n", sep = "")

form.data <- lapply(db.schema$activities, function(form) {
  # Check if the data in the form is reported monthly or just once:
  monthly <- switch(as.character(form$reportingFrequency), "0"=FALSE, "1"=TRUE)
  
  form.data <- getFormData(form, include.comments)
  
  if (!monthly) {
    form.data$report.id <- rep(NA_character_, nrow(form.data))
  }
  
  form.data
})

# Remove forms without data entries:
form.data <- form.data[!vapply(form.data, is.null, logical(1L))]

# Find common column names for combining all results into a single table:
common.column.names <- Reduce(intersect, lapply(form.data, names))

# Combine all data into a single table:
values <- do.call(rbind, lapply(form.data, function(table) table[, common.column.names]))

# Warn the user if any column(s) is or are missing in the combined result:
all.column.names <- unique(do.call(c, lapply(form.data, names)))
missing.columns <- setdiff(all.column.names, common.column.names)
if (length(missing.columns) > 0L) {
  warning("the following column(s) is or are not shared by all forms: ",
          paste(missing.columns, sep = ", "))
}

#-------------------------------------------------------------------------------
# Script body: data preparation for dashboards
#-------------------------------------------------------------------------------

# First selection of columns to output (format of column names is not entirely
# consistent, but must match variables used in dashboards at
# https://github.com/unhcr-jordan/unhcr-jordan.github.io/tree/master/sectors/2016):
output <- data.frame(
  Indicator  = values[["indicator.name"]],
  Units      = values[["indicator.units"]],
  Value      = values[["indicator.value"]],
  Partner    = values[["partner.label"]],
  StartDate  = format(as.Date(values[["start_date"]], "%Y-%m-%d"), "%d/%m/%Y"),
  appeal     = values[["2-3RP Implementation Type"]],
  Fundedby   = values[["3-3RP appeal through"]],
  allocation = values[["4-Allocation according to 3RP"]],
  activity   = values[["form"]],
  region     = values[["location.adminlevel.region"]],
  stringsAsFactors = FALSE
)

sec.cat.obj <- local({
  # Determine sector, category and objective based on form category names:
  form.categories <- unique(values$form.category)
  
  do.call(rbind, lapply(form.categories, function(s) {
    m <- regexec("^([A-Z /]+)(\\[([A-Z]*)(.+)\\])(.+)$", s)[[1]]
    if (length(m) == 1L && m == -1) {
      warning("could not find sector, category and objective in form category name '",
              s, "'")
      data.frame(
        form.category = s,
        Sector = NA_character_,
        Category = NA_character_,
        Objective = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      matches <- mapply(function(start, length) {
        substr(s, start, start + length - 1)
      }, m, attr(m, "match.length"))
      # Matched expressions:
      # matches[1] = complete string
      # matches[2] = sector label
      # matches[3] = code (in brackets)
      # matches[4] = code: category label (REF or RES)
      # matches[5] = code: category number
      # matches[6] = objective
      sector.label <- trimws(matches[2])
      sector <- switch(sector.label,
                       "BN" = "BASICNEEDS",
                       "EDU" = "EDUCATION",
                       "FOOD/LIV" = "FOOD",
                       "HLTH" = "HEALTH",
                       "PROT" = "PROTECTION",
                       "SHLT" = "SHELTER",
                       # 'WASH' and 'JUS' remain as they are:
                       sector.label
      )
      category.label <- matches[4]
      category <- switch(category.label,
                         "RES" = "Resilience",
                         "REF" = "Refugee",
                         NA_character_)
      data.frame(
        form.category = s,
        Sector = sector,
        Category = category,
        Objective = trimws(matches[6]),
        stringsAsFactors = FALSE
      )
    }
  }))
})

output$Sector <- sec.cat.obj$Sector[match(values$form.category,
                                          sec.cat.obj$form.category)]

# Create a 'Governorate' column which contains the (capitalized) name of the
# governorate OR the name of a camp OR 'Countrywide':
output$Governorate <- local({
  regionactivityinfocode <- read.csv(
    file = file.path("data", "regionactivityinfocode.csv"),
    colClasses = "character",
    stringsAsFactors = FALSE
  )
  gov <- regionactivityinfocode$gov[match(
    values$location.adminlevel.governorate,
    regionactivityinfocode$governorate
  )]
  
  # Identify special locations and replace the governorate name:
  gov[grepl("^Zaatari (District|Camp)", values$location.name)] <- "ZaatariCamp"
  gov[grepl("^Azraq Camp", values$location.name)] <- "AzraqCamp"
  gov[values$location.name == "Emirati Jordanian Camp (EJC)"] <- "EJ Camp"
  gov[values$location.name == "Country Wide"] <- "Countrywide"
  gov
})

# Create a table to use for identification of gender and population type in the
# indicator names:
gender.poptype <- read.csv(text = "#
string,gender,poptype
SYRIAN_WOMEN_IN_URBAN,Women,Urban
SYRIAN_WOMEN_IN_CAMPS,Women,Camp
SYRIAN_MEN_IN_URBAN,Men,Urban
SYRIAN_MEN_IN_CAMPS,Men,Camp
SYRIAN_GIRLS_IN_URBAN,Girls,Urban
SYRIAN_GIRLS_IN_CAMPS,Girls,Camp
SYRIAN_BOYS_IN_CAMPS,Boys,Camp
SYRIAN_BOYS_IN_URBAN,Boys,Urban
EXISTING FEMALE SYRIAN CHILDREN (0 to 17) IN CAMPS,Girls,Camp
EXISTING FEMALE SYRIAN CHILDREN (0 to 17) IN URBAN,Girls,Urban
EXISTING MALE SYRIAN CHILDREN (0 to 17) IN CAMPS,Boys,Camp
EXISTING MALE SYRIAN CHILDREN (0 to 17) IN URBAN,Girls,Urban
MALE_IN_HOST_COMMUNITIES,Men,Host Community
FEMALE_IN_HOST_COMMUNITIES,Women,Host Community
WOMEN_IN_HOST_COMMUNITY,Women,Host Community
MEN_IN_HOST_COMMUNITY,Men,Host Community
BOYS_IN_HOST_COMMUNITY,Boys,Host Community
GIRLS_IN_HOST_COMMUNITY,Girls,Host Community",
                           comment.char = "#",
                           stringsAsFactors = FALSE,
                           colClasses = "character")

output <- cbind(output, local({
  tmp <- data.frame(Gender = rep(NA_character_, nrow(output)),
                    poptype = rep(NA_character_, nrow(output)),
                    stringsAsFactors = FALSE)
  
  for (row in seq(nrow(gender.poptype))) {
    is.match <- grepl(gender.poptype$string[row], values$indicator.name)
    tmp$Gender[is.match] <- gender.poptype$gender[row]
    tmp$poptype[is.match] <- gender.poptype$poptype[row]
  }
  tmp
}))

# Write dashboard data out per sector:
for (sector in unique(output$Sector)) {
  write.csv(
    subset(output, Sector == sector),
    file.path("out", "monitor", "2016", tolower(sector), "data.csv"),
    na = "(unknown)"
  )
}
