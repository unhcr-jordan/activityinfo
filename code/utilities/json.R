library(rjson)

# You can pass directly the filename
my.JSON <- fromJSON(file="data/unhcr-relief-3w-mali.geojson")

df <- lapply(my.JSON, function(play) # Loop through each "play"
{
  # Convert each group to a data frame.
  # This assumes you have 6 elements each time
  data.frame(matrix(unlist(play), ncol=6, byrow=T))
})

# Now you have a list of data frames, connect them together in
# one single dataframe
df <- do.call(rbind, df)

# Make column names nicer, remove row names
colnames(df) <- names(my.JSON[[1]][[1]])
rownames(df) <- NULL