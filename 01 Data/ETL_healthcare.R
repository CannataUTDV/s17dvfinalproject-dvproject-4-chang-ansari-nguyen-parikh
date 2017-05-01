require(readr)
require(plyr)
require("jsonlite")
require("RCurl")


# Set the Working Directory to the 00 Doc folder
# Download the cannata/diamonds file into a folder ../../CSVs and rename the file PreETL_Diamonds.csv
print("Here are the columns of the CSV we are dealing with...")

file_path = "../../CSVs/PreETL_healthcare.csv"
df <- readr::read_csv(file_path)

#str(df) # Uncomment this line and  run just the lines to here to get column types to use for getting the list of measures.

measures <- c("Provider Id", "Provider Zip Code", "Total Discharges", "Average Covered Charges", "Average Total Payments", "Average Medicare Payments")

dimensions <- setdiff(names(df), measures)
dimensions

na2emptyString <- function (x) {
  x[is.na(x)] <- ""
  return(x)
}
if( length(dimensions) > 0) {
  for(d in dimensions) {
    # Change NA to the empty string.
    df[d] <- data.frame(lapply(df[d], na2emptyString))
    # Get rid of " and ' in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
    # Change & to and in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
    # Change : to ; in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[^ -~]",replacement= ""))
  }
}

na2zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions, and change NA to 0.
if( length(measures) > 1) {
  for(m in measures) {
    df[m] <- data.frame(lapply(df[m], na2zero))
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^ -~]",replacement= ""))
    df[m] <- data.frame(lapply(df[m], as.numeric)) # This is needed to turn measures back to numeric because gsub turns them into strings.
  }
}

df["Provider State"] <- data.frame(lapply(df["Provider State"], toupper))
#str(df)

write.csv(df, gsub("PreETL_", "", file_path), row.names=FALSE, na = "")
print("Done! Check your CSV Folder.")

