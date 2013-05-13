missingColumns <- function(df, expected.columns) {
  missing.columns <- setdiff(expected.columns, names(df))
  return(missing.columns)
}

insertMissingColumns <- function(df, expected.columns) {
  mia <- missingColumns(df, expected.columns)
  new.columns <- list()
  
  for (i in expected.columns) {
    if (i %in% mia) {
      # Create missing column
      new.columns[[i]] <- rep.int(NA, nrow(df))
    } else {
      # Use existing column
      if (class(df[, i]) == "factor") {
        # Factor
        new.columns[[i]] <- as.character(df[, i])
      } else {
        # Not a factor
        new.columns[[i]] <- df[, i]
      }
    }
  }
  return(data.frame(new.columns, stringsAsFactors = F))
}

parseMonth <- function(text) {
  text <- as.character(text)
  require(stringr)
  months <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
  for (month in months) {
    if (str_detect(tolower(text), month)) {return(which(months == month))}
  }
  return(NULL)
}
