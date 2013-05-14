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

#' Guess numeric month value from a string.
#' 
#' Parse character strings for the presence of fully spelled month names.
#' @param text A character string
#' @return Numeric value between 1 and 12 for detected month.
#' @import stringr
#' @export
#' @note Results will be uncertain if the text contains more than one month name.
#' 
parseMonth <- function(text) {
  text <- as.character(text)
  months <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
  for (month in months) {
    if (str_detect(tolower(text), month)) {return(which(months == month))}
  }
  return(NULL)
}
