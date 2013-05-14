\name{parseMonth}
\alias{parseMonth}
\title{Guess numeric month value from a string.}
\usage{
  parseMonth(text)
}
\arguments{
  \item{text}{A character string}
}
\value{
  Numeric value between 1 and 12 for detected month.
}
\description{
  Parse character strings for the presence of fully spelled
  month names.
}
\note{
  Results will be uncertain if the text contains more than
  one month name.
}

