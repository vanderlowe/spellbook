\name{getDateRange}
\alias{getDateRange}
\title{Fetch data for a date range}
\usage{
  getDateRange(country, start.date, end.date)
}
\arguments{
  \item{country}{A valid two- or three-letter ISO 3166-1
  alpha code.}

  \item{start.date}{A character string in the format
  YYYY-MM-DD.}

  \item{end.date}{A character string in the format
  YYYY-MM-DD.}
}
\value{
  A data frame.
}
\description{
  Queries \code{cpw_meta} for Facebook friendship data for
  a country between two dates.
}
\note{
  Because the underlying data are indexed by year and
  month, the DD-portion (day) of start and end dates will
  be ignored and data are returned at month-level
  resolution.
}

