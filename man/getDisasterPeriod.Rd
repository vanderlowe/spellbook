\name{getDisasterPeriod}
\alias{getDisasterPeriod}
\title{Fetch before—after data for a natural disaster}
\usage{
  getDisasterPeriod(country, date, offset, dummy = T,
    countdown = T)
}
\arguments{
  \item{country}{Two- or three-letter ISO alpha code or
  country name}

  \item{date}{A character string in the format of
  YYYY-MM-DD.}

  \item{offset}{Integer to indicate how many months of data
  before/after disaster to include}
}
\value{
  A data frame.
}
\description{
  Queries \code{cpw_meta} for Facebook friendship data
  three months before and after a natural disaster.
}

