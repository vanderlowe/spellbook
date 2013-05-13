\name{plot.facebook}
\alias{plot.facebook}
\title{Plot country's Facebook friendships over time with optional event markers}
\usage{
  plot.facebook(country, events)
}
\arguments{
  \item{country}{Two-letter ISO-alpha2 code of the country}

  \item{events}{An optional \code{data.frame} object with
  columns \code{date} (in YYYY-MM-DD format) and
  \code{name}, both as character strings.}
}
\value{
  A \code{ggplot2} graph
}
\description{
  This plots the month-to-month changes in FB friendships
  both internationally (incoming and outgoing relations)
  and domestically.
}
\examples{
\dontrun{
plot.facebook("JP")}
}

