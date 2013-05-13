\name{plot.crises}
\alias{plot.crises}
\title{Plot country's Facebook friendships with markers indicating natural disasters}
\usage{
  plot.crises(country)
}
\arguments{
  \item{country}{Two- or three-letter ISO code of the
  country}
}
\value{
  A \code{ggplot2} graph
}
\description{
  This plots the month-to-month changes in FB friendships
  both internationally (incoming and outgoing relations)
  and domestically. Additionally, the graph shows major
  natural disasters that occurred during that time.
}
\examples{
\dontrun{
plot.crises("HTI")}
}

