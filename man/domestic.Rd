\name{domestic}
\alias{domestic}
\title{FB friendships within a country}
\usage{
  domestic(country)
}
\arguments{
  \item{country}{Two-letter ISO code of the country}
}
\value{
  A \code{data.frame}
}
\description{
  Accesses \code{cpw_meta} to retrieve friendships within
  the country (i.e., domestic).
}
\examples{
#' \dontrun{
domestic("FI")}
}

