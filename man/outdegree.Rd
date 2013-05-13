\name{outdegree}
\alias{outdegree}
\title{Directed friendships}
\usage{
  outdegree(country)
}
\arguments{
  \item{country}{Two-letter ISO code of the country}
}
\value{
  A \code{data.frame}
}
\description{
  Accesses \code{cpw_meta} database to retrieve data on new
  friendships relating to the target country.
}
\examples{
\dontrun{
indegree("FI")}
\dontrun{
outdegree("FI")}
#' \dontrun{
domestic("FI")}
}

