\name{getFacebookData}
\alias{getFacebookData}
\title{Get FB data on new international and domestic friendships by country}
\usage{
  getFacebookData(country, where.statement = NULL)
}
\arguments{
  \item{country}{Two- or three letter ISO code of the
  country (or a valid country name)}

  \item{where.statement}{A vector of WHERE conditions
  amended to the query}
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
getFacebookData("FI")
getFacebookData("FIN")
getFacebookData("Finland")
getFacebookData("Finland", "year = 2006")
getFacebookData("Finland", c("year = 2006", "Count > 100"))
}
}

