\name{getFacebookData}
\alias{getFacebookData}
\title{Get FB data on new international and domestic friendships by country}
\usage{
  getFacebookData(country)
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
getFacebookData("FI")
}
}

