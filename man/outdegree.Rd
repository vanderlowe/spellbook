\name{outdegree}
\alias{outdegree}
\title{Outgoing FB friendships for a country}
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
  Accesses \code{cpw_meta} to retrieve friendships that
  originate from the country (i.e., outdegree).
}
\examples{
\dontrun{
outdegree("FI")}
}

