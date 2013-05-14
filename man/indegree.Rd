\name{indegree}
\alias{indegree}
\title{Incoming FB friendships for a country}
\usage{
  indegree(country)
}
\arguments{
  \item{country}{Two-letter ISO code of the country}
}
\value{
  A \code{data.frame}
}
\description{
  Accesses \code{cpw_meta} to retrieve friendships that the
  country receives from other countries (i.e., indegree).
}
\examples{
\dontrun{
indegree("FI")
}
}

