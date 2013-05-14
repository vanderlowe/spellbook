\name{getEmergencies}
\alias{getEmergencies}
\title{Information about emergencies in a country}
\usage{
  getEmergencies(country)
}
\arguments{
  \item{country}{Three-letter ISO-alpha3 code of the
  country}
}
\value{
  Data frame of emergencies
}
\description{
  This function queries Financial Tracking Service for
  emergency information.
}
\examples{
\dontrun{
getEmergencies("HTI")
}
}

