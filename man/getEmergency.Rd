\name{getEmergency}
\alias{getEmergency}
\title{Emergencies in a country during a certain year/month}
\usage{
  getEmergency(country, year, month,
    where.statement = NULL)
}
\arguments{
  \item{country}{Two- or three-letter ISO-alpha code of the
  country or a country name.}
}
\value{
  Data frame of emergencies (usually just one; not checked)
}
\description{
  Retrieves emergency information from \code{cpw_meta}.
}
\examples{
\dontrun{
getEmergency("HTI", 2010, 1)
}
}

