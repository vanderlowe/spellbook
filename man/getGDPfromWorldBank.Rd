\name{getGDPfromWorldBank}
\alias{getGDPfromWorldBank}
\title{Retrieve GDP data from World Bank}
\usage{
  getGDPfromWorldBank(iso.code, start.year = 2006,
    end.year = 2012)
}
\arguments{
  \item{iso.code}{ISO alpha 2 code for the country}

  \item{start.year}{Optional start year (default is 2006)}

  \item{end.year}{Optional end year (default is 2012)}
}
\value{
  A data table
}
\description{
  This function gets the GDP data for a given country
  indicated in present-day US dollars.
}

