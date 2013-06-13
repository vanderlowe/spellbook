\name{getGlobalIncomingFacebookRelationsByDisaster}
\alias{getGlobalIncomingFacebookRelationsByDisaster}
\title{Get incoming Facebook relations from all countries to target country on the month of disaster}
\usage{
  getGlobalIncomingFacebookRelationsByDisaster(id)
}
\arguments{
  \item{id}{Emergency id that identifies a natural disaster
  in FTS data.}
}
\value{
  This function returns a \code{data.table} with 251 rows,
  one for each country in the world.
}
\description{
  Based on an emergency id, this function returns incoming
  friendships from each world country to the disaster
  country. If no data is available for a country, a zero
  will be reported.
}

