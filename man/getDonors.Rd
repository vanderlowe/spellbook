\name{getDonors}
\alias{getDonors}
\title{Information about donors to specific emergency}
\usage{
  getDonors(emergency.id)
}
\arguments{
  \item{emergency.id}{FTS emergency id (e.g., 15797 for
  Haiti 2011 earthquake)}
}
\value{
  Data frame of donors and amounts
}
\description{
  This function queries Financial Tracking Service for
  donor information.
}
\examples{
\dontrun{
getDonors(15797)
}
}

