#' Outgoing FB friendships for a country
#' 
#' Accesses \code{cpw_meta} to retrieve friendships that originate from the country (i.e., outdegree).
#' @param country Two-letter ISO code of the country
#' @return A \code{data.frame}
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' outdegree("FI")
#' }

outdegree <- function(country) {
  friendships <- data.table(getFacebookData(iso2(country)))
  results <- friendships[Friender_Country == country & !Friended_Country == country, list(Friendships = sum(Count, na.rm = T)), by = date]
  results$type <- "Out"
  return(results)
}

#' Incoming FB friendships for a country
#' 
#' Accesses \code{cpw_meta} to retrieve friendships that the country receives from other countries (i.e., indegree).
#' @param country Two-letter ISO code of the country
#' @return A \code{data.frame}
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' indegree("FI")
#' }

indegree <- function(country) {
  friendships <- data.table(getFacebookData(iso(country)))
  results <- friendships[!Friender_Country == country & Friended_Country == country, list(Friendships = sum(Count, na.rm = T)), by = date]
  results$type <- "In"
  return(results)
}

#' FB friendships within a country
#' 
#' Accesses \code{cpw_meta} to retrieve friendships within the country (i.e., domestic).
#' @param country Two-letter ISO code of the country
#' @return A \code{data.frame}
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' domestic("FI")
#' }

domestic <- function(country) {
  friendships <- data.table(getFacebookData(country))
  results <- friendships[Friender_Country == country & Friended_Country == country, list(Friendships = sum(Count, na.rm = T)), by = date]
  results$type <- "Within"
  return(results)
}
