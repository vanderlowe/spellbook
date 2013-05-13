#' Directed friendships
#' 
#' Accesses \code{cpw_meta} database to retrieve data on new friendships relating to the target country.
#' @param country Two-letter ISO code of the country
#' @return A \code{data.frame}
#' @import magic
#' @export
#' @examples
#' \dontrun{
#' indegree("FI")}
#' \dontrun{
#' outdegree("FI")}
#' #' \dontrun{
#' domestic("FI")}

outdegree <- function(country) {
  dyads.friendship <- data.table(getFacebookData(country))
  results <- dyads.friendship[Friender_Country == country & !Friended_Country == country, list(Friendships = sum(Count, na.rm = T)), by = date]
  results$type <- "Out"
  return(results)
}

indegree <- function(country) {
  dyads.friendship <- data.table(getFacebookData(country))
  results <- dyads.friendship[!Friender_Country == country & Friended_Country == country, list(Friendships = sum(Count, na.rm = T)), by = date]
  results$type <- "In"
  return(results)
}

domestic <- function(country) {
  dyads.friendship <- data.table(getFacebookData(country))
  results <- dyads.friendship[Friender_Country == country & Friended_Country == country, list(Friendships = sum(Count, na.rm = T)), by = date]
  results$type <- "Within"
  return(results)
}
