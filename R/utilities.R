#' @title List data sources presently supported
#' @description List data sources presently supported
#' @export
occ_sources <- function(){
  try(data(package = "occr"))
}
