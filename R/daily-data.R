#' @title Daily volumes by date and API
#' @description Daily volumes by date and API
#' @param api.filter character; API numbers to filter
#' @param exclude logical; should \code{api.filter} by exclusionary? The default (\code{FALSE})
#' will return only API numbers specified in \code{api.filter}.
#' @param after date or object coercible to date; A non-null entry with return results after
#' the specified; otherwise all data are returned.
#' @param ... additional paramters sent to \code{\link{dcast}}
#' @export
#' @examples
#' \dontrun{
#' #
#' # Plot total injection across Oklahoma
#' # (complete since early 2015)
#' #
#'
#' require(dplyr)
#'
#' dv <- occ_daily_volume()
#' # each API gets a column in the result
#' # and date is given in `Report.Date`
#' head(names(dv))
#'
#' Dates <- dv$Report.Date
#' Vols <- dplyr::select(dv,-Report.Date)
#'
#' plot(Dates, rowSums(Vols, na.rm=TRUE), type='l')
#'
#' }
occ_daily_volume <- function(api.filter=NULL, exclude=FALSE, after=NULL, ...){
  dly <- Dly1012d
  if (!is.null(api.filter)){
    api.filter <- as.vector(as.character(api.filter))
    if (exclude){
      dly %>% dplyr::filter(., !(API %in% api.filter)) -> dly
    } else {
      dly %>% dplyr::filter(., API %in% api.filter) -> dly
    }
  }
  dly.cast <- data.table::dcast(setDT(dly), Report.Date ~ API,
                    value.var=c('Volume.BPD'),
                    drop=FALSE, fun.aggregate=mean, ...)
  dly.cast <- tbl_df(dly.cast)
  if (!is.null(after)){
    after <- as.Date(after)
    stopifnot(length(after) == 1)
    dly.cast %>% dplyr::filter(., Report.Date > after) -> dly.cast
  }
  return(dly.cast)
}
