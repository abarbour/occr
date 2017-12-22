#.onUnload <- function(libpath) {
# library.dynam.unload("occr", libpath)
#}
##
# executed after .onLoad is executed, once the namespace is visible to user
.onAttach <- function(...) {
  ##
  pack <- "occr"
  last.comcat <- "20171222"
  # ^^ update this if ComCat catalog changes
  packv <- utils::packageVersion(pack)
  packvp <- strftime(as.Date(as.character(packv[1,3]), format='%Y%m%d', tz='UTC'), format='%a, %b %d, %Y')
  packageStartupMessage(
    sprintf("Loaded %s (%s) -- injection data in Oklahoma from OCC\n  Note:\n    %s\n    %s\n    %s",
            pack, packv,
            paste("1) sub-version number shows last update (", packvp, ')'),
            "2) some data (e.g., 1012d) are daily, but filings may be incomplete through previous week",
            paste("3) Last update to earthquake catalog (ComCat) was", last.comcat)))
}
