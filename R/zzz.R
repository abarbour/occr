#.onUnload <- function(libpath) {
# library.dynam.unload("occr", libpath)
#}
##
# executed after .onLoad is executed, once the namespace is visible to user
.onAttach <- function(...) {
  ##
  pack <- "occr"
  packageStartupMessage(
    sprintf("Loaded %s (%s) -- injection data in Oklahoma -- sub-version number shows last update",
            pack, utils::packageVersion(pack)))
}
