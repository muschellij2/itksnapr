#' @title Get ITK-SNAP command
#'
#' @description Gets ITK-SNAP command
#' @param arch architecture used (i686 or x86_64
#' @export
#' @return Path of ITK-SNAP
itksnap_cmd <- function(
    sysname = tolower(Sys.info()["sysname"])
  ){
  if (sysname %in% "windows") {
    fname = Sys.which("ITK-SNAP")
  } else {fname = "itksnap"}
  return(fname)
}