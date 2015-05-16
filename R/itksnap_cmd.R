#' @title Get ITK-SNAP command
#'
#' @description Gets ITK-SNAP command
#' @param arch architecture used (i686 or x86_64
#' @export
#' @return Path of ITK-SNAP
itksnap_cmd <- function(
  arch =  R.Version()$arch # architecture used (i686 or x86_64)
  ){
  sysname = tolower(Sys.info()["sysname"])
  folder = file.path(paste0(sysname, "-", arch), "bin")
  
  ######## get the itk snap command
  fname = "itksnap"
  if (sysname %in% "windows"){
    fname = paste0(fname, ".exe")
  }
  fname = file.path(folder, fname)
  cmd = system.file(fname, package = "itksnapr")
  return(cmd)
}