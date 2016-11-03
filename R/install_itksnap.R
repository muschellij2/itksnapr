#' @title Install ITK-SNAP Binary
#'
#' @description Install ITK-SNAP executable
#' @param arch architecture used (i686 or x86_64)
#' @param lib.loc a character vector with path names of R libraries. 
#' Passed to \code{\link{system.file}}   
#' @export
#' @return Logical if the binary was downloaded
#' @importFrom utils download.file
install_itksnap <- function(
  arch =  R.Version()$arch, # architecture used (i686 or x86_64)
  lib.loc = NULL
){
  sysname = tolower(Sys.info()["sysname"])
  type = paste0(sysname, "-", arch)

  cmd = itksnap_cmd(arch = arch)

  if (cmd == "") {
    url = paste0("https://github.com/muschellij2/", 
                 "itksnapr/raw/gh-pages/",
                 type, ".zip")    
    snap_dir = system.file(package = "itksnapr",
                           lib.loc = lib.loc)
    destfile = file.path(
      snap_dir, 
      basename(url))
    download.file(url, destfile, quiet = TRUE)
    files = unzip(destfile, 
                  exdir = snap_dir)
    for (ifile in files) {
      system(sprintf("chmod +x %s", ifile))
    }
    x = file.remove(destfile)
    rm(x)
  }
  
  cmd = itksnap_cmd(arch = arch)
  return(cmd != "")
}