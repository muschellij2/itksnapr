#' @title Install ITK-SNAP Binary
#'
#' @description Install ITK-SNAP executable
#' @param arch architecture used (i686 or x86_64)
#' @param lib.loc a character vector with path names of R libraries. 
#' Passed to \code{\link{system.file}}   
#' @param quiet passed to [utils::download.file()], set `FALSE` for 
#' more verbosity/messages
#' @param ... additional arguments to pass to [utils::download.file()],
#' other than `quiet`
#' @export
#' @return Logical if the binary was downloaded
#' @importFrom utils download.file unzip
install_itksnap <- function(
    arch =  R.Version()$arch, # architecture used (i686 or x86_64)
    lib.loc = NULL,
    ...,
    quiet = TRUE
){
  sysname = tolower(Sys.info()["sysname"])
  type = paste0(sysname, "-", arch)
  
  cmd = itksnap_cmd(arch = arch)
  
  if (cmd == "") {
    # url = paste0("https://github.com/muschellij2/", 
    #              "itksnapr/raw/gh-pages/",
    #              type, ".zip")  
    url = paste0("https://raw.githubusercontent.com/muschellij2/",
                 "itksnapr/gh-pages/",
                 type, ".zip")
    snap_dir = system.file(package = "itksnapr",
                           lib.loc = lib.loc)
    destfile = file.path(
      snap_dir, 
      basename(url))
    download.file(url, destfile, quiet = quiet, mode  = "wb",
                  ...)
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