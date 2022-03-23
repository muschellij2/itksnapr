#' @title Run ITK-SNAP
#'
#' @description Executes ITK-SNAP
#' @param grayscale filenames or nifti objects to be displayed. 
#' @param overlay filenames or nifti objects to be set as overlays 
#' @param segmentation segmentation image using \code{-s} option
#' @param labels label descriptions using \code{-s} option
#' @param zoom initial zoom in screen pixels/mm
#' @param verbose Print out the command executed
#' @param ... arguments to pass to \code{\link{itksnap_cmd}}
#' @export
#' @importFrom neurobase checkimg
#' @importFrom oro.nifti is.nifti
#' @return Return of \code{\link{system}} command
#' @examples 
#' dims = rep(10, 3)
#' arr = array(rnorm(prod(dims)), dim = dims)
#' nim = oro.nifti::nifti(arr)
#' \dontrun{
#' itksnap(nim)
#' }
itksnap <- function(
  grayscale, # filenames or nifti objects to be displayed.  
  overlay = NULL,
  segmentation = NULL, # segmentation image using \code{-s} option
  labels = NULL, # label descriptions using \code{-s} option
  zoom = NULL, # initial zoom in screen pixels/mm
  verbose = TRUE, # Print out the command executed
  ... # arguments to pass to \code{\link{itksnap_cmd}}
){
  maker = function(x){
    if (is.nifti(x)) {
      x = checkimg(x)
    }
    x = sapply(x, checkimg)
  }
  
  ################
  # Force to names
  ################
  grayscale = maker(grayscale)
  names(grayscale) = rep("-g", length(grayscale))
  
  maker2 = function(vec, lab){
    if (length(vec) > 0){
      vec = maker(vec)
      names(vec) = rep(lab, length(vec))
    }    
    return(vec)
  }
  
  overlay = maker2(overlay, "-o")
  segmentation = maker2(segmentation, "-s")
  labels = maker2(labels, "-l")

  if (length(zoom) > 0){
    names(zoom) = "-z"
  }
  
  allfiles = c(grayscale, overlay, segmentation, labels, zoom)
  file.names = names(allfiles)
  allfiles = normalizePath(allfiles)
  allfiles = shQuote(allfiles)
  cmd = paste(file.names, 
              allfiles, 
              collapse = " ")
  cmd = sprintf('%s %s', shQuote(itksnap_cmd(...)), cmd)
  sysname = tolower(Sys.info()["sysname"])
  if (sysname %in% "windows") {
    cmd = capture.output(cat(paste0(cmd)))
    res = shell(cmd)
  }else{
  if (verbose){
    cat(paste0(cmd, "\n"))
  }
  res = system(cmd, ignore.stderr = TRUE, ignore.stdout = TRUE)
  }
  return(res)
}

