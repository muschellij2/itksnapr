#' @title Run ITK-SNAP
#'
#' @description Executes ITK-SNAP
#' @param files filenames or nifti objects to be displayed. 
#' If multiple files are given assumed the first to load and the rest are overlays
#' @param segmentation segmentation image using \code{-s} option
#' @param labels label descriptions using \code{-s} option
#' @param zoom initial zoom in screen pixels/mm
#' @param verbose Print out the command executed
#' @param ... arguments to pass to \code{\link{itksnap_cmd}}
#' @export
#' @importFrom fslr checkimg
#' @importFrom oro.nifti is.nifti
#' @return Return of \code{\link{system}} command
itksnap <- function(
  files, # filenames or nifti objects to be displayed.  If multiple files are given, assumed the first to load and the rest are overlays
  segmentation = NULL, # segmentation image using \code{-s} option
  labels = NULL, # label descriptions using \code{-s} option
  zoom = NULL, # initial zoom in screen pixels/mm
  verbose = TRUE, # Print out the command executed
  ... # arguments to pass to \code{\link{itksnap_cmd}}
){
  
  maker = function(x){
    if (is.nifti(x)){
      x = checkimg(x)
    }
    x = sapply(x, checkimg)
  }
  
  ################
  # Force to names
  ################
  files = maker(files)
  names(files) = "-o"
  names(files)[1] = "-g"
  
  if (length(segmentation) > 0){
    segmentation = maker(segmentation)
    names(segmentation) = "-s"
  }
  
  if (length(labels) > 0){
    labels = maker(labels)
    names(labels) = "-l"
  }
  if (length(zoom) > 0){
    names(zoom) = "-z"
  }
  
  allfiles = c(files, segmentation, labels, zoom)
  file.names = names(allfiles)
  allfiles = normalizePath(allfiles)
  allfiles = shQuote(allfiles)
  cmd = paste(file.names, 
              allfiles, 
              collapse = " ")
  cmd = sprintf('%s %s', shQuote(itksnap_cmd(...)), cmd)
  if (verbose){
    cat(paste0(cmd, "\n"))
  }
  res = system(cmd, ignore.stderr = TRUE, ignore.stdout = TRUE)
  return(res)
}

