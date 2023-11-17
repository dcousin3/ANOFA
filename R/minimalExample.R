###################################################################################
#' @title minimalExample
#'
#' @name minimalExample
#'
#' @description The data in compiled format are analyzed with an 
#'    Analysis of Frequency Data method (described in \insertCite{lc23b}{ANOFA}.
#'
#' @md
#'
#' @docType data
#'
#' @format An object of class data.frame.
#'
#' @keywords datasets
#'
#' @references 
#' \insertAllCited{}
#'
#' @source \doi{10.20982/tqmp.19.2.p173}
#'
#' @examples
#' library(ANOFA)
#'
#' # the minimalExample data (it has absolutely no effect...) 
#' minimalExample
#' 
#' # perform an anofa on this dataset
#' w <- anofa( Frequency ~ Intensity * Pitch, minimalExample)
#' 
#' # We analyse the intensity by levels of pitch
#'    e <- emFrequencies(w, ~ Intensity | Pitch)
#' 
#' # decompose by 
#' f <- contrastFrequencies(e, list(
#'       "low & medium compared to high" = c(1,1,-2)/2, 
#'       "low compared to medium       " = c(1,-1,0)))
#' 
#' 
"minimalExample"