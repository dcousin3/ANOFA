#' LightMargolin1971 data
#'
#' The data, taken from \insertCite{lm71}{ANOFA}, is an example where the
#' educational aspiration of a large sample of N =
#' 617 adolescents. The participants are classified by their
#' gender (2 levels) and by their educational aspiration (
#' complete secondary school, complete vocational training, 
#' become college teacher, complete gymnasium, or complete
#' university; 5 levels). 
#'
#' @md
#'
#' @docType data
#'
#' @usage LightMargolin1971
#'
#' @format An object of class data.frame.
#'
#' @keywords datasets
#'
#' @references 
#' \insertAllCited{}
#'
#' @source \doi{10.1080/01621459.1971.10482297}
#'
#' @examples
#' library(ANOFA)
#' 
#' options(superb.feedback = 'none') # shut down 'warnings' and 'design' interpretation messages
#' 
#' # Lets run the analysis
#' L <- anofa( obsfreq ~ vocation * gender, LightMargolin1971)
#' summary(L)
#'
#' # a quick plot
#' anofaPlot(L) 
#' 
#' # Some simple effects.
#' e <- emFrequencies(L, ~ gender | vocation )
#' summary(e)
#' 
#' # some contrasts:
#' e <- emFrequencies(L, ~ vocation | gender )
#' f <- contrastFrequencies(e, list(
#'             "teacher college vs. gymnasium"=c( 0, 0, 1,-1, 0),
#'             "vocational vs. university"   = c( 0, 1, 0, 0,-1),
#'             "another"                     = c( 0, 1,-1,-1,+1)/2,
#'             "to exhaust the df"           = c( 4,-1,-1,-1,-1)/4
#'             )
#'         )
#' 
#' 
"LightMargolin1971"