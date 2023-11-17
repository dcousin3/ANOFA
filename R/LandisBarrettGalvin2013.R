#' LandisBarrettGalvin2013 data
#'
#' The data, taken from \insertCite{lbg13;textual}{ANOFA}, is a dataset where the
#' participants (n = 553) are classified according to two factors, 
#' first, how  modalities of care in a family
#' medicine residency program were given. The possible cases were `Collocated
#' Behavioral Health service` (CBH), a `Primary-Care 
#' Behavioral Health service` (PBH) and a `Blended Model` (BM).
#' Second, how a patient’s care was financed:
#' `Medicare` (MC), `Medicaid` (MA), a `mix of Medicare/Medicaid`
#' (MC/MA), `Personal insurance` (PI), or `Self-paid` ($P). This
#' design therefore has 5 x 3 = 15 cells. It was thoroughly examined
#' in \insertCite{s15}{ANOFA} and analyzed in \insertCite{lc23b}{ANOFA}.
#'
#' @md
#'
#' @docType data
#'
#' @usage LandisBarrettGalvin2013
#'
#' @format An object of class data.frame.
#'
#' @keywords datasets
#'
#' @references 
#' \insertAllCited{}
#'
#' @source \doi{10.1037/a0033410}
#'
#' @examples
#'
#' # running the anofa
#' L <- anofa( obsfreq ~ provider * program, LandisBarrettGalvin2013)
#' 
#' # getting a plot
#' anofaPlot(L)
#' 
#' # the G table shows a significant interaction
#' summary(L)
#' 
#' # getting the simple effect
#' e <- emFrequencies(L, ~ program | provider ) 
#' 
#' ## Getting some contrast by provider (i.e., on e)
#' f <- contrastFrequencies(e, list(
#'          "(PBH & CBH) vs. BM"=c(1,1,-2)/2, 
#'          "PBH vs. CBH"=c(1,-1,0))
#'      )
#' 
#' 
"LandisBarrettGalvin2013"