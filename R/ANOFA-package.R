##############################################################################
#' @details ANOFA library for analyses of frequency data
#' 
#' @md
#' 
#' @description `ANOFA` is a library to perform frequency data analyses.
#' It is based on the G statistics (first developed by Fisher).
#' This statistics is fully additive and can be decomposed in 
#' main effects and interaction effects, in simple effects in the
#' decomposition of a significant interaction, in contrasts, etc.
#' The present library performs these analyses and also can be used
#' to plan statistical power for the analysis of frequency, obtain
#' plots of the various effects, etc. It aims at replicating the most
#' commonly-used ANOVA commands so that using this package should be
#' easy.
#' 
#' The data supplied to an ANOFA can be in three formats: (i) long format,
#' (ii) wide format, (iii) compiled format, or (iv) raw format. Check 
#' the `anofa` commands for more precision (in what follow, we assume 
#' the compiled format where the frequencies are given in a column name `Freq`)
#' 
#' The main function is
#' 
#'    \code{w <- anofa(formula, data)}  
#' 
#' where \code{formula} is a formula giving the factors, e.g., "Freq ~ A * B".
#'
#' For more details on the underlying math, see \insertCite{lc23b;textual}{ANOFA}.
#' 
#' An omnibus analysis may be followed by simple effects or contrasts analyses:
#'   \code{emFrequencies(w, formula)}
#'   \code{contrast(w, listOfContrasts)}
#' 
#' As usual, the output can be obtained with
#'   \code{print(w) #implicite}
#'   \code{summary(w) # or summarize(w) for the G statistics table}
#'   \code{explain(w) # for human-readable output}
#' 
#' Data format can be converted to other format with
#'   \code{toLong(w)}
#'   \code{toWide(w)}
#'   \code{toCompiled(w)}
#'   \code{toRaw(w)}
#'   \code{toTabulated(w) # the only format that cannot be used as input to anofa}
#' 
#' The package includes additional, helper, functions: \itemize{
#'      \item{\code{powerXXX()}} to compute sample size given effect size;
#'      \item{\code{anofaPlot()}} to obtain a plot of the frequencies with error bars;
#'      \item{\code{effectsizeXXX()}} to compute the effect size;
#'      \item{\code{grf()}} to generate random frequencies from a given design.
#' }
#' and example datasets, some described in the article:  \itemize{
#'      \item{\code{LandisBarrettGalvin2013}} illustrates a 5 x 3 design;
#'      \item{\code{LightMargolin1971}} illustrates a 5 x 2 design;
#'      \item{\code{Gillet1993}} illustrates a 2 x 3 x 2 design;
#'      \item{\code{Detergent}} illustrates a 2 x 2 x 2 x 3 design;
#' }
#' 
#' The functions uses the following options: \itemize{
#'     \item{\code{ANOFA.feedback}} ((currently unused));
#'     \item{\code{ANOFA.digits}} for the number of digits displayed in G statistics tables.
#' }
#' 
#' @references
#' \insertAllCited{}
#' 
##############################################################################
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"
##############################################################################

# Create a local environment for one temporary variable...
ANOFA.env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {    
    # Set the default feedback messages displayed and the number of digits:
    # You can use 'all' to see all the messages, or 'none'.
    options( ANOFA.feedback = c('design','warnings','summary'), ANOFA.digits = 4 )

}

.onDetach <- function(libpath) {
    # remove the options
    options( ANOFA.feedback = NULL, ANOFA.digits = NULL )
}

##############################################################################
# to inhibit "no visible binding for global variable" errors from :
# globalVariables(c("xxx","xxx","xxx"))
##############################################################################

