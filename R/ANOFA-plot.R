######################################################################################
#' @title anofaPlot.
#'
#' @aliases anofaPlot count init.count CI.count
#'
#' @md
#'
#' @description The function `anofaPlot()` performs a plot of frequencies for designs 
#'      with up to 4 factors according to the
#'      `ANOFA` framework. See \insertCite{lc23b;textual}{ANOFA} for more. The plot is 
#'      realized using the `suberb` library; see \insertCite{cgh21;textual}{ANOFA}.
#'      The functions `count()`, `init.count()` and `CI.count()` are internal functions.
#'
#' @usage anofaPlot(w, formula, confidenceLevel = .95, showPlotOnly = TRUE, plotStyle = "line", 
#'     errorbarParams  = list( width =0.5, linewidth=0.75 ), ...)
#'
#' @usage count(n)
#' @usage init.count(df)
#' @usage CI.count(n, gamma =0.95)
#'
#' @param w An ANOFA object obtained with `anofa()`;
#'
#' @param formula (optional) Use formula to plot just specific terms of the omnibus test.
#'       For example, if your analysis stored in `w` has factors A, B and C, then
#'       `anofaPlot(w, ~ A * B)` will only plot the factors A and B.
#' @param confidenceLevel Provide the confidence level for the confidence intervals.
#'       (default is 0.95, i.e., 95%).
#'
#' @param plotStyle (optional; default "line") How to plot the frequencies. See superb for other layouts
#'      (e.g., "line")
#'
#' @param showPlotOnly (optional, default True) shows only the plot or else shows the numbers
#'      needed to make the plot yourself.
#' 
#' @param errorbarParams (optional; default list( width =0.5, linewidth=0.75 ) ) A list of 
#'      attributes used to plot the error bars. See superb for more.
#'
#' @param ... Other directives sent to superb(), typically 'plotStyle', 'errorbarParams', etc.
#'
#' @param n the count for which a confidence interval is required
#' @param df a data frame for initialization of the CI function
#' @param gamma the confidence level
#'
#' @return a ggplot2 object of the given frequencies. 
#'   
#'
#' @details The plot shows the frequencies (the count of cases) on the vertical axis as a 
#'   function of the factors (the first on the horizontal axis, the second if any in a legend;
#'   and if a third or even a fourth factors are present, as distinct rows and columns).
#'   It also shows 95% confidence intervals of the frequency, adjusted for between-cells
#'   comparisons. The confidence intervals are based on the Clopper and Pearson method
#'   \insertCite{cp34}{ANOFA} using the Leemis and Trivedi
#'    analytic formula \insertCite{lt96}{ANOFA}. This "stand-alone" confidence
#'   interval is then adjusted for between-cell comparisons using the superb framework
#'   \insertCite{cgh21}{ANOFA}.
#' 
#' See the vignette `DataFormatsForFrequencies` for more on data format and how to write their 
#'    formula. See the vignette `ConfidenceInterval` for details on the adjustment and its purpose.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # 
#' # The Landis et al. (2013) example has two factors, program of treatment and provider of services.
#' LandisBarrettGalvin2013
#'
#' # This examine the omnibus analysis, that is, a 5 (provider) x 3 (program):
#' w <- anofa(obsfreq ~ provider * program, LandisBarrettGalvin2013) 
#' 
#' # Once processed into w, we can ask for a standard plot
#' anofaPlot(w)
#' 
#' # We place the factor `program` on the x-axis:
#' anofaPlot(w,  factorOrder = c("program","provider"))
#' 
#' # The above example can also be obtained with a formula:
#' anofaPlot(w, ~ program * provider)
#' 
#' # Change the style for a plot with bars instead of lines
#' anofaPlot(w, plotStyle = "bar")
#' 
#' # Changing the error bar style
#' anofaPlot(w, plotStyle = "bar", errorbarParams = list( width =0.1, linewidth=0.1 ) )
#' 
#' # An example with 4 factors:
#' \dontrun{
#' dta <- data.frame(Detergent)
#' dta
#'
#' w <- anofa( Freq ~ Temperature * M_User * Preference * Water_softness, dta )
#' anofaPlot(w)
#' anofaPlot(w, factorOrder = c("M_User","Preference","Water_softness","Temperature")) 
#' 
#'
#' # Illustrating the main effect of Temperature (not interacting with other factors)
#' # and the interaction Preference * Previously used M brand
#' # (Left and right panels of Figure 4 of the main article)
#' anofaPlot(w, ~ Temperature)
#' anofaPlot(w, ~ Preference * M_User)
#' 
#' # All these plots are ggplot2 so they can be followed with additional directives, e.g.
#' library(ggplot2)
#' anofaPlot(w, ~ Temperature) + ylim(200,800) + theme_classic()
#' anofaPlot(w, ~ Preference * M_User) + ylim(100,400) + theme_classic()
#' }
#' # etc. Any ggplot2 directive can be added to customize the plot to your liking.
#' # See the vignette `Example2`.
#'
######################################################################################
#'
#' @export anofaPlot
#' @export count
#' @export init.count
#' @export CI.count
#' @importFrom utils capture.output
#' @importFrom Rdpack reprompt
#
######################################################################################



# First, we need the summary function that computes the frequency.
# this is actually the datum when the data are in compiled format, so there is nothing to do.
count <- function(n) n[1]

# Second, we need an initalizer that will fetch the total sample size
# and dump it in the global environment for later use
init.count <- function(df) {
    # ANOFAtotalcount <<- sum(df$DV)
    assign('ANOFAtotalcount', sum(df$DV), ANOFA.env) 
}

# Third, we compute the limits using Clopper & Pearson 1934 approach.
# This is the Leemis and Trivedi (1996) analytic form.
CI.count <- function(n, gamma=0.95) {
    #N <- ANOFAtotalcount 
    N <- ANOFA.env$ANOFAtotalcount # shorter to write...
    # Clopper & Pearson CI from Leemis & Trivedi, 1996
    plow <- (1+(N-n+1)/((n+0)*qf(1/2-gamma/2,2*(n+0),2*(N-n+1))))^(-1)
    phig <- (1+(N-n+0)/((n+1)*qf(1/2+gamma/2,2*(n+1),2*(N-n+0))))^(-1)
    # convert to CI on counts
    nlow <- N * plow
    nhig <- N * phig
    # multiply width by 2 for difference- and correlation-adjustments
    2 * c( nlow[1]-n[1], nhig[1]-n[1] ) + n[1]
}


################################################################
# This is it! let's make the plot                              #
################################################################

# make the plot: just a proxy for suberbPlot
anofaPlot <- function(w, 
                formula         = NULL,
                confidenceLevel = 0.95,
                showPlotOnly    = TRUE,
                plotStyle       = "line",                             # lines by default
                errorbarParams  = list( width =0.5, linewidth=0.75 ), # thicker error bars
                ...  # will be transmitted to superb as is
){

    ##############################################################################
    ## STEP 1: validating the formula if one is given
    ##############################################################################
    if (!is.null(formula)) {
        # 1.1 is it a legitimate formula?
        if (!(is.formula(formula))) 
            stop("ANOFA::error(12): The formula argument is not a legitimate formula. Exiting...")
        # 1.2 If the dependent variable is named (optional), is it the correct variable
        if (length(formula)==3) {
             if (formula[[2]] != w$freqColumn)
                 stop("ANOFA::error(13): The lhs of formula is not the correct frequency variable. Exiting...")
        }
        # if everything ok, let's compile the data
        bsfact <- all.vars(formula)[ ! all.vars(formula) == w$freqColumn]
        cdata  <- aggregate(as.formula(paste(w$freqColumn, paste(bsfact,collapse="+"), sep="~")), 
                    data = w$compiledData, sum)
    } else {
        bsfact <- w$factColumns
        cdata <- w$compiledData
    }
    if ((confidenceLevel >= 1)||(confidenceLevel <=0.0))
        stop("ANOFA::error(14): The confidence level is not within 0 and 1 (i.e., within 0% and 100%). Exiting...")
    
    ##############################################################################
    ## All done! ask for the plot or the data
    ##############################################################################
    # quiet superb's information
    opt <- getOption("superb.feedback")
    on.exit(options(opt))
    options(superb.feedback = 'none')

    if (showPlotOnly) { # generate the plot
        res <- superb::superbPlot( cdata,
            BSFactors      = bsfact,
            variables      = as.character(w$freqColumn), # as it may be a symbol
            statistic      = "count",       # the summary statistics defined above
            errorbar       = "CI",          # its precision define above
            gamma          = confidenceLevel,
            # the following is for the look of the plot
            plotStyle      = plotStyle,
            errorbarParams = errorbarParams,   
            ...## passed as is to superb
        ) + ggplot2::ylab("Freqency") 
    } else { # only compute the summary statistics
        res <- superb::superbData( cdata,
            BSFactors      = bsfact,
            variables      = as.character(w$freqColumn), # as it may be a symbol
            statistic      = "count",       # the summary statistics defined above
            errorbar       = "CI",          # its precision define above
            gamma          = confidenceLevel
        )$summaryStatistics
    }
    # restore superb's information: done with on.exit()
    # options(superb.feedback = opt)

    return(res)
}
