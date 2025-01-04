######################################################################################
#' @title contrastFrequencies: contrasts analysis of frequency data.
#'
#' @md
#'
#' @description The function `contrastFrequencies()` performs contrasts analyses 
#'    of frequencies after an omnibus analysis has been obtained with `anofa()`
#'    according to the ANOFA framework. See \insertCite{lc23b;textual}{ANOFA} for more.
#'      
#' @param w An ANOFA object obtained from `anofa()` or `emFrequencies()`;
#'
#' @param contrasts A list that gives the weights for the contrasts to analyze. 
#'   The contrasts within the list can be given names to distinguish them.
#'   The contrast weights must sum to zero and their cross-products must equal 0 
#'   as well.
#'
#' @return a model fit of the constrasts. 
#' 
#' @details contrastFrequencies computes the Gs for the contrasts,
#'   testing the hypothesis that it equals zero.
#'   The contrasts are each 1 degree of freedom, and the sum of the contrasts' 
#'   degrees of freedom totalize the effect being decomposed.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # Basic example using a two-factors design with the data in compiled format. 
#' # Ficticious data present frequency of observation classified according
#' # to Intensity (three levels) and Pitch (two levels) for 6 possible cells.
#' minimalExample
#'
#' # performs the omnibus analysis first (mandatory):
#' w <- anofa(Frequency ~ Intensity * Pitch, minimalExample) 
#' summary(w)
#'
#' # execute the simple effect of Pitch for every levels of Intensity
#' e <- emFrequencies(w, ~ Intensity | Pitch)
#' summary(e)
#'
#' # For each Pitch, contrast the three intensities, first
#' # by comparing the first two levels to the third, second
#' # by comparing the first to the second level:
#' w3 <- contrastFrequencies( e, list(
#'          contrast1 = c(1,  1, -2)/2,
#'          contrast2 = c(1, -1,  0) )
#'       )
#' summary(w3)
#'
#' # Example using the Landis et al. (2013) data, a 3 x 5 design involving 
#' # program of care (3 levels) and provider of care (5 levels).
#' LandisBarrettGalvin2013
#'
#' # performs the omnibus analysis first (mandatory):
#' w <- anofa(obsfreq ~ provider * program, LandisBarrettGalvin2013) 
#' summary(w)
#'
#' # execute the simple effect of Pitch for every levels of Intensity
#' e <- emFrequencies(w, ~ program | provider)
#' summary(e)
#'
#' # For each Pitch, contrast the three intensities, first
#' # by comparing the first two levels to the third, second
#' # by comparing the first to the second level:
#' w3 <- contrastFrequencies( e, list(
#'          contrast1 = c(1,  1, -2)/2,
#'          contrast2 = c(1, -1,  0) )
#'       )
#' summary(w3)
#' 
#' 
######################################################################################
#'
#' @export contrastFrequencies
#
######################################################################################


contrastFrequencies <- function(
    w         = NULL,
    contrasts = NULL
){
    ##############################################################################
    ## STEP 1: VALIDATION OF INPUT
    ##############################################################################
    # 1.1: Is w of the right type 
    if (!("ANOFAobject" %in% class(w))) 
        stop("ANOFA::error(31): The argument w is not an ANOFA object. Exiting...")

    # 1.2: Are the contrasts of the right length
    if (min(unlist(lapply(contrasts, length))) != max(unlist(lapply(contrasts, length))))
        stop("ANOFA::error(32): The constrasts have differing lengths. Exiting...")
    relevantlevels = prod(if (w$type == "ANOFAomnibus") {
        w$nlevels
    } else {
        w$nlevels[w$factColumns %in% w$subFactors]
    } )
    if (!(all(unlist(lapply(contrasts, length)) == relevantlevels )))
        stop("ANOFA::error(33): The contrats lengths does not match the number of levels. Exiting...")

    # 1.3: Are the contrasts legitimate (a) sum to zero; 
    if (!(all(round(unlist(lapply(contrasts,sum)),8)==0))) 
        stop("ANOFA::error(34): Some of the constrats do not sum to 0. Exiting...")
    # 1.3: Are the contrasts legitimate (b) cross-product sum to zero
    sums = c()
    for (i in names(contrasts)) {for (j in setdiff(names(contrasts), i)) {
        sums <-append(sums, round(sum(contrasts[[i]]*contrasts[[j]]) ),8) } }
    if (!(all(sums == 0)))
        stop("ANOFA::error(35): Some of the cross-products of contrasts do not totalize 0. Exiting...")

    # 1.4: is there an acceptable number of contrasts
    if (length(contrasts) > relevantlevels-1)
        stop("ANOFA::error(37): There are more contrasts defined than degrees of freedom. Exiting...")
    # 1.5: Normalize the contrasts so that they sum to 1
    for (i in names(contrasts)) {
        contrasts[[i]] <- contrasts[[i]] / sum(abs(contrasts[[i]])) * 2
    }

    
    
    ##############################################################################
    ## STEP 2: Run the analysis 
    ##############################################################################
    # 2.1: identify the factors

    # 2.2: perform the analysis based on ???
    analysis <- if(w$type == "ANOFAomnibus") {
        ## Contrasts on the full design
        cst1way(w$compiledData, w$freqColumn, contrasts)
    } else { # "ANOFAsimpleeffects"
        ## Contrasts on sub-data based on nesting factor(s)
        cstMway(w$compiledData, w$freqColumn, w$subFactors, w$nestedFactors, contrasts)
    }

    ##############################################################################
    # STEP 3: Return the object
    ##############################################################################
    # 3.1:  preserve everything in an object of class ANOFAobject
    res <- list(
        type          = "ANOFAcontrasts",
        formula       = as.formula(w$formula),
        compiledData  = w$compiledData,
        freqColumn    = w$freqColumn,
        factColumns   = w$factColumns,
        nlevels       = w$nlevels,
        clevels       = w$clevels,
        # new information added
        results       = analysis
    )
    class(res) <- c("ANOFAobject", class(res) )
    return( res )

}

# ################################################################
# # Lets run the orthogonal contrasts                            #
# ################################################################

# there is only two possible cases:
# a) the contrasts are on the full data             ==> cst1way
# b) the contrasts are nested with some factor(s)   ==> cstMway
cst1way <- function(dataC, freqcol, contrasts) {
    # extract the frequencies and the total frequency
    ns <- dataC[[freqcol]]
    nd <- sum(ns)

    # get the G statistics for each contrast
    Gs <- c()
    for (i in contrasts) {
        Gs <- c(Gs, 2 * sum( contrastObsd(i, ns) * (
                log( contrastObsd(i, ns) ) - 
                log( contrastNull(i, ns) ) 
            )
        ) )
    }

    # compute the correction factor cf
    cf <- 1+ (length(ns)^2-1) / ( 6 * (length(ns)-1) * nd)
    
    # compute the p values on the corrected G as usual
    pcontrasts <- 1- pchisq(Gs/ cf, df = 1)

    # This is it! let's put the results in a table
    resultsGContrasts <- data.frame(
        G = Gs,
        df = rep(1, length(Gs)),
        Gcorrected = c(Gs/ cf),
        pvalue = pcontrasts
    )
    rownames(resultsGContrasts) <- names(contrasts)
    resultsGContrasts

}

cstMway <- function(datasC, freqcol, subfact, nestfact, contrasts) {
    ## run cst1way on every levels of the nesting factor(s)
    splitting <- paste(nestfact, collapse = "+")
    dtas      <- split(datasC, as.formula(paste("~",splitting)))

    # in case other factors are not named, collapse over these
    frm <- paste(freqcol, paste(subfact, collapse = "+"), sep=" ~ ")
    dtas <- lapply(dtas, \(x) aggregate(x, as.formula(frm), sum))

    Gs <- data.frame()
    for (i in names(dtas)) {
        subGs <- cst1way( dtas[[i]], freqcol, contrasts)
        rownames(subGs) <- paste( rownames(subGs), i, sep=" | ")
        Gs <- rbind(Gs, subGs)
    }
    Gs
}

# ################################################################
# # Sub-functions to get observed and expected  frequencies      #
# ################################################################

# the null hypothesis for the conditions implicated in the contrast
# ns is a vector
contrastNull <- function(contrast, ns) { 
  pp <- sign(abs(contrast))
  pp <- pp * sum(ns * pp)/sum(pp) + (1 - pp) * ns
  pp[contrast!=0]
}


# the contrast hypothesis for the conditions implicated in the contrast
# ns is a vector
contrastObsd <- function(contrast, ns) {
  nn <- rep(0, length(ns) )
  
  ss <- contrast
  ss[sign(contrast)<=0] <- 0
  nn <- nn + sign(abs(ss)) * sum(ns * ss)
  
  ss <- -contrast
  ss[sign(contrast)>=0] <- 0
  nn <- nn + sign(abs(ss)) * sum(ns * ss)

  ss <- contrast
  ss[sign(contrast)==0] <- 1
  ss[sign(contrast)!=0] <- 0
  nn <- nn + sign(abs(ss)) * sum(ns * ss)
  nn[contrast!=0] 
}

