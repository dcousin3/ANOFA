######################################################################################
#' @title emFrequencies: simple effect analysis of frequency data.
#'
#' @md
#'
#' @description The function ``emFrequencies()`` performs a simple effect analyses 
#'    of frequencies after an omnibus analysis has been obtained with ``anofa()``
#'    according to the ANOFA framework. See \insertCite{lc23b}{ANOFA} for more.
#'
#' @usage emFrequencies(w, formula)
#'
#' @param w An ANOFA object obtained from ``anofa()``;
#'
#' @param formula A formula which indicates what simple effect to analyze. 
#'   only one simple effect formula at a time can be analyzed. The formula
#'   is given using a vertical bar, e.g., `` ~ factorA | factorB`` to obtain 
#'   the effect of Factor A within every level of the Factor B.
#'
#' @return a model fit of the simple effect. 
#' 
#' @details emFrequencies computes expected marginal frequencies and 
#'   analyze the hypothesis of equal frequencies. 
#'   The sum of the Gs of the simple effects are equal to the 
#'   interaction and main effect Gs, as this is an additive decomposition
#'   of the effects.
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
#' e <- emFrequencies(w, ~ Pitch | Intensity)
#' summary(e)
#'
#' # As a check, you can verify that the Gs are decomposed additively
#' sum(e$results[,1])
#' w$results[3,1]+w$results[4,1] 
#' 
#' # Real-data example using a two-factor design with the data in compiled format:
#' LandisBarrettGalvin2013
#'
#' w <- anofa( obsfreq ~ provider * program, LandisBarrettGalvin2013)
#' anofaPlot(w)
#' summary(w)
#' 
#' # there is an interaction, so look for simple effects
#' e <- emFrequencies(w, ~ program | provider )
#' summary(e)
#' 
#' # Example from Gillet1993 : 3 factors for appletrees 
#' Gillet1993
#'
#' w <- anofa( Freq ~ species * location * florished, Gillet1993)
#' e <- emFrequencies(w, ~ florished | location )
#'
#' # Again, as a check, you can verify that the Gs are decomposed additively
#' w$results[4,1]+w$results[7,1] # B + B:C
#' sum(e$results[,1])
#' 
#' # You can ask easier outputs with
#' summarize(w) # or summary(w) for the ANOFA table only
#' explain(w)   # human-readable ouptut ((pending))
#' 
######################################################################################
#'
#' @export emFrequencies
#' @importFrom utils capture.output
#
######################################################################################


emFrequencies <- function(
    w       = NULL,
    formula = NULL
){
    ##############################################################################
    ## STEP 1: VALIDATION OF INPUT
    ##############################################################################
    # 1.1 Is w of the right type and having more than 1 factor
    if (!("ANOFAobject" %in% class(w))) 
        stop("ANOFA::error(21): The argument w is not an ANOFA object. Exiting...")
    if (length(w$factColumns)==1)
        stop("ANOFA::error(22): There must be at least two factors in the design. Exiting...")

    # 1.2 If formula indeed a formula
    if (!(is.formula(formula))) 
        stop("ANOFA::error(23): The formula argument is not a legitimate formula. Exiting...")

    # 1.3 Is the rhs having a | sign, and only one
    if (!(has.nested.terms(formula)))
        stop("ANOFA::error(24): The rhs of formula must have a variable nested within another variable. Exiting... ")
    if (length(tmp <- sub.formulas(formula, "|"))!=1) 
        stop("ANOFA::error(25): The rhs of formula must contain a single nested equation. Exiting...")
    if (tmp[[1]][[2]]==tmp[[1]][[3]])
        stop("ANOFA::error(26): The two variables on either side of | must differ. Exiting...")

    # 1.4 If the dependent variable is named (optional), is it the correct variable
    if (length(formula)==3) {
         if (formula[[2]] != w$freqColumn)
             stop("ANOFA::error(27): The lhs of formula is not the correct frequency variable. Exiting...")
    }

    # 1.5 Are the factors named in formula present in w
    vars <- all.vars(formula) # extract variables in cbind and with | alike
    if (!(all(vars %in% names(w$compiledData)))) 
        stop("ANOFA::error(28): variables in `formula` are not all in the data. Exiting...")


    ##############################################################################
    ## STEP 2: Run the analysis based on the number of factors
    ##############################################################################
    # 2.1: identify the factors
    subfactors <- all.vars(tmp[[1]][[2]])
    nestfactor <- all.vars(tmp[[1]][[3]])

    # 2.2: perform the analysis based on the number of factors
    analysis <- switch( length(subfactors),
        emf1way(w, subfactors, nestfactor),
        emf2way(w, subfactors, nestfactor),
        emf3way(w, subfactors, nestfactor),
    )


    ##############################################################################
    # STEP 3: Return the object
    ##############################################################################
    # 3.1:  preserve everything in an object of class ANOFAobject
    res <- list(
        type          = "ANOFAsimpleeffects",
        formula       = as.formula(w$formula),
        compiledData  = w$compiledData,
        freqColumn    = w$freqColumn,
        factColumns   = w$factColumns,
        nlevels       = w$nlevels,
        clevels       = w$clevels,
        # new information added
        formulasimple = as.formula(formula),
        nestedFactors = nestfactor,
        subFactors    = subfactors,
        nestedLevels  = dim(analysis)[1],
        results       = analysis
    )
    class(res) <- c("ANOFAobject", class(res) )
    return( res )

}


emf1way <- function(w, subfact, nestfact){
    ## Herein, the sole factor analyzed is called factor A, which is nested within factor B,
    ## that is, the effect of A within b1, the effect of A within b2, ... A within bj.
    ## There can only be a single nesting variable (e.g., B)
    ## We can get here if (a) there are 2 factors, (b) there are 3 factors, (c) there are 4 factors

    # The factors position in the factor list and their number of levels
    posA <- (1:length(w$factColumns))[w$factColumns==subfact]
    A    <- w$nlevels[posA]

    #    posB <- (1:length(w$factColumns))[w$factColumns==nestfact]
    #    B   <- w$nlevels[posB]
    if (length(nestfact)==1) {
        posB <- (1:length(w$nlevels))[w$factColumns==nestfact]
        B    <- w$nlevels[posB]
        lvls <- w$clevels[[posB]]
    } else {
        B <-1
        lvls <- c()
        for (i in nestfact) {
            pos <- (1:length(w$nlevels))[w$factColumns==i]
            B <- B * w$nlevels[pos]
            lvls <- unlist(lapply(w$clevels[[pos]], function(x) paste(lvls,x, sep=":")))
        }
        nestfact <- paste(nestfact, collapse = "+")
    }

    # the total n and the vector marginals
    ndd <- sum(w$compiledData[[w$freqColumn]])
    ndj <- aggregate(as.formula(paste(w$freqColumn, nestfact, sep="~")), 
              data = w$compiledData, sum)[[w$freqColumn]]

    by2 <- paste(c(nestfact, subfact), collapse=" + ")
    nij <- matrix(aggregate(as.formula(paste(w$freqColumn, by2, sep="~")), 
              data = w$compiledData, sum)[[w$freqColumn]], B)

    # First, we compute the expected frequencies e separately for each levels of factor B
    eigivenj <- ndj / A


    # Second, we get the G statistics for each level
    Gs <- c()
    for (j in 1:B) {
        Gs[j] <- 2 * sum(nij[j,] * (log(nij[j,])- log(eigivenj[j])))
    }

    # Third, the correction factor (Williams, 1976) for each 
    cf <- 1+ (A^2-1) / ( 6 * (A-1) * ndd)

    # Finally, getting the p-values for each corrected effect
    ps = 1- pchisq( Gs / cf, df = B-1)

    # This is it! let's put the results in a table
    resultsSimpleEffects <- data.frame(
      G          = Gs,
      df         = rep(A-1, B),
      Gcorrected = Gs / cf,
      pvalue     = ps,
      etasq      = apply( nij/ndj, 1, anofaES)
    )
    rownames(resultsSimpleEffects) <- paste(subfact, lvls, sep=" | ")
    resultsSimpleEffects
}


emf2way <- function(w, subfacts, nestfact){
    ## Herein, the factors decomposed are called factors A*B,
    ## that is, the effects of (A, B, A*B) within c1, the effects of (A, B, A*B) within c2, etc.
    ## We can get here if (a) there are 3 factors, (b) there are 4 factors.

    # The factor positions in the factor list and their number of levels
    posA <- (1:length(w$nlevels))[w$factColumns==subfacts[1]]
    A    <- w$nlevels[posA]
    posB <- (1:length(w$nlevels))[w$factColumns==subfacts[2]]
    B    <- w$nlevels[posB]

    # posC <- (1:length(w$factColumns))[w$factColumns==nestfact]
    # C   <- w$nlevels[posC]
    if (length(nestfact)==1) {
        posC <- (1:length(w$nlevels))[w$factColumns==nestfact]
        C    <- w$nlevels[posC]
        lvls <- w$clevels[[posC]]
    } else {
        C <-1
        lvls <- c()
        for (i in nestfact) {
            pos <- (1:length(w$nlevels))[w$factColumns==i]
            C <- C * w$nlevels[pos]
            lvls <- unlist(lapply(w$clevels[[pos]], function(x) paste(x,lvls, sep=":")))
        }
        nestfact <- paste(nestfact, collapse = "+")
    }


    # the total n and the matrix marginals
    nddd <- sum(w$compiledData[[w$freqColumn]])
    nddk <- aggregate(as.formula(paste(w$freqColumn, nestfact, sep="~")), 
              data = w$compiledData, sum)[[w$freqColumn]]

    by2 <- paste(subfacts, nestfact, sep="+")
    nidk <- matrix(aggregate(as.formula(paste(w$freqColumn, by2[[1]],sep="~")), 
        data = w$compiledData, sum)[[w$freqColumn]], A,C )
    ndjk <- matrix(aggregate(as.formula(paste(w$freqColumn, by2[[2]],sep="~")), 
        data = w$compiledData, sum)[[w$freqColumn]], B,C )

#    # here is ok for a three factor design only... use tt uu with %in%
#    nijd <- array(aggregate(as.formula(paste(w$freqColumn, paste(subfacts, collapse=" + "), sep="~")),
#        data = w$compiledData, sum)[[w$freqColumn]],c(A,B,C))

    by3 <- paste(paste(subfacts, collapse=" + "), nestfact, sep = " + ")
    nijk <- array(aggregate(as.formula(paste(w$freqColumn, by3, sep="~")),
        data = w$compiledData, sum)[[w$freqColumn]],c(A,B,C))

    # First, we compute the expected frequencies e separately for each levels of factor C
    eidgivenk <- nddk / A
    edjgivenk <- nddk / B
    eijgivenk <- nddk / (A * B)

    # Second, we get the G statistics for each level
    Gs <- c()
    for (k in 1:C) {
        Gs[(k-1)*3+1] <- 2 * sum(nijk[,,k] * (log(nijk[,,k])- log(eijgivenk[k])))
        Gs[(k-1)*3+2] <- 2 * sum(nidk[,k] * (log(nidk[,k])- log(eidgivenk[k])))
        Gs[(k-1)*3+3] <- 2 * sum(ndjk[,k] * (log(ndjk[,k])- log(edjgivenk[k])))
        Gs[(k-1)*3+1] <- Gs[(k-1)*3+1] - Gs[(k-1)*3+2] - Gs[(k-1)*3+3] # remove simple effects
    }

    # Third, the correction factor (Williams, 1976) for each 
    cfA  <- 1+ (A^2-1) / ( 6 * (A-1) * nddd)
    cfB  <- 1+ (B^2-1) / ( 6 * (B-1) * nddd)
    cfAB <- 1+ ((A*B)^2-1) / ( 6 * (A-1) * (B-1) * nddd)

    alletasq <- c()
    for (k in 1:C) {
        alletasq[(k-1)*3+1] <- anofaES(as.vector(nijk[,,k])/nddk[k])
        alletasq[(k-1)*3+2] <- anofaES(nidk[,k]/nddk[k])
        alletasq[(k-1)*3+3] <- anofaES(ndjk[,k]/nddk[k])
    }
    
    # Finally, getting the p-values for each corrected effect
    ps <- 1- pchisq( Gs / c(cfAB, cfA, cfB), df = c((A-1)*(B-1), A-1, B-1) )

    # This is it! let's put the results in a table
    resultsSimpleEffects <- data.frame(
      G          = Gs,
      df         = rep(c((A-1)*(B-1), A-1, B-1), C),
      Gcorrected = Gs / c(cfAB, cfA, cfB),
      pvalue     = ps,
      etasq      = alletasq
    )
    lbl <- c(paste(subfacts, collapse=":"), subfacts )
    rownames(resultsSimpleEffects) <- unlist(lapply(lvls, \(x) paste(lbl, x, sep=" | ")))
    
    resultsSimpleEffects

}

##########################################
#                                        #
#    ██╗  ██╗███████╗██████╗ ███████╗    #
#    ██║  ██║██╔════╝██╔══██╗██╔════╝    #
#    ███████║█████╗  ██████╔╝█████╗      #
#    ██╔══██║██╔══╝  ██╔══██╗██╔══╝      #
#    ██║  ██║███████╗██║  ██║███████╗    #
#    ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚══════╝    #
#                                        #
##########################################
# Coding third-order simple effects      #
# is postponed until interest is shown.  #
##########################################

emf3way <- function(w, subfacts, nestfact){
    ## Herein, the factor decomposed is called factor D,
    ## that is, the effect of (A, B, C, AB, AC, BC, A*B*C) within d1, 
    ## the effect of (A, B, C, AB, AC, BC, A*B*C) within d2, etc.
    ## There is only one way to get here: a 4 factor design is decomposed.
    
    "If you need this functionality, please contact the author...\n"
}


