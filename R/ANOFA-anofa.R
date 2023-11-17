####################################################################################
#' @title anofa: analysis of frequency data.
#'
#' @md
#'
#' @description The function `anofa()` performs an anofa of frequencies for designs with up to 4 factors
#'      according to the `anofa` framework. See \insertCite{lc23b}{ANOFA} for more.
#'      
#'
#' @param data Dataframe in one of wide, long, raw or compiled format;
#'
#' @param formula A formula with the factors on the left-hand side. See below for writing the 
#' formula according to the data format.
#'
#' @param factors For raw data formats, provide the factor names.
#'
#' @return a model fit to the given frequencies. The model must always be an omnibus model (for 
#'    decomposition of the main model, follow the analysis with `emFrequencies()` or `contrastFrequencies()`)
#'         
#'
#' @details The data can be given in four formats:
#' * `wide`: In the wide format, there is one line for each participant, and
#'      one column for each factor in the design. In the column(s), the level must
#'      of the factor is given (as a number, a string, or a factor).
#' * `long`: In the long format, there is an identifier column for each participant, 
#'      a factor column and a level number for that factor. If there are n participants
#'      and m factors, there will be in total n x m lines.
#' * `raw`: In the raw column, there are as many lines as participants, and as many columns as
#'      there are levels for each factors. Each cell is a 0|1 entry.
#' * `compiled`: In the compiled format, there are as many lines as there are cells in the
#'      design. If there are two factors, with two levels each, there will be 4 lines.
#' See the vignette `DataFormatsForFrequencies` for more on data format and how to write their formula.
#'
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # Basic example using a single-factor design with the data in compiled format. 
#' # Ficticious data present frequency of observation classified according
#' # to Intensity (three levels) and Pitch (two levels) for 6 possible cells.
#' minimalExample
#'
#' formula <- Frequency ~ Intensity * Pitch
#' w <- anofa(formula, minimalExample) 
#' summary(w)
#'
#' # To know more about other ways to format the datasets, 
#' # see, e.g., `toRaw()`, `toLong()`, `toWide()`
#' w <- anofa(formula, minimalExample)
#' toWide(w)
#' # See the vignette `DataFormatsForFrequencies` for more.
#'
#' # Real-data example using a two-factor design with the data in compiled format:
#' LandisBarrettGalvin2013
#'
#' w <- anofa( obsfreq ~ program * provider, LandisBarrettGalvin2013 )
#' summary(w)
#'
#' # You can ask easier outputs
#' w <- anofa(formula, minimalExample)
#' summarize(w) # or summary(w) for the ANOFA table
#' explain(w)   # human-readable ouptut
#' 
####################################################################################
#' @importFrom stats aggregate pchisq qchisq qf as.formula
#' @importFrom utils combn
#' @export anofa
#' @importFrom utils capture.output
#
####################################################################################


anofa <- function(
    formula       = NULL,  #mandatory: the design of the data
    data          = NULL,  #mandatory: the data itself
    factors       = NULL   #optional: if the data are in raw format, name the factors
) {
    ##############################################################################
    # STEP 0: preliminary preparations...
    ##############################################################################
	data <- as.data.frame(data) # coerce to data.frame if tibble or compatible

    ##############################################################################
    # STEP 1: Input validation
    ##############################################################################    
    # 1.1: is the formula actually a valid formula?
    if (!is.formula(formula)) 
        stop("ANOFA::error(11): `formula` argument is not a legitimate formula. Exiting...")

    # 1.2: has the formula having 0 or 1 DV?
    if (length(formula)==3) {
        if (length(all.vars(formula[[2]]))!=1) 
            stop("ANOFA::error(12): formula argument has more than 0 or 1 DV. Exiting...")
    }

    # 1.3: are the data actually data?
    if( (!is.data.frame(data)) || (dim(data)[2] <= 1))
        stop("ANOFA::error(13): `data` argument is not a data.frame or similar data structure. Exiting...")

    # 1.4: are the columns named in the formula present in the data?
    vars <- all.vars(formula) # extract variables in cbind and with | alike
    if (!(all(vars %in% names(data)))) 
        stop("ANOFA::error(14): variables in `formula` are not all in `data`. Exiting...")

    ##############################################################################
    # STEP 2: Harmonize the data format
    ##############################################################################
    # 2.1: Keep only the columns named
    data <- data[, names(data) %in% vars]

    # 2.2: Convert data to compiled format based on the formula
    if (is.one.sided( formula ) ) {
        # if no left-side terms: must be wide or raw formats
        if (has.cbind.terms( formula )) { # | : list all the variables
            # if there are cbind in rhs, must be raw format
            freq <- "Frequency"
            internalData <- rtoc(data, freq, formula, factors )
        } else {
            # otherwise, must be wide format
            freq  <- "Frequency"
            internalData <- wtoc(data, freq )
        }
    } else {
        # if left-side term: must be compiled or long formats
        if (has.nested.terms( formula )) {
            # if    | in lhs: must be long format
            freq  <- "Frequency"
            internalData <- ltoc(data, freq, formula)
        } else {
            # if no | in lhs: must be compiled format: NOTHING TO DO!
            freq <- formula[[2]]
            internalData <- data
        }
    }

    # 2.3: Keep the factor names
    fact <- names(internalData)[ ! names(internalData) == freq]
    if( (length(fact)>4) || (length(fact) < 1))
        stop("ANOFA::error(15): Too many factors. Exiting...")


    ##############################################################################
    # STEP 3: run the analysis, depending on the number of factors
    ##############################################################################
    # 3.1: to avoid integer overflow, convert frequencies to num
    internalData[[freq]] = as.numeric( internalData[[freq]])

    # 3.2: perform the analysis based on the number of factors
    analysis <- switch( length(fact),
        anofa1way(internalData, freq, fact),
        anofa2way(internalData, freq, fact),
        anofa3way(internalData, freq, fact),
        anofa4way(internalData, freq, fact),
    )
    

    ##############################################################################
    # STEP 4: return the object
    ##############################################################################
    # 4.1:  preserve everything in an object of class ANOFAobject
    res <- list(
        type          = "ANOFAomnibus",
        formula       = as.formula(formula),
        compiledData  = internalData,
        freqColumn    = freq,
        factColumns   = fact,
        nlevels       = unlist(analysis[[1]]),
        clevels       = analysis[[2]],
        results       = analysis[[3]]
    )
    class(res) <- c("ANOFAobject", class(res) )
    return( res )
   
}


##############################################################################
# Subfunctions
##############################################################################


anofa1way <- function(dataC, freq, factor) {
    # First, we determine the number of levels for the factor
    A <- length(unique(dataC[[factor]]))
    clevels <- list(as.character(unique(dataC[[factor]])))
    
    # Second, we compute the cell frequencies ni
    nd <- sum(dataC[[freq]])
    ni <- unlist(dataC[[freq]])
    
    # Third, we computed the expected frequencies e
    ei <- nd * rep(1/A, A)

    # Fourth, get the G statistics
    Gtotal <- 2 * sum(sum(ni * (log(ni)- log(ei))))

    # Fifth, the correction factor (Williams, 1976) 
    ctotal <- 1+ (A^2-1)/ ( 6 * (A-1) * nd)

    # Finally, getting the p-values for each corrected effect
    ptotal = 1- pchisq(Gtotal/ ctotal, df = A-1)

    results <- data.frame(
        G          = Gtotal,
        df         = A-1,
        Gcorrected = Gtotal/ ctotal,
        pvalue     = ptotal,
        etasq      = anofaES(ni / nd)
    )
    rownames(results) <-  factor
    
    return( list( list(A), clevels, results ))

}


anofa2way <- function(dataC, freq, factors ) {
    # First, we determine the number of levels for each factors
    A <- length(unique(dataC[[factors[1]]]))
    B <- length(unique(dataC[[factors[2]]]))

    # Second, we compute the marginal frequencies n
    ndd <- sum(dataC[[freq]])
    nid <- aggregate(as.formula(paste(freq, factors[1], sep="~")), 
        data = dataC, sum)[[freq]]
    ndj <- aggregate(as.formula(paste(freq, factors[2], sep="~")), 
        data = dataC, sum)[[freq]]

    # ... and the observed frequencies in a matrix
#    nij <- matrix(rep(0, A*B), ncol = B)
#    for (i in 1:A) {
#        for (j in 1:B) {
#            nij[i,j] = dataC[dataC[[factors[1]]]==unique(dataC[[factors[1]]])[i] & dataC[[factors[2]]]==unique(dataC[[factors[2]]])[j],][[freq]]
#        }
#    }
#print(nij)
#print("Ss")
#print(paste(freq, paste(factors, collapse="+"),sep="~"))
    temp    <- aggregate(as.formula(paste(freq, paste(factors, collapse="+"),sep="~")), data = dataC, sum)
#print(temp)
    nij     <- matrix(temp[[freq]], A)
#print(nij)
    clevels <- list(as.character(unique(temp[[factors[[1]]]])), 
                    as.character(unique(temp[[factors[[2]]]])))
#print(clevels)

    # Third, we computed the expected frequencies e
    edd <- ndd / (A * B)
    eid <- ndd / A
    edj <- ndd / B
    eij <- outer(nid, ndj)/ndd

#print(dim(nij))
#print(dim(eij))

    # Fourth, get the G statistics
    Gtotal <- 2 * sum(sum(nij * (log(nij)- log(edd))))
    Grow   <- 2 * sum(sum(nid * (log(nid)- log(eid))))
    Gcol   <- 2 * sum(sum(ndj * (log(ndj)- log(edj))))
    Grxc   <- 2 * sum(sum(nij * (log(nij)- log(eij))))

    # Fifth, the correction factor (Williams, 1976) for each factor
    crow <- 1+ (A^2-1)/ ( 6 * (A-1) * ndd)
    ccol <- 1+ (B^2-1)/ ( 6 * (B-1) * ndd)
    crxc <- 1+ ((A*B)^2-1)/ ( 6 * (A-1) * (B-1) * ndd)

    # Finally, getting the p-values for each corrected effect
    prow = 1- pchisq(Grow/ crow, df = A-1)
    pcol = 1- pchisq(Gcol/ ccol, df = B-1)
    prxc = 1- pchisq(Grxc/ crxc, df = (A-1)*(B-1))

    # assembling the results in a table
    results <- data.frame(
        G          = c(Gtotal, Grow, Gcol, Grxc),
        df         = c(A*B-1, A-1, B-1, (A-1)*(B-1)),
        Gcorrected = c(NA, Grow/ crow, Gcol/ ccol, Grxc/ crxc),
        pvalue     = c(NA, prow, pcol, prxc),
        etasq      = c(NA, anofaES(nid/ndd), anofaES(ndj/ndd), anofaES(nij/ndd) )
    )
    rownames(results) <-c("Total", factors, paste(factors, collapse=":"))
    
    return( list( list(A, B), clevels, results ))

}


anofa3way <- function(dataC, freq, factors ) {
    # First, we determine the number of levels for each factors
    A <- length(unique(dataC[[factors[1]]]))
    B <- length(unique(dataC[[factors[2]]]))
    C <- length(unique(dataC[[factors[3]]]))

    # Second, we compute the grand total and the marginal frequencies in n variables
    # The factors are noted with the letters i, j,  k and l
    # In this notation, "d" denotes the dot.
    nddd <- sum(dataC[[freq]])

    # first-order marginals
    nidd <- aggregate(as.formula(paste(freq, factors[1], sep="~")), 
        data = dataC, sum)[[freq]]
    ndjd <- aggregate(as.formula(paste(freq, factors[2], sep="~")),
        data = dataC, sum)[[freq]]
    nddk <- aggregate(as.formula(paste(freq, factors[3], sep="~")), 
        data = dataC, sum)[[freq]]
    # second-order marginals
    by2 <-lapply(combn(factors,2,simplify=FALSE), function(x) paste(x, collapse=" + "))
    nijd <- matrix(aggregate(as.formula(paste(freq, by2[[1]],sep="~")), 
        data = dataC, sum)[[freq]], A)
    nidk <- matrix(aggregate(as.formula(paste(freq, by2[[2]],sep="~")), 
        data = dataC, sum)[[freq]], A)
    ndjk <- matrix(aggregate(as.formula(paste(freq, by2[[3]],sep="~")), 
        data = dataC, sum)[[freq]], B)

    # and finally the observed frequencies in a matrix
    nijk <- array( (temp <- aggregate(as.formula(paste(freq, paste(factors, collapse="*"),sep="~")),
        data = dataC, sum))[[freq]],c(A,B,C))

    # we preserve the label names in the order of the aggregate (which tend to change ordering...)
    clevels <- list(as.character(unique(temp[[factors[[1]]]])), 
                as.character(unique(temp[[factors[[2]]]])), 
                as.character(unique(temp[[factors[[3]]]])))

    # Third, we computed the expected frequencies e
    eddd <- nddd / (A * B * C)
    # first-order expectation
    eidd <- rep(nddd / A, A)
    edjd <- rep(nddd / B, B)
    eddk <- rep(nddd / C, C)

    # second-order expectation
    eijd <- outer(nidd, ndjd)/nddd
    eidk <- outer(nidd, nddk)/nddd
    edjk <- outer(ndjd, nddk)/nddd
    # third-order expectation (there might be a matrix operation for that?)
    eijk <- array(NA, c(A,B,C))
    for(i in 1:A) for(j in 1:B) for(k in 1:C) {
        eijk[i,j,k] <- nddd * ( nijd[i,j] * nidk[i,k] * ndjk[j,k] 
                                 ) / (
                                  nidd[i] * ndjd[j] * nddk[k]  
                                 )
    }

    # Fourth, get the G statistics
    Gtotal <- 2 * sum(sum(sum(sum(nijk * (log(nijk)- log(eddd))))))
    # for the main effects
    Gidd   <- 2 * sum(nidd * (log(nidd)- log(eidd)))
    Gdjd   <- 2 * sum(ndjd * (log(ndjd)- log(edjd)))
    Gddk   <- 2 * sum(nddk * (log(nddk)- log(eddk)))
    # for the 2-way interactions
    Gijd   <- 2 * sum(nijd * (log(nijd)- log(eijd)))
    Gidk   <- 2 * sum(nidk * (log(nidk)- log(eidk)))
    Gdjk   <- 2 * sum(ndjk * (log(ndjk)- log(edjk)))
    # for the 3-way interactions
    Gijk   <- 2 * sum(nijk * (log(nijk)- log(eijk)))

    # Fifth, the correction factor (Williams, 1976) for each factor
    c1    <- 1+ ((A)^2-1)/ ( 6 * (A-1) * nddd)
    c2    <- 1+ ((B)^2-1)/ ( 6 * (B-1) * nddd)
    c3    <- 1+ ((C)^2-1)/ ( 6 * (C-1) * nddd)
    c12   <- 1+ ((A*B)^2-1)/ ( 6 * (A-1) * (B-1) * nddd)
    c13   <- 1+ ((A*C)^2-1)/ ( 6 * (A-1) * (C-1) * nddd)
    c23   <- 1+ ((B*C)^2-1)/ ( 6 * (B-1) * (C-1) * nddd)
    c123  <- 1+ ((A*B*C)^2-1)/ ( 6 * (A-1) * (B-1) * (C-1) * nddd)

    # Finally, getting the p-values for each corrected effect
    pidd = 1- pchisq(Gidd/ c1, df = A-1)
    pdjd = 1- pchisq(Gdjd/ c2, df = B-1)
    pddk = 1- pchisq(Gddk/ c3, df = C-1)

    pijd = 1- pchisq(Gijd/ c12, df = (A-1)*(B-1) )
    pidk = 1- pchisq(Gidk/ c13, df = (A-1)*(C-1) )
    pdjk = 1- pchisq(Gdjk/ c23, df = (B-1)*(C-1) )

    pijk = 1- pchisq(Gijk/ c123, df = (A-1)*(B-1)*(C-1))

    # assembling the results in a table
    results <- data.frame(
        G      = c(Gtotal, 
                    Gidd, Gdjd, Gddk, 
                    Gijd, Gidk, Gdjk, 
                    Gijk),
        df     = c(A*B*C-1, 
                    A-1, B-1, C-1,
                    (A-1)*(B-1), (A-1)*(C-1), (B-1)*(C-1),
                    (A-1)*(B-1)*(C-1)  ),
        Gcorrected = c(NA, 
                    Gidd/c1, Gdjd/c2, Gddk/c3, 
                    Gijd/c12, Gidk/c13, Gdjk/c23,
                    Gijk/c123 ),
        pvalue = c( NA, 
                    pidd, pdjd, pddk, 
                    pijd, pidk, pdjk,
                    pijk),
        etasq  = c( NA, 
                    anofaES(nidd/nddd), anofaES(ndjd/nddd), anofaES(nddk/nddd), 
                    anofaES(nijd/nddd), anofaES(nidk/nddd), anofaES(ndjk/nddd),
                    anofaES(nijk/nddd)                    
                    )
    )
    
    by2 <-lapply(combn(factors,2,simplify=FALSE), function(x) paste(x, collapse=":"))
    rownames(results) <-c("Total", factors, 
            by2, paste(factors, collapse=":"))

    return( list( list(A, B, C), clevels, results ))

}

anofa4way <- function(dataC, freq, factors ) {
    # First, we determine the number of levels for each factors
    A <- length(unique(dataC[[factors[1]]]))
    B <- length(unique(dataC[[factors[2]]]))
    C <- length(unique(dataC[[factors[3]]]))
    D <- length(unique(dataC[[factors[4]]]))

    # Second, we compute the grand total and the marginal frequencies in n variables
    # The factors are noted with the letters i, j,  k and l
    # In this notation, "d" denotes the dot.
    ndddd <- sum(dataC[[freq]])

    # first-order marginals
    niddd <- aggregate(as.formula(paste(freq, factors[1], sep="~")), 
        data = dataC, sum)[[freq]]
    ndjdd <- aggregate(as.formula(paste(freq, factors[2], sep="~")),
        data = dataC, sum)[[freq]]
    nddkd <- aggregate(as.formula(paste(freq, factors[3], sep="~")), 
        data = dataC, sum)[[freq]]
    ndddl <- aggregate(as.formula(paste(freq, factors[4], sep="~")), 
        data = dataC, sum)[[freq]]

    # second-order marginals
    by2 <-lapply(combn(factors,2,simplify=FALSE), function(x) paste(x, collapse=" + "))
    nijdd <- matrix(aggregate(as.formula(paste(freq, by2[[1]],sep="~")), 
        data = dataC, sum)[[freq]], A)
    nidkd <- matrix(aggregate(as.formula(paste(freq, by2[[2]],sep="~")), 
        data = dataC, sum)[[freq]], A)
    niddl <- matrix(aggregate(as.formula(paste(freq, by2[[3]],sep="~")), 
        data = dataC, sum)[[freq]], A)
    ndjkd <- matrix(aggregate(as.formula(paste(freq, by2[[4]],sep="~")), 
        data = dataC, sum)[[freq]], B)
    ndjdl <- matrix(aggregate(as.formula(paste(freq, by2[[5]],sep="~")), 
        data = dataC, sum)[[freq]], B)
    nddkl <- matrix(aggregate(as.formula(paste(freq, by2[[6]],sep="~")), 
        data = dataC, sum)[[freq]], C)

    # third-order marginals
    by3 <-lapply(combn(factors,3,simplify=FALSE), function(x) paste(x, collapse=" + "))

    nijkd <- array(aggregate(as.formula(paste(freq, by3[[1]],sep="~")), 
        data = dataC, sum)[[freq]], c(A,B,C))
    nijdl <- array(aggregate(as.formula(paste(freq, by3[[2]],sep="~")), 
        data = dataC, sum)[[freq]], c(A,B,D))
    nidkl <- array(aggregate(as.formula(paste(freq, by3[[3]],sep="~")), 
        data = dataC, sum)[[freq]], c(A,C,D))
    ndjkl <- array(aggregate(as.formula(paste(freq, by3[[4]],sep="~")), 
        data = dataC, sum)[[freq]], c(B,C,D))

    # and finally the observed frequencies in a matrix
    nijkl <- array((temp<-aggregate(as.formula(paste(freq, paste(factors, collapse="*"),sep="~")),
        data = dataC, sum))[[freq]],c(A,B,C,D))

    clevels <- list(as.character(unique(temp[[factors[[1]]]])), 
                    as.character(unique(temp[[factors[[2]]]])),
                    as.character(unique(temp[[factors[[3]]]])), 
                    as.character(unique(temp[[factors[[4]]]])) )

    # Third, we computed the expected frequencies e
    edddd <- ndddd / (A * B * C * D)
    # first-order expectation
    eiddd <- rep(ndddd / A, A)
    edjdd <- rep(ndddd / B, B)
    eddkd <- rep(ndddd / C, C)
    edddl <- rep(ndddd / D, D)
    # second-order expectation
    eijdd <- outer(niddd, ndjdd)/ndddd
    eidkd <- outer(niddd, nddkd)/ndddd
    eiddl <- outer(niddd, ndddl)/ndddd
    edjkd <- outer(ndjdd, nddkd)/ndddd
    edjdl <- outer(ndjdd, ndddl)/ndddd
    eddkl <- outer(nddkd, ndddl)/ndddd
    # third-order expectation (there might be a matrix operation for that?)
    eijkd <- array(NA, c(A,B,C))
    for(i in 1:A) for(j in 1:B) for(k in 1:C) {
        eijkd[i,j,k] <- ndddd * nijdd[i,j] * nidkd[i,k] * ndjkd[j,k] /(niddd[i] * ndjdd[j] * nddkd[k] )
    } 
    eijdl <- array(NA, c(A,B,D))
    for(i in 1:A) for(j in 1:B) for(l in 1:D) {
        eijdl[i,j,l] <- ndddd * nijdd[i,j] * niddl[i,l] * ndjdl[j,l] /(niddd[i] * ndjdd[j] * ndddl[l] )
    }
    eidkl <- array(NA, c(A,C,D))
    for(i in 1:A) for(k in 1:C) for(l in 1:D) {
        eidkl[i,k,l] <- ndddd * nidkd[i,k] * niddl[i,l] * nddkl[k,l] /(niddd[i] * nddkd[k] * ndddl[l] )
    }
    edjkl <- array(NA, c(B,C,D))
    for(j in 1:B) for(k in 1:C) for(l in 1:D) {
        edjkl[j,k,l] <- ndddd * ndjkd[j,k] * ndjdl[j,l] * nddkl[k,l] /(ndjdd[j] * nddkd[k] * ndddl[l] )
    }
    # forth-order expectation (there might be a matrix operation for that?)
    eijkl <- array(NA, c(A,B,C,D))
    for(i in 1:A) for(j in 1:B) for(k in 1:C) for(l in 1:D) {
        eijkl[i,j,k,l] <- 1/ndddd * ( niddd[i] * ndjdd[j] * nddkd[k] * ndddl[l] * 
                                      nijkd[i,j,k] * nijdl[i,j,l] * nidkl[i,k,l] * ndjkl[j,k,l] 
                                    ) / (
                                      nijdd[i,j] * nidkd[i,k] * niddl[i,l] * ndjkd[j,k] * ndjdl[j,l] * nddkl[k,l]
                                    )
    }

    # Fourth, get the G statistics
    Gtotal <- 2 * sum(sum(sum(sum(nijkl * (log(nijkl)- log(edddd))))))
    # for the main effects
    Giddd   <- 2 * sum(niddd * (log(niddd)- log(eiddd)))
    Gdjdd   <- 2 * sum(ndjdd * (log(ndjdd)- log(edjdd)))
    Gddkd   <- 2 * sum(nddkd * (log(nddkd)- log(eddkd)))
    Gdddl   <- 2 * sum(ndddl * (log(ndddl)- log(edddl)))
    # for the 2-way interactions
    Gijdd   <- 2 * sum(nijdd * (log(nijdd)- log(eijdd)))
    Gidkd   <- 2 * sum(nidkd * (log(nidkd)- log(eidkd)))
    Giddl   <- 2 * sum(niddl * (log(niddl)- log(eiddl)))
    Gdjkd   <- 2 * sum(ndjkd * (log(ndjkd)- log(edjkd)))
    Gdjdl   <- 2 * sum(ndjdl * (log(ndjdl)- log(edjdl)))
    Gddkl   <- 2 * sum(nddkl * (log(nddkl)- log(eddkl)))
    # for the 3-way interactions
    Gijkd   <- 2 * sum(nijkd * (log(nijkd)- log(eijkd)))
    Gijdl   <- 2 * sum(nijdl * (log(nijdl)- log(eijdl)))
    Gidkl   <- 2 * sum(nidkl * (log(nidkl)- log(eidkl)))
    Gdjkl   <- 2 * sum(ndjkl * (log(ndjkl)- log(edjkl)))
    # and the 4-way interaction
    Gijkl   <- 2 * sum(nijkl * (log(nijkl)- log(eijkl)))

    # Fifth, the correction factor (Williams, 1976) for each factor
    c1    <- 1+ ((A)^2-1)/ ( 6 * (A-1) * ndddd)
    c2    <- 1+ ((B)^2-1)/ ( 6 * (B-1) * ndddd)
    c3    <- 1+ ((C)^2-1)/ ( 6 * (C-1) * ndddd)
    c4    <- 1+ ((D)^2-1)/ ( 6 * (D-1) * ndddd)
    c12   <- 1+ ((A*B)^2-1)/ ( 6 * (A-1) * (B-1) * ndddd)
    c13   <- 1+ ((A*C)^2-1)/ ( 6 * (A-1) * (C-1) * ndddd)
    c14   <- 1+ ((A*D)^2-1)/ ( 6 * (A-1) * (D-1) * ndddd)
    c23   <- 1+ ((B*C)^2-1)/ ( 6 * (B-1) * (C-1) * ndddd)
    c24   <- 1+ ((B*D)^2-1)/ ( 6 * (B-1) * (D-1) * ndddd)
    c34   <- 1+ ((C*D)^2-1)/ ( 6 * (C-1) * (D-1) * ndddd)
    c123  <- 1+ ((A*B*C)^2-1)/ ( 6 * (A-1) * (B-1) * (C-1) * ndddd)
    c124  <- 1+ ((A*B*D)^2-1)/ ( 6 * (A-1) * (B-1) * (D-1) * ndddd)
    c134  <- 1+ ((A*C*D)^2-1)/ ( 6 * (A-1) * (C-1) * (D-1) * ndddd)
    c234  <- 1+ ((B*C*D)^2-1)/ ( 6 * (B-1) * (C-1) * (D-1) * ndddd)
    c1234 <- 1+ ((A*B*C*D)^2-1)/ ( 6 * (A-1) * (B-1) * (C-1) * (D-1) * ndddd)

    # Finally, getting the p-values for each corrected effect
    piddd = 1- pchisq(Giddd/ c1, df = A-1)
    pdjdd = 1- pchisq(Gdjdd/ c2, df = B-1)
    pddkd = 1- pchisq(Gddkd/ c3, df = C-1)
    pdddl = 1- pchisq(Gdddl/ c4, df = D-1)

    pijdd = 1- pchisq(Gijdd/ c12, df = (A-1)*(B-1) )
    pidkd = 1- pchisq(Gidkd/ c13, df = (A-1)*(C-1) )
    piddl = 1- pchisq(Giddl/ c14, df = (A-1)*(D-1) )
    pdjkd = 1- pchisq(Gdjkd/ c23, df = (B-1)*(C-1) )
    pdjdl = 1- pchisq(Gdjdl/ c24, df = (B-1)*(D-1) )
    pddkl = 1- pchisq(Gddkl/ c34, df = (C-1)*(D-1) )

    pijkd = 1- pchisq(Gijkd/ c123, df = (A-1)*(B-1)*(C-1))
    pijdl = 1- pchisq(Gijdl/ c124, df = (A-1)*(B-1)*(D-1))
    pidkl = 1- pchisq(Gidkl/ c134, df = (A-1)*(C-1)*(D-1))
    pdjkl = 1- pchisq(Gdjkl/ c234, df = (B-1)*(C-1)*(D-1))

    pijkl = 1- pchisq(Gijkl/ c1234, df = (A-1)*(B-1)*(C-1)*(D-1) )

    # assembling the results in a table
    results <- data.frame(
        G      = c(Gtotal, 
                    Giddd, Gdjdd, Gddkd, Gdddl,
                    Gijdd, Gidkd, Giddl, Gdjkd, Gdjdl, Gddkl,
                    Gijkd, Gijdl, Gidkl, Gdjkl, 
                    Gijkl),
        df     = c(A*B*C*D-1, 
                    A-1, B-1, C-1, D-1,
                    (A-1)*(B-1), (A-1)*(C-1), (A-1)*(D-1),(B-1)*(C-1),(B-1)*(D-1),(C-1)*(D-1),
                    (A-1)*(B-1)*(C-1), (A-1)*(B-1)*(D-1), (A-1)*(C-1)*(D-1),(B-1)*(C-1)*(D-1),
                    (A-1)*(B-1)*(C-1)*(D-1) ),
        Gcorrected = c(NA, 
                    Giddd/c1, Gdjdd/c2, Gddkd/c3, Gdddl/c4,
                    Gijdd/c12, Gidkd/c13, Giddl/c14, Gdjkd/c23, Gdjdl/c24, Gddkl/c34,
                    Gijkd/c123, Gijdl/c124, Gidkl/c124, Gdjkl/c234,
                    Gijkl/c1234),
        pvalue = c(NA, 
                    piddd, pdjdd, pddkd, pdddl,
                    pijdd, pidkd, piddl, pdjkd, pdjdl, pddkl,
                    pijkd, pijdl, pidkl, pdjkl, 
                    pijkl),
        etasq  = c(NA, 
                    anofaES(niddd/ndddd), anofaES(ndjdd/ndddd), anofaES(nddkd/ndddd), anofaES(ndddl/ndddd), 
                    anofaES(nijdd/ndddd), anofaES(nidkd/ndddd), anofaES(niddl/ndddd), 
                    anofaES(ndjkd/ndddd), anofaES(ndjdl/ndddd), anofaES(nddkl/ndddd), 
                    anofaES(nijkd/ndddd), anofaES(nijdl/ndddd), anofaES(nidkl/ndddd), anofaES(ndjkl/ndddd), 
                    anofaES(nijkl/ndddd)    
                    )
    )
    
    by2 <-lapply(combn(factors,2,simplify=FALSE), function(x) paste(x, collapse=":"))
    by3 <-lapply(combn(factors,3,simplify=FALSE), function(x) paste(x, collapse=":"))
    rownames(results) <-c("Total", factors, 
            by2, by3, paste(factors, collapse=":"))

    return( list( list(A, B, C, D), clevels, results ))

}


