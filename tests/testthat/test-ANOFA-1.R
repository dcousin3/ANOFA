context("ANOFA:: Testing main function (4 sections)")
    # expect_output( str(res), "data.frame")
    # expect_equal( "ggplot" %in% class(plt), TRUE)
    # expect_message( p <- superbPlot(dta2a, etc ))
    # expect_error(), expect_warning(), expect_condition( , class = "")

test_that("Testing the input formats (1/4)", {

    formulaW <- ~ Intensity * Pitch                         # wide
    formulaR <- ~ cbind(Low,Medium,High) * cbind(Soft, Hard)# raw factor names guessed
    formulaC <- Frequency ~ Intensity * Pitch               # compiled
    formulaL <- Level ~ Factor | Id                         # long
    # minimalExample has the following
    nfactors = 2
    nsubject = 20
    nlevel1  = 3
    nlevel2  = 2
    nlevels  = nlevel1 * nlevel2
    sumlevels  = nlevel1 + nlevel2
    
    w1 <- anofa( formulaC, minimalExample )
    expect_s3_class(w1, "ANOFAobject")
    
    tt <- toWide(w1)
    expect_s3_class(tt, "data.frame")
    expect_equal( dim(tt), c( nsubject, nfactors))

    w2 <- anofa( formulaW, tt )
    expect_s3_class(w2, "ANOFAobject")
    expect_equal( dim(w2$compiledData), c(nlevels, nfactors +1 ) )

    tt <- toLong(w1)
    expect_s3_class(tt, "data.frame")
    expect_equal( dim(tt), c(nfactors * nsubject,3 ))

    w3 <- anofa( formulaL, tt )
    expect_s3_class(w3, "ANOFAobject")
    expect_equal( dim(w3$compiledData), c(nlevels, nfactors +1 ) )

    tt <- toRaw(w1)
    expect_s3_class(tt, "data.frame")
    expect_equal( dim(tt), c( nsubject, sumlevels))

    w4 <- anofa( formulaR, tt )
    expect_s3_class(w4, "ANOFAobject")
    expect_equal( dim(w4$compiledData), c(nlevels, nfactors +1 ) )

    w5 <- anofa( formulaR, tt, factors =c("pressure","texture") )
    expect_s3_class(w5, "ANOFAobject")
    expect_equal( dim(w5$compiledData), c(nlevels, nfactors +1 ) )

})

test_that("Testing errors (2/4)", {

    formulaC <- Frequency ~ Intensity * Pitch               # compiled
    formulaW <- ~ Intensity * Pitch                         # wide
    formulaR <- ~ cbind(Low,Medium,High) * cbind(Soft, Hard)# raw factor names guessed
    formulaL <- Level ~ Factor | Id                         # long

    # error(1) not a formula
    expect_error( anofa(23, minimalExample) )
    # error(2) more than one vd
    expect_error( anofa(a* b ~ Intensity * Pitch , minimalExample) )
    # error(3) not a dataset
    expect_error( anofa( formulaC, 2 ) )
    # error(4) too many columns named
    expect_error( anofa(Frequency ~ Intensity * Pitch * a, minimalExample) )

    # wrong formulas
    expect_error( anofa(formulaR, minimalExample) )
    expect_error( anofa(formulaL, minimalExample) )

})

test_that("Testing paper's examples (3/4)", {
    # first example of the paper
    res <- anofa( obsfreq ~ provider * program, LandisBarrettGalvin2013)
    expect_equal( res$results[1,1], 533.1874, tolerance = 0.0001)
    expect_equal( res$results[4,4],   0.0166, tolerance = 0.0001)

    # second example of the paper
    res <-anofa( obsfreq ~ vocation * gender, LightMargolin1971)
    expect_equal( res$results[1,1], 266.8894, tolerance = 0.0001)
    expect_equal( res$results[3,4],   0.1589, tolerance = 0.0001)

    # third example of the paper 
# Removed because too slow
#    dta <- data.frame(Detergent)
#    res <- anofa( Freq ~ Temperature * M_User * Preference * Water_softness, dta )
#    expect_equal( res$results[1,1], 118.6269, tolerance = 0.0001)
#    expect_equal( res$results[4,4],   0.8011, tolerance = 0.0001)

})

test_that("Testing artificial data sets (4/4)", {

    # a case with one factor
    set.seed(42)
    dta <- GRF( list(A=c("a1","a2","a3")),100)
    res <- anofa( Freq ~ A, dta)
    expect_equal( res$results[1,1], 2.9836, tolerance = 0.0001)

    # a case with two factors
    set.seed(42)
    dta <- GRF( list(A=c("a1","a2","a3"), B=c("b1","b2")),100)
    res <- anofa( Freq ~ A * B, dta)
    expect_equal( res$results[1,1], 5.4671, tolerance = 0.0001)
    expect_equal( sum(res$results[-1,1]), res$results[1,1]) # total G = sum of the effets' Gs

    # a case with three factors
    set.seed(42)
    dta <- GRF( list(A=c("a1","a2","a3"), B=c("b1","b2"), C=c("c1","c2")),100)
    res <- anofa( Freq ~ A * B * C, dta)
    expect_equal( res$results[1,1], 6.2404, tolerance = 0.0001)
    expect_equal( sum(res$results[-1,1]), res$results[1,1]) # total G = sum of the effets' Gs

    # a case with four factors
    set.seed(42)
    dta <- GRF( list(A=c("a1","a2","a3"), B=c("b1","b2"), C=c("c1","c2"), D=c("d1","d2")),200)
    res <- anofa( Freq ~ A * B * C * D, dta)
    expect_equal( res$results[1,1], 26.9939, tolerance = 0.0001)
    expect_equal( sum(res$results[-1,1]), res$results[1,1]) # total G = sum of the effets' Gs

})
