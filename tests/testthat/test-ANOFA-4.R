context("ANOFA:: Testing contrastFrequencies function")
    # expect_output( str(res), "data.frame")
    # expect_equal( "ggplot" %in% class(plt), TRUE)
    # expect_message( p <- superbPlot(dta2a, etc ))
    # expect_error(), expect_warning(), expect_condition( , class = "")


test_that("TESTS of contrastFrequencies function(1/2)", {

    w <- anofa(Frequency ~ Intensity * Pitch, minimalExample) 
    c <- contrastFrequencies(w, 
        list(c1=c(1,-1,0,0,0,0)/1, 
             c2=c(1,1,-2,0,0,0)/2,
             c3=c(1,1,1,-3,0,0)/3,
             c4=c(1,1,1,1,-4,0)/4,
             c5=c(1,1,1,1,1,-5)/5
        )
    )
    expect_equal(sum(c$results[,1]), w$results[1,1], tolerance = 0.0001)

    e <- emFrequencies(w, ~ Intensity | Pitch)
    f <- contrastFrequencies(e, list(c1=c(1,1,-2)/2, c2=c(1,-1,0)))
    expect_equal(sum(f$results[,1]), w$results[1,1], tolerance = 0.0001)

    # not an anofa object (error 31)
    expect_error( contrastFrequencies(22, list(c1=c(1,1,-2)/1, c2=c(1,-1,0))) )
    # contrast unequal length (error 32)
    expect_error( contrastFrequencies(w, list(c1=c(1,1,-2)/1, c2=c(1,-1))) )
    # contrast length does not match design (error 33)
    expect_error( contrastFrequencies(w, list(c1=c(1,-1)/1, c2=c(1,-1))) )
    # too many contrasts (error 34)
    expect_error( contrastFrequencies(e, list(c1=c(1,1,-2)/1, c2=c(1,-1,0), c3=c(0,1,01))) ) 
    # cross product does not sum to 1 (error 35)
    expect_error( contrastFrequencies(e, list(c1=c(1,1,-2)/2, c2=c(1,0,-1))) ) 
    # amplitude not 1 (error 36)
    expect_error( contrastFrequencies(e, list(c1=c(1,1,-2)/1, c2=c(1,-1,0))) ) 

})

test_that("TESTS of contrastFrequencies function (2/2)", {

    ##########################################################
    # Testing the dataset's example
    ##########################################################

    #### LANDIS ET AL., 2013 ####
    L <- anofa( obsfreq ~ provider * program, LandisBarrettGalvin2013)
    c <- contrastFrequencies(L, list(
        c1=c(1,-01,0,0,0,0,0,0,0,0,0,0,0,0,0)/1, 
        c2=c(1,1,-02,0,0,0,0,0,0,0,0,0,0,0,0)/2, 
        c3=c(1,1,1,-03,0,0,0,0,0,0,0,0,0,0,0)/3, 
        c4=c(1,1,1,1,-04,0,0,0,0,0,0,0,0,0,0)/4, 
        c5=c(1,1,1,1,1,-05,0,0,0,0,0,0,0,0,0)/5, 
        c6=c(1,1,1,1,1,1,-06,0,0,0,0,0,0,0,0)/6, 
        c7=c(1,1,1,1,1,1,1,-07,0,0,0,0,0,0,0)/7, 
        c8=c(1,1,1,1,1,1,1,1,-08,0,0,0,0,0,0)/8, 
        c9=c(1,1,1,1,1,1,1,1,1,-09,0,0,0,0,0)/9, 
        cA=c(1,1,1,1,1,1,1,1,1,1,-10,0,0,0,0)/10, 
        cB=c(1,1,1,1,1,1,1,1,1,1,1,-11,0,0,0)/11, 
        cC=c(1,1,1,1,1,1,1,1,1,1,1,1,-12,0,0)/12, 
        cD=c(1,1,1,1,1,1,1,1,1,1,1,1,1,-13,0)/13, 
        cE=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,-14)/14
        ))
    expect_equal(sum(c$results[,1]), L$results[1,1], tolerance = 0.0001)

    e <- emFrequencies(L, ~ program | provider)
    f <- contrastFrequencies(e, list(
            "(PBH & CBH) vs. BM"=c(1,1,-2)/2, 
            "PBH vs. CBH"=c(1,-1,0))
        )
    expect_equal(f$results[1,1], 18.6215, tolerance = 0.0001)
    expect_equal(sum(f$results[,1]) + L$results[2,1], L$results[1,1], tolerance = 0.0001)


    #### LIGHT & MARGOLIN, 1971 ####
    L <- anofa( obsfreq ~ vocation * gender, LightMargolin1971)
    c <- contrastFrequencies(L, list(
        c1=c(1,-01,0,0,0,0,0,0,0,0)/1, 
        c2=c(1,1,-02,0,0,0,0,0,0,0)/2, 
        c3=c(1,1,1,-03,0,0,0,0,0,0)/3, 
        c4=c(1,1,1,1,-04,0,0,0,0,0)/4, 
        c5=c(1,1,1,1,1,-05,0,0,0,0)/5, 
        c6=c(1,1,1,1,1,1,-06,0,0,0)/6, 
        c7=c(1,1,1,1,1,1,1,-07,0,0)/7, 
        c8=c(1,1,1,1,1,1,1,1,-08,0)/8, 
        c9=c(1,1,1,1,1,1,1,1,1,-09)/9
        ))
    expect_equal(sum(c$results[,1]), L$results[1,1], tolerance = 0.0001)

    e <- emFrequencies(L, ~ vocation | gender )
    f <- contrastFrequencies(e, list(
            "teacher college vs. gymnasium"=c( 0, 0, 1,-1, 0),
            "vocational vs. university"   = c( 0, 1, 0, 0,-1),
            "another"                     = c( 0, 1,-1,-1,+1)/2,
            "to exhaust the df"           = c( 4,-1,-1,-1,-1)/4
            )
        )
    expect_equal(f$results[1,1], 0.8325, tolerance = 0.0001)
    expect_equal(sum(f$results[,1]) + L$results[3,1], L$results[1,1], tolerance = 0.0001)


    #### Les greffons GILLET, 1993 ####
    G <- anofa( Freq ~ species * location * florished, Gillet1993)
    e <- emFrequencies(G, ~ location | species * florished) 
    f <- contrastFrequencies(e, list(
            "order 1 vs. 2&3"    = c( 2,-1,-1)/2,
            "order 2 vs order 3" = c( 0, 1,-1)
            )
        )
    expect_equal(f$results[1,1], 91.7686, tolerance = 0.0001)
    expect_equal(sum(f$results[,1])+G$results[4,1]+G$results[2,1]+G$results[6,1], G$results[1,1], tolerance = 0.0001)


    #### Detergent RIES ET SMITH, 1963 ####
# Removed because Prof Ripley is not happy
#    dta <- data.frame(Detergent)
#    R <- anofa( Freq ~  Temperature * M_User * Preference * Water_softness, dta)
#    e <- emFrequencies(R, ~ Water_softness | Temperature ) 

#    f <- contrastFrequencies(e, list(
#            "soft vs. medium" = c( 1,-1, 0),
#            "both vs. hard"   = c( 1, 1,-2)/2
#            )
#        )
#    expect_equal(sum(f$results[,1]), sum(e$results[,1]), tolerance = 0.0001)
#    expect_equal(sum(f$results[,1]), sum(R$results[c(5,8),1]), tolerance = 0.0001)

})

test_that("TESTS of contrastFrequencies function (3/3)", {

    ##########################################################
    # Testing contrasts with random frequencies
    ##########################################################

    # ==============================================================
    # testing (2x3) design
    set.seed(42)
    dta <- GRF( list(A=c("a1","a2"), B=c("b1","b2","b3") ), 100,
        c(rep(1/10,5),1/2)    )    ## results in an interaction A:B
    w <- anofa( Freq ~ A * B, dta)

    # decomposition of B for each level of A
    e <- emFrequencies(w, ~ B | A )
    f <- contrastFrequencies(e, list(
            "a1 vs. a2"      = c( 1,-1, 0),
            "(a1&a2) vs. a3" = c( 1, 1,-2)/2  ))
    gA <- sum(w$results[c(3,4),1])   # B et B:A
    gB <- sum(e$results[,1])
    gC <- sum(f$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)
    expect_equal(gA, gC, tolerance = 0.0001)

    # ==============================================================
    # testing (2x3x4) design: B | A, B | A*C, A*B | C
    set.seed(42)
    dta <- GRF( list(A=c("a1","a2"), B=c("b1","b2","b3"), C=c("c1","c2","c3","c4") ), 100,
        c(rep(1/50,23),.54)    )    ## results in an interaction A:B
    w <- anofa( Freq ~ A * B * C, dta)

    # decomposition of B for each level of A
    e <- emFrequencies(w, ~ B | A )
    f <- contrastFrequencies(e, list(
            "a1 vs. a2"      = c( 1,-1, 0),
            "(a1&a2) vs. a3" = c( 1, 1,-2)/2  ))
    gA <- sum(w$results[c(3,5),1])   # B et B:A
    gB <- sum(e$results[,1])
    gC <- sum(f$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)
    expect_equal(gA, gC, tolerance = 0.0001)

    # decomposition of B for each level of A*C
    e <- emFrequencies(w, ~ B | A*C )
    f <- contrastFrequencies(e, list(
            "a1 vs. a2"      = c( 1,-1, 0),
            "(a1&a2) vs. a3" = c( 1, 1,-2)/2  ))
    gA <- sum(w$results[c(3,5,7,8),1])   # B, B:A, B:C, B:A:C
    gB <- sum(e$results[,1])
    gC <- sum(f$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)
    expect_equal(gA, gC, tolerance = 0.0001)

    # decomposition of A*B for each level of C
    e <- emFrequencies(w, ~ A*B | C )
    f <- contrastFrequencies(e, list(
            "c1" = c( 1,-1, 0, 0, 0, 0)/1,
            "c2" = c( 1, 1,-2, 0, 0, 0)/2,
            "c3" = c( 1, 1, 1,-3, 0, 0)/3,
            "c4" = c( 1, 1, 1, 1,-4, 0)/4,
            "c5" = c( 1, 1, 1, 1, 1,-5)/5
        ))
    gA <- sum(w$results[c(2,3,5,6,7,8),1])   # A, B, A:B, A:C, B:C, A:B:C
    gB <- sum(e$results[,1])
    gC <- sum(f$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)
    expect_equal(gA, gC, tolerance = 0.0001)


    # ==============================================================
    # testing (2x3x2x2) design: B | A, B | A*C, A*B | C
    set.seed(42)
    dta <- GRF( list(A=c("a1","a2"), B=c("b1","b2","b3"), C=c("c1","c2"), D=c("d1","d2") ), 200,
        c(rep(1/50,23),.54)    )    ## results in an interaction A:B
    w <- anofa( Freq ~ A * B * C * D, dta)

    # decomposition of B for each level of A
    e <- emFrequencies(w, ~ B | A )
    f <- contrastFrequencies(e, list(
            "a1 vs. a2"      = c( 1,-1, 0),
            "(a1&a2) vs. a3" = c( 1, 1,-2)/2  ))
    gA <- sum(w$results[c(3,6),1])   # B et B:A
    gB <- sum(e$results[,1])
    gC <- sum(f$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)
    expect_equal(gA, gC, tolerance = 0.0001)

    # decomposition of B for each level of A*C
    e <- emFrequencies(w, ~ B | A*C )
    f <- contrastFrequencies(e, list(
            "a1 vs. a2"      = c( 1,-1, 0),
            "(a1&a2) vs. a3" = c( 1, 1,-2)/2  ))
    gA <- sum(w$results[c(3,6,9,12),1])   # B, B:A, B:C, B:A:C
    gB <- sum(e$results[,1])
    gC <- sum(f$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)
    expect_equal(gA, gC, tolerance = 0.0001)

    # decomposition of A*B for each level of C
    e <- emFrequencies(w, ~ A*B | C )
    f <- contrastFrequencies(e, list(
            "c1" = c( 1,-1, 0, 0, 0, 0)/1,
            "c2" = c( 1, 1,-2, 0, 0, 0)/2,
            "c3" = c( 1, 1, 1,-3, 0, 0)/3,
            "c4" = c( 1, 1, 1, 1,-4, 0)/4,
            "c5" = c( 1, 1, 1, 1, 1,-5)/5
        ))
    gA <- sum(w$results[c(2,3,6,7,9,12),1])   # A, B, A:B, A:C, B:C, A:B:C
    gB <- sum(e$results[,1])
    gC <- sum(f$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)
    expect_equal(gB, gC, tolerance = 0.0001)

    # decomposition of A*B for each level of C*D
    e <- emFrequencies(w, ~ A*B | C*D )
    f <- contrastFrequencies(e, list(
            "c1" = c( 1,-1, 0, 0, 0, 0)/1,
            "c2" = c( 1, 1,-2, 0, 0, 0)/2,
            "c3" = c( 1, 1, 1,-3, 0, 0)/3,
            "c4" = c( 1, 1, 1, 1,-4, 0)/4,
            "c5" = c( 1, 1, 1, 1, 1,-5)/5
        ))
    gA <- sum(w$results[c(2,3,6,7,8,9,10,12,13,14,15,16),1])   # tout sauf C, D, C*D
    gB <- sum(e$results[,1])
    gC <- sum(f$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)
    expect_equal(gB, gC, tolerance = 0.0001)







})