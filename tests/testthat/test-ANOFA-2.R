context("ANOFA:: Testing emFrequencies function")
    # expect_output( str(res), "data.frame")
    # expect_equal( "ggplot" %in% class(plt), TRUE)
    # expect_message( p <- superbPlot(dta2a, etc ))
    # expect_error(), expect_warning(), expect_condition( , class = "")


test_that("TESTS of emFrequencies function (1/3)", {

    ##########################################################
    # Testing the errors
    ##########################################################

    w <- anofa(Frequency ~ Intensity * Pitch, minimalExample) 

    expect_error( emFrequencies(2, ~ A | B ))
    expect_error( emFrequencies(w, 2))
    expect_error( emFrequencies(w, ~ A * B ))
    expect_error( emFrequencies(w, ~ A | B + C | D ))
    expect_error( emFrequencies(w, AliBaba ~ A | A ))
    expect_error( emFrequencies(w, ~ A | B ))

})

test_that("TESTS of emFrequencies function (2/3)", {

    ##########################################################
    # Testing the dataset's example
    ##########################################################

    #### LANDIS ET AL., 2013 ####
    L <- anofa( obsfreq ~ provider * program, LandisBarrettGalvin2013)
    expect_equal(L$results[1,1], 533.18742, tolerance = 0.0001)
    expect_equal(L$results[4,1],  18.8455, tolerance = 0.0001)

    plt <- anofaPlot(L)
    expect_output( str(plt), "list")
    expect_equal( "ggplot" %in% class(plt), TRUE)
    
    e <- emFrequencies(L, ~ program | provider ) 
    expect_equal(e$results[1,1],  18.6486, tolerance = 0.0001)
    expect_equal(e$results[5,1], 137.0773, tolerance = 0.0001)

    expect_equal(L$results[3,1]+L$results[4,1], sum(e$results[,1]), tol = 0.0001)

    #### LIGHT & MARGOLIN, 1971 ####
    w <- anofa( obsfreq ~ vocation * gender, LightMargolin1971)
    expect_equal(w$results[1,1], 266.88945, tolerance = 0.0001)
    expect_equal(w$results[4,1],  49.88669, tolerance = 0.0001)

    plt <- anofaPlot(w)
    expect_output( str(plt), "list")
    expect_equal( "ggplot" %in% class(plt), TRUE)
    
    e <- emFrequencies(w, ~ gender | vocation ) 
    expect_equal(e$results[1,1],  0.0081, tolerance = 0.0001)
    expect_equal(e$results[5,1], 42.3478, tolerance = 0.0001)

    expect_equal(w$results[3,1]+w$results[4,1], sum(e$results[,1]), tol = 0.0001)

    #### Les greffons GILLET, 1993 ####
    w <- anofa( Freq ~ species * location * florished, Gillet1993)
    expect_equal(w$results[1,1], 515.9772, tolerance = 0.0001)
    expect_equal(w$results[4,1],  24.9552, tolerance = 0.0001)

    plt <- anofaPlot(w)
    expect_output( str(plt), "list")
    expect_equal( "ggplot" %in% class(plt), TRUE)

    e <- emFrequencies(w, ~ species | florished ) 
    expect_equal(e$results[1,1], 43.8481, tolerance = 0.0001)
    expect_equal(e$results[2,1], 33.7782, tolerance = 0.0001)

    expect_equal(w$results[2,1]+w$results[6,1], sum(e$results[,1]), tol = 0.0001)

    #### Detergent RIES ET SMITH, 1963 ####
    dta <- data.frame(Detergent)
    w <- anofa( Freq ~  Temperature * M_User * Preference * Water_softness, dta)
    expect_equal(w$results[5,1],  0.5015, tolerance = 0.0001)
    expect_equal(w$results[9,1], 20.5815, tolerance = 0.0001)

    plt <- anofaPlot(w)
    expect_output( str(plt), "list")
    expect_equal( "ggplot" %in% class(plt), TRUE)
    
    e <- emFrequencies(w, ~ M_User | Preference ) 
    expect_equal(e$results[1,1], 17.4943, tolerance = 0.0001)
    expect_equal(e$results[2,1],  5.0084, tolerance = 0.0001)

    expect_equal(w$results[3,1]+w$results[9,1], sum(e$results[,1]), tol = 0.0001)

})


test_that("TESTS of emFrequencies function (3/3)", {

    ##########################################################
    # Testing decompositions TWO FACTOR DESIGN
    ##########################################################

    # testing A|B and B|A of A*B (2x3) design
    set.seed(42)
    dta <- GRF( list(A=c("a1","a2"), B=c("b1","b2","b3") ), 100,
        c(rep(1/10,5),1/2)    )    ## results in an interaction A:B
    w <- anofa( Freq ~ A * B, dta)

    # decomposition of A for each level of B
    gA <- w$results[[2,1]] + w$results[[4,1]]    # A et A:B
    tt <- emFrequencies(w, ~ A | B )      
    gB <- sum(tt$results[,1]) #
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B for each level of A
    gA <- w$results[[3,1]] + w$results[[4,1]]    # B et A:B
    tt <- emFrequencies(w, ~ B | A )      
    gB <- sum(tt$results[,1]) #
    expect_equal(gA, gB, tolerance = 0.0001)


    ##########################################################
    # Testing decompositions THREE FACTOR DESIGN
    ##########################################################

    set.seed(42)
    dta <- GRF( list(A=c("a1","a2"), B=c("b1","b2","b3"), C=c("c1","c2","c3","c4")), 1000,
        c(rep(1/50,22),.28,.28)    )    ## results in an interaction B:C
    w <- anofa( Freq ~ A * B * C, dta)

    #··························································
    # 1 factor within 1 factor
    #··························································
    # testing A|B, A|C, B|C, B|A, C|A, C|B      of A*B*C design
    #··························································
    # decomposition of A | B
    gA <- w$results[[2,1]] + w$results[[5,1]]    # A et A:B
    tt <- emFrequencies(w, ~ A | B )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of A | C
    gA <- w$results[[2,1]] + w$results[[6,1]]    # A et A:C
    tt <- emFrequencies(w, ~ A | C )      
    gB <- sum(tt$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B | A
    gA <- w$results[[3,1]] + w$results[[5,1]]    # B et A:B
    tt <- emFrequencies(w, ~ B | A )      
    gB <- sum(tt$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B | C
    gA <- w$results[[3,1]] + w$results[[7,1]]    # B et B:C
    tt <- emFrequencies(w, ~ B | C )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of C | A
    gA <- w$results[[4,1]] + w$results[[6,1]]    # C et A:C
    tt <- emFrequencies(w, ~ C | A )      
    gB <- sum(tt$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of C | B
    gA <- w$results[[4,1]] + w$results[[7,1]]    # C et B:C
    tt <- emFrequencies(w, ~ C | B )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    #··························································
    # 2 factors within 1 factor
    #··························································
    # testing A*B|C, A*C|B, B*C|A               of A*B*C design
    #··························································
    # decomposition of A*B | C
    gA <- sum(w$results[c(2,3,5,6,7,8),1])    # A + B + AB + A:C + B:C + AB:C
    tt <- emFrequencies(w, ~ A * B | C )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of A*C | B
    gA <- sum(w$results[c(2,4,6,5,7,8),1])    # A + C + AC + A:B + C:B + AC:B
    tt <- emFrequencies(w, ~ A * C | B )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B*C | A
    gA <- sum(w$results[c(3,4,7,5,6,8),1])    # B + C + BC + B:A + C:A + BC:A
    tt <- emFrequencies(w, ~ B * C | A )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    #··························································
    # 1 factor within 2 factors
    #··························································
    # testing A|B*C, B|A*C, C|A*B               of A*B*C design
    #··························································
    # decomposition of A | B*C
    gA <- sum(w$results[c(2,5,6,8),1])    # (A) + (A):B + (A):C + (A):BC
    tt <- emFrequencies(w, ~ A | B*C )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B | A*C
    gA <- sum(w$results[c(3,5,7,8),1])    # (B) + (B):A + (B):C + (B):AC
    tt <- emFrequencies(w, ~ B | A*C )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of C | A*B
    gA <- sum(w$results[c(4,6,7,8),1]) #(C) + (C):A + (C):B + (C):AB
    tt <- emFrequencies(w, ~ C | A * B )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)



    ##########################################################
    # Testing decompositions FOUR FACTOR DESIGN
    ##########################################################

    set.seed(42)
    dta <- GRF( list(A=c("a1","a2"), B=c("b1","b2","b3"), 
                     C=c("c1","c2","c3","c4"), D=c("d1","d2","d3","d4","d5")), 2000,
        c(rep(1/200,118),.41/2,.41/2)    )    ## results in an interaction B:C
    w <- anofa( Freq ~ A * B * C * D, dta)

    #··························································
    # 1 factor within 1 factor
    #··························································
    # testing A|B, A|C, A|D, B|A, B|C, B|D, C|A, C|B, C|D, 
    #         D|A, D|B, D|C                   of A*B*C*D design
    #··························································
    # decomposition of A | B
    gA <- w$results[[2,1]] + w$results[[6,1]]    # A et A:B
    tt <- emFrequencies(w, ~ A | B )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of A | C
    gA <- w$results[[2,1]] + w$results[[7,1]]    # A et A:C
    tt <- emFrequencies(w, ~ A | C )     
    gB <- sum(tt$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of A | D
    gA <- w$results[[2,1]] + w$results[[8,1]]    # A et A:D
    tt <- emFrequencies(w, ~ A | D )      
    gB <- sum(tt$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B | A
    gA <- w$results[[3,1]] + w$results[[6,1]]    # B et A:B
    tt <- emFrequencies(w, ~ B | A )      
    gB <- sum(tt$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B | C
    gA <- w$results[[3,1]] + w$results[[9,1]]    # B et B:C
    tt <- emFrequencies(w, ~ B | C )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B | D
    gA <- w$results[[3,1]] + w$results[[10,1]]    # B et B:D
    tt <- emFrequencies(w, ~ B | D )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of C | A
    gA <- w$results[[4,1]] + w$results[[7,1]]    # C et A:C
    tt <- emFrequencies(w, ~ C | A )      
    gB <- sum(tt$results[,1])
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of C | B
    gA <- w$results[[4,1]] + w$results[[9,1]]    # C et B:C
    tt <- emFrequencies(w, ~ C | B )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of C | D
    gA <- w$results[[4,1]] + w$results[[11,1]]    # C et D:C
    tt <- emFrequencies(w, ~ C | D )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of D | A
    gA <- w$results[[5,1]] + w$results[[8,1]]    # D et D:A
    tt <- emFrequencies(w, ~ D | A )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of D | B
    gA <- w$results[[5,1]] + w$results[[10,1]]    # D et D:B
    tt <- emFrequencies(w, ~ D | B )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of D | C
    gA <- w$results[[5,1]] + w$results[[11,1]]    # D et D:C
    tt <- emFrequencies(w, ~ D | C )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    #··························································
    # 2 factors within 1 factor
    #··························································
    # testing A*B|C, A*B|D, A*C|B, A*C|D, A*D|B, A*D|C, 
    #         B*C|A, B*C|D, B*D|A, B*D|C,
    #         C*D|A, C*D|B                    of A*B*C*D design
    #··························································

    # decomposition of A*B | C
    gA <- sum(w$results[c(2,3,6,7,9,12),1])   # A + B + AB + A:C + B:C + AB:C
    tt <- emFrequencies(w, ~ A *B | C )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of A*B | D
    gA <- sum(w$results[c(2,3,6,8,10,13),1])   # A + B + AB + A:D + B:D + AB:D
    tt <- emFrequencies(w, ~ A *B | D )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of A*C | B
    gA <- sum(w$results[c(2,4,7,6,9,12),1])   # A + C + AC + A:B + C:B + AC:B
    tt <- emFrequencies(w, ~ A *C | B )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of A*C | D
    gA <- sum(w$results[c(2,4,7,8,11,14),1])   # A + C + AC + A:D + C:D + AC:D
    tt <- emFrequencies(w, ~ A *C | D )      
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of A*D | B
    gA <- sum(w$results[c(2,5,8,6,10,13),1])   # A + D + AD + A:B + D:B + AD:B
    tt <- emFrequencies(w, ~ A * D | B )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of A*D | C
    gA <- sum(w$results[c(2,5,8,7,11,14),1])   # A + D + AD + A:C + D:C + AD:C
    tt <- emFrequencies(w, ~ A * D | C )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B*C | A
    gA <- sum(w$results[c(3,4,9,6,7,12),1])   # B + C + BC + B:A + C:A + BC:A
    tt <- emFrequencies(w, ~ B * C | A )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B*C | D
    gA <- sum(w$results[c(3,4,9,10,11,15),1])   # B + C + BC + B:D + C:D + BC:D
    tt <- emFrequencies(w, ~ B * C | D )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B*D | A
    gA <- sum(w$results[c(3,5,10,6,8,13),1])   # B + D + BD + B:A + D:A + BD:A
    tt <- emFrequencies(w, ~ B * D | A )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B*D | C
    gA <- sum(w$results[c(3,5,10,9,11,15),1])   # B + D + BD + B:C + D:C + BD:C
    tt <- emFrequencies(w, ~ B * D | C )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of C*D | A
    gA <- sum(w$results[c(4,5,11,7,8,14),1])   # C + D + CD + C:A + D:A + CD:A
    tt <- emFrequencies(w, ~ C * D | A )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of C*D | B
    gA <- sum(w$results[c(4,5,11,9,10,15),1])   # C + D + CD + C:B + D:B + CD:B
    tt <- emFrequencies(w, ~ C * D | B )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    #··························································
    # 2 factors within 2 factors
    #··························································
    # testing A*B|C*D, A*C|B*D, A*D|B*C, 
    # B*C|A*D, B*D|A*C, C*D|A*B               of A*B*C*D design
    #··························································

    # decomposition of A*B | C*D
    gA <- sum(w$results[c(2,3,6,7,8,9,10,12,13,14,15,16),1])   # tout sauf C, D, C*D
    tt <- emFrequencies(w, ~ A * B | C*D )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of A*C | B*D
    gA <- sum(w$results[c(2,4,6,7,8,9,11,12,13,14,15,16),1])   # tout sauf B, D, B*D
    tt <- emFrequencies(w, ~ A * C | B*D )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of A*D | B*C
    gA <- sum(w$results[c(2,5,6,7,8,10,11,12,13,14,15,16),1])   # tout sauf B, C, B*C
    tt <- emFrequencies(w, ~ A * D | B*C )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B*C | A*D
    gA <- sum(w$results[c(3,4,6,7,9,10,11,12,13,14,15,16),1])   # tout sauf A, D, A*D
    tt <- emFrequencies(w, ~ B * C | A*D )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of B*D | A*C
    gA <- sum(w$results[c(3,5,6,8,9,10,11,12,13,14,15,16),1])   # tout sauf A, C, A*C
    tt <- emFrequencies(w, ~ B * D | A*C )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)

    # decomposition of C*D | A*B
    gA <- sum(w$results[c(4,5,7,8,9,10,11,12,13,14,15,16),1])   # tout sauf A, B, A*B
    tt <- emFrequencies(w, ~ C * D | A*B )
    gB <- sum(tt$results[,1]) 
    expect_equal(gA, gB, tolerance = 0.0001)


    #··························································
    # 3 factors within 1 factor
    #··························································
    # testing A*B*C|D, A*B*D|C, A*C*D|B, B*C*D|A
    #                                         of A*B*C*D design
    #··························································

    ## Coding third-order simple effects has been postpone until some interest is shown...



})





