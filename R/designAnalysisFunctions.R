designAnalysis.nagR2 <-
  function(designList,
           Data,
           group,
           match,
           bootSims,
           type,
           hypTrueEff,
           alpha,
           difFlagScale,
           sigOnly) {
    if (hypTrueEff == "") {
      if (difFlagScale == "zt") {
        hypTrueEff <- c(0, 0.13, 0.26)
      } else {
        hypTrueEff <- c(0, 0.035, 0.07)
      }
      labels <- c("Null", "Moderate", "Large")
    } else {
      hypTrueEff <- as.numeric(hypTrueEff)
      labels <- c("Custom Hyp.")
    }
    
    GC <-
      data.frame(
        "label" = rep(as.character(), times = length(designList) * length(hypTrueEff)),
        "item" = rep(as.character(), times = length(designList) * length(hypTrueEff)),
        "obsEff" = rep(as.numeric(), times = length(designList) *
                         length(hypTrueEff)),
        "hypTrueEff" = rep(as.numeric(), times = length(designList) *
                             length(hypTrueEff)),
        "typeM" = rep(as.numeric(), times = length(designList) *
                        length(hypTrueEff)),
        "power" = rep(as.numeric(), times = length(designList) *
                        length(hypTrueEff)),
        "bootSE" = rep(as.numeric(), times = length(designList) *
                         length(hypTrueEff)),
        stringsAsFactors = FALSE
      )
    
    
    for (item in 1:length(designList)) {
      empDATA <-
        cbind(Item = jmvcore::toNumeric(Data[, designList[item]]),
              jmvcore::toNumeric(group),
              match)
      colnames(empDATA) <-
        c(colnames(Data)[item], "GROUP", "SCORES")
      
      myBoot <-
        boot.empDist(empDATA,
                     R = bootSims,
                     type = type,
                     coefficients = FALSE)
      
      tick <- length(hypTrueEff) - 1
      
      for (hypInd in 1:length(hypTrueEff)) {
        retroDesignRes <-
          retroDesign.nagR2(
            hypTrueEff = hypTrueEff[hypInd],
            myBoot,
            alpha = alpha,
            sigOnly = sigOnly
          )
        
        GC[item * length(hypTrueEff) - tick, 1] <-
          labels[hypInd]
        GC[item * length(hypTrueEff) - tick, 2] <-
          designList[item]
        GC[item * length(hypTrueEff) - tick, 3] <-
          as.character((hypTrueEff[hypInd]))
        GC[item * length(hypTrueEff) - tick, 4:7] <-
          as.numeric(retroDesignRes)
        tick <- tick - 1
      }
    }
    
    return(GC)
  }


# -----

designAnalysis.coefficients <- function(designList,
                                        Data,
                                        group,
                                        match,
                                        bootSims,
                                        type,
                                        hypTrueEff,
                                        difFlagScale,
                                        sigOnly) {
  if (hypTrueEff == "") {
    hypTrueEff <- 0
    labels <- "Null"
  } else {
    hypTrueEff <- as.numeric(hypTrueEff)
    labels <- c("Custom Hyp.")
  }
  
  GC <-
    data.frame(
      "label" = rep(as.character(), times = length(designList) * length(hypTrueEff)),
      "item" = rep(as.character(), times = length(designList) * length(hypTrueEff)),
      "obsMain" = rep(as.numeric(), times = length(designList) *
                        length(hypTrueEff)),
      "obsInt" = rep(as.numeric(), times = length(designList) *
                       length(hypTrueEff)),
      "hypTrueEff" = rep(as.numeric(), times = length(designList) *
                           length(hypTrueEff)),
      "bootSE" = rep(as.numeric(), times = length(designList) *
                       length(hypTrueEff)),
      "typeMMain" = rep(as.numeric(), times = length(designList) *
                          length(hypTrueEff)),
      "typeSMain" = rep(as.numeric(), times = length(designList) *
                          length(hypTrueEff)),
      "typeMInt" = rep(as.numeric(), times = length(designList) *
                         length(hypTrueEff)),
      "typeSInt" = rep(as.numeric(), times = length(designList) *
                         length(hypTrueEff)),
      "power" = rep(as.numeric(), times = length(designList) *
                      length(hypTrueEff)),
      stringsAsFactors = FALSE
    )
  
  for (item in 1:length(designList)) {
    curItem <- designList[item]
    empDATA <-
      cbind(Item = jmvcore::toNumeric(Data[, curItem]),
            jmvcore::toNumeric(group),
            match)
    colnames(empDATA) <-
      c(colnames(Data)[item], "GROUP", "SCORES")
    tick <- length(hypTrueEff) - 1
    if (coefficients == FALSE) {
      myBoot <-
        boot.empDist(empDATA,
                     R = bootSims,
                     type = type,
                     coefficients = FALSE)
    } else {
      myBoot <-
        boot.empDist(empDATA,
                     R = bootSims,
                     type = type,
                     coefficients = TRUE)
    }
    for (hypInd in 1:length(hypTrueEff)) {
      private$.checkpoint()
      if (coefficients == FALSE) {
        retroDesignRes <-
          retroDesign.nagR2(
            hypTrueEff = hypTrueEff[hypInd],
            myBoot,
            alpha = alpha,
            sigOnly = sigOnly
          )
      } else {
        retroDesignRes <-
          retroDesign.coefficients(
            hypTrueEff = hypTrueEff[hypInd],
            myBoot,
            alpha = alpha,
            sigOnly = sigOnly
          )
      }
      GC[item * length(hypTrueEff) - tick, 1] <-
        labels[hypInd]
      GC[item * length(hypTrueEff) - tick, 2] <-
        designList[item]
      GC[item * length(hypTrueEff) - tick, 3] <-
        as.character((hypTrueEff[hypInd]))
      GC[item * length(hypTrueEff) - tick, 4:7] <-
        as.numeric(retroDesignRes)
      tick <- tick - 1
    }
  }
  
  return(GC)
}