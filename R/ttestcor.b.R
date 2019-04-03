


# This file is a generated template, your changes will not be overwritten

ttestCorClass <- if (requireNamespace('jmvcore'))
  R6::R6Class("ttestCorClass",
              inherit = ttestCorBase,
              private = list(
                .run = function() {
                  if (is.null(self$options$labelVar) |
                      is.null(self$options$hypTrueCor) |
                      is.null(self$options$n) |
                      is.null(self$options$observedCor)) {
                    self$results$instructions$setVisible(visible = TRUE)
                    self$results$instructions$setRow(rowNo = 1,
                                                     value = list(frank = "1) Input the 'Label'"))
                    self$results$instructions$setRow(
                      rowNo = 2,
                      value = list(frank = "2) Input the 'Hypothesized True Correlation'")
                    )
                    self$results$instructions$setRow(rowNo = 3,
                                                     value = list(frank = "3) Input the 'Sample size' (N)"))
                    self$results$instructions$setRow(
                      rowNo = 4,
                      value = list(frank = "4) Input ONE OF 'Observed Correlation' OR 'Observed Standard Error'")
                    )
                    return()
                  }
                  
                  data <- self$data
                  data <- na.omit(data)
                  
                  labels <- as.character(data[, self$options$labelVar])
                  
                  hypTrueCor <-
                    jmvcore::toNumeric(data[, self$options$hypTrueCor])
                  
                  observedCor <-
                    jmvcore::toNumeric(data[, self$options$observedCor])
                  
                  n <- jmvcore::toNumeric(data[, self$options$n])
                  df <- n - 2
                  
                  nSims <- self$options$bootSims
                  
                  observedSE <- switch(self$options$corType,
                                       # PEARSON'S RHO
                                       pearson = sqrt((1 - observedCor ^ 2) / df),
                                       # SPEARMAN'S RHO SMALL: ftp://biostat.wisc.edu/pub/chappell/800/hw/spearman.pdf
                                       spearman = ((1 - (observedCor ^ 2)) / (df)) ^ 0.5)
                  
                  D <- (observedCor - hypTrueCor) / observedSE
                  
                  alpha <- self$options$alpha
                  
                  retroDesign <- function(D,
                                          observedSE,
                                          alpha,
                                          df,
                                          nSims = 10000) {
                    z <- qt(1 - alpha / 2, df)
                    p.hi <- 1 - pt(z - D / observedSE, df)
                    p.lo <- pt(-z - D / observedSE, df)
                    power <- p.hi + p.lo
                    typeS <- p.lo / power
                    # lambda <- D / observedSE
                    # typeM <-
                    #   (dt(lambda + z, df = df) + dt(lambda - z, df = df) +
                    #      lambda * (pt(lambda + z, df = df) + pt(lambda - z, df = df) - 1)) /
                    #   (lambda * (1 - pt(lambda + z, df = df) + pt(lambda - z, df = df)))
                    estimate <- D + observedSE * rt(nSims, df)
                    significant <- abs(estimate) > observedSE * z
                    typeM <- mean(abs(estimate)[significant]) / D
                    return(list(
                      typeS = typeS,
                      typeM = typeM,
                      power = power
                    ))
                  }
                  
                  retroDesignEmp <- function(D,
                                             observedSE,
                                             alpha,
                                             df,
                                             nSims) {
                    z <- qt(1 - alpha / 2, df)
                    estimate <- D + observedSE * rt(nSims, df)
                    return(list(
                      estimate,
                      rep(D, times = nSims),
                      rep(z, times = nSims),
                      rep(observedSE, times = nSims)
                    ))
                  }
                  
                  results <- matrix(ncol = 4, nrow = nrow(data))
                  colnames(results) <- c("D", "typeS", "typeM", "power")
                  
                  for (i in 1:nrow(results)) {
                    resultsRow <- retroDesign(abs(D[i]),
                                              observedSE[i],
                                              alpha,
                                              df[i],
                                              nSims = nSims)
                    results[i, 1] <- D[i]
                    results[i, 2] <- resultsRow$typeS
                    results[i, 3] <- resultsRow$typeM
                    results[i, 4] <- resultsRow$power
                    
                  }
                  
                  # Sensitivity ----
                  
                  # Proposed HTE ----
                  if (self$options$sensHyp) {
                    sensRange <- seq(-.99, .99, length.out = 5000)
                    sensRes <-
                      matrix(ncol = 5,
                             nrow = length(observedCor) * length(sensRange))
                    for (i in 1:length(observedCor)) {
                      for (j in 1:length(sensRange)) {
                        proposedCor <- (observedCor[i] - sensRange[j]) / observedSE[i]
                        res <- retroDesign(abs(proposedCor),
                                           observedSE[i],
                                           alpha,
                                           df[i])
                        sensRes[((i - 1) * length(sensRange)) + j, 1] <-
                          observedCor[i]
                        sensRes[((i - 1) * length(sensRange)) + j, 2] <-
                          proposedCor
                        sensRes[((i - 1) * length(sensRange)) + j, 3:5] <-
                          unlist(res)
                      }
                    }
                    
                    plotDataHTE <-
                      data.frame(cbind(rep(sensRange, nrow(data)), sensRes))
                    
                    colnames(plotDataHTE) <-
                      c("sensVar",
                        "hypTrueGroup",
                        "proposedCor",
                        "typeS",
                        "typeM",
                        "power")
                    plotDataHTE <- plotDataHTE[plotDataHTE$power != 1,]
                    imageHTE <- self$results$plotHTE
                    imageHTE$setState(plotDataHTE)
                  }
                  
                  # HTE Viz ----
                  if (self$options$HTEViz) {
                    for (i in 1:length(hypTrueCor)) {
                      self$results$plotHTEViz$addItem(i)
                    }
                    resultsHTEViz <-
                      data.frame(
                        "estimate" = as.numeric(),
                        "D" = as.numeric(),
                        "observedSE" = as.numeric(),
                        "z" = as.numeric()
                      )
                    
                    for (i in 1:nrow(data)) {
                      resultsHTEViz <- retroDesignEmp(D[i],
                                                      observedSE[i],
                                                      alpha,
                                                      df[i],
                                                      nSims = nSims)
                      
                      resultsHTEViz <- as.data.frame(resultsHTEViz)
                      
                      colnames(resultsHTEViz) = c("estimate", "D", "observedSE", "z")
                      # self$results$debug$setContent(str(resultsHTEViz))
                      imageHTEViz <- self$results$plotHTEViz$get(key = i)
                      imageHTEViz$setState(resultsHTEViz)
                    }
                    
                  }
                  
                  # Proposed N ----
                  
                  if (self$options$sensN) {
                    n <- data[, self$options$n]
                    sensRange <- seq(3, (2 * n[1]))
                    sensRes <-
                      matrix(ncol = 5,
                             nrow = nrow(data) * length(sensRange))
                    for (i in 1:nrow(data)) {
                      proposedDF <- sensRange - 2
                      proposedSE <- switch(
                        self$options$corType,
                        # PEARSON'S RHO
                        pearson = sqrt((1 - observedCor[i] ^ 2) /
                                         proposedDF),
                        # SPEARMAN'S RHO SMALL: ftp://biostat.wisc.edu/pub/chappell/800/hw/spearman.pdf
                        spearman = ((1 - (
                          observedCor[i] ^ 2
                        )) / (proposedDF)) ^ 0.5
                      )
                      proposedD <- (observedCor - hypTrueCor) / proposedSE
                      for (j in 1:length(sensRange)) {
                        res <- retroDesign(
                          D = abs(proposedD[i]),
                          observedSE = proposedSE[j],
                          alpha = alpha,
                          df = proposedDF[j]
                        )
                        
                        sensRes[((i - 1) * length(sensRange)) + j, 1] <-
                          observedCor[i]
                        sensRes[((i - 1) * length(sensRange)) + j, 2] <-
                          sensRange[j]
                        sensRes[((i - 1) * length(sensRange)) + j, 3:5] <-
                          unlist(res)
                      }
                    }
                    # self$results$debug$setContent(sensRes)
                    plotDataN <-
                      data.frame(cbind(rep(sensRange, nrow(data)), sensRes))
                    
                    colnames(plotDataN) <-
                      c("senseRange",
                        "hypTrueGroup",
                        "sensVar",
                        "typeS",
                        "typeM",
                        "power")
                    
                    plotDataN <- plotDataN[plotDataN$power != 1,]
                    
                    imageN <- self$results$plotN
                    imageN$setState(plotDataN)
                  }
                  
                  # Proposed Obs ----
                  
                  if (self$options$sensObs) {
                    sensRange <- seq(-1, 1, length.out = self$options$lengthOut)
                    sensRes <-
                      matrix(ncol = 5,
                             nrow = nrow(data) * length(sensRange))
                    for (i in 1:nrow(data)) {
                      for (j in 1:length(sensRange)) {
                        proposedSE <- switch(
                          self$options$corType,
                          # PEARSON'S RHO
                          pearson = sqrt((1 - sensRange[j] ^ 2) / df[i]),
                          # SPEARMAN'S RHO SMALL: ftp://biostat.wisc.edu/pub/chappell/800/hw/spearman.pdf
                          spearman = ((1 - sensRange[j] ^ 2) / (df[i])) ^ 0.5)
                        proposedD <- (sensRange[j] - hypTrueCor[i]) / proposedSE
                          
                          res <- retroDesign(
                            D = abs(proposedD),
                            observedSE = proposedSE,
                            alpha = alpha,
                            df = df[i]
                          )
                          
                          sensRes[((i - 1) * length(sensRange)) + j, 1] <-
                            observedCor[i]
                          sensRes[((i - 1) * length(sensRange)) + j, 2] <-
                            sensRange[j]
                          sensRes[((i - 1) * length(sensRange)) + j, 3:5] <-
                            unlist(res)
                      }
                      
                    }
                    
                    plotDataObs <-
                      data.frame(cbind(rep(sensRange, nrow(data)), sensRes))
                    
                    colnames(plotDataObs) <-
                      c("senseRange",
                        "hypTrueGroup",
                        "sensVar",
                        "typeS",
                        "typeM",
                        "power")
                    
                    plotDataObs <- plotDataObs[plotDataObs$power != 1,]
                    plotDataObs$typeM[plotDataObs$typeM == Inf] <- 1000
                    
                    imageObs <- self$results$plotObs
                    imageObs$setState(plotDataObs)
                  }
                  
                  # Results ----
                  
                  table <- self$results$rdTTestCor
                  
                  for (i in 1:nrow(results)) {
                    table$addRow(
                      rowKey = i,
                      values = list(
                        label = labels[[i]],
                        obsCor = as.character(observedCor[i]),
                        obsN = df[i] + 2,
                        hypTrueCor = as.character(hypTrueCor[i]),
                        obsSE = observedSE[i],
                        hypTrueEffSD = results[i, "D"],
                        typeS = results[i, "typeS"],
                        typeM = results[i, "typeM"],
                        power = results[i, "power"]
                      )
                    )
                  }
                },
                .plotHTE = function(imageHTE, ...) {
                  if (is.null(self$options$labelVar) |
                      is.null(self$options$hypTrueCor) |
                      is.null(self$options$n) |
                      is.null(self$options$observedCor)) {
                    return()
                  }
                  
                  plotData <- imageHTE$state
                  
                  plot <- ggplot(plotData) +
                    geom_line(aes(
                      x = sensVar,
                      y = typeS * (max(typeM)),
                      colour = "typeS"
                    )) +
                    geom_line(aes(x = sensVar, y = typeM, colour = "typeM")) +
                    geom_line(aes(
                      x = sensVar,
                      y = power * (max(typeM)),
                      colour = "power"
                    )) +
                    scale_y_continuous(name = "Type-M", sec.axis = sec_axis(name = "Type-S/Power", trans = ~./max(plotData$typeM))) +
                    scale_x_continuous(name = "Propsed HTE +/- 2 Observed SE") +
                    theme_classic() +
                    facet_wrap(~ hypTrueGroup, scales = "free")
                  
                  print(plot)
                  # ggplotly(plot)
                  TRUE
                },
                .plotN = function(imageN, ...) {
                  if (is.null(self$options$labelVar) |
                      is.null(self$options$hypTrueCor) |
                      is.null(self$options$n) |
                      is.null(self$options$observedCor)) {
                    return()
                  }
                  plotData <- imageN$state
                  # self$results$debug$setContent(plotData)
                  
                  plot <- ggplot(plotData) +
                    geom_line(aes(
                      x = sensVar,
                      y = typeS * (max(typeM)),
                      colour = "typeS"
                    )) +
                    geom_line(aes(x = sensVar,
                                  y = typeM,
                                  colour = "typeM")) +
                    geom_line(aes(
                      x = sensVar,
                      y = power * (max(typeM)),
                      colour = "power"
                    )) +
                    scale_y_continuous(name = "Type-M", sec.axis = sec_axis(name = "Type-S/Power", trans = ~./max(plotData$typeM))) +
                    scale_x_continuous(name = "Half to Double Observed N") +
                    theme_classic() +
                    facet_wrap(~ hypTrueGroup, scales = "free")
                  
                  print(plot)
                  # ggplotly(plot)
                  TRUE
                },
                .plotObs = function(imageObs, ...) {
                  if (is.null(self$options$labelVar) |
                      is.null(self$options$hypTrueCor) |
                      is.null(self$options$n) |
                      is.null(self$options$observedCor)) {
                    return()
                  }
                  
                  plotData <- imageObs$state

                  plot <- ggplot(plotData) +
                    geom_line(aes(
                      x = sensVar,
                      y = typeS * (max(typeM)),
                      colour = "typeS"
                    )) +
                    geom_line(aes(x = sensVar,
                                  y = typeM,
                                  colour = "typeM")) +
                    geom_line(aes(
                      x = sensVar,
                      y = power * (max(typeM)),
                      colour = "power"
                    )) +
                    scale_y_continuous(name = "Type-M", sec.axis = sec_axis(name = "Type-S/Power", trans = ~./max(plotData$typeM))) +
                    scale_x_continuous(name = "Possible Obs. Corr. where power != 1") +
                    theme_classic() +
                    facet_wrap(~ hypTrueGroup, scales = "free")

                  print(plot)
                  # ggplotly(plot)
                  TRUE
                },
                .plotHTEViz = function(imageHTEViz, ...)
                {
                          if (is.null(self$options$labelVar) |
                             is.null(self$options$hypTrueCor) |
                             is.null(self$options$n) |
                             is.null(self$options$observedCor)){
                    return()
                  }
                  plotData <- imageHTEViz$state
                  # self$results$debug$setContent(list(plotData, str(plotData)))
                  
                  plot <-
                    ggplot(plotData, aes(x = seq_along(estimate), y = estimate)) +
                    geom_point(pch = ifelse((plotData$estimate > plotData$observedSE * plotData$z) &
                                              (!plotData$estimate < (plotData$observedSE * -1) * plotData$z),
                                            15,
                                            ifelse((!plotData$estimate > plotData$observedSE * plotData$z) &
                                                     (plotData$estimate < (plotData$observedSE * -1) * plotData$z),
                                                   17,
                                                   ifelse((!plotData$estimate > plotData$observedSE * plotData$z) &
                                                            (!plotData$estimate < (plotData$observedSE * -1) * plotData$z),
                                                          16,
                                                          16
                                                   )
                                            )
                    ),
                    col = ifelse((!plotData$estimate > plotData$observedSE * plotData$z) &
                                   (!plotData$estimate < (plotData$observedSE * -1) * plotData$z),
                                 "grey",
                                 "black"
                    )) +
                    geom_hline(yintercept = plotData$observedSE * plotData$z,
                               size = 1) +
                    geom_hline(yintercept = (plotData$observedSE * -1) * plotData$z,
                               size = 1) +
                    geom_hline(yintercept = plotData$D,
                               linetype = "dashed",
                               size = 1) +
                    xlab("n") +
                    ggtitle(unique(plotData$D))
                  
                  
                  print(plot)
                  TRUE
                }
                        ))