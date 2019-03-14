




# This file is a generated template, your changes will not be overwritten

rdTTestClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "rdTTestClass",
    inherit = rdTTestBase,
    private = list(
      .run = function() {
        if (is.null(self$options$labelVar) |
            is.null(self$options$hypTrueEff) |
            (is.null(self$options$observedP) &
             is.null(self$options$observedSE))) {
          self$results$instructions$setVisible(visible = TRUE)
          self$results$instructions$setRow(rowNo = 1,
                                           value = list(frank = "1) Input the 'Label'"))
          self$results$instructions$setRow(rowNo = 2,
                                           value = list(frank = "2) Input the 'Observed Effect'"))
          self$results$instructions$setRow(
            rowNo = 3,
            value = list(frank = "3) Input the 'Hypothesized True Effect'")
          )
          self$results$instructions$setRow(
            rowNo = 4,
            value = list(frank = "4) Input ONE OF 'Observed Standard Error' OR 'Observed P-Value'")
          )
          return()
        }
        
        data <- self$data
        data <- na.omit(data)
        
        labels <- as.character(data[, self$options$labelVar])
        
        hypTrueEff <-
          jmvcore::toNumeric(data[, self$options$hypTrueEff])
        
        if (!is.null(self$options$observedSE)) {
          observedSE <- jmvcore::toNumeric(data[, self$options$observedSE])
        } else {
          observedSE <- NULL
        }
        if (!is.null(self$options$observedP)) {
          observedP <- jmvcore::toNumeric(data[, self$options$observedP])
        } else {
          observedP <- NULL
        }
        
        if (!is.null(self$options$n)) {
          df <- jmvcore::toNumeric(data[, self$options$n]) - 1
        } else {
          df <- Inf
        }
        nSims <- self$options$nSims
        alpha <- self$options$alpha
        
        if (!is.null(observedP)) {
          z <- qt(1 - alpha / 2, df)
          observedSE <- observedP / z
        }
        
        retroDesign <- function(D,
                                observedSE,
                                alpha,
                                df,
                                nSims) {
          z <- qt(1 - alpha / 2, df)
          p.hi <- 1 - pt(z - D / observedSE, df)
          p.lo <- pt(-z - D / observedSE, df)
          power <- p.hi + p.lo
          typeS <- p.lo / power
          lambda <- D / observedSE
          if (self$options$scatterViz){
            estimate <- D + observedSE * rt(nSims, df)
            significant <- abs(estimate) > observedSE * z
            typeM <- mean(abs(estimate)[significant]) / D
          } else {
             typeM <-
            (dt(lambda + z, df = df) + dt(lambda - z, df = df) +
               lambda * (pt(lambda + z, df = df) + pt(lambda - z, df = df) - 1)) /
            (lambda * (1 - pt(lambda + z, df = df) + pt(lambda - z, df = df)))
             estimate <- NULL
          }

          return(list(
            power = power,
            typeS = typeS,
            typeM = typeM,
            estimate = estimate
          ))
        }
        
        results <- matrix(ncol = 4, nrow = nrow(data))
        colnames(results) <- c("D", "typeS", "typeM", "power")
        
        for (i in 1:nrow(results)) {
          resultsRow <- retroDesign(abs(hypTrueEff[i]),
                                    observedSE[i],
                                    alpha,
                                    df[i],
                                    nSims)
          results[i, 1] <- hypTrueEff[i]
          results[i, 2] <- resultsRow$typeS
          results[i, 3] <- resultsRow$typeM
          results[i, 4] <- resultsRow$power
          
        }
        
        # Sensitivity ----
        
        # Proposed HTE ----
        if (self$options$sensHyp) {
          sensRange <- seq(-2, 2, length.out = 500)
          sensRes <-
            matrix(ncol = 6,
                   nrow = nrow(data) * length(sensRange))
          
          for (i in 1:nrow(data)) {
            hypTrueEffSens <- hypTrueEff[i] + (observedSE[i] * sensRange)
            for (j in 1:length(sensRange)) {
              res <- retroDesign(abs(hypTrueEffSens[j]),
                                 observedSE[i],
                                 alpha,
                                 df[i],
                                 nSims)
              sensRes[((i - 1) * length(sensRange)) + j, 1] <-
                hypTrueEff[i]
              sensRes[((i - 1) * length(sensRange)) + j, 2] <-
                hypTrueEffSens[j]
              sensRes[((i - 1) * length(sensRange)) + j, 3:6] <-
                unlist(res)
            }
          }
          
          plotData <-
            data.frame(cbind(rep(sensRange, nrow(data)), sensRes))
          
          colnames(plotData) <-
            c("SD",
              "hypTrueGroup",
              "sensVar",
              "power",
              "typeS",
              "typeM",
              "estimate")
          
          imageHTE <- self$results$plotHTE
          imageHTE$setState(plotData[,1:6])
          imageHTEViz <- self$results$HTEViz
          imageHTEViz$setState(plotData[,7])
        }
        
        # Proposed N ----
        
        if (self$options$sensN) {
          sensRange <- seq(0.1, 2, length.out = 500)
          sensRes <-
            matrix(ncol = 5,
                   nrow = nrow(data) * length(sensRange))
          n <- data[, self$options$n]
          for (i in 1:nrow(data)) {
            proposedN <- round(n[i] * sensRange)
            for (j in 1:length(sensRange)) {
              df <- proposedN[j] - 1
              res <- retroDesign(abs(hypTrueEff[i]),
                                 observedSE[i],
                                 alpha,
                                 df,
                                 nSims)
              sensRes[((i - 1) * length(sensRange)) + j, 1] <-
                hypTrueEff[i]
              sensRes[((i - 1) * length(sensRange)) + j, 2] <-
                proposedN[j]
              sensRes[((i - 1) * length(sensRange)) + j, 3:5] <-
                unlist(res)
            }
          }
          
          plotDataN <-
            data.frame(cbind(rep(sensRange, nrow(data)), sensRes))
          
          colnames(plotDataN) <-
            c("SD",
              "hypTrueGroup",
              "sensVar",
              "power",
              "typeS",
              "typeM")
          imageN <- self$results$plotN
          imageN$setState(plotDataN)
          }
        
        # Proposed SE ----
        
        if (self$options$sensSE) {
          sensRange <- seq(0.1, 2, length.out = 500)
          sensRes <-
            matrix(ncol = 5,
                   nrow = nrow(data) * length(sensRange))
          for (i in 1:nrow(data)) {
            proposedSE <- observedSE[i] * sensRange
            for (j in 1:length(sensRange)) {
              res <- retroDesign(abs(hypTrueEff[i]),
                                 proposedSE[j],
                                 alpha,
                                 df[i],
                                 nSims)
              sensRes[((i - 1) * length(sensRange)) + j, 1] <-
                hypTrueEff[i]
              sensRes[((i - 1) * length(sensRange)) + j, 2] <-
                proposedSE[j]
              sensRes[((i - 1) * length(sensRange)) + j, 3:5] <-
                unlist(res)
            }
          }
          
          plotDataSE <-
            data.frame(cbind(rep(sensRange, nrow(data)), sensRes))
          
          colnames(plotDataSE) <-
            c("SD",
              "hypTrueGroup",
              "sensVar",
              "power",
              "typeS",
              "typeM")
          imageSE <- self$results$plotSE
          imageSE$setState(plotDataSE)
        }
        
        # Results ----
        
        table <- self$results$rdTTest
        
        for (i in 1:nrow(results)) {
          table$addRow(
            rowKey = i,
            values = list(
              label = labels[[i]],
              D = as.character(results[i, "D"]),
              typeS = results[i, "typeS"],
              typeM = results[i, "typeM"],
              power = results[i, "power"]
            )
          )
        }
      },
      .plotHTE = function(imageHTE, ...) {
        if (is.null(self$options$labelVar) |
            is.null(self$options$hypTrueEff) |
            (is.null(self$options$observedP) &
             is.null(self$options$observedSE))) {
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
          # scale_y_continuous(name = "Type-M", sec.axis = sec_axis(name = "Type-S/Power", trans = ~./max(plotData$typeM))) +
          scale_x_continuous(name = "Propsed HTE +/- 2 Observed SE") +
          theme_classic() +
          facet_wrap(~ hypTrueGroup, scales = "free")
        
        print(plot)
        # ggplotly(plot)
        TRUE
      },
      .plotN = function(imageN, ...) {
        if (is.null(self$options$labelVar) |
            is.null(self$options$hypTrueEff) |
            (is.null(self$options$observedP) &
             is.null(self$options$observedSE))) {
          return()
        }
        plotData <- imageN$state

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
          # scale_y_continuous(name = "Type-M", sec.axis = sec_axis(name = "Type-S/Power", trans = ~./max(plotData$typeM))) +
          scale_x_continuous(name = "Half to Double Observed N") +
          theme_classic() +
          facet_wrap( ~ hypTrueGroup, scales = "free")

        print(plot)
        # ggplotly(plot)
        TRUE
      },
      .plotSE = function(imageSE, ...)
      {
        if (is.null(self$options$labelVar) |
            is.null(self$options$hypTrueEff) |
            (is.null(self$options$observedP) &
             is.null(self$options$observedSE)))
        {
          return()
        }
        plotData <- imageSE$state

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
          # scale_y_continuous(name = "Type-M", sec.axis = sec_axis(name = "Type-S/Power", trans = ~./max(plotData$typeM))) +
          scale_x_continuous(name = "10% to Double Observed SE") +
          theme_classic() +
          facet_wrap( ~ hypTrueGroup, scales = "free")

        print(plot)
        # ggplotly(plot)
        TRUE
      },
      .plotHTEViz = function(imageHTEViz, ...)
        {
        
      }
    )
  )
