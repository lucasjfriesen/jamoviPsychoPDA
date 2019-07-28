







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
          self$results$instructions$setContent(
            "<html>
            <head>
            <style>
            
            div.instructions {
            width: 500px;
            height: 225px;
            display: flex;
            flex-wrap: wrap;
            align-content: center;
            }
            </style>
            </head>
            <body>
            <div class='instructions'>
            <p><b>This analysis is still in development. Please report any errors or requests <a href='https://github.com/lucasjfriesen/jamoviPsychoPDA/issues' target = '_blank'>here</a></b></p>
            <p>Welcome to PsychoPDA's T-Test Design Analysis module. To get started:</p>
            <ol>
            <li>Input the 'Label'<br /><br /></li>
            <li>Input the 'Observed Effect'<br /><br /></li>
            <li>Input the 'Hypothesized True Effect<br /><br /></li>
            <li>Input ONE OF 'Observed Standard Error' OR 'Observed P-Value'</li>
            </ol>
            <p>If you encounter any errors, or have questions, please see the <a href='https://lucasjfriesen.github.io/jamoviPsychoPDA_docs/meanDiffDA.html' target = '_blank'>documentation</a>.</p>
            </div>
            </body>
            </html>"
          )
          return()
        } else {
          self$results$instructions$setVisible(visible = FALSE)
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
        
        # Sensitivity ----
        
        
        # Results ----
        
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
        
        # Proposed HTE ----
        if (self$options$sensHyp) {
          sensRange <- seq(-2, 2, length.out = 500)
          sensRes <-
            matrix(ncol = 5,
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
              sensRes[((i - 1) * length(sensRange)) + j, 3:5] <-
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
              "typeM")
          
          imageHTE <- self$results$plotHTE
          imageHTE$setState(plotData)
        }
        
        if (self$options$HTEViz) {
          for (i in 1:length(hypTrueEff)) {
            self$results$plotHTEViz$addItem(i)
          }
          resultsHTEViz <-
            data.frame(
              "estimate" = as.numeric(),
              "D" = as.numeric(),
              "observedSE" = as.numeric(),
              "z" = as.numeric()
            )
          # c("estimate", "observedSE", "z")
          # HTE Viz ----
          for (i in 1:nrow(data)) {
            resultsHTEViz <- retroDesignEmp(hypTrueEff[i],
                                            observedSE[i],
                                            alpha,
                                            df[i],
                                            nSims)
            
            # self$results$debug$setContent(self$results$plotHTEViz$length.)
            resultsHTEViz <- as.data.frame(resultsHTEViz)
            
            colnames(resultsHTEViz) = c("estimate", "D", "observedSE", "z")
            
            imageHTEViz <- self$results$plotHTEViz$get(index = i)
            imageHTEViz$setState(resultsHTEViz)
          }
          
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
              proposedDF <- proposedN[j] - 1
              res <- retroDesign(abs(hypTrueEff[i]),
                                 observedSE[i],
                                 alpha,
                                 proposedDF,
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
          scale_y_continuous(name = "Type-M",
                             sec.axis = sec_axis(name = "Type-S/Power", trans = ~ . / max(plotData$typeM))) +
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
          scale_y_continuous(name = "Type-M",
                             sec.axis = sec_axis(name = "Type-S/Power", trans = ~ . / max(plotData$typeM))) +
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
        
        # self$results$debug$setContent(plotData)
        
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
          scale_y_continuous(name = "Type-M",
                             sec.axis = sec_axis(name = "Type-S/Power", trans = ~ . / max(plotData$typeM))) +
          scale_x_continuous(name = "10% to Double Observed SE") +
          theme_classic() +
          facet_wrap( ~ hypTrueGroup, scales = "free")
        
        print(plot)
        # ggplotly(plot)
        TRUE
      },
      .plotHTEViz = function(imageHTEViz, ...)
      {
        plotData <- imageHTEViz$state
        
        plot <-
          ggplot(plotData, aes(x = seq_along(estimate), y = estimate)) +
          geom_point(pch = ifelse((plotData$estimate > plotData$observedSE * plotData$z) &
                                    (!plotData$estimate < (-1 * plotData$observedSE) * plotData$z),
                                  15,
                                  ifelse((!plotData$estimate > plotData$observedSE * plotData$z) &
                                           (plotData$estimate < (-1 * plotData$observedSE) * plotData$z),
                                         17,
                                         ifelse((!plotData$estimate > plotData$observedSE * plotData$z) &
                                                  (!plotData$estimate < (-1 * plotData$observedSE) * plotData$z),
                                                16,
                                                16
                                         )
                                  )
          ),
          col = ifelse((!plotData$estimate > plotData$observedSE * plotData$z) &
                         (!plotData$estimate < (-1 * plotData$observedSE) * plotData$z),
                       "grey",
                       "black"
          )) +
          geom_hline(yintercept = plotData$observedSE * plotData$z,
                     size = 1) +
          geom_hline(yintercept = (-1 * plotData$observedSE) * plotData$z,
                     size = 1) +
          geom_hline(yintercept = plotData$D,
                     linetype = "dashed",
                     size = 1) +
          xlab("n") +
          ggtitle(unique(plotData$D))
        
        print(plot)
        TRUE
      }
          )
    )
