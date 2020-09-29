#options(jamovi_home = "C:\\Program Files\\jamovi 1.6.3.0")


# This file is a generated template, your changes will not be overwritten

nonParametricIRTClass <-
  if (requireNamespace('jmvcore'))
    R6::R6Class(
      "nonParametricIRTClass",
      inherit = nonParametricIRTBase,
      private = list(
        .run = function() {
          if (is.null(self$data) || is.null(self$options$item)) {
            self$results$instructions$setContent(
              "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>Welcome to PsychoPDA's Non-Parametric Item Response Theory analysis. To get started:</p>
            <ol>
            <li>Place items to be analyzed 'Items' slot.<br /><br /></li>
            <li><b>The first row will be used as the key for each item.</b><br /><br /></li>
            <li>[<em>Optional</em>] Indicate any further specifications.<br /><br /></li>
            </ol>
            <p>If you encounter any errors, or have questions, please see the <a href='https://lucasjfriesen.github.io/jamoviPsychoPDA_docs/DIF_index.html' target = '_blank'>documentation</a></p>
            </div>
            </body>
            </html>"
            )
          }
          if (is.null(self$data) ||
              is.null(self$options$item))
          {
            return()
          } else {
            self$results$instructions$setVisible(visible = FALSE)
          }
            # Data wrangling ----
            
            data = self$data
            
            if (is.null(self$options$group)) {
              groups <- FALSE
            } else {
              groups <- data[2:nrow(data), self$options$group]
            }
            
            # if (is.null(self$options$key)){
            key = data[1, self$options$item]
            data = data[2:nrow(data), self$options$item]
            # } else {
            #     key = data[1:length(self$options$item), self$options$key]
            #     data = data[, self$options$item]
            # }
            
            format <- switch(
              self$options$format,
              formatMC = 1,
              formatPartial = 2,
              formatNominal = 3
            )
            
            if (format == 3) {
              SubRank = data[, self$options$SubRank]
            } else {
              SubRank = NULL
            }
            
            thetadist <-
              as.list(unlist(strsplit(self$options$thetadist, ", ")))
            thetadist[2:length(thetadist)] <-
              as.numeric(thetadist[2:length(thetadist)])
            
            # Functions ----
            
            runResults <- function() {
              results <-
                KernSmoothIRT::ksIRT(
                  responses = data,
                  key = key,
                  format = format,
                  kernel = self$options$kernel,
                  itemlabels = self$options$item,
                  # weights = weights, # The ksIRT argument requires a matrix per item. Unsure how to implement in jamovi
                  miss = self$options$miss,
                  NAweight = self$options$NAweight,
                  # evalpoints = self$options$evalpoints, # The ksIRT argument requires a vector of numbers. Unsure how to implement in jamovi
                  nevalpoints = self$options$nevalpoints,
                  bandwidth = self$options$bandwidth,
                  RankFun = match.fun(self$options$RankFun),
                  SubRank = SubRank,
                  thetadist = thetadist,
                  groups = groups
                )
              return(results)
            }
            
            # States ----
            
            # Frequency Table for items by option, Sample size, including NA
            # OCC DIF | Text selection for option
            # Pairwise expected Scores DIF
            
            # Results table ----
            
            resultState <- self$results$resTable$state
            
            if (!is.null(resultState)) {
              # ... populate the table from the state
            } else {
                # ... create the table and the state
                resultState <- runResults()
                table <- self$results$resTable
                x <-
                  KernSmoothIRT:::print.ksIRT(resultState)
                for (i in 1:nrow(x)) {
                  table$setRow(
                    rowNo = i,
                    values = list(
                      Item = x$Item[i],
                      Correlation = x$Correlation[i]
                    )
                  )
                }
                if (self$options$resTable) {
                  table$setVisible(visible = TRUE)
                }
            }

            # Test level plots ----
            # Expected ----
            if (self$options$testPlotExpected) {
              if (self$results$testPlotExpected$isNotFilled()) {
                expectedPlotResults <- self$results$testPlotExpected
                expectedPlotResults$setState(resultState)
              }
            }
            
            if (self$options$testPlotExpectedDIF) {
              if (self$results$testPlotExpectedDIF$isNotFilled()) {
                expectedPlotDIFResults <- self$results$testPlotExpectedDIF
                expectedPlotDIFResults$setState(resultState)
              }
            }
            
            # Density ----
            
            if (self$options$testPlotDensity) {
              if (self$results$testPlotDensity$isNotFilled()) {
                densityPlotResults <- self$results$testPlotDensity
                densityPlotResults$setState(resultState)
              }
            }
            
            if (self$options$testPlotDensityDIF) {
              if (self$results$testPlotDensityDIF$isNotFilled()) {
                densityPlotDIFResults <- self$results$testPlotDensityDIF
                densityPlotDIFResults$setState(resultState)
              }
            }
            
            
            # SD ----
            
            if (self$options$testPlotSD) {
              if (self$results$testPlotSD$isNotFilled()) {
                sdPlotResults <- self$results$testPlotSD
                sdPlotResults$setState(resultState)
              }
            }
            
            if (self$options$testPlotSDDIF) {
              if (self$results$testPlotSDDIF$isNotFilled()) {
                sdPlotDIFResults <- self$results$testPlotSDDIF
                sdPlotDIFResults$setState(resultState)
              }
            }
            
            
            
            # item level plots ----
            # Pairwise ----
            if (self$options$pairwisePlotsDIF) {
              if (self$results$pairwisePlotsDIF$isNotFilled()) {
                for (i in self$options$itemPlotSupplier) {
                  if (!all(self$options$itemPlotSupplier %in% self$options$item)) {
                    stop(
                      paste0(
                        "Not all items selected to be plotted have been included in the model, please remove: ",
                        self$options$itemPlotSupplier[!self$options$itemPlotSupplier %in% self$options$item]
                      ),
                      call. = FALSE
                    )
                  } else {
                    pairwisePlotDataDIF <-
                      self$results$pairwisePlotsDIF$get(key = i)
                    pairwisePlotDataDIF$setState(list(
                      resultState,
                      item = i,
                      option = self$options$OCCoption
                    ))
                  }
                }
              }
            }
            
            # OCC ----
            if (self$options$itemPlotOCC) {
              if (self$results$occPlots$isNotFilled()) {
                for (i in self$options$itemPlotSupplier) {
                  if (!all(self$options$itemPlotSupplier %in% self$options$item)) {
                    stop(
                      paste0(
                        "Not all items selected to be plotted have been included in the model, please remove: ",
                        self$options$itemPlotSupplier[!self$options$itemPlotSupplier %in% self$options$item]
                      ),
                      call. = FALSE
                    )
                  } else {
                    occPlotData <-
                      self$results$occPlots$get(key = i)
                    occPlotData$setState(list(resultState, item = i))
                  }
                }
              }
            }
            if (self$options$itemPlotOCCDIF) {
              if (self$results$occPlotsDIF$isNotFilled()) {
                for (i in self$options$itemPlotSupplier) {
                  if (!all(self$options$itemPlotSupplier %in% self$options$item)) {
                    stop(
                      paste0(
                        "Not all items selected to be plotted have been included in the model, please remove: ",
                        self$options$itemPlotSupplier[!self$options$itemPlotSupplier %in% self$options$item]
                      ),
                      call. = FALSE
                    )
                  } else {
                    occPlotDataDIF <-
                      self$results$occPlotsDIF$get(key = i)
                    occPlotDataDIF$setState(list(
                      resultState,
                      item = i,
                      option = self$options$OCCoption
                    ))
                  }
                }
              }
            }
            
            # EIS ----
            if (self$options$itemPlotEIS) {
              if (self$results$eisPlots$isNotFilled()) {
                for (i in self$options$itemPlotSupplier) {
                  if (!all(self$options$itemPlotSupplier %in% self$options$item)) {
                    stop(
                      paste0(
                        "Not all items selected to be plotted have been included in the model, please remove: ",
                        self$options$itemPlotSupplier[!self$options$itemPlotSupplier %in% self$options$item]
                      ),
                      call. = FALSE
                    )
                  } else {
                    eisPlotData <-
                      self$results$eisPlots$get(key = i)
                    eisPlotData$setState(list(resultState, item = i))
                  }
                }
              }
            }
            
            if (self$options$itemPlotEISDIF) {
              if (self$results$eisPlotDataDIF$isNotFilled()) {
                for (i in self$options$itemPlotSupplier) {
                  if (!all(self$options$itemPlotSupplier %in% self$options$item)) {
                    stop(
                      paste0(
                        "Not all items selected to be plotted have been included in the model, please remove: ",
                        self$options$itemPlotSupplier[!self$options$itemPlotSupplier %in% self$options$item]
                      ),
                      call. = FALSE
                    )
                  } else {
                    eisPlotDataDIF <-
                      self$results$eisPlotsDIF$get(key = i)
                    eisPlotDataDIF$setState(list(resultState, item = i))
                  }
                }
              }
            }
            
        },
        
        # Test-level plots ----
        
        .testPlotExpected = function(testPlotData, ggtheme, theme, ...) {
          # if (is.null(self$data) | is.null(self$options$item))
          # {
          #   return()
          # }
          #
          # if (is.null(testPlotData$state)) {
          #   return(FALSE)
          # }
          
          plotData <- testPlotData$state
          p <-
            buildExpected(plotData,
                          ggtheme,
                          theme,
                          ...)
          
          print(p)
          TRUE
        },
        
        .testPlotExpectedDIF = function(testPlotData, ggtheme, theme, ...) {
          # if (is.null(self$data) | is.null(self$options$item))
          # {
          #   return()
          # }
          #
          # if (is.null(testPlotData$state)) {
          #   return(FALSE)
          # }
          
          plotData <- testPlotData$state
          p <-
            buildExpectedDIF(plotData,
                             ggtheme,
                             theme,
                             ...)
          
          print(p)
          TRUE
        },
        
        .testPlotDensity = function(testPlotData, ggtheme, theme, ...) {
          # if (is.null(testPlotData$state)) {
          #   return(FALSE)
          # }
          
          plotData <- testPlotData$state
          
          p <-
            buildDensity(plotData,
                         ggtheme,
                         theme,
                         ...)
          
          print(p)
          TRUE
        },
        
        .testPlotDensityDIF = function(testPlotData, ggtheme, theme, ...) {
          # if (is.null(testPlotData$state)) {
          #   return(FALSE)
          # }
          
          plotData <- testPlotData$state
          
          p <-
            buildDensityDIF(plotData,
                            ggtheme,
                            theme,
                            ...)
          
          print(p)
          TRUE
        },
        
        
        .testPlotSD = function(testPlotData, ggtheme, theme, ...) {
          #
          # if (is.null(testPlotData$state)) {
          #   return(FALSE)
          # }
          
          plotData <- testPlotData$state
          
          p <-
            buildSD(plotData,
                    ggtheme,
                    theme,
                    ...)
          
          print(p)
          TRUE
        },
        
        .testPlotSDDIF = function(testPlotData, ggtheme, theme, ...) {
          #
          # if (is.null(testPlotData$state)) {
          #   return(FALSE)
          # }
          
          plotData <- testPlotData$state
          
          p <-
            buildSDDIF(plotData,
                       ggtheme,
                       theme,
                       ...)
          
          print(p)
          TRUE
        },
        # Item-level plots ----
        
        .occPlot = function(itemPlotData, ggtheme, theme, ...) {
          if (is.null(itemPlotData$state)) {
            return(FALSE)
          }
          
          plotData <- itemPlotData$state[[1]]
          item = itemPlotData$state[[2]]
          
          p <-
            buildOCC(plotData,
                     item = item,
                     ggtheme,
                     theme,
                     ...)
          
          print(p)
          TRUE
        },
        
        .occPlotDIF = function(itemPlotData, ggtheme, theme, ...) {
          if (is.null(itemPlotData$state)) {
            return(FALSE)
          }
          
          plotData <- itemPlotData$state[[1]]
          item = itemPlotData$state[[2]]
          option = itemPlotData$state[[3]]
          
          p <-
            buildOCC(plotData,
                     item = item,
                     option = option,
                     ggtheme,
                     theme,
                     ...)
          
          print(p)
          TRUE
        },
        
        .eisPlot = function(itemPlotData, ggtheme, theme, ...) {
          if (is.null(itemPlotData$state)) {
            return(FALSE)
          }
          
          plotData <- itemPlotData$state[[1]]
          item = itemPlotData$state[[2]]
          
          p <-
            buildEIS(plotData,
                     item = item,
                     alpha = NULL,
                     ggtheme,
                     theme,
                     ...)
          
          print(p)
          TRUE
        },
        
        .eisPlotDIF = function(itemPlotData, ggtheme, theme, ...) {
          if (is.null(itemPlotData$state)) {
            return(FALSE)
          }
          
          plotData <- itemPlotData$state[[1]]
          item = itemPlotData$state[[2]]
          
          p <-
            buildEISDIF(plotData,
                        item = item,
                        alpha = NULL,
                        ggtheme,
                        theme,
                        ...)
          
          print(p)
          TRUE
        },
        
        .pairwisePlotsDIF = function(itemPlotData, ggtheme, theme, ...) {
          if (is.null(itemPlotData$state)) {
            return(FALSE)
          }
          
          plotData <- itemPlotData$state[[1]]
          item = itemPlotData$state[[2]]
          
          p <-
            buildPairwiseDIF(plotData,
                             item = item,
                             alpha = NULL,
                             ggtheme,
                             theme,
                             ...)
          
          print(p)
          TRUE
        }
        
      )
    )
