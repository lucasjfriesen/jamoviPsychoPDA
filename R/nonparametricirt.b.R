


# This file is a generated template, your changes will not be overwritten

nonParametricIRTClass <-
    if (requireNamespace('jmvcore'))
        R6::R6Class(
            "nonParametricIRTClass",
            inherit = nonParametricIRTBase,
            private = list(
                .run = function() {
                    if (is.null(self$data) | is.null(self$options$item) |
                        is.null(self$options$format)) {
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
                    if (is.null(self$data) |
                        is.null(self$options$item) |
                        is.null(self$options$format))
                         {
                        return()
                    } else {
                        self$results$instructions$setVisible(visible = FALSE)
                    
                    # Data wrangling ----
                      
                    data = self$data
                    
                    # if (is.null(self$options$key)){
                        key = data[1, self$options$item]
                        data = data[2:nrow(data), self$options$item]
                    # } else {
                    #     key = data[1:length(self$options$item), self$options$key]
                    #     data = data[, self$options$item]
                    # }

                    format <- switch(self$options$format,
                                     formatMC = 1,
                                     formatPartial = 2, 
                                     formatNominal = 3)
                     
                    if (format == 3){
                      SubRank = data[, self$options$SubRank]
                    } else {
                      SubRank = NULL
                    }
                    
                    thetadist <- as.list(unlist(strsplit(self$options$thetadist, ", ")))
                    thetadist[2:length(thetadist)] <- as.numeric(thetadist[2:length(thetadist)])
                    
                    if (is.null(self$options$group)){
                      groups <- FALSE
                    } else {
                      groups <- data[, self$options$group]
                    }
                    
                    # Functions ----
                    
                    runResults <- function(){
                    results <-
                        KernSmoothIRT::ksIRT(responses = data,
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
                    groups = groups)
                    return(results)
                    }
                    
                    # States ----
                    
                    # resultState <- self$results$text$state
                    # if (!is.null(resultState)) {
                    #     # ... populate the table from the state
                    # } else {
                        # ... create the table and the state
                        resultState <- runResults()
                        x <- KernSmoothIRT:::print.ksIRT(resultState)
                        self$results$text$setState(x)
                    # }
                    
                    # self$results$text$setContent(KernSmoothIRT:::print.ksIRT(results))
                    
                    for (i in self$options$itemPlotSupplier){
                      if (!all(self$options$itemPlotSupplier %in% self$options$item)) {
                        stop(
                          paste0(
                            "Not all items selected to be plotted have been included in the model, please remove: ",
                            self$options$itemPlotSupplier[!self$options$itemPlotSupplier %in% self$options$item]
                          ),
                          call. = FALSE
                        )
                      }
                      
                    occPlotData <-
                        self$results$occPlots$get(key = i)
                    occPlotData$setState(list(resultState, item = i))
                    
                    eisPlotData <-
                      self$results$eisPlots$get(key = i)
                    eisPlotData$setState(list(resultState, item = i))
                    }
                        # Expected
                    if (self$options$testPlotExpected) {
                      expectedPlotResults <- self$results$testPlotExpected
                      expectedPlotResults$setState(resultState)
                    }
                        # Density
                    if (self$options$testPlotDensity) {
                      densityPlotResults <- self$results$testPlotDensity
                      densityPlotResults$setState(resultState)
                    }
                    if (self$options$testPlotDensityDIF){
                      densityPlotDIFResults <- self$results$testPlotDensityDIF
                      densityPlotDIFResults$setState(resultState)
                    }
                        # SD
                    if (self$options$testPlotSD) {
                      sdPlotResults <- self$results$testPlotSD
                      sdPlotResults$setState(resultState)
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
                }
            )
        )
