


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
                    
                    
                    data = self$data
                    
                    if (is.null(self$options$key)){
                        key = data[1, self$options$item]
                        data = data[2:nrow(data), self$options$item]
                    } else {
                        key = data[1:length(self$options$item), self$options$key]
                        data = data[, self$options$item]
                    }

                    format <- switch(self$options$format,
                                     formatMC = 1,
                                     formatPartial = 2, 
                                     formatNominal = 3)
                     
                    if (format == 3){
                      SubRank = data[, self$options$SubRank]
                    } else {
                      SubRank = NULL
                    }
                    
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
                    thetadist = self$options$thetadist,
                    groups = data[, self$options$group]
                    )
                    return(results)
                    }
                    
                    # State Savers ----
                    # resultState <- self$results$text$state
                    # if (!is.null(resultState)) {
                    #     # ... populate the table from the state
                    # } else {
                        # ... create the table and the state
                        resultState <- runResults()
                        self$results$text$setContent(KernSmoothIRT:::print.ksIRT(resultState))
                    # }
                    
                    # self$results$text$setContent(KernSmoothIRT:::print.ksIRT(results))
                    
                    for (i in self$options$itemPlotSupplier){    
                      itemPlotData <-
                          self$results$itemPlots$get(key = self$options$itemPlotSupplier[[i]])
                      itemPlotData$setState(list(resultState, items = list(self$options$itemPlotSupplier)))
                      }
                    }
                },

                .itemPlots = function(itemPlotData, ggtheme, theme, ...) {
                    if (is.null(self$data) | is.null(self$options$item) |
                        is.null(self$options$format))
                         {
                        return()
                    }

                    if (!all(self$options$plotItems %in% self$options$item)) {
                        stop(
                            paste0(
                                "Not all items selected to be plotted have been evaluated, please remove: ",
                                self$options$plotItems[!self$options$plotItems %in% self$options$item]
                            ),
                            call. = FALSE
                        )
                    }

                    if (is.null(itemPlotData$state)) {
                        return(FALSE)
                    }

                    plotData <- itemPlotData$state[[1]]

                    p <-
                        buildPlots(plotData,
                                   ggtheme,
                                   theme,
                                   plottype = "density",
                                   axistype = "scores",
                                   ...)

                    print(p)
                    TRUE
                }
            )
        )
