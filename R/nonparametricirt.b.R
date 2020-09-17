


# This file is a generated template, your changes will not be overwritten

nonParametricIRTClass <-
    if (requireNamespace('jmvcore'))
        R6::R6Class(
            "nonParametricIRTClass",
            inherit = nonParametricIRTBase,
            private = list(
                .run = function() {
                    self$results$debug$setContent(list(is.null(self$data) , is.null(self$options$responses) ,
                                                      is.null(self$options$format) ,
                                                      is.null(self$options$key)))
                    if (is.null(self$data) | is.null(self$options$responses) |
                        is.null(self$options$format) |
                        is.null(self$options$key)) {
                        self$results$instructions$setContent(
                            "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>Welcome to PsychoPDA's Non-Parametric Item Response Theory analysis. To get started:</p>
            <ol>
            <li>Place items to be analyzed 'Responses' slot.<br /><br /></li>
            <li>Place the coluimn containing the item key in the 'Key' slot.<br /><br /></li>
            <li>[<em>Optional</em>] Indicate any further specifications.<br /><br /></li>
            </ol>
            <p>If you encounter any errors, or have questions, please see the <a href='https://lucasjfriesen.github.io/jamoviPsychoPDA_docs/DIF_index.html' target = '_blank'>documentation</a></p>
            </div>
            </body>
            </html>"
                        )
                    }
                    if (is.null(self$data) |
                        is.null(self$options$responses) |
                        is.null(self$options$format) |
                        is.null(self$options$key)) {
                        return()
                    } else {
                        self$results$instructions$setVisible(visible = FALSE)
                    
                    
                    data = self$data
                    
                    results <-
                        KernSmoothIRT::ksIRT(responses = data[, self$options$responses],
                                             key = data[, self$options$key],
                                             format = 1)
                    # format = data[, self$options$format],
                    # kernel = self$options$kernel,
                    # itemlabels = self$options$itemlabels,
                    # weights = self$options$weights,
                    # miss = self$options$miss,
                    # NAweight = self$options$NAweight,
                    # # evalpoints,
                    # # nevalpoints,
                    # bandwidth = self$options$bandwidth,
                    # RankFun = self$options$RankFun,
                    # SubRank = data[, self$options$SubRank],
                    # thetadist = list("norm", 0, 1)
                    # groups = data[, self$options$group],
                    # nsubj = nrow(data)
                    # )
                    
                    self$results$text$setContent(KernSmoothIRT:::print.ksIRT(results))
                    
                    itemPlotData <-
                        self$results$itemPlots$get(key = self$options$plotItems[[1]])
                    itemPlotData$setState(list(results, items = list(self$options$plotItems)))
                    }
                    
                },
                
                .itemPlots = function(itemPlotData, ggtheme, theme, ...) {
                    if (is.null(self$data) | is.null(self$options$responses) |
                        is.null(self$options$format) |
                        is.null(self$options$key)) {
                        return()
                    }
                    
                    if (!all(self$options$plotItems %in% self$options$responses)) {
                        stop(
                            paste0(
                                "Not all items selected to be plotted have been evaluated, please remove: ",
                                self$options$plotItems[!self$options$plotItems %in% self$options$responses]
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
