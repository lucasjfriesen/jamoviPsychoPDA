


# This file is a generated template, your changes will not be overwritten

ordinalReliabilityClass <-
    if (requireNamespace('jmvcore'))
        R6::R6Class(
            "ordinalReliabilityClass",
            inherit = ordinalReliabilityBase,
            private = list(
                .run = function() {
                    if (is.null(self$options$items)) {
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
                    <p>Welcome to PsychoPDA's Ordinal Reliability analysis To get started:</p>
                    <ol>
                    <li>Input the 'Items'<br /><br /></li>
                    </ol>
                    <li>Select which reliability computations you would like to view<br /><br /></li>
                    <p>If you encounter any errors, or have questions, please see the <a href='https://lucasjfriesen.github.io/jamoviPsychoPDA_docs/ordinalReliability.html' target = '_blank'>documentation</a>.</p>
                    </div>
                    </body>
                    </html>"
                        )
                        return()
                    } else {
                      self$results$instructions$setVisible(visible = FALSE)
                    }
                    
                    data = self$data
                    
                    # if (length(self$options$items < 2)){
                      # stop("More than one item is required to compute reliability")
                    # } else {
                      items = data[, self$options$items]
                    # }
                    # groups = data[, self$options$groups]
                    
                    ordinalRhos <- function(data) {
                        polychoricRho <- psych::polychoric(data)$rho
                        ordinalAlpha <- psych::alpha(polychoricRho)
                        ordinalOmega <-
                            psych::omega(polychoricRho, plot = FALSE)
                        ordinalGuttman <-
                            psych::splitHalf(polychoricRho)
                        
                        # Theta coefficient
                        # theta = [p/(p-1)]*[1-(1/theta[1])]
                        # Zumbo et al 2007
                        
                        numFactors <-
                            dim(ordinalOmega$schmid$sl)[2] - 3
                        eigenValues <-
                            diag(t(ordinalOmega$schmid$sl[, 1:numFactors]) %*% ordinalOmega$schmid$sl[, 1:numFactors])
                        ordinalTheta <-
                            data.frame(ordinalTheta = (ncol(data) / (ncol(data) - 1)) * (1 - (1 / max(
                                eigenValues
                            ))))
                        
                        # polychoricRho[upper.tri(polychoricRho)] <-
                        #     NA
                        
                        return(
                            list(
                                "ordinalAlpha" = ordinalAlpha,
                                "ordinalOmega" = ordinalOmega,
                                "ordinalGuttman" = ordinalGuttman,
                                "ordinalTheta" = ordinalTheta,
                                "polychoricRho" = polychoricRho
                            )
                        )
                    }
                    
                    rhos <- ordinalRhos(items)
                    # State functions ----
                    calculateSummaryTableAlpha <- function(rhos){
                        self$results$summaryTableAlpha$setRow(
                            rowNo = 1,
                            value = list(
                                raw_alpha = rhos$ordinalAlpha$total$raw_alpha,
                                std.alpha = rhos$ordinalAlpha$total$std.alpha,
                                G6 = rhos$ordinalAlpha$total$`G6(smc)`,
                                average_r = rhos$ordinalAlpha$total$average_r,
                                SN = rhos$ordinalAlpha$total$`S/N`,
                                median_r = rhos$ordinalAlpha$total$median_r
                            )
                        )
                    }
                    calculateSummaryTableGuttman <- function(rhos){
                    self$results$summaryTableGuttman$setRow(
                        rowNo = 1,
                        value = list(
                            maxSHR = rhos$ordinalGuttman$maxrb,
                            guttmanL6 = rhos$ordinalGuttman$lambda6,
                            avgSHR = rhos$ordinalGuttman$meanr,
                            alpha = rhos$ordinalGuttman$alpha,
                            minSHR = rhos$ordinalGuttman$minrb
                        )
                    )
                    }
                    calculateSummaryTableOmega <- function(rhos){
                    self$results$summaryTableOmega$setRow(
                        rowNo = 1,
                        value = list(
                            omega_h = rhos$ordinalOmega$omega_h,
                            omega.lim = rhos$ordinalOmega$omega.lim,
                            alpha = rhos$ordinalOmega$alpha,
                            omega.tot = rhos$ordinalOmega$omega.tot,
                            G6 = rhos$ordinalOmega$G6
                        )
                    )
                    }
                    calculateSummaryTableTheta <- function(rhos){
                    self$results$summaryTableTheta$setRow(rowNo = 1,
                                                          value = list(ordinalTheta = rhos$ordinalTheta[[1]]))
                    }
                    calculatePolychoricRho <- function(rhos){
                    self$results$polychoricRho$setContent(rhos$polychoricRho)
                    }
                    
                    # Alpha ----
                    summaryTableAlphaState <- self$results$summaryTableAlpha$state
                    if (!is.null(summaryTableAlphaState)) {
                        # ... populate the table from the state
                    } else {
                        summaryTableAlphaState <- calculateSummaryTableAlpha(rhos)
                        self$results$summaryTableAlpha$setState(summaryTableAlphaState)
                    }
                    
                    # Gutmann ----
                    summaryTableGuttmanState <- self$results$summaryTableGuttman$state
                    if (!is.null(self$results$summaryTableGuttman$state)) {
                        # ... populate the table from the state
                    } else {
                        summaryTableGuttmanState <- calculateSummaryTableGuttman(rhos)
                        self$results$summaryTableGuttman$setState(summaryTableGuttmanState)
                            
                    }
                    
                    # Omega ----
                    summaryTableOmegaState <- self$results$summaryTableOmega$state
                    if (!is.null(self$results$summaryTableAlpha$state)) {
                        # ... populate the table from the state
                    } else {
                        summaryTableOmegaState <- calculateSummaryTableOmega(rhos)
                        self$results$summaryTableOmega$setState(self$results$summaryTableOmega$state)
                    }
                    
                    # Theta ----
                    summaryTableThetaState <- self$results$summaryTableTheta$state
                    if (!is.null(self$results$summaryTableTheta$state)) {
                        # ... populate the table from the state
                    } else {
                        summaryTableThetaState <- calculateSummaryTableTheta(rhos)
                        self$results$summaryTableTheta$setState(self$results$summaryTableTheta$state)
                    }
                    
                    # Polychoric table ----
                    polychoricRhoState <- self$results$polychoricRho$state
                    if (!is.null(self$results$summaryTableAlpha$state)) {
                        # ... populate the table from the state
                    } else {
                        polychoricRhoState <- calculatePolychoricRho(rhos)
                        self$results$polychoricRho$setState(self$results$polychoricRho$state)
                    }
                    
                    # Plot data ----
                    if (self$options$omegaPlot){
                        # if (!is.null(groups)){
                        #     for (i in unique(groups)){
                        #         self$results$omegaPlot$addItem(key = i)
                        #     }
                        # }
                        self$results$omegaPlot$addItem(key = "a")
                        image <- self$results$omegaPlot$get(key = "a")
                        plotData <- rhos$polychoricRho
                        image$setState(plotData)
                    }
                    },
            .plot = function(image, ggtheme, theme,...) {
                if (is.null(image$state)){
                    return(FALSE)
                }
                plotData <- image$state
                psych::omega(plotData, plot = TRUE)
                TRUE
            }
            )
        )
