
# This file is a generated template, your changes will not be overwritten

rdTTestClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "rdTTestClass",
    inherit = rdTTestBase,
    private = list(
        .run = function() {
          
          data <- self$data
          hypTrueEff <- data[, self$options$hypTrueEff]
          
          if (!is.null(self$options$observedSE)){
            observedSE <- data[, self$options$observedSE]
          } else {
            observedSE <- NULL
          }
          if (!is.null(self$options$observedP)){
            observedP <- data[, self$options$observedP]
          } else {
            observedP <- NULL
          }
          
          if (!is.null(self$options$df)){
            df <- data[, self$options$df]
          } else { 
            df <- Inf
            }
          nSims <- self$options$nSims
          alpha <- self$options$alpha
          
          if (!is.null(observedP)){
            z <- qt(1-alpha/2, df)
            observedSE <- observedP/z
          }
          
          retroDesign <- function(hypTrueEff,
                                  observedSE,
                                  alpha,
                                  df,
                                  nSims) {
            z <- qt(1-alpha/2, df)
            p.hi <- 1 - pt(z-hypTrueEff/observedSE, df)
            p.lo <- pt(-z-hypTrueEff/observedSE, df)
            power <- p.hi + p.lo
            typeS <- p.lo/power
            estimate <- hypTrueEff + observedSE*rt(nSims,df)
            significant <- abs(estimate) > observedSE*z
            typeM <- mean(abs(estimate)[significant])/hypTrueEff
            return(list(power=power, typeS=typeS, typeM=typeM))
          }
          
          results <- matrix(ncol = 4, nrow = nrow(data))
          colnames(results) <- c("hypTrueEff", "typeS", "typeM", "power")
          
          for (i in 1:nrow(results)){
            resultsRow <- retroDesign(abs(hypTrueEff[i]), # Confirm this | | assumption
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
          
          for (i in 1:nrow(results)){
            table$addRow(
              rowKey = i,
              values = list(
                hypTrueEffLabel = results[i, "hypTrueEff"],
                typeS = results[i,"typeS"],
                typeM = results[i,"typeM"],
                power = results[i,"power"]
              )
            )
          }
        })
)
