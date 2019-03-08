
# This file is a generated template, your changes will not be overwritten

ttestCorClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ttestCorClass",
    inherit = ttestCorBase,
    private = list(
        .run = function() {
          
          if (is.null(self$options$labelVar) | is.null(self$options$hypTrueCor) | is.null(self$options$n) | (is.null(self$options$observedCor) & is.null(self$options$observedSE))){
            self$results$rdTTestCor$setVisible(visible = FALSE)
            self$results$instructions$setVisible(visible = TRUE)
            self$results$instructions$setRow(rowNo = 1, value = list(
              frank = "1) Label"
            ))
            self$results$instructions$setRow(rowNo = 2, value = list(
              frank = "2) Hypothesized True Correlation"
            ))
            self$results$instructions$setRow(rowNo = 3, value = list(
              frank = "3) Sample size (N)"
            ))
            self$results$instructions$setRow(rowNo = 4, value = list(
              frank = "4) ONE OF Observed Correlation OR Observed Standard Error"
            ))
            return()
          }
          
          data <- self$data
          
          labels <- as.character(data[, self$options$labelVar])
          
          hypTrueCor <- data[, self$options$hypTrueCor]
          
          observedCor <- data[, self$options$observedCor]
          
          df <- data[, self$options$n] - 2
          
          if (!is.null(self$options$observedSE)){
            observedSE <- data[, self$options$observedSE]
          } else {
            observedSE <- sqrt((1 - observedCor^2)/df)
          }
          
          D <- (observedCor - hypTrueCor)/observedSE
          
          alpha <- self$options$alpha
          
          retroDesign <- function(D,
                                  observedSE,
                                  alpha,
                                  df,
                                  nSims) {
            
            z <- qt(1-alpha/2, df)
            p.hi <- 1 - pt(z-D/observedSE, df)
            p.lo <- pt(-z-D/observedSE, df)
            power <- p.hi + p.lo
            typeS <- p.lo/power
            estimate <- D + observedSE*rt(n = nSims, df = df)
            significant <- abs(estimate) > observedSE*z
            typeM <- mean(abs(estimate)[significant])/D
            return(list(power=power, typeS=typeS, typeM=typeM))
          }
          
          results <- matrix(ncol = 4, nrow = nrow(data))
          colnames(results) <- c("D", "typeS", "typeM", "power")
          
          for (i in 1:nrow(results)){
            resultsRow <- retroDesign(abs(D[i]),
                                  observedSE[i],
                                  alpha,
                                  df[i],
                                  nSims = 10000)
            results[i, 1] <- D[i]
            results[i, 2] <- resultsRow$typeS
            results[i, 3] <- resultsRow$typeM
            results[i, 4] <- resultsRow$power
            
          }
          
          table <- self$results$rdTTestCor
          
          for (i in 1:nrow(results)){
            table$addRow(
              rowKey = i,
              values = list(
                label = labels[[i]],
                obsCor = as.character(observedCor[i]),
                obsN = df[i] + 2,
                hypTrueCor = as.character(hypTrueCor[i]),
                obsSE = observedSE[i],
                hypTrueEffSD = results[i, "D"],
                typeS = results[i,"typeS"],
                typeM = results[i,"typeM"],
                power = results[i,"power"]
              )
            )
          }
        })
)
