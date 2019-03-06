
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
              frank = "Place the things in the places"
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
          
          alpha <- self$options$alpha
          
          # self$results$debug$setContent(nSims)
          
          retroDesign <- function(hypTrueCor,
                                  observedCor,
                                  observedSE,
                                  alpha,
                                  df,
                                  nSims = 100000) {
            # D <- abs((hypTrueCor - observedCor)/observedSE)
            D <- abs(hypTrueCor)
            z <- qt(1-alpha/2, df)
            p.hi <- 1 - pt(z-D/observedSE, df)
            p.lo <- pt(-z-D/observedSE, df)
            power <- p.hi + p.lo
            typeS <- p.lo/power
            estimate <- D + observedSE*rt(nSims,df)
            significant <- abs(estimate) > observedSE*z
            typeM <- mean(abs(estimate)[significant])/D
            return(list(power=power, typeS=typeS, typeM=typeM))
            # t <- qt(1-alpha/2, df)
            # p.hi <- 1 - pt(t - (D - nullCor) / observedSE, df)
            # p.lo <- pt(-t - (D - nullCor) / observedSE, df)
            # power <- p.hi + p.lo
            # typeS <- p.lo/power
            # 
            # tStatistic <- ((D + observedSE*rt(nSims,df)) - nullCor) / observedSE
            # 
            # significant <- abs(tStatistic) > t#*observedSE
            # typeM <- mean(abs(tStatistic)[significant])/D
            # 
            # 
            # return(list(power=power, typeS=typeS, typeM=typeM))
          }
          
          results <- matrix(ncol = 4, nrow = nrow(data))
          colnames(results) <- c("hypTrueCor", "typeS", "typeM", "power")
          
          for (i in 1:nrow(results)){
            resultsRow <- retroDesign(hypTrueCor[i], # Confirm this | | assumption
                                  observedCor[i],
                                  observedSE[i],
                                  alpha,
                                  df[i])
            results[i, 1] <- hypTrueCor[i]
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
                hypTrueCorLabel = as.character(results[i, "hypTrueCor"]),
                obsCor = observedCor[i],
                typeS = results[i,"typeS"],
                typeM = results[i,"typeM"],
                power = results[i,"power"]
              )
            )
          }
        })
)
