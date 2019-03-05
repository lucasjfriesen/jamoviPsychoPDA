
# This file is a generated template, your changes will not be overwritten

ttestCorClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ttestCorClass",
    inherit = ttestCorBase,
    private = list(
        .run = function() {
          
          if (is.null(self$options$labelVar) | is.null(self$options$hypTrueCor) | is.null(self$options$n) | (is.null(self$options$observedCor) & is.null(self$options$observedSE))){
            return()
          }
          
          data <- self$data
          
          labels <- as.character(data[, self$options$labelVar])
          
          hypTrueCor <- data[, self$options$hypTrueCor]
          
          observedCor <- data[, self$options$observedCor]
          
          df <- data[, self$options$n] - 2
          
          nullCor <- self$options$nullCor
          
          # self$results$debug$setContent(as.character(labels))
          
          if (!is.null(self$options$observedSE)){
            observedSE <- data[, self$options$observedSE]
          } else {
            observedSE <- sqrt((1 - observedCor^2)/df)
          }
          
          nSims <- self$options$nSims
          
          alpha <- self$options$alpha
          
          retroDesign <- function(hypTrueCor,
                                  observedSE,
                                  alpha,
                                  df,
                                  nSims) {
            t <- qt(1-alpha/2, df)
            p.hi <- 1 - pt(t - (hypTrueCor - nullCor) / observedSE, df)
            p.lo <- pt(-t - (hypTrueCor - nullCor) / observedSE, df)
            power <- p.hi + p.lo
            typeS <- p.lo/power
            
            tStatistic <- ((hypTrueCor + observedSE*rt(nSims,df)) - nullCor) / observedSE
            
            significant <- abs(tStatistic) > t#*observedSE
            typeM <- mean(abs(tStatistic)[significant])/hypTrueCor
            
            return(list(power=power, typeS=typeS, typeM=typeM))
          }
          
          results <- matrix(ncol = 4, nrow = nrow(data))
          colnames(results) <- c("hypTrueCor", "typeS", "typeM", "power")
          
          for (i in 1:nrow(results)){
            resultsRow <- retroDesign(hypTrueCor[i], # Confirm this | | assumption
                                  observedSE[i],
                                  alpha,
                                  df[i],
                                  nSims)
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
                typeS = results[i,"typeS"],
                typeM = results[i,"typeM"],
                power = results[i,"power"]
              )
            )
          }
        })
)
