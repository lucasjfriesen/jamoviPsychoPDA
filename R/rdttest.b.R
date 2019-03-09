
# This file is a generated template, your changes will not be overwritten

rdTTestClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "rdTTestClass",
    inherit = rdTTestBase,
    private = list(
        .run = function() {
          
          if (is.null(self$options$labelVar) | is.null(self$options$hypTrueEff) | (is.null(self$options$observedP) & is.null(self$options$observedSE))){
            self$results$rdTTest$setVisible(visible = FALSE)
            self$results$instructions$setVisible(visible = TRUE)
            self$results$instructions$setRow(rowNo = 1, value = list(
              frank = "1) Label"
            ))
            self$results$instructions$setRow(rowNo = 2, value = list(
              frank = "2) Observed Effect in SD units"
            ))
            self$results$instructions$setRow(rowNo = 3, value = list(
              frank = "3) Hypothesized True Effect in SD units"
            ))
            self$results$instructions$setRow(rowNo = 4, value = list(
              frank = "4) ONE OF Observed Standard Error OR Observed P-Value"
            ))
            return()
          }
          
          data <- self$data
          data <- na.omit(data)
          
          labels <- as.character(data[, self$options$labelVar])
          
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
          
          if (!is.null(self$options$n)){
            df <- data[, self$options$n] - 1
          } else { 
            df <- Inf
            }
          nSims <- self$options$nSims
          alpha <- self$options$alpha
          
          if (!is.null(observedP)){
            z <- qt(1-alpha/2, df)
            observedSE <- observedP/z
          }
          
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
            estimate <- D + observedSE*rt(nSims,df)
            significant <- abs(estimate) > observedSE*z
            typeM <- mean(abs(estimate)[significant])/D
            return(list(power=power, typeS=typeS, typeM=typeM))
          }
          
          results <- matrix(ncol = 4, nrow = nrow(data))
          colnames(results) <- c("D", "typeS", "typeM", "power")
          
          for (i in 1:nrow(results)){
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
          
          # Sensitivity ----
          
          sensRange <- seq(-1,1, length.out = 50)
          sensRes <- matrix(ncol = 5, nrow = nrow(data)*length(sensRange))
          
          for (i in 1:nrow(data)){
            hypTrueEffSens <- hypTrueEff[i] + (observedSE[i] * sensRange)
            for (j in 1:length(sensRange)){
              res <- retroDesign(abs(hypTrueEffSens[j]),
                                    observedSE[i],
                                    alpha,
                                    df[i],
                                    nSims)
              sensRes[((i-1)*length(sensRange))+j, 1] <- hypTrueEff[i]
              sensRes[((i-1)*length(sensRange))+j, 2] <- hypTrueEffSens[j]
              sensRes[((i-1)*length(sensRange))+j,3:5] <- unlist(res)
            }
          }

          plotData <- data.frame(cbind(rep(sensRange, nrow(data)), sensRes))
          
          colnames(plotData) <- c("SD", "hypTrueGroup", "hypTrueEffSens","power", "typeS", "typeM")
          
          image <- self$results$sensPlot
          image$setState(plotData)
          
          # Results ----
          
          table <- self$results$rdTTest
          
          for (i in 1:nrow(results)){
            table$addRow(
              rowKey = i,
              values = list(
                label = labels[[i]],
                D = as.character(results[i, "D"]),
                typeS = results[i,"typeS"],
                typeM = results[i,"typeM"],
                power = results[i,"power"]
              )
            )
          }
        },
        .plot=function(image, ...) {
          plotData <- image$state
          self$results$debug$setContent(plotData)
          plot <- ggplot(plotData) +
            geom_line(aes(x=hypTrueEffSens, y = typeS, colour = "typeS")) +
            geom_line(aes(x=hypTrueEffSens, y = typeM, colour = "typeM")) +
            geom_line(aes(x=hypTrueEffSens, y = power, colour = "power")) +
            scale_y_continuous(name = "Type-M", sec.axis = dup_axis(trans = ~ .)) +
            theme_classic() +
            facet_wrap(~hypTrueGroup, scales = "free")

          print(plot)
          TRUE
        }
        )
)
