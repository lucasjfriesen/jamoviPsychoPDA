# retroDesign ----
        retroDesign <- function(hypTrueEff, myBoot) {
          alpha <- self$options$alpha
          rdRes <- matrix(0, nrow = 1, ncol = 4)
          rdRes[1, 1] <- myBoot$t0
          
          # se of empirical distribution
          rdRes[1, 4] <- observedSE <- print.bootSE(myBoot)[[3]]
          
          # Observed R^2
          D <- myBoot$t0
          
          # Empirical cumulative density function on the bootstrapped data
          qUpper <- ecdf(myBoot$t)
          if (self$options$designAnalysisSigOnly) {
            # Quantile matching the upper 1 - alpha in the emp. dist.
            qUpper <- quantile(qUpper,  1 - (alpha))
          } else {
            # Quantile matching the observed value
            qUpper <- qEmp(qUpper, pEmp(qUpper, D))
          }
          
          ## shifts distribution by the difference between the observed effect size and the empirical effect size
          myBoot.Shifted <- ecdf(myBoot$t + hypTrueEff)
          rdRes[1, 3] <- power <- 1 - myBoot.Shifted(qUpper)
          # typeM error rate via Estimation
          estimate <-
            D + sample(myBoot$t, replace = T, size = 10000)
          significant <- estimate > qUpper
          rdRes[1, 2] <-
            typeMError <- mean(estimate[significant]) / D
          return(rdRes)
        }
        
        retroDesignCoefficients <- function(hypTrueEff, myBoot) {
          alpha <- self$options$alpha
          rdRes <- matrix(0, nrow = 1, ncol = 4)
          rdRes[1, 1:2] <- myBoot$t0[3:4]
          
          # se of emp. dist.
          observedSE <- print.bootSE(myBoot)[3:4,3]
          rdRes[1, 5:6] <- observedSE
          
          # D <- abs(myBoot$t0 - hypTrueEff/observedSE)
          D <- abs(myBoot$t0 - hypTrueEff)
          lambda <- D / observedSE
          self$results$debug$setContent(list(D,print.bootSE(myBoot)))
          # Quantile matching the upper 1 - alpha in the emp. dist.
          empFn <- ecdf(myBoot$t)
          z <- quantile(empFn,  1 - (alpha))
          
          exp1P <- pEmp(empFn,-z - lambda)
          exp2P <- pEmp(empFn, z - lambda)
          
          power <- exp1P + 1 - exp2P
          typeS <- exp1P / (exp1P + 1 - exp2P)
          # rdRes[1, 4] <- typeS
          rdRes[1, 3] <- power
          
          # typeM error rate
          exp1M <- dEmp(empFn, qEmp, lambda + z, myBoot$t)
          exp2M <- dEmp(empFn, qEmp, lambda - z, myBoot$t)
          exp3M <- pEmp(empFn, lambda + z)
          exp4M <- pEmp(empFn, lambda - z)
          
          typeM <- (exp1M + exp2M +
                      lambda * (exp3M + exp4M - 1)) /
            (lambda * (1 - exp3M + exp4M))
          
          rdRes[1, 2] <- typeM
          return(rdRes)
        }