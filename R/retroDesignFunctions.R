# retroDesign ----
        retroDesign.nagR2 <- function(hypTrueEff, myBoot, alpha, sigOnly) {
          rdRes <- matrix(0, nrow = 1, ncol = 4)
          rdRes[1, 1] <- myBoot$t0
          
          # se of empirical distribution
          rdRes[1, 4] <- observedSE <- boot.printSE(myBoot)[[3]]
          
          # Observed R^2
          D <- myBoot$t0
          
          # Empirical cumulative density function on the bootstrapped data
          qUpper <- ecdf(myBoot$t)
          if (sigOnly) {
            # Quantile matching the upper 1 - alpha in the emp. dist.
            qUpper <- quantile(qUpper,  1 - (alpha))
          } else {
            # Quantile matching the observed value
            qUpper <- boot.qEmp(qUpper, boot.pEmp(qUpper, D))
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
        
        retroDesign.coefficients <- function(hypTrueEff, coefficient, coefficientsSE, alpha, df, sigOnly) {
          D <- abs(hypTrueEff - coefficient)
          lambda <- D / coefficientsSE
          
          if (sigOnly) {
            # Quantile matching the upper 1 - alpha in the emp. dist.
            z <- qt(1 - (alpha), df)
          } else {
            # Quantile matching the observed value
            z <- qt(pt(lambda, df = df), df = df)
          }
          
          exp1S <- pt(-z - lambda, df)
          exp2S <- pt(z - lambda, df)
          
          power <- exp1S + 1 - exp2S
          typeS <- exp1S / (exp1S + 1 - exp2S)
          
          # typeM error rate
          exp1M <- dt(lambda + z, df)
          exp2M <- dt(lambda - z, df)
          exp3M <- pt(lambda + z, df)
          exp4M <- pt(lambda - z, df)
          
          typeM <- (exp1M + exp2M +
                      lambda * (exp3M + exp4M - 1)) /
            (lambda * (1 - exp3M + exp4M))
          return(list("obsEff" = lambda, "typeS"=typeS, "typeM"=typeM, "power"=power, "label"=hypTrueEff, "bootSE"=coefficientsSE))
        }
        