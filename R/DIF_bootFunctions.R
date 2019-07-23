# Bootstrap GC functions ----
        
        boot.printSE <- function(x,
                                 digits = getOption("digits"),
                                 index = 1L:ncol(boot.out$t),
                                 ...)
        {
          boot.out <- x
          sim <- boot.out$sim
          cl <- boot.out$call
          t <- matrix(boot.out$t[, index], nrow = nrow(boot.out$t))
          allNA <- apply(t, 2L, function(t)
            all(is.na(t)))
          ind1 <- index[allNA]
          index <- index[!allNA]
          t <- matrix(t[,!allNA], nrow = nrow(t))
          rn <- paste("t", index, "*", sep = "")
          if (length(index) == 0L)
            op <- NULL
          else if (is.null(t0 <- boot.out$t0)) {
            if (is.null(boot.out$call$weights))
              op <- cbind(apply(t, 2L, mean, na.rm = TRUE),
                          sqrt(apply(t, 2L, function(t.st)
                            var(t.st[!is.na(t.st)]))))
            else {
              op <- NULL
              for (i in index)
                op <-
                  rbind(op, imp.moments(boot.out, index = i)$rat)
              op[, 2L] <- sqrt(op[, 2])
            }
            dimnames(op) <- list(rn, c("mean", "std. error"))
            return(op)
          }
          else {
            t0 <- boot.out$t0[index]
            if (is.null(boot.out$call$weights)) {
              op <- cbind(t0, apply(t, 2L, mean, na.rm = TRUE) - t0,
                          sqrt(apply(t, 2L, function(t.st)
                            var(t.st[!is.na(t.st)]))))
              dimnames(op) <-
                list(rn, c("original", " bias  ", " std. error"))
              return(op)
            }
            else {
              op <- NULL
              for (i in index)
                op <-
                  rbind(op, imp.moments(boot.out, index = i)$rat)
              op <- cbind(t0, op[, 1L] - t0, sqrt(op[, 2L]),
                          apply(t, 2L, mean, na.rm = TRUE))
              dimnames(op) <- list(rn, c("original", " bias  ",
                                         " std. error", " mean(t*)"))
              return(op)
            }
          }
          
        }
        
        boot.retroStat <-
          function(DATA, ind, coefficients = FALSE, type) {
            ITEM <- (DATA[, 1])
            SCORE <- (DATA[, 3])
            GROUP <- (DATA[, 2])
            n <- nrow(DATA)
            
            m0 <- switch(
              type,
              both = glm(ITEM[ind] ~ SCORE * GROUP,
                         family = "binomial"),
              udif = glm(ITEM[ind] ~ SCORE +
                           GROUP, family = "binomial"),
              nudif = glm(ITEM[ind] ~ SCORE *
                            GROUP, family = "binomial")
            )
            
            m1 <-
              switch(
                type,
                both = glm(ITEM[ind] ~ SCORE, family = "binomial"),
                udif = glm(ITEM[ind] ~ SCORE, family = "binomial"),
                nudif = glm(ITEM[ind] ~
                              SCORE + GROUP, family = "binomial")
              )
            
            deltaNagR2 <- function(m0, m1, n) {
              R2cox0 <-  1 - exp((m0$deviance - m0$null.deviance) / n)
              R2nag0 <-  R2cox0 / (1 - exp((-m0$null.deviance) / n))
              
              R2cox1 <-  1 - exp((m1$deviance - m1$null.deviance) / n)
              R2nag1 <-  R2cox1 / (1 - exp((-m1$null.deviance) / n))
              deltaNagR2 <- R2nag0 - R2nag1
              return(deltaNagR2)
            }
            
            if (coefficients) {
              res <- coefficients(m0)
            } else {
              res <- deltaNagR2(m0, m1, n)
            }
            
            return(res)
          }
        
        boot.qEmp <- function(empFn, x) {
          quantile(empFn, x)
        }
        
        boot.pEmp <- function(empFn, x) {
          empFn(x)
        }
        
        boot.dEmp <- function(empFn, qEmp, dPoint, values) {
          dens <- density(values)
          i <- dens$x[which.min(abs(dens$x - dPoint))]
          dens_x <- dens$y[dens$x == i]
          dens_x
        }
        
        boot.empDist <- function(DATA, R, type, coefficients) {
          # Get bootstrapped distribution
          myBoot <- boot(DATA, boot.retroStat, R = R, type = type, coefficients = coefficients)
          if (!all(!is.na(myBoot$t))) {
            self$results$gcTable$setNote(
              key = colnames(DATA)[1],
              note = paste0(
                "WARNING: ",
                length(myBoot$t) - length(na.omit(myBoot$t)),
                " of ",
                length(myBoot$t),
                " simulations did not converge for item '",
                colnames(DATA)[1],
                "'"
              )
            )
            myBoot$t <- na.omit(myBoot$t)
          }
          return(myBoot)
        }