










# Data Wrangling ----

glmDIFClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "glmDIFClass",
    inherit = glmDIFBase,
    private = list(
      .run = function() {
        if (is.null(self$options$group) |
            is.null(self$data) | is.null(self$options$item)) {
          self$results$instructions$setVisible(visible = TRUE)
          self$results$instructions$setRow(
            rowNo = 1,
            value = list(ted = "1) Place items to be assessed for DIF in the 'Item(s) for analysis' slot.")
          )
          self$results$instructions$setRow(
            rowNo = 2,
            value = list(ted = "2) [Optional] Place the remaining measure items in the 'Anchor Items' slot. This is not needed if a Matching Variable is supplied.")
          )
          self$results$instructions$setRow(
            rowNo = 3,
            value = list(ted = "3) [Optional] Place an external matching variable in the 'Matching Variable' slot. Measure total score will be calculated if this option is omitted.")
          )
          self$results$instructions$setRow(
            rowNo = 4,
            value = list(ted = "4) Place the grouping variable in the 'Grouping Variable' slot.")
          )
          return()
        }
        # The full DF
        data <- self$data
        
        # Data frame containing all items selected for analysis
        Data <-
          data.frame(jmvcore::toNumeric(data[, self$options$item]))
        colnames(Data) <- self$options$item
        for (i in 1:length(self$options$item)) {
          if (!all(unique(Data[, i]) %in% c(0, 1, NA))) {
            stop(
              paste(
                "One or more rows contains an invalid value in column: ",
                colnames(Data)[i]
              ),
              ". (Item responses must be one of c(0,1,NA))",
              call. = FALSE
            )
          }
        }
        
        if (is.null(self$options$anchor)) {
          anchor <- NULL
        } else {
          anchor <-
            data.frame(jmvcore::toNumeric(data[, self$options$anchor]))
          colnames(anchor) <- self$options$anchor
        }
        
        # Vector containing grouping data
        group <- data[, self$options$group]
        
        # Vector containing matching data
        match <-
          data.frame(jmvcore::toNumeric(data[, self$options$matchVar]))
        if (length(match) == 0) {
          match <- "score"
        } else {
          colnames(match) <- self$options$matchVar
          match <- unlist(match)
        }
        
        focalName <- levels(group)[1]
        
        groupType <- self$options$groupType
        
        type <- self$options$type
        
        criterion <- self$options$criterion
        
        alpha <- self$options$alpha
        
        purify <- self$options$purify
        
        nIter <- self$options$nIter
        
        pAdjustMethod <- self$options$pAdjustMethod
        
        # DIF ----
        # difLogReg is a starter function to determine whether GLM or LM should be used, as well as QA on the arguments input
        
        difLogistic <-
          function (DATA,
                    group,
                    focalName,
                    anchor = NULL,
                    memberType = "group",
                    match = "score",
                    type = "both",
                    criterion = "LRT",
                    alpha = 0.05,
                    purify = FALSE,
                    nrIter = 10,
                    pAdjustMethod = NULL)
          {            
            
            if (purify & match[1] != "score") {
              stop(
                "purification not allowed when using a custom matching variable. Please remove the custom matching variable or deselect 'Purification'.")
              return()
            }
            internalLog <- function() {
              if (memberType == "group") {
                Group <- rep(0, NROW(DATA))
                Group[group == focalName] <- 1
              }
              else
                Group <- group
              sigThreshold <-
                switch(
                  type,
                  both = qchisq(1 - alpha, 2),
                  udif = qchisq(1 -
                                  alpha, 1),
                  nudif = qchisq(1 - alpha, 1)
                )
              anchorNames <- self$options$anchor
              DF <- ifelse(type == "both", 2, 1)
              
              # Purification == FALSE ----
              
              if (!purify) {
                PROV <- Logistik(
                  DATA,
                  Group,
                  memberType = memberType,
                  match = match,
                  type = type,
                  criterion = criterion,
                  anchor = anchor
                )
                chiSquared <- PROV$stat
                PVAL <- 1 - pchisq(chiSquared, DF)
                deltaR2 <- PROV$deltaR2
                if (max(chiSquared) <= sigThreshold) {
                  DIFitems <- "No DIF item detected"
                }
                else {
                  DIFitems <- (1:NCOL(DATA))[chiSquared > sigThreshold]
                  }
                
                RES <-
                  list(
                    Logistik = chiSquared,
                    p.value = PVAL,
                    deltaR2 = deltaR2,
                    alpha = alpha,
                    thr = sigThreshold,
                    DIFitems = DIFitems,
                    memberType = memberType,
                    match = PROV$match,
                    type = type,
                    pAdjustMethod = pAdjustMethod,
                    adjusted.p = NULL,
                    purification = purify,
                    names = colnames(DATA),
                    anchor.names = anchorNames,
                    criterion = criterion,
                    focalName = focalName,
                    matchScores = PROV$matchScores,
                    ZT = as.character(symnum(
                      deltaR2,
                      c(0, 0.13, 0.26, 1),
                      symbols = c("A", "B", "C"),
                      legend = FALSE
                    )),
                    JG = as.character(symnum(
                      deltaR2,
                      c(0, 0.035, 0.07, 1),
                      symbols = c("A", "B", "C"),
                      legend = FALSE
                    ))
                  )
                
              }
              # Purification == TRUE ----
              else {
                nrPur <- 0
                difPur <- NULL
                noLoop <- FALSE
                logistikRes1 <-
                  Logistik(
                    DATA,
                    Group,
                    memberType = memberType,
                    match = match,
                    anchor = anchor,
                    type = type,
                    criterion = criterion
                  )
                
                chiSquared1 <- logistikRes1$stat
                deltaR2 <- logistikRes1$deltaR2
                if (max(chiSquared1) <= sigThreshold) {
                  DIFitems <- "No DIF item detected"
                  noLoop <- TRUE
                }
                else {
                  dif <- (1:NCOL(DATA))[chiSquared1 > sigThreshold]
                  difPur <- rep(0, length(chiSquared1))
                  difPur[dif] <- 1
                  repeat {
                    if (nrPur >= nrIter) {
                      break
                    } else {
                      nrPur <- nrPur + 1
                      nodif <- NULL
                      if (is.null(dif)) {
                        nodif <- 1:NCOL(DATA)
                      } else {
                        for (i in 1:NCOL(DATA)) {
                          if (sum(i == dif) == 0) {
                            nodif <- cbind(anchor, DATA[i])
                          }
                        }
                      }
                      logistikRes2 <- Logistik(
                        DATA,
                        Group,
                        anchor = nodif,
                        memberType = memberType,
                        match = match,
                        type = type,
                        criterion = criterion
                      )
                      chiSquared2 <- logistikRes2$stat
                      deltaR2 <- logistikRes2$deltaR2
                      if (max(chiSquared2) <= sigThreshold) {
                        dif2 <- NULL
                      } else {
                        dif2 <- (1:NCOL(DATA))[chiSquared2 > sigThreshold]
                        difPur <- rbind(difPur, rep(0, NCOL(DATA)))
                        difPur[nrPur + 1, dif2] <- 1
                      } 
                      
                      if (length(dif) != length(dif2)) {
                        dif <- dif2
                        } else {
                        dif <- sort(dif)
                        dif2 <- sort(dif2)
                        if (sum(dif == dif2) == length(dif)) {
                          noLoop <- TRUE
                          break
                      } else {
                        dif <- dif2
                      }
                    }
                  }
                }
                  logistikRes1 <- logistikRes2
                  chiSquared1 <- chiSquared2
                  PVAL <- 1 - pchisq(chiSquared1, DF)
                  deltaR2 <- deltaR2
                  DIFitems <- (1:NCOL(DATA))[chiSquared1 > sigThreshold]
                }
                if (is.null(difPur) == FALSE) {
                  ro <- co <- NULL
                  for (ir in 1:NROW(difPur))
                    ro[ir] <- paste("Step",
                                    ir - 1, sep = "")
                  for (ic in 1:NCOL(difPur))
                    co[ic] <- paste("Item",
                                    ic, sep = "")
                  rownames(difPur) <- ro
                  colnames(difPur) <- co
                }
                RES <-
                  list(
                    Logistik = chiSquared1,
                    deltaR2 = deltaR2,
                    alpha = alpha,
                    thr = sigThreshold,
                    DIFitems = DIFitems,
                    memberType = memberType,
                    match = logistikRes1$match,
                    type = type,
                    pAdjustMethod = pAdjustMethod,
                    adjusted.p = NULL,
                    purification = purify,
                    nrPur = nrPur,
                    difPur = difPur,
                    convergence = noLoop,
                    names = colnames(DATA),
                    anchor.names = NULL,
                    criterion = criterion,
                    focalName = focalName,
                    matchScores = logistikRes1$matchScores,
                    ZT = as.character(symnum(
                      deltaR2,
                      c(0, 0.13, 0.26, 1),
                      symbols = c("A", "B", "C"),
                      legend = FALSE
                    )),
                    JG = as.character(symnum(
                      deltaR2,
                      c(0, 0.035, 0.07, 1),
                      symbols = c("A", "B", "C"),
                      legend = FALSE
                    ))
                  )
              }
              # p-Adjust ----
              df <- switch(
                self$options$type,
                both = 2,
                udif = 1,
                nudif = 1
              )
              pval <- 1 - pchisq(RES$Logistik, df)
              RES$adjusted.p <-
                p.adjust(pval, method = pAdjustMethod)
              if (min(RES$adjusted.p, na.rm = TRUE) > alpha) {
                RES$DIFitems <- "No DIF item detected"
              } else {
                RES$DIFitems <- which(RES$adjusted.p < alpha)
              }
              class(RES) <- "Logistic"
              return(RES)
            }
            
            resToReturn <- internalLog()
            
            return(resToReturn)
          }
        
        Logistik <-
          function (data,
                    member,
                    memberType = "group",
                    match = "score",
                    anchor = NULL,
                    type = "both",
                    criterion = "LRT")
          {
            R2 <-
              function(m, n)
                1 - (exp(-m$null.deviance / 2 + m$deviance / 2)) ^ (2 / n)
            R2max <- function(m, n)
              1 - (exp(-m$null.deviance / 2)) ^ (2 / n)
            R2DIF <- function(m, n)
              R2(m, n) / R2max(m, n)
            dev <- R2full <- R2simple <- deltaR <- NULL
            mFull <-
              mSimple <- seFull <- seSimple <- matrix(0, NCOL(data),
                                                      4)
              cov.matM0 <- cov.matM1 <- NULL
            if (memberType == "group")
              GROUP <- as.factor(member)
            else
              GROUP <- member
            for (item in 1:ncol(data)) {
              private$.checkpoint()
              if (match[1] == "score") {
                if (!is.null(anchor)) {
                  data2 <- cbind(data, anchor)
                  SCORES <-
                    rowSums(sapply(data2, as.numeric), na.rm = TRUE)
                } else {
                  SCORES <- rowSums(sapply(data, as.numeric))
                }
              }
              else {
                SCORES <- match
              }
              
              ITEM <- data[, item]
              m0 <- switch(
                type,
                both = glm(ITEM ~ SCORES * GROUP,
                           family = "binomial"),
                udif = glm(ITEM ~ SCORES +
                             GROUP, family = "binomial"),
                nudif = glm(ITEM ~ SCORES *
                              GROUP, family = "binomial")
              )
              
              m1 <-
                switch(
                  type,
                  both = glm(ITEM ~ SCORES, family = "binomial"),
                  udif = glm(ITEM ~ SCORES, family = "binomial"),
                  nudif = glm(ITEM ~
                                SCORES + GROUP, family = "binomial")
                )
              if (criterion == "LRT") {
                dev[item] <- deviance(m1) - deviance(m0)
              } else {
                if (criterion != "Wald")
                  stop("'criterion' must be either 'LRT' or Wald'",
                       call. = FALSE)
                else {
                  coeff <- as.numeric(coefficients(m0))
                  covMat <- summary(m0)$cov.scaled
                  if (type == "udif")
                    C <- rbind(c(0, 0, 1))
                  else {
                    if (type == "nudif")
                      C <- rbind(c(0, 0, 0, 1))
                    else
                      C <- rbind(c(0, 0, 1, 0), c(0, 0, 0, 1))
                  }
                  dev[item] <-
                    t(C %*% coeff) %*% solve(C %*% covMat %*%
                                               t(C)) %*% C %*% coeff
                }
              }
              
              R2full[item] <- R2DIF(m0, NROW(data))
              R2simple[item] <- R2DIF(m1, NROW(data))
              deltaR[item] <-
                R2DIF(m0, NROW(data)) - R2DIF(m1, NROW(data))
              mFull[item, 1:length(m0$coefficients)] <-
                m0$coefficients
              mSimple[item, 1:length(m1$coefficients)] <-
                m1$coefficients
              seFull[item, 1:length(m0$coefficients)] <-
                sqrt(diag(vcov(m0)))
              seSimple[item, 1:length(m1$coefficients)] <-
                sqrt(diag(vcov(m1)))
            }

            res <-
              list(
                stat = dev,
                deltaR2 = deltaR,
                criterion = criterion,
                memberType = memberType,
                match = ifelse(match[1] ==
                                 "score", "score", "matching variable"),
                matchScores = SCORES
              )
            return(res)
          }
        
        # Bootstrap GC functions ----
        
        print.bootSE <- function(x,
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
          t <- matrix(t[, !allNA], nrow = nrow(t))
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
        
        NagR2 <- function(DATA, ind) {
          ITEM <- (DATA[, 1])
          SCORE <- (DATA[, 3])
          GROUP <- (DATA[, 2])
          n <- nrow(DATA)
          
          m0 <- switch(
            self$options$type,
            both = glm(ITEM[ind] ~ SCORE * GROUP,
                       family = "binomial"),
            udif = glm(ITEM[ind] ~ SCORE +
                         GROUP, family = "binomial"),
            nudif = glm(ITEM[ind] ~ SCORE *
                          GROUP, family = "binomial")
          )
          
          m1 <-
            switch(
              self$options$type,
              both = glm(ITEM[ind] ~ SCORE, family = "binomial"),
              udif = glm(ITEM[ind] ~ SCORE, family = "binomial"),
              nudif = glm(ITEM[ind] ~
                            SCORE + GROUP, family = "binomial")
            )
          
          R2cox0 <-  1 - exp((m0$deviance - m0$null.deviance) / n)
          R2nag0 <-  R2cox0 / (1 - exp((-m0$null.deviance) / n))
          
          R2cox1 <-  1 - exp((m1$deviance - m1$null.deviance) / n)
          R2nag1 <-  R2cox1 / (1 - exp((-m1$null.deviance) / n))
          
          deltaNagR2 <- R2nag0 - R2nag1
          return(deltaNagR2)
        }
        
        qEmp <- function(empFn, x) {
          quantile(empFn, x)
        }

        pEmp <- function(empFn, x) {
          empFn(x)
        }

        dEmp <- function(empFn, qEmp, dPoint, values) {
          dens <- density(values)
          i <- dens$x[which.min(abs(dens$x - dPoint))]
          dens_x <- dens$y[dens$x == i]
          dens_x
        }
        
        empDist <- function(DATA, R = 1000) {
          # Get bootstrapped distribution
          myBoot <- boot(DATA, NagR2, R = self$options$bootSims)
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
        # retroDesignOG ----
        retroDesign <- function(hypTrueEff, myBoot){
          alpha <- self$options$alpha
          rdRes <- matrix(0, nrow = 1, ncol = 4)
          rdRes[1,1] <- myBoot$t0
          # se of empirical distribution
          rdRes[1,4] <- observedSE <- print.bootSE(myBoot)[[3]]
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
          # typeM error rate VIA Estimation
          estimate <-
            D + sample(myBoot$t, replace = T, size = 10000)
          significant <- estimate > qUpper
          rdRes[1, 2] <- typeMError <- mean(estimate[significant]) / D
          return(rdRes)
        }
        #  
        # # retroDesignSE ----
        # retroDesign <- function(hypTrueEff, myBoot){
        #   alpha <- self$options$alpha
        #   rdRes <- matrix(0, nrow = 1, ncol = 4)
        #   rdRes[1,1] <- myBoot$t0
        #   # se of emp. dist.
        #   
        #   observedSE <- print.bootSE(myBoot)[[3]]
        #   rdRes[1,4] <- observedSE
        #   
        #   # D <- abs(myBoot$t0 - hypTrueEff/observedSE)
        #   D <- abs(myBoot$t0 - hypTrueEff)
        #   lambda <- D/observedSE
        #   # Quantile matching the upper 1 - alpha in the emp. dist.
        #   empFn <- ecdf(myBoot$t)
        #   z <- quantile(empFn,  1 - (alpha))
        # 
        #   exp1P <- pEmp(empFn, -z - lambda)
        #   exp2P <- pEmp(empFn, z - lambda)
        # 
        #   power <- exp1P + 1 - exp2P
        #   typeS <- exp1P/(exp1P + 1 - exp2P)
        #   # rdRes[1, 4] <- typeS
        #   rdRes[1, 3] <- power
        # 
        #   # typeM error rate
        #   exp1M <- dEmp(empFn, qEmp, lambda + z, myBoot$t)
        #   exp2M <- dEmp(empFn, qEmp,lambda - z, myBoot$t)
        #   exp3M <- pEmp(empFn, lambda + z)
        #   exp4M <- pEmp(empFn, lambda - z)
        # 
        #   typeM <- (exp1M + exp2M +
        #                 lambda*(exp3M + exp4M - 1))/
        #                     (lambda*(1 - exp3M + exp4M))
        # 
        #   rdRes[1, 2] <- typeM
        #   return(rdRes)
        # }
        
        # Results functions ----
        
        highlight <- function(table, column, i) {
          table$addFormat(rowNo = i,
                          col = column,
                          format = jmvcore::Cell.NEGATIVE)
        }
        
        designAnalysis <- function(designList, Data, group, match) {
            if (self$options$D == "") {
              if (self$options$difFlagScale == "zt") {
                hypTrueEff <- c(0, 0.13, 0.26)
              } else {
                hypTrueEff <- c(0, 0.035, 0.07)
              }
            } else {
              hypTrueEff <- as.numeric(self$options$D)
            }
            
            GC <-
              data.frame("item" = rep(as.character(), times = length(designList)*length(hypTrueEff)),
                         "obsEff" = rep(as.numeric(), times = length(designList)*length(hypTrueEff)),
                         "hypTrueEff"= rep(as.numeric(), times = length(designList)*length(hypTrueEff)),
                         "typeM" = rep(as.numeric(), times = length(designList)*length(hypTrueEff)),
                         "power" = rep(as.numeric(), times = length(designList)*length(hypTrueEff)),
                         # "typeS" = rep(as.numeric(), times = length(designList)*length(hypTrueEff)),
                         "bootSE" = rep(as.numeric(), times = length(designList)*length(hypTrueEff)),
                         stringsAsFactors = FALSE)
            
          for (item in 1:length(designList)) {
            curItem <- designList[item]
            if (is.na(curItem)) {
              stop("No items have been flagged for DIF")
              return()
            }
            empDATA <- cbind(Item = jmvcore::toNumeric(Data[, curItem]), jmvcore::toNumeric(group), match)
            colnames(empDATA) <-
              c(colnames(Data)[item], "GROUP", "SCORES")
            myBoot <- empDist(empDATA)
            tick <- length(hypTrueEff) - 1
            for (hypInd in 1:length(hypTrueEff)) {
              private$.checkpoint()
              GC[item*length(hypTrueEff) - tick, 1] <- designList[item]
              GC[item*length(hypTrueEff) - tick, 2] <- as.character((hypTrueEff[hypInd]))
              GC[item*length(hypTrueEff) - tick, 3:6] <- as.numeric(retroDesign(hypTrueEff = hypTrueEff[hypInd],myBoot))
              tick <- tick - 1
              }
          }
            
            return(GC)
          }
        
        designAnalysisSensitivity <- function(designList, Data, group, match, nMin, nMax, points) {
          nList <- seq(nMin, nMax, length.out = points)
          if (self$options$D == "") {
            if (self$options$difFlagScale == "zt") {
              hypTrueEff <- c(0, 0.13, 0.26)
            } else {
              hypTrueEff <- c(0, 0.035, 0.07)
            }
          } else {
            hypTrueEff <- as.numeric(self$options$D)
          }
          
          sensMatrix <- matrix(nrow = points*length(designList)*length(hypTrueEff), ncol = 7)
          for (i in nList){
            nListTick <- length(nList) - 1
            # Sample i rows from Data with replacement
            Data <- Data[sample(nrow(Data), round(i), replace = TRUE),]
            for (item in 1:length(designList)) {
              curItem <- designList[item]
              if (is.na(curItem)) {
                stop("No items have been flagged for DIF")
                return()
              }
              empDATA <- cbind(Item = jmvcore::toNumeric(Data[, curItem]), jmvcore::toNumeric(group), match)
              colnames(empDATA) <-
                c(colnames(Data)[item], "GROUP", "SCORES")
              myBoot <- empDist(empDATA)
              tick <- length(hypTrueEff) - 1
              for (hypInd in 1:length(hypTrueEff)) {
                private$.checkpoint()
                sensMatrix[length(nList) - nListTick * item * length(hypTrueEff) - tick, 1] <- i
                sensMatrix[length(nList) - nListTick * item * length(hypTrueEff) - tick, 2] <- designList[item]
                sensMatrix[length(nList) - nListTick * item * length(hypTrueEff) - tick, 3] <- as.character((hypTrueEff[hypInd]))
                sensMatrix[length(nList) - nListTick * item * length(hypTrueEff) - tick, 4:7] <- as.numeric(retroDesign(hypTrueEff = hypTrueEff[hypInd],myBoot))
                tick <- tick - 1
                }
            }
              nListTick <- nListTick - 1
          }
          return(sensMatrix)
        }
          
        buildGC <- function(GC) {
          table <- self$results$gcTable
          for (item in 1:nrow(GC)){
            if (GC[item, 1] != 0) {
              table$addRow(
                rowKey = item,
                values = list(
                  itemName = GC[item, 1],
                  obsEff = GC[item, 3],
                  bootSE = GC[item, 6],
                  hypTrueEff = GC[item, 2],
                    # ifelse(
                    # self$options$D == "",
                    # switch(rownames(GC)[item],
                    #       0.00000001 = paste0(round(as.numeric(rownames(GC)[item]),3), " (NULL)"),
                    #       0.13 = paste0(round(as.numeric(rownames(GC)[item]),3), " (B)"),
                    #       0.26 = paste0(round(as.numeric(rownames(GC)[item]),3), " (C)"),
                    #       0.035 = paste0(round(as.numeric(rownames(GC)[item]),3), " (B)"),
                    #       0.07 = paste0(round(as.numeric(rownames(GC)[item]),3), " (C)"))
                    # 
                  typeM = GC[item, 4],
                  # typeS = GC[item, 6],
                  power = GC[item, 5]
                )
              )
            }
          }
        }
        
        # Model ----
        
        model <-
          difLogistic(
            DATA = Data,
            group = group,
            focalName = focalName,
            anchor = anchor,
            # groupType = groupType,
            match = match,
            type = type,
            criterion = criterion,
            alpha = alpha,
            purify = purify,
            pAdjustMethod = pAdjustMethod
          )
        
        
        
        if (self$options$designAnalysis) {
          if (self$options$designAnalysisSigOnly) {
            designList <- model$names[model$DIFitems]
          } else{
            designList <- model$names
          }
          GCTable = designAnalysis(designList = designList, Data = Data, group = group, match = model$matchScores)
          buildGC(GCTable)
          }
        
        # Description Results Table ----
        calculateDESCtable <- function() {
          if (self$results$DESCtable$isNotFilled() |
              self$results$DESCtable$rowCount == 0) {
            table <- self$results$DESCtable
            mess1 <-
              switch(model$type,
                     both = " both types of ",
                     nudif = " nonuniform ",
                     udif = " uniform ")
            if (model$purification) {
              pur <- "with "
            } else {
              pur <- "without "
            }
            
            if (class(model) == "Logistic") {
              df <- ifelse(type == "both", 2, 1)
            } else {
              df <-
                ifelse(type == "both", 2 * length(focalName), length(focalName))
            }
            
            table$addRow(rowKey = 1, values = list(
              bob =
                paste0(
                  "Detection of",
                  mess1,
                  "Differential Item Functioning using the "
                  ,
                  switch(
                    class(model),
                    Logistic = "Logistic regression method ",
                    genLogistic = "Generalized logistic regression method "
                  ),
                  pur,
                  "item purification and with ",
                  # length(model$focalName),
                  #  " reference group(s) and ",
                  df,
                  " degree(s) of freedom."
                )
            ))
            
            table$addRow(rowKey = self$results$DESCtable$rowCount + 1,
                         values = list(bob = ""))
            table$addRow(
              rowKey = self$results$DESCtable$rowCount + 1,
              values = list(bob = paste0(
                "DIF flagging criterion: ",
                ifelse(
                  model$criterion == "Wald",
                  paste0(
                    "Wald test of joint significance on ",
                    df,
                    " degree(s) of freedom"
                  ),
                  "Likelihood ratio test"
                )
              ))
            )
            
            table$addRow(rowKey = self$results$DESCtable$rowCount + 1,
                         values = list(bob = ""))
            if (model$pAdjustMethod == "none")
              table$addRow(
                rowKey = self$results$DESCtable$rowCount + 1,
                values = list(bob = "No p-value adjustment for multiple comparisons")
              )
            else {
              pAdjMeth <- switch(
                model$pAdjustMethod,
                bonferroni = "Bonferroni",
                holm = "Holm",
                hochberg = "Hochberg",
                hommel = "Hommel",
                BH = "Benjamini-Hochberg",
                BY = "Benjamini-Yekutieli"
              )
              table$addRow(
                rowKey = self$results$DESCtable$rowCount + 1,
                values = list(
                  bob = paste(
                    "Multiple comparisons made with ",
                    pAdjMeth,
                    " adjustment of p-values."
                  )
                )
              )
            }
            table$addRow(rowKey = self$results$DESCtable$rowCount + 1,
                         values = list(bob = ""))
            
            if (model$purification) {
              if (model$nrPur <= 1) {
                word <- " iteration"
              } else {
                word <- " iterations"
              }
              if (!model$convergence) {
                table$addRow(
                  rowKey = self$results$DESCtable$rowCount + 1,
                  values = list(
                    bob = paste(
                      "WARNING: no item purification convergence after ",
                      model$nrPur,
                      word,
                      sep = " "
                    )
                  )
                )
                loop <- NULL
                for (i in 1:model$nrPur) {
                  loop[i] <- sum(model$difPur[1,] == model$difPur[i + 1,])
                }
                if (max(loop) != length(model$genLogistik)) {
                  table$addRow(
                    rowKey = self$results$DESCtable$rowCount + 1,
                    values = list(
                      bob = paste(
                        "(Note: no loop detected in less than ",
                        model$nrPur,
                        word,
                        ")",
                        sep = ""
                      )
                    )
                  )
                } else {
                  table$addRow(
                    rowKey = self$results$DESCtable$rowCount + 1,
                    values = list(
                      bob = paste(
                        "(Note: loop of length ",
                        min((1:model$nrPur)[loop ==
                                              length(model$genLogistik)]),
                        " in the item purification process)",
                        sep = " "
                      )
                    )
                  )
                  table$addRow(
                    rowKey = self$results$DESCtable$rowCount + 1,
                    values = list(
                      bob = paste(
                        "WARNING: following results based on the last iteration of the purification"
                      )
                    )
                  )
                }
              } else {
                table$addRow(
                  rowKey = self$results$DESCtable$rowCount + 1,
                  values = list(
                    bob = paste("Convergence reached after ", model$nrPur, word, sep = " ")
                    
                  )
                )
                table$addRow(
                  rowKey = self$results$DESCtable$rowCount + 1,
                  values = list(bob = "")
                )
              }
            }
            
            table$addRow(
              rowKey = self$results$DESCtable$rowCount + 1,
              values = list(bob = paste0(
                "Grouping variable(s): ", list(self$options$group)
              ))
            )
            
            if (model$match[1] == "score") {
              table$addRow(
                rowKey = self$results$DESCtable$rowCount + 1,
                values = list(bob = "Matching variable: Test score")
              )
            } else {
              table$addRow(
                rowKey = self$results$DESCtable$rowCount + 1,
                values = list(bob = paste0(
                  "Matching variable: ", self$options$matchVar
                ))
              )
            }
            table$addRow(rowKey = self$results$DESCtable$rowCount + 1,
                         values = list(bob = ""))
            if (is.null(model$anchor.names) |
                model$match != "score") {
              table$addRow(
                rowKey = self$results$DESCtable$rowCount + 1,
                values = list(bob = "No set of anchor items was provided")
              )
            }
            else {
              table$addRow(
                rowKey = self$results$DESCtable$rowCount + 1,
                values = list(bob =  "Anchor items (provided by the user): ")
              )
              for (i in 1:length(self$options$anchor)) {
                table$addRow(
                  rowKey = self$results$DESCtable$rowCount +
                    1,
                  values = list(bob = self$options$anchor[[i]])
                )
              }
            }
            table$addRow(rowKey = self$results$DESCtable$rowCount + 1,
                         values = list(bob = ""))
            
            table$addRow(
              rowKey = self$results$DESCtable$rowCount + 1,
              values = list(bob =  "Effect size (Nagelkerke's 'R\u00B2'):")
            )
            table$addRow(
              rowKey = self$results$DESCtable$rowCount + 1,
              values = list(bob =  "'A': negligible effect")
            )
            table$addRow(
              rowKey = self$results$DESCtable$rowCount + 1,
              values = list(bob =  "'B': moderate effect")
            )
            table$addRow(
              rowKey = self$results$DESCtable$rowCount + 1,
              values = list(bob =  "'C': large effect")
            )
          }
        }
        
        DESCstate <- self$results$DESCtable$state
        
        if (!is.null(DESCstate)) {
          # ... populate the table from the state
        } else {
          DESCstate <- calculateDESCtable()
          # ... populate the table from the state
          self$results$DESCtable$setState(DESCstate)
        }
        
        # DIF Results Table ----
        
        calculateDIFTable <- function() {
          for (i in 1:length(Data)) {
            if (self$results$DIFtable$isNotFilled()) {
              table <- self$results$DIFtable
              for (i in 1:length(Data)) {
                table$setRow(
                  rowNo = i,
                  values = list(
                    item = model$names[i],
                    chiSquare = model$Logistik[i],
                    p = model$adjusted.p[i],
                    difType = ifelse(
                      model$adjusted.p[i] <= alpha,
                      ifelse(model$logitPar[i, 4] == 0, "Uni", "Non-Uni"),
                      "No DIF"
                    ),
                    #p = paste0(round(model$p.value[i], 3), symnum(model$p.value[i], c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", ""))),
                    effSize = model$deltaR2[i],
                    ZT = ifelse(model$adjusted.p[i] <= alpha, model$ZT[i], ""),
                    JG = ifelse(model$adjusted.p[i] <= alpha, model$JG[i], "")
                  )
                )
              }
            }
            
            # Highlight DIF results table
            if (self$options$difFlagScale == "zt") {
              if (model$adjusted.p[i] <= alpha) {
                highlight(table, "ZT", i)
                highlight(table, "item", i)
              }
            } else {
              if (model$adjusted.p[i] <= alpha) {
                highlight(table, "JG", i)
                highlight(table, "item", i)
              }
            }
          }
          
          df <-
            ifelse(type == "both", 2 * length(focalName), length(focalName))
          table$setNote(
            key = "df",
            note = paste0(
              "Tests of significance conducted using: ",
              df,
              " degrees of freedom"
            )
          )
          # Set notes
          if (self$options$difFlagScale == "zt") {
            table$setNote(key = "ZT", note = "Zumbo & Thomas (ZT): 'A' = 'R\u00B2' 0 <> 0.13, 'B' = 'R\u00B2' 0.13 <> 0.26, 'C' = 'R\u00B2' 0.26 <> 1")
          } else {
            table$setNote(key = "JG", note = "Jodoin & Gierl (JG): 'A' = 'R\u00B2' 0 <> 0.035, 'B' = 'R\u00B2' 0.035 <> 0.07, 'C' = 'R\u00B2' 0.07 <> 1")
          }
        }
        
        DIFstate <- self$results$DIFtable$state
        
        if (!is.null(DIFstate)) {
          # ... populate the table from the state
        } else {
          DIFstate <- calculateDIFTable()
          # ... populate the table from the state
          self$results$DIFtable$setState(DIFstate)
        }
        
        table <- self$results$gcTable
        if (table$rowCount == 0) {
          table$addRow(
            rowKey = 1,
            values = list(item = "No items were flagged as exhibiting statistically significant DIF", NULL, NULL, NULL)
          )
        }
        
        # ICC plot data ----
        
        if (!is.null(self$options$plotVarsICC)) {
          if (self$results$ICCplots$isNotFilled()) {
            items <- self$options$plotVarsICC
            
            for (i in unique(items)) {
              private$.checkpoint()
              
              if (!is.null(anchor)) {
                data2 <- cbind(data, anchor)
                match <-
                  rowSums(sapply(data2, as.numeric), na.rm = TRUE)
              } else {
                match <- rowSums(sapply(data, as.numeric))
              }
              
              plotData <-
                data.frame(jmvcore::toNumeric(Data[, colnames(Data) == i]), match, group)
              
              colnames(plotData) <-
                c(i, "match", "group")
              
              imageICC <- self$results$ICCplots$get(key = i)
              imageICC$setState(list(plotData, model))
            }
          }
        }
      },
      
      .plotICC = function(imageICC, ...) {
        plotData <- data.frame(imageICC$state[[1]])
        model <- imageICC$state[[2]]
        
        if (!all(self$options$plotVarsICC %in% self$options$item)) {
          stop(
            paste0(
              "Not all items selected to be plotted have been evaluated, please remove: ",
              self$options$plotVarsICC[!self$options$plotVarsICC %in% self$options$item]
            ),
            call. = FALSE
          )
        }
        
        p <- ggplot(data = as.data.frame(plotData),
                    aes(
                      x = as.numeric(plotData$match),
                      y = as.integer(plotData[, 1]),
                      colour = as.character(plotData$group)
                    )) +
          geom_smooth(
            method = "glm",
            level = 1 - self$options$alpha,
            se = TRUE,
            method.args = (family = "binomial")
          ) +
          labs(colour = "Group membership") +
          ggtitle(
            paste("Item: ", colnames(plotData)),
            subtitle = paste(
              "Effect Size: ",
              round(model$deltaR2[model$names == colnames(plotData)[1]], 3),
              " | DIF Classification: ZT = ",
              model$ZT[model$names == colnames(plotData)[1]],
              ", JG = ",
              model$JG[model$names == colnames(plotData)[1]]
            )
          ) +
          xlab(ifelse(
            is.null(self$options$matchVar),
            "Total sore",
            "Supplied matching variable range"
          )) +
          ylab("Probability of endorsement") +
          theme_classic()
        
        print(p)
        TRUE
      }
    )
  )
