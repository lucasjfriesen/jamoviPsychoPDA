# DIF ----
# difLogReg is a starter function to determine whether GLM or LM should be used, as well as QA on the arguments input

binaryDIF.logistic <-
    function (DATA,
              group,
              groupOne,
              anchor,
              anchorNames,
              groupType,
              match,
              type,
              criterion,
              alpha,
              purify,
              nIter,
              pAdjustMethod)
    {
        if (purify & match[1] != "score") {
            stop(
                "purification not allowed when using a custom matching variable. Please remove the custom matching variable or deselect 'Purification'."
            )
            return()
        }

        internalLog <- function() {
          DF <- ifelse(type == "both", 2 * length(groupOne), length(groupOne))
            if (groupType == "group") {
                # Group <- rep(0, NROW(DATA))
                # Group[group == groupOne] <- 1
                # for (i in 1:DF) Group[group == groupOne[i]] <- i
              Group <- group
            } else {
                Group <- group
            }

            sigThreshold <-
                switch(
                    type,
                    both = qchisq(1 - alpha, 2 * length(groupOne)),
                    udif = qchisq(1 -
                                      alpha, length(groupOne)),
                    nudif = qchisq(1 - alpha, length(groupOne))
                )

            # Purification == FALSE ----

            if (!purify) {
                PROV <- Logistik(
                    DATA,
                    Group,
                    groupType = groupType,
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
                        coefficients = PROV$coefficients,
                        alpha = alpha,
                        thr = sigThreshold,
                        DIFitems = DIFitems,
                        groupType = groupType,
                        match = PROV$match,
                        type = type,
                        pAdjustMethod = pAdjustMethod,
                        adjusted.p = NULL,
                        purification = purify,
                        names = colnames(DATA),
                        anchor.names = anchorNames,
                        criterion = criterion,
                        groupOne = groupOne,
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
                        )),
                        sigThreshold = sigThreshold,
                                            m0 = PROV$m0,
                    m1 = PROV$m1,
                    errors = PROV$coefficientsSE
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
                        groupType = groupType,
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
                        if (nrPur >= nIter) {
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
                                groupType = groupType,
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
                    DIFitems <-
                        (1:NCOL(DATA))[chiSquared1 > sigThreshold]
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
                        coefficients = logistikRes1$coefficients,
                        alpha = alpha,
                        sigThreshold = sigThreshold,
                        DIFitems = DIFitems,
                        groupType = groupType,
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
                        groupOne = groupOne,
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
                        )),
                        m0 = logistikRes1$m0,
                        m1 = logistikRes1$m1,
                        errors = logistikRes1$coefficientsSE
                        
                    )
            }
            # p-Adjust ----
            df <- switch(type,
                         both = 2,
                         udif = 1,
                         nudif = 1)
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
              groupType,
              match,
              anchor,
              type,
              criterion)
    {
        R2 <-
            function(m, n)
                1 - (exp(-m$null.deviance / 2 + m$deviance / 2)) ^ (2 / n)
        R2max <- function(m, n)
            1 - (exp(-m$null.deviance / 2)) ^ (2 / n)
        R2DIF <- function(m, n)
            R2(m, n) / R2max(m, n)
        nGroup <- length(unique(member)) - 1
        dev <- R2full <- R2simple <- deltaR <- NULL
        mFull <-
            mSimple <-
            seFull <- seSimple <- matrix(0, NCOL(data), 2 + 2 * nGroup)
        cov.matM0 <- cov.matM1 <- NULL
        if (groupType == "group") {
            GROUP <- as.factor(member)
        } else {
            GROUP <- member
        }
        for (item in 1:ncol(data)) {
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
                if (criterion != "Wald"){
                    stop("'criterion' must be either 'LRT' or Wald'",
                         call. = FALSE)
                } else {
                    coeff <- as.numeric(coefficients(m0))
                    covMat <- summary(m0)$cov.scaled
                    if (type == "udif") {
                        C <- rbind(c(0, 0, 1))
                    }                     else {
                        if (type == "nudif") {
                            C <- matrix(0, nGroup, length(coeff))
                            for (tt in 1:nGroup)
                                C[tt, 2 + nGroup + tt] <- 1
                        }
                        else {
                            C <- matrix(0, nGroup * 2, length(coeff))
                            for (tt in 1:(2 * nGroup))
                                C[tt, 2 + tt] <- 1
                        }
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
        }
        names <- c("(Intercept)", "Match Variable")
        groupNames <- sort(unique(member))
        for (i in 2:length(groupNames))
            names <- c(names, paste(
                                    groupNames[i], sep = ""))
        for (i in 2:length(groupNames))
            names <- c(names, paste("Match Variable : ",
                                    groupNames[i], sep = ""))
        colnames(mFull) <- colnames(mSimple) <- names
        errors <- summary(m0)$coefficients[, 2]
        names(errors) <- names
            res <-
                list(
                    stat = dev,
                    m0 = m0,
                    m1 = m1,
                    deltaR2 = deltaR,
                    coefficients = mFull,
                    coefficientsSE = errors,
                    criterion = criterion,
                    groupType = groupType,
                    match = ifelse(match[1] ==
                                       "score", "score", "matching variable"),
                    matchScores = SCORES
                )
            return(res)
        }
