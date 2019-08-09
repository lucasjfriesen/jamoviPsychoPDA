# Ordinal DIF ----
ordinal.logistic <- function (items,
                              group,
                              anchor,
                              anchorNames,
                              groupType,
                              match,
                              type,
                              criterion,
                              alpha,
                              purify,
                              nIter,
                              pAdjustMethod) {
    # if (purify & match[1] != "score") {
    #     stop(
    #         "purification not allowed when using a custom matching variable. Please remove the custom matching variable or deselect 'Purification'."
    #     )
    #     return()
    # }
    
    internalLog <- function() {
        sigThreshold <-
            list(
                baseline = qchisq(1 - alpha, 1),
                uniform = qchisq(1 -
                                     alpha, 1),
                both = qchisq(1 - alpha, 2 * 1)
            )
        
        # Model fitting ----
        
        modelResults <- ordinalLogistik(
            data = items,
            group = group,
            groupType = groupType,
            match = match,
            type = type,
            criterion = criterion,
            anchor = anchor,
            alpha = alpha,
            pAdjustMethod
        )
        
        RES <-
            list(
                models = modelResults$models,
                betaChange = modelResults$betaChange,
                deviance = modelResults$deviance,
                chiSquared = modelResults$chiSquared,
                degreesOfFreedom = modelResults$degreesOfFreedom,
                pValue = modelResults$pValue,
                deltaR2 = modelResults$deltaR2,
                R2 = modelResults$R2,
                flags = modelResults$flags,
                ZT = modelResults$ZT,
                JG = modelResults$JG,
                alpha = alpha,
                sigThreshold = sigThreshold,
                names = colnames(items)
            )
        class(RES) <- "ordinalLogistic"
        return(RES)
    }
    
    resToReturn <- internalLog()
    
    return(resToReturn)
}


R2 <- function(nullDeviance, fullDeviance, n) {
    1 - (exp(-nullDeviance / 2 + fullDeviance / 2) ^ (2 / n))
}

R2max <- function(nullDeviance, n) {
    1 - (exp(-nullDeviance / 2) ^ (2 / n))
}

R2DIF <- function(nullDeviance, fullDeviance, n) {
    R2(nullDeviance, fullDeviance, n) / R2max(nullDeviance, n)
}

ordinalLogistik <-
    function (data,
              group,
              groupType,
              match,
              anchor,
              type,
              criterion,
              pAdjustMethod,
              alpha,
              betaChangeThreshold = 0.10) {
        
        itemRes <- list(
            models = list(),
            betaChange = list(),
            deviance = list(),
            chiSquared = list(),
            degreesOfFreedom = list(),
            flags = list(),
            ZT = list(),
            JG = list(),
            R2 = list(),
            deltaR2 = list()
        )
        
        sigThreshold <-
            list(
                baseline = qchisq(1 - alpha, 1),
                uniform = qchisq(1 - alpha, 1),
                both = qchisq(1 - alpha, 2 * 1)
            )
        
        nGroup <- length(unique(group)) - 1
        
        if (groupType == "group") {
            GROUP <- as.factor(group)
            # GROUP <- relevel(GROUP, ref = "Reference Group")
        } else {
            GROUP <- group
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
            
            ITEM <- as.factor(data[, item])
            
            nullModel <- MASS::polr(ITEM ~ 1, Hess = TRUE)
            baselineModel <- MASS::polr(ITEM ~ SCORES, Hess = TRUE)
            uniformModel <-
                MASS::polr(ITEM ~ SCORES + GROUP, Hess = TRUE)
            bothModel <-
                MASS::polr(ITEM ~ SCORES * GROUP, Hess = TRUE)
            
            uniformBetaChange <- list(matchingVar = round(abs((
                uniformModel$coefficients[[1]] - baselineModel$coefficients[[1]]) / baselineModel$coefficients[[1]]
            ), 4))
            
            bothBetaChange <-
                list(matchingVar = round(abs((bothModel$coefficients[[1]] - uniformModel$coefficients[[1]]) / uniformModel$coefficients[[1]]
                ), 4),
                groupingVar = round(abs((bothModel$coefficients[[2]] - uniformModel$coefficients[[2]]) / uniformModel$coefficients[[2]]
                ), 4))
            
            nullDeviance <- nullModel$deviance
            baselineDeviance <- baselineModel$deviance
            uniformDeviance <- uniformModel$deviance
            bothDeviance <- bothModel$deviance
            
            nObs = length(ITEM)
            
            R2 <-
                list(
                    baseline = R2DIF(nullDeviance, baselineDeviance, nObs),
                    uniform = R2DIF(nullDeviance, uniformDeviance, nObs),
                    both = R2DIF(nullDeviance, bothDeviance, nObs)
                )
            
            deltaR2 <- list(
                baseline = R2$uniform - R2$baseline,
                uniform = R2$both - R2$uniform,
                both = R2$both - R2$baseline
            )
            
            degreesOfFreedom <- list(
                baseline = nGroup,
                uniform = nGroup,
                both = 2 * nGroup
            )
            
            likelihoodRatioChiSquared = list(
                baseline = nullDeviance - baselineDeviance,
                uniform = uniformDeviance - bothDeviance,
                both = baselineDeviance - bothDeviance
            )
            
            likelihoodRatioPValue <-
                list(
                    baseline = round(
                        1 - pchisq(likelihoodRatioChiSquared[["baseline"]],
                                   degreesOfFreedom[["baseline"]]),
                        4
                    ),
                    uniform = round(
                        1 - pchisq(likelihoodRatioChiSquared[["uniform"]],
                                   degreesOfFreedom[["uniform"]]),
                        4
                    ),
                    both = round(
                        1 - pchisq(likelihoodRatioChiSquared[["both"]],
                                   degreesOfFreedom[["both"]]),
                        4
                    )
                )
            
            itemRes$flags[[item]] <- list(
                chiSquared = likelihoodRatioChiSquared >= unlist(sigThreshold),
                pValue = likelihoodRatioPValue <= alpha,
                uniformBetaChange = uniformBetaChange >= betaChangeThreshold,
                bothBetaChange = bothBetaChange >= betaChangeThreshold
            )
            
            # p-Adjust
            if (!is.null(pAdjustMethod)) {
                RES$adjusted.p <-
                    p.adjust(modelResults$likelihoodRatioPValue, method = pAdjustMethod)
                if (min(RES$adjusted.p, na.rm = TRUE) > alpha) {
                    RES$DIFitems <- "No DIF item detected"
                } else {
                    RES$DIFitems <- which(RES$adjusted.p < alpha)
                }
            }
            
            itemRes$ZT[[item]] = as.character(symnum(
                unlist(deltaR2),
                c(0, 0.13, 0.26, 1),
                symbols = c("A", "B", "C"),
                legend = FALSE
            ))
            
            itemRes$JG[[item]] = as.character(symnum(
                unlist(deltaR2),
                c(0, 0.035, 0.07, 1),
                symbols = c("A", "B", "C"),
                legend = FALSE
            ))
            
            itemRes$models[[item]] = list(
                nullModel = nullModel,
                baselineModel = baselineModel,
                uniformModel = uniformModel,
                bothModel = bothModel
            )
            itemRes$betaChange[[item]] = list(uniformBetaChange = uniformBetaChange,
                                              bothBetaChange = bothBetaChange)
            itemRes$deviance[[item]] = list(
                nullDeviance = nullDeviance,
                baselineDeviance = baselineDeviance,
                uniformDeviance = uniformDeviance,
                bothDeviance = bothDeviance
            )
            
            itemRes$chiSquared[[item]] = likelihoodRatioChiSquared
            itemRes$pValue[[item]] = likelihoodRatioPValue
            itemRes$degreesOfFreedom[[item]] = degreesOfFreedom
            itemRes$R2[[item]] = R2
            itemRes$deltaR2[[item]] = deltaR2
            
        }
        itemRes <- lapply(itemRes, setNames, c(names(data)))
        return(itemRes)
    }
