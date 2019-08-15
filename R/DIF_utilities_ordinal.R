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
                uniformModel$coefficients[[1]] - baselineModel$coefficients[[1]]
            ) / baselineModel$coefficients[[1]]
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
            
            # # p-Adjust
            # if (!is.null(pAdjustMethod)) {
            #     itemRes$adjusted.p <-
            #         p.adjust(itemRes$likelihoodRatioPValue, method = pAdjustMethod)
            #     if (min(itemRes$adjusted.p, na.rm = TRUE) > alpha) {
            #         itemRes$DIFitems <- "No DIF item detected"
            #     } else {
            #         itemRes$DIFitems <- which(RES$adjusted.p < alpha)
            #     }
            # }
            
            itemRes$ZT[[item]] = as.character(symnum(
                unlist(deltaR2),
                c(0, 0.13, 0.26, 1),
                symbols = c("A", "B", "C"),
                legend = FALSE
            ))
            names(itemRes$ZT[[item]]) <- c("baseline", "uniform", "both")
            
            itemRes$JG[[item]] = as.character(symnum(
                unlist(deltaR2),
                c(0, 0.035, 0.07, 1),
                symbols = c("A", "B", "C"),
                legend = FALSE
            ))
            names(itemRes$JG[[item]]) <- c("baseline", "uniform", "both")
            
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

difResultsFormatter <- function(model){
    table <- data.frame(
            item = NA,
            model = NA,
            logOdds_matchingVar = NA,
            logOdds_groupingVar = NA,
            deltaBeta_matchingVar = NA,
            deltaBeta_groupingVar = NA,
            deltaOR_matchingVar = NA,
            deltaOR_groupingVar = NA,
            deltanagR2 = NA,
            chiSquare = NA,
            p = NA,
            deltaBetaFlag_matchingVar =  NA,
            deltaBetaFlag_groupingVar =  NA,
            ZT = NA,
            JG = NA)
    
    models <- c("uniform", "both")
    
    for (model_ in models) {
      for (item in model$names) {
          row <- list(
            item = item,
            model = model_,
            logOdds_matchingVar = ,
            logOdds_groupingVar,
            deltaBeta_matchingVar = model$betaChange[[item]][[paste0(model_, "BetaChange")]][["matchingVar"]],
            deltaBeta_groupingVar = ifelse(length(model$betaChange[[item]][[paste0(model_, "BetaChange")]]) == 2,
                                           model$betaChange[[item]][[paste0(model_, "BetaChange")]][["groupingVar"]],
                                           NA),
            
            deltaNagR2 = model$deltaR2[[item]][[model_]],
            chiSquare = model$chiSquared[[item]][[model_]],
            p = model$pValue[[item]][[model_]],
            deltaBetaFlag_matchingVar = model$flags[[item]][[paste0(model_, "BetaChange")]][["matchingVar"]],
            deltaBetaFlag_groupingVar = ifelse(length(model$flags[[item]][[paste0(model_, "BetaChange")]]) == 2,
                                               model$flags[[item]][[paste0(model_, "BetaChange")]][["groupingVar"]],
                                               NA),
            ZT = model$ZT[[item]][[model_]],
            JG = model$JG[[item]][[model_]]
          )
          if (is.na(table[1,1])){
            table[1,] <- row
          } else {
            table <- rbind(table, row)
          }
      }
    }
    
    table <- table %>% 
        arrange(item)
    return(table)
}


# plotData <- rbind(
#     data.frame(
#         match = seq(1:40),
#         gender = rep(1, times = 40),
#         key = tidyr::gather(as.data.frame(
#             predict(x, newdata = data.frame(GROUP = 1, SCORES = 1:40), type = "p")
#         ), key = "key", value = "probability")
#     ),
#     data.frame(
#         match = seq(1:40),
#         gender = rep(0, times = 40),
#         key = tidyr::gather(as.data.frame(
#             predict(x, newdata = data.frame(GROUP = 0, SCORES = 1:40), type = "p")
#         ), key = "key", value = "probability")
#     )
# )
# 
# ggplot(plotData) +
#     geom_line(aes(x = match, y = key.probability, linetype = as.factor(gender), colour = key.key), size = 1) +
#     xlab("Theta") +
#     ylab("Probability of Endorsement")
