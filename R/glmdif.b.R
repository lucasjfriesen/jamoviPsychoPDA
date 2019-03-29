











# Data Wrangling ----

glmDIFClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "glmDIFClass",
    inherit = glmDIFBase,
    private = list(
          .init=function() {
            if (is.null(self$options$group) |
              is.null(self$data) | is.null(self$options$item)) {
              self$results$DESCtable$setVisible(visible = FALSE)
              self$results$DIFtable$setVisible(visible = FALSE)
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
            } else {
              self$results$instructions$setVisible(visible = FALSE)
            }
        },
      .run = function() {
        if (is.null(self$options$group) |
              is.null(self$data) | is.null(self$options$item)){return()}
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
        
        # Vector containing matching data
        match <-
          data.frame(jmvcore::toNumeric(data[, self$options$matchVar]))
        if (length(match) == 0) {
          match <- "score"
        } else {
          colnames(match) <- self$options$matchVar
          match <- unlist(match)
        }
        
        
        # Vector containing grouping data
        group <- as.character(data[, self$options$group])
        groupType <- self$options$groupType
        
        if (groupType == "group") {
          groupValueList <- unique(group)
          groupOne <- unique(group)[1]
          group <-
            as.factor(ifelse(group == groupOne, "Group A", "Group B"))
          groupOne <- unique(group)[1]
        }
        
        type <- self$options$type
        
        criterion <- self$options$criterion
        
        alpha <- self$options$alpha
        
        purify <- self$options$purify
        
        nIter <- self$options$nIter
        
        pAdjustMethod <- self$options$pAdjustMethod
        
        bootSims = self$options$bootSims
        
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
        
        retroStat <-
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
        
        empDist <- function(DATA, R, type, coefficients) {
          # Get bootstrapped distribution
          myBoot <- boot(DATA, retroStat, R = R, type = type, coefficients = coefficients)
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
        
        # Results functions ----
        
        highlight <- function(table, row, column) {
          for (i in column){
             table$addFormat(rowNo = row,
                  col = i,
                  format = jmvcore::Cell.NEGATIVE) 
          }
        }
        
        blankRow <- function() {
          self$results$DESCtable$addRow(rowKey = self$results$DESCtable$rowCount + 1,
                                        values = list(bob = ""))
        }
        
        designAnalysis <-
          function(designList,
                   Data,
                   group,
                   match,
                   bootSims,
                   type,
                   coefficients = FALSE) {
            if (coefficients == FALSE) {
              if (self$options$D == "") {
                if (self$options$difFlagScale == "zt") {
                  hypTrueEff <- c(0, 0.13, 0.26)
                } else {
                  hypTrueEff <- c(0, 0.035, 0.07)
                }
                labels <- c("Null", "Moderate", "Large")
              } else {
                hypTrueEff <- as.numeric(self$options$D)
                labels <- c("Custom Hyp.")
              }
              
              GC <-
                data.frame(
                  "label" = rep(as.character(), times = length(designList) * length(hypTrueEff)),
                  "item" = rep(as.character(), times = length(designList) * length(hypTrueEff)),
                  "obsEff" = rep(as.numeric(), times = length(designList) *
                                   length(hypTrueEff)),
                  "hypTrueEff" = rep(as.numeric(), times = length(designList) *
                                       length(hypTrueEff)),
                  "typeM" = rep(as.numeric(), times = length(designList) *
                                  length(hypTrueEff)),
                  "power" = rep(as.numeric(), times = length(designList) *
                                  length(hypTrueEff)),
                  "bootSE" = rep(as.numeric(), times = length(designList) *
                                   length(hypTrueEff)),
                  stringsAsFactors = FALSE
                )
            } else {
              if (self$options$D == "") {
                hypTrueEff <- 0
                labels <- "Null"
              } else {
                  hypTrueEff <- as.numeric(self$options$D)
                  labels <- c("Custom Hyp.")
              }
            
              GC <-
                data.frame(
                  "label" = rep(as.character(), times = length(designList) * length(hypTrueEff)),
                  "item" = rep(as.character(), times = length(designList) * length(hypTrueEff)),
                  "obsMain" = rep(as.numeric(), times = length(designList) *
                                   length(hypTrueEff)),
                  "obsInt" = rep(as.numeric(), times = length(designList) *
                                   length(hypTrueEff)),
                  "hypTrueEff" = rep(as.numeric(), times = length(designList) *
                                       length(hypTrueEff)),
                  "bootSE" = rep(as.numeric(), times = length(designList) *
                                   length(hypTrueEff)),
                  "typeMMain" = rep(as.numeric(), times = length(designList) *
                                  length(hypTrueEff)),
                  "typeSMain" = rep(as.numeric(), times = length(designList) *
                                  length(hypTrueEff)),
                  "typeMInt" = rep(as.numeric(), times = length(designList) *
                                  length(hypTrueEff)),
                  "typeSInt" = rep(as.numeric(), times = length(designList) *
                                  length(hypTrueEff)),
                  "power" = rep(as.numeric(), times = length(designList) *
                                  length(hypTrueEff)),
                  stringsAsFactors = FALSE
                )
            }
            
            for (item in 1:length(designList)) {
              curItem <- designList[item]
              if (is.na(curItem)) {
                self$results$gcTable$addRow(
                  rowKey = item,
                  values = list(itemName = "No items flagged as exhibitting DIF.")
                )
                return()
              }
              empDATA <-
                cbind(Item = jmvcore::toNumeric(Data[, curItem]),
                      jmvcore::toNumeric(group),
                      match)
              colnames(empDATA) <-
                c(colnames(Data)[item], "GROUP", "SCORES")
              tick <- length(hypTrueEff) - 1
              if (coefficients == FALSE) {
                myBoot <- empDist(empDATA, R = bootSims, type = type, coefficients = FALSE)
              } else {
                myBoot <- empDist(empDATA, R = bootSims, type = type, coefficients = TRUE)
              }
              for (hypInd in 1:length(hypTrueEff)) {
                private$.checkpoint()
                if (coefficients == FALSE) {
                  retroDesignRes <- retroDesign.nagR2(hypTrueEff = hypTrueEff[hypInd], myBoot, alpha = alpha, sigOnly = self$options$designAnalysisSigOnly)
                } else {
                  retroDesignRes <- retroDesign.coefficients(hypTrueEff = hypTrueEff[hypInd], myBoot, alpha = alpha, sigOnly = self$options$designAnalysisSigOnly)
                }
                GC[item * length(hypTrueEff) - tick, 1] <- labels[hypInd]
                GC[item * length(hypTrueEff) - tick, 2] <-
                  designList[item]
                GC[item * length(hypTrueEff) - tick, 3] <-
                  as.character((hypTrueEff[hypInd]))
                GC[item * length(hypTrueEff) - tick, 4:7] <-
                  as.numeric(retroDesignRes)
                tick <- tick - 1
              }
            }
            
            return(GC)
          }
        
        buildGC <- function(GC, table) {
          for (item in 1:nrow(GC)) {
            table$addRow(
              rowKey = item,
              values = list(
                label = GC[item, 1],
                itemName = GC[item, 2],
                obsEff = GC[item, 4],
                bootSE = GC[item, 7],
                hypTrueEff = GC[item, 3],
                typeM = GC[item, 5],
                power = GC[item, 6]
              )
            )
            if (GC[item, 4] < GC[item, 3]){
              highlight(table, item, 1)
              table$setNote("interpretGC", "Several items (flagged red) have observed effect sizes below the hypothesized true effect. For a guide to interpretation see: https://bit.ly/2I274JY")
            }
          }
        }
        
        # Model ----
        
        model <-
          binaryDIF.logistic(
            DATA = Data,
            group = group,
            groupOne = groupOne,
            anchor = anchor,
            anchorNames <- self$options$anchor,
            groupType = groupType,
            match = match,
            type = type,
            criterion = criterion,
            alpha = alpha,
            purify = purify,
            nIter = nIter,
            pAdjustMethod = pAdjustMethod
          )
        
        # Build GC tables ----
        runDesignAnalysis <- function(){
          if (self$options$designAnalysis) {
            if (self$options$designAnalysisSigOnly) {
              designList <- model$names[model$DIFitems]
            } else{
              designList <- model$names
            }
            GCTable = designAnalysis(
              designList = designList,
              Data = Data,
              group = group,
              match = model$matchScores,
              bootSims = bootSims,
              type = type
            )
            if (length(GCTable) != 0) {
              table <- self$results$gcTable
              buildGC(GCTable, table)
            }
          }
          
          if (self$options$designAnalysisEffectType == "coefficients") {
            if (self$options$designAnalysisSigOnly) {
              designList <- model$names[model$DIFitems]
            } else {
              designList <- model$names
            }
            gcTableCoefficients = designAnalysis(
              designList = designList,
              Data = Data,
              group = group,
              match = model$matchScores,
              coefficients = TRUE,
              bootSims = bootSims,
              type = type
            )
            if (length(gcTableCoefficients) != 0) {
              table <- self$results$gcTableCoefficients
              buildGC(gcTableCoefficients, table)
            }
          }
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
                ifelse(type == "both", 2 * length(groupOne), length(groupOne))
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
                  # length(model$groupOne),
                  #  " reference group(s) and ",
                  df,
                  " degree(s) of freedom."
                )
            ))
            
            blankRow()
            
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
            
            blankRow()
            
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
            
            blankRow()
            
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
                  loop[i] <- sum(model$difPur[1, ] == model$difPur[i + 1, ])
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
                
                blankRow()
                
              }
            }
            
            table$addRow(
              rowKey = self$results$DESCtable$rowCount + 1,
              values = list(bob = paste0(
                "Grouping variable: ", list(self$options$group)
              ))
            )
            
            if (length(groupValueList) > 2 & groupType == "group") {
              blankRow()
              table$addRow(
                rowKey = self$results$DESCtable$rowCount + 1,
                values = list(
                  bob = paste0(
                    "(The data file provided non-binary groupings. Please see below for the recoding legend.)"
                  )
                )
              )
              blankRow()
            }
            
            if (groupType == "group") {
              table$addRow(
                rowKey = self$results$DESCtable$rowCount + 1,
                values = list(bob = paste0("Group coding: "))
              )
              for (i in 1:length(groupValueList)) {
                table$addRow(
                  rowKey = self$results$DESCtable$rowCount + 1,
                  values = list(bob = paste0(
                    groupValueList[i],
                    " : ",
                    ifelse(i == 1, "Group A", "Group B")
                  ))
                )
              }
            }
            
            blankRow()
            
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
            blankRow()
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
            blankRow()
            
            table$addRow(
              rowKey = self$results$DESCtable$rowCount + 1,
              values = list(
                bob =  paste0("Effect size (Nagelkerke's 'R\u00B2') scale: ", switch(
                  self$options$difFlagScale,
                  zt = "Zumbo-Thomas",
                  jg = "Jodoin-Gierl"
                ))
              )
            )
            table$addRow(
              rowKey = self$results$DESCtable$rowCount + 1,
              values = list(bob =  switch(
                self$options$difFlagScale,
                zt = "'A': Negligible effect ('R\u00B2' 0 <> 0.13)",
                jg = "'A': Negligible effect ('R\u00B2' 0 <> 0.035)"
              ))
            )
            table$addRow(
              rowKey = self$results$DESCtable$rowCount + 1,
              values = list(bob =  switch(
                self$options$difFlagScale,
                zt = "'B': Moderate effect ('R\u00B2' 0.13 <> 0.26)",
                jg = "'B': Moderate effect ('R\u00B2' 0.035 <> 0.07)"
              ))
            )
            table$addRow(
              rowKey = self$results$DESCtable$rowCount + 1,
              values = list(bob =  switch(
                self$options$difFlagScale,
                zt = "'C': Large effect ('R\u00B2' 0.26 <> 1)",
                jg = "'C': Large effect ('R\u00B2' 0.07 <> 1)"
              ))
            )
            blankRow()
            
            if (self$options$designAnalysis) {
              table$addRow(
                rowKey = self$results$DESCtable$rowCount + 1,
                values = list(bob = paste0("Post-Data Design Analysis performed on ",
                                           ifelse(self$options$designAnalysisSigOnly, "only flagged ", "all "),
                                           "items using ",
                                           self$options$bootSims,
                                           "bootstraps to create an empirical distribution for ",
                                           "\u0394 Naeglekirke R\u00B2."
                                           ))
              )
            }
            
          }
        }
        
        # DIF Results Table ----
        
        calculateDIFTable <- function() {
          # self$results$debug$setContent(list(model))
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
                    effSize = model$deltaR2[i],
                    coeffMain = model$coefficients[i, 3],
                    coeffInteraction = switch(
                      self$options$type,
                      both = model$coefficients[i, 4],
                      udif = "",
                      nudif = model$coefficients[i, 4]
                    ),
                    ZT = ifelse(model$adjusted.p[i] <= alpha, model$ZT[i], ""),
                    JG = ifelse(model$adjusted.p[i] <= alpha, model$JG[i], "")
                  )
                )
              }
            }
            
            # Highlight DIF results table
            if (self$options$difFlagScale == "zt") {
              if (model$adjusted.p[i] <= alpha) {
                highlight(table, i, "ZT")
                highlight(table, i, "item")
              }
            } else {
              if (model$adjusted.p[i] <= alpha) {
                highlight(table, i, "JG")
                highlight(table, i, "item")
              }
            }
          }
          
          df <-
            ifelse(type == "both", 2 * length(groupOne), length(groupOne))
          table$setNote(
            key = "df",
            note = paste0(
              "Tests of significance conducted using: ",
              df,
              " degrees of freedom"
            )
          )
        }
        # State Savers ----
        
        DESCstate <- self$results$DESCtable$state
        
        if (!is.null(DESCstate)) {
          # ... populate the table from the state
        } else {
          DESCstate <- calculateDESCtable()
          # ... populate the table from the state
          self$results$DESCtable$setState(DESCstate)
        }
        
        DIFstate <- self$results$DIFtable$state
        
        if (!is.null(DIFstate)) {
          # ... populate the table from the state
        } else {
          DIFstate <- calculateDIFTable()
          # ... populate the table from the state
          self$results$DIFtable$setState(DIFstate)
        }
        
        gcState <- self$results$gcTable$state
        
        if (!is.null(gcState)) {
          # ... populate the table from the state
        } else {
          # ... populate the table from the state
          gcState <- runDesignAnalysis()
          self$results$gcTable$setState(gcState)
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
          ggtitle(paste("Item: ", colnames(plotData)),
                  subtitle = paste(
                    "Effect Size: ",
                    round(model$deltaR2[model$names == colnames(plotData)[1]], 3),
                    " | p = ",
                    round(model$adjusted.p[model$names == colnames(plotData)[1]], 3)
                  )) +
          xlab(ifelse(
            is.null(self$options$matchVar),
            "Total sore",
            "Supplied matching variable range"
          )) +
          ylab("Prediicted probability of endorsement") +
          theme_classic()
        
        print(p)
        TRUE
      }
    )
  )
