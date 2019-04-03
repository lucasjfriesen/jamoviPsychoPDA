












# Data Wrangling ----

glmDIFClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "glmDIFClass",
    inherit = glmDIFBase,
    private = list(
      .init = function() {
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
            is.null(self$data) |
            is.null(self$options$item)) {
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
        
        # Results functions ----
        
        highlight <- function(table, row, column) {
          for (i in column) {
            table$addFormat(
              rowNo = row,
              col = i,
              format = jmvcore::Cell.NEGATIVE
            )
          }
        }
        
        blankRow <- function(table) {
          table[nrow(table) + 1, "bob"] = ""
          return(table)
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
            if (GC[item, 4] < GC[item, 3]) {
              highlight(table, item, 1)
              table$setNote(
                "interpretGC",
                "Several items (flagged red) have observed effect sizes below the hypothesized true effect. For a guide to interpretation see: https://bit.ly/2I274JY"
              )
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
        runDesignAnalysis <- function() {
          if (self$options$designAnalysis) {
            if (self$options$designAnalysisSigOnly) {
              designList <- model$names[model$DIFitems]
            } else{
              designList <- model$names
            }
            
            if (is.na(designList[1])) {
              self$results$gcTable$addRow(
                rowKey = "twat",
                values = list(itemName = "No items flagged as exhibitting DIF.")
              )
              return()
            }
            
            GCTable = designAnalysis.nagR2(
              designList = designList,
              Data = Data,
              group = group,
              match = model$matchScores,
              bootSims = bootSims,
              type = type,
              hypTrueEff = self$options$D,
              difFlagScale = self$options$difFlagScale,
              sigOnly = self$options$designAnalysisSigOnly
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
            gcTableCoefficients = designAnalysis.coefficients(
              designList = designList,
              Data = Data,
              group = group,
              match = model$matchScores,
              coefficients = TRUE,
              bootSims = bootSims,
              type = type,
              hypTrueEff = self$options$D,
              difFlagScale = self$options$difFlagScale
            )
            if (length(gcTableCoefficients) != 0) {
              table <- self$results$gcTableCoefficients
              buildGC(gcTableCoefficients, table)
            }
          }
        }
        # Description Results Table ----
        calculateDESCtable <- function() {
          resDescTable <- data.frame(bob = NA)
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
          
          resDescTable[1, "bob"] =
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
          
          resDescTable <- blankRow(resDescTable)
          
          resDescTable[nrow(resDescTable) + 1, "bob"] = paste0(
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
          )
          
          resDescTable <- blankRow(resDescTable)
          
          if (model$pAdjustMethod == "none") {
            resDescTable[nrow(resDescTable) + 1, "bob"] = "No p-value adjustment for multiple comparisons"
          } else {
            pAdjMeth <- switch(
              model$pAdjustMethod,
              bonferroni = "Bonferroni",
              holm = "Holm",
              hochberg = "Hochberg",
              hommel = "Hommel",
              BH = "Benjamini-Hochberg",
              BY = "Benjamini-Yekutieli"
            )
            resDescTable[nrow(resDescTable) + 1, "bob"] = paste("Multiple comparisons made with ",
                                                                pAdjMeth,
                                                                " adjustment of p-values.")
          }
          
          resDescTable <- blankRow(resDescTable)
          
          if (model$purification) {
            if (model$nrPur <= 1) {
              word <- " iteration"
            } else {
              word <- " iterations"
            }
            if (!model$convergence) {
              resDescTable[nrow(resDescTable) + 1, "bob"] = paste("WARNING: no item purification convergence after ",
                                                                  model$nrPur,
                                                                  word,
                                                                  sep = " ")
              loop <- NULL
              for (i in 1:model$nrPur) {
                loop[i] <- sum(model$difPur[1,] == model$difPur[i + 1,])
              }
              if (max(loop) != length(model$genLogistik)) {
                resDescTable[nrow(resDescTable) + 1, "bob"] = paste("(Note: no loop detected in less than ",
                                                                    model$nrPur,
                                                                    word,
                                                                    ")",
                                                                    sep = "")
              } else {
                resDescTable[nrow(resDescTable) + 1, "bob"] = paste("(Note: loop of length ",
                                                                    min((1:model$nrPur)[loop ==
                                                                                          length(model$genLogistik)]),
                                                                    " in the item purification process)",
                                                                    sep = " ")
                resDescTable[nrow(resDescTable) + 1, "bob"] = paste("WARNING: following results based on the last iteration of the purification")
              }
            } else {
              resDescTable[nrow(resDescTable) + 1, "bob"] = paste("Convergence reached after ", model$nrPur, word, sep = " ")
              resDescTable <- blankRow(resDescTable)
            }
          }
          
          resDescTable[nrow(resDescTable) + 1, "bob"] = paste0("Grouping variable: ", list(self$options$group))
          
          if (length(groupValueList) > 2 & groupType == "group") {
            resDescTable <- blankRow(resDescTable)
            resDescTable[nrow(resDescTable) + 1, "bob"] = paste0(
              "(The data file provided non-binary groupings. Please see below for the recoding legend.)"
            )
            resDescTable <- blankRow(resDescTable)
          }
          
          if (groupType == "group") {
            resDescTable[nrow(resDescTable) + 1, "bob"] = paste0("Group coding: ")
            for (i in 1:length(groupValueList)) {
              resDescTable[nrow(resDescTable) + 1, "bob"] = paste0(groupValueList[i],
                                                                   " : ",
                                                                   ifelse(i == 1, "Group A", "Group B"))
            }
          }
          
          resDescTable <- blankRow(resDescTable)
          
          if (model$match[1] == "score") {
            resDescTable[nrow(resDescTable) + 1, "bob"] = "Matching variable: Test score"
          } else {
            resDescTable[nrow(resDescTable) + 1, "bob"] = paste0("Matching variable: ", self$options$matchVar)
          }
          resDescTable <- blankRow(resDescTable)
          if (is.null(model$anchor.names) |
              model$match != "score") {
            resDescTable[nrow(resDescTable) + 1, "bob"] = "No set of anchor items was provided"
          }
          else {
            resDescTable[nrow(resDescTable) + 1, "bob"] =  "Anchor items (provided by the user): "
            for (i in 1:length(self$options$anchor)) {
              resDescTable[nrow(resDescTable) + 1, "bob"] = self$options$anchor[[i]]
            }
          }
          resDescTable <- blankRow(resDescTable)
          
          resDescTable[nrow(resDescTable) + 1, "bob"] =  paste0("Effect size (Nagelkerke's 'R\u00B2') scale: ", switch(self$options$difFlagScale,
                                                                                                                       zt = "Zumbo-Thomas",
                                                                                                                       jg = "Jodoin-Gierl"))
          resDescTable[nrow(resDescTable) + 1, "bob"] =  switch(self$options$difFlagScale,
                                                                zt = "'A': Negligible effect ('R\u00B2' 0 <> 0.13)",
                                                                jg = "'A': Negligible effect ('R\u00B2' 0 <> 0.035)")
          resDescTable[nrow(resDescTable) + 1, "bob"] =  switch(self$options$difFlagScale,
                                                                zt = "'B': Moderate effect ('R\u00B2' 0.13 <> 0.26)",
                                                                jg = "'B': Moderate effect ('R\u00B2' 0.035 <> 0.07)")
          resDescTable[nrow(resDescTable) + 1, "bob"] =  switch(self$options$difFlagScale,
                                                                zt = "'C': Large effect ('R\u00B2' 0.26 <> 1)",
                                                                jg = "'C': Large effect ('R\u00B2' 0.07 <> 1)")
          resDescTable <- blankRow(resDescTable)
          
          if (self$options$designAnalysis) {
            resDescTable[nrow(resDescTable) + 1, "bob"] = paste0(
              "Post-Data Design Analysis performed on ",
              ifelse(
                self$options$designAnalysisSigOnly,
                "only flagged ",
                "all "
              ),
              "items using ",
              self$options$bootSims,
              " bootstraps to create an empirical distribution for ",
              "\u0394 Naeglekirke R\u00B2."
            )
          }
          table <- self$results$DESCtable

          for (i in 1:nrow(resDescTable)) {
            table$addRow(rowKey = i,
                         values = list(bob = resDescTable$bob[i]))
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
        # These don't work for DESCtable or gcTable
        
        DESCstate <- self$results$DESCtable$state
        if (!is.null(DESCstate)) {
          # ... populate the table from the state
        } else {
          # ... calculate the state
          DESCstate <- calculateDESCtable()
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
