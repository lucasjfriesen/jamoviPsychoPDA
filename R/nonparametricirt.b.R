#options(jamovi_home = "C:\\Program Files\\jamovi 1.6.3.0")


# This file is a generated template, your changes will not be overwritten

nonParametricIRTClass <-
  if (requireNamespace('jmvcore'))
    R6::R6Class(
      "nonParametricIRTClass",
      inherit = nonParametricIRTBase,
      private = list(
        .postInit = function() {
          state <- self$results$resTable$state[[1]]
          freqTable <- self$results$resTable$state[[2]]
          
          if (!is.null(state)) {
            corrTable <- KernSmoothIRT:::print.ksIRT(state)
            table <- self$results$resTable
            
            data <- self$data
            data <- data[2:nrow(data), self$options$item]
            
            for (i in 1:length(self$options$item)) {
              names(freqTable)[is.na(names(freqTable))] <- "MISSING"
              
              table$addRow(
                rowKey = i,
                values = list(
                  Item = self$options$item[i],
                  Option = "Total",
                  Correlation = corrTable[i, "Correlation"],
                  N = sum(freqTable[[i]])
                )
              )
              for (j in 1:length(freqTable[[i]])) {
                table$addRow(
                  rowKey = i,
                  values = list(
                    Item = self$options$item[i],
                    Option = names(freqTable[[i]])[j],
                    Correlation = corrTable[i, "Correlation"],
                    N = freqTable[[i]][j]
                  )
                )
              }
            }
          }
        },
        .run = function() {
          if (is.null(self$data) || is.null(self$options$item)) {
            self$results$instructions$setContent(
              "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>Welcome to PsychoPDA's Non-Parametric Item Response Theory analysis. To get started:</p>
            <ol>
            <li>Place items to be analyzed 'Items' slot.<br /><br /></li>
            <li><b>The first row will be used as the key for each item.</b><br /><br /></li>
            <li>[<em>Optional</em>] Indicate any further specifications.<br /><br /></li>
            </ol>
            <p>If you encounter any errors, or have questions, please see the <a href='https://lucasjfriesen.github.io/jamoviPsychoPDA_docs/DIF_index.html' target = '_blank'>documentation</a></p>
            </div>
            </body>
            </html>"
            )
          }
          if (is.null(self$data) ||
              is.null(self$options$item))
          {
            self$results$instructions$setVisible(visible = TRUE)
            return()
          } else {
            self$results$instructions$setVisible(visible = FALSE)
            
          }
          
          # Procedure Notes ----
          blankRow <- function(table) {
            table[nrow(table) + 1, "bob"] = ""
            return(table)
          }
          procedureNotes <- function(){
            self$results$procedureNotes$setVisible(visible = TRUE)
            notesTable <- data.frame(bob = NA)
            
            notesTable[1, "bob"] = paste0("Data Overview: ")
            
            notesTable <- blankRow(notesTable)
            
            notesTable[nrow(notesTable) + 1, "bob"] = paste0("Number of items: ", length(self$options$item))
    
            notesTable[nrow(notesTable) + 1, "bob"] = paste0("Number of responses: ", nrow(data))
            
            # notesTable[nrow(notesTable) + 1, "bob"] = paste0("Number of incomplete responses: ", as.character)sum(!complete.cases(data)))
            
            notesTable[nrow(notesTable) + 1, "bob"] = paste0("Number of groups: ", ifelse(is.null(self$options$group), 1, length(groups)))
            
            notesTable <- blankRow(notesTable)
            
            notesTable[nrow(notesTable) + 1, "bob"] = paste0("Response Options: ")

            notesTable <- blankRow(notesTable)
            
            notesTable[nrow(notesTable) + 1, "bob"] = paste0(
              "Item format: ", switch(self$options$format,
                                          formatMC = "Binary",
                                          formatPartial = "Rating-scale",
                                          formatNominal = "Nominal")
            )
            
            notesTable[nrow(notesTable) + 1, "bob"] = paste0(
              "Missing responses: ", switch(self$options$miss,
                                                option = "Permitted",
                                                random.unif = "Random fill uniform",
                                                random.multinom = "Random fill multinomial",
                                                omit = "Omit")
            )
            
            notesTable[nrow(notesTable) + 1, "bob"] = paste0(
              "NA weight: ", self$options$NAweight
            )
            
            notesTable <- blankRow(notesTable)
            
            return(notesTable)
          }
          
          # Data wrangling ----
          
          data = self$data
          
          if (is.null(self$options$group)) {
            groups <- FALSE
          } else {
            groups <- data[2:nrow(data), self$options$group]
          }
          
          # if (is.null(self$options$key)){
          key = data[1, self$options$item]
          data = data[2:nrow(data), self$options$item]
          # } else {
          #     key = data[1:length(self$options$item), self$options$key]
          #     data = data[, self$options$item]
          # }
          
          format <- switch(
            self$options$format,
            formatMC = 1,
            formatPartial = 2,
            formatNominal = 3
          )
          
          if (format == 3) {
            SubRank = data[, self$options$SubRank]
          } else {
            SubRank = NULL
          }
          
          # Functions ----
          
          runResults <- function() {
            modelResults <-
              KernSmoothIRT::ksIRT(
                responses = data,
                key = key,
                format = format,
                # kernel = self$options$kernel,
                itemlabels = self$options$item,
                # weights = weights, # The ksIRT argument requires a matrix per item. Unsure how to implement in jamovi
                miss = self$options$miss,
                NAweight = self$options$NAweight,
                # evalpoints = self$options$evalpoints, # The ksIRT argument requires a vector of numbers. Unsure how to implement in jamovi
                # nevalpoints = self$options$nevalpoints,
                # bandwidth = self$options$bandwidth,
                # RankFun = match.fun(self$options$RankFun),
                # SubRank = SubRank,
                # thetadist = thetadist,
                groups = groups
              )
            # modelResults$binaryresp <- NULL
            modelResults$RCC <- NULL
            for (i in 1:length(modelResults$DIF)){
              # modelResults$DIF[[i]]$binaryresp <- NULL
              modelResults$DIF[[i]]$RCC <- NULL
            }
            return(modelResults)
          }
          
          calculateResTable <- function() {
            corrTable <- KernSmoothIRT:::print.ksIRT(modelResults)
            table <- self$results$resTable
            freqTable <- list()
            for (i in 1:length(self$options$item)) {
              freqTable[[i]] <- table(data[, i], exclude = NULL)
              names(freqTable)[is.na(names(freqTable))] <-
                "MISSING"
              
              table$addRow(
                rowKey = i,
                values = list(
                  Item = self$options$item[i],
                  Option = "Total",
                  Correlation = corrTable[i, "Correlation"],
                  N = sum(freqTable[[i]])
                )
              )
              for (j in 1:length(freqTable[[i]])) {
                table$addRow(
                  rowKey = i,
                  values = list(
                    Item = self$options$item[i],
                    Option = names(freqTable[[i]])[j],
                    Correlation = corrTable[i, "Correlation"],
                    N = freqTable[[i]][j]
                  )
                )
              }
            }
            freqTable <<- freqTable
          }
          
          modelResults <- runResults()

          # States ----
          
          # Procedure Notes ----
          notesState <- self$results$procedureNotes$state
          if (!is.null(notesState)) {
            # ... populate the table from the state
            table <- self$results$procedureNotes
            for (i in 1:nrow(notesState)) {
              table$addRow(rowKey = i,
                           values = list(bob = notesState$bob[i]))
            }
          } else {
            # ... calculate the state
            table <- self$results$procedureNotes
            notesState <- procedureNotes()
            for (i in 1:nrow(notesState)) {
              table$addRow(rowKey = i,
                           values = list(bob = notesState$bob[i]))
            }
            self$results$procedureNotes$setState(notesState)
          }
          
          # Results table ----
          
          resState <- self$results$resTable$state

          if (!is.null(resState)) {
            # ... populate the table from the state
          } else {
            # ... create the table and the state
            resState <- calculateResTable()
            self$results$resTable$setState(list(modelResults, freqTable))
          }
          
          # printState <- function(x) {
          #   if (!is.null(x$state))
          #     print(paste(self$path, ':', length(serialize(
          #       x$state, connection = NULL
          #     ))))
          #   if (inherits(x, 'Group') || inherits(x, 'Array')) {
          #     for (child in x$items)
          #       printState(child)
          #   }
          # }
          # 
          
          
          # Test level plots ----
          axisTypeTest = self$options$axisTypeTest
          
          # Expected ----
          if (self$options$testPlotExpected) {
            if (self$results$testPlotExpected$isNotFilled()) {
              expectedPlotResults <- self$results$testPlotExpected
              expectedPlotResults$setState(buildExpectedData(modelResults, axisTypeTest))
            }
          }
          
          if (self$options$testPlotExpectedDIF) {
            if (self$results$testPlotExpectedDIF$isNotFilled()) {
              expectedPlotDIFResults <- self$results$testPlotExpectedDIF
              expectedPlotDIFResults$setState(buildExpectedDIFData(modelResults, axisTypeTest))
            }
          }
          
          # Density ----
          
          if (self$options$testPlotDensity) {
            if (self$results$testPlotDensity$isNotFilled()) {
              densityPlotResults <- self$results$testPlotDensity
              densityPlotResults$setState(buildDensityData(modelResults, axisTypeTest))
            }
          }
          
          if (self$options$testPlotDensityDIF) {
             if (self$results$testPlotDensityDIF$isNotFilled()) {
              densityPlotDIFResults <- self$results$testPlotDensityDIF
              densityPlotDIFResults$setState(buildDensityDIFData(modelResults, axisTypeTest))
            }
          }
          
          
          # SD ----
          
          if (self$options$testPlotSD) {
            if (self$results$testPlotSD$isNotFilled()) {
              sdPlotResults <- self$results$testPlotSD
              sdPlotResults$setState(buildSDData(modelResults, axisTypeTest))
            }
          }
          
          if (self$options$testPlotSDDIF) {
            if (self$results$testPlotSDDIF$isNotFilled()) {
              sdPlotDIFResults <- self$results$testPlotSDDIF
              sdPlotDIFResults$setState(buildSDDIFData(modelResults, axisTypeTest))
            }
          }
          
          
          
          # item level plots ----
          
          axisTypeItem = self$options$axisTypeItem
          
          # Pairwise ----
          if (self$options$pairwisePlotsDIF) {
            if (self$results$pairwisePlotsDIF$isNotFilled()) {
              for (i in self$options$itemPlotSupplier) {
                if (!all(self$options$itemPlotSupplier %in% self$options$item)) {
                  stop(
                    paste0(
                      "Not all items selected to be plotted have been included in the model, please remove: ",
                      self$options$itemPlotSupplier[!self$options$itemPlotSupplier %in% self$options$item]
                    ),
                    call. = FALSE
                  )
                } else {
                  pairwisePlotDataDIF <-
                    self$results$pairwisePlotsDIF$get(key = i)
                  pairwisePlotDataDIF$setState(list(
                    buildPairwiseDIFData(modelResults, item = i),
                    item = i,
                    option = self$options$OCCoption
                  ))
                }
              }
            }
          }
          
          # OCC ----
          if (self$options$itemPlotOCC) {
            if (self$results$occPlots$isNotFilled()) {
              for (i in self$options$itemPlotSupplier) {
                if (!all(self$options$itemPlotSupplier %in% self$options$item)) {
                  stop(
                    paste0(
                      "Not all items selected to be plotted have been included in the model, please remove: ",
                      self$options$itemPlotSupplier[!self$options$itemPlotSupplier %in% self$options$item]
                    ),
                    call. = FALSE
                  )
                } else {
                  occPlotData <-
                    self$results$occPlots$get(key = i)
                  occPlotData$setState(list(buildOCCData(modelResults, i, axisTypeItem), item = i))
                }
              }
            }
          }
          
          if (self$options$itemPlotOCCDIF) {
            if (self$results$occPlotsDIF$isNotFilled()) {
              for (i in self$options$itemPlotSupplier) {
                if (!all(self$options$itemPlotSupplier %in% self$options$item)) {
                  stop(
                    paste0(
                      "Not all items selected to be plotted have been included in the model, please remove: ",
                      self$options$itemPlotSupplier[!self$options$itemPlotSupplier %in% self$options$item]
                    ),
                    call. = FALSE
                  )
                } else {
                  occPlotDataDIF <-
                    self$results$occPlotsDIF$get(key = i)
                  occPlotDataDIF$setState(list(
                    buildOCCDIFData(
                      modelResults,
                      i,
                      option = self$options$OCCoption,
                      axisTypeItem
                    ),
                    item = i,
                    option = self$options$OCCoption
                  ))
                }
              }
            }
          }
          
          # EIS ----
          if (self$options$itemPlotEIS) {
            if (self$results$eisPlots$isNotFilled()) {
              for (i in self$options$itemPlotSupplier) {
                if (!all(self$options$itemPlotSupplier %in% self$options$item)) {
                  stop(
                    paste0(
                      "Not all items selected to be plotted have been included in the model, please remove: ",
                      self$options$itemPlotSupplier[!self$options$itemPlotSupplier %in% self$options$item]
                    ),
                    call. = FALSE
                  )
                } else {
                  eisPlotData <-
                    self$results$eisPlots$get(key = i)
                  eisPlotData$setState(list(buildEISData(modelResults, i), item = i))
                }
              }
            }
          }
          
          if (self$options$itemPlotEISDIF) {
            if (self$results$eisPlotsDIF$isNotFilled()) {
              for (i in self$options$itemPlotSupplier) {
                if (!all(self$options$itemPlotSupplier %in% self$options$item)) {
                  stop(
                    paste0(
                      "Not all items selected to be plotted have been included in the model, please remove: ",
                      self$options$itemPlotSupplier[!self$options$itemPlotSupplier %in% self$options$item]
                    ),
                    call. = FALSE
                  )
                } else {
                  eisPlotDataDIF <-
                    self$results$eisPlotsDIF$get(key = i)
                  eisPlotDataDIF$setState(list(buildEISDIFData(modelResults, i), item = i))
                }
              }
            }
          }
          
        },
        
        # Test-level plots ----
        
        .testPlotExpected = function(testPlotData, ggtheme, theme, ...) {
          plotData <- testPlotData$state
          p <-
            buildExpected(plotData,
                          ggtheme,
                          theme,
                          axistype = self$options$axisTypeTest,
                          ...)
          
          print(p)
          TRUE
        },
        
        .testPlotExpectedDIF = function(testPlotData, ggtheme, theme, ...) {

          if (is.null(self$options$group)){
            stop("DIF plots require a grouping variable to be supplied")
          }
          
          plotData <- testPlotData$state
          p <-
            buildExpectedDIF(plotData,
                             ggtheme,
                             theme,
                             axistype = self$options$axisTypeTest,
                             ...)
          
          print(p)
          TRUE
        },
        
        .testPlotDensity = function(testPlotData, ggtheme, theme, ...) {
          # if (is.null(testPlotData$state)) {
          #   return(FALSE)
          # }
          
          plotData <- testPlotData$state
          
          p <-
            buildDensity(plotData,
                         ggtheme,
                         theme,
                         axistype = self$options$axisTypeTest,
                         ...)
          
          print(p)
          TRUE
        },
        
        .testPlotDensityDIF = function(testPlotData, ggtheme, theme, ...) {
          # if (is.null(testPlotData$state)) {
          #   return(FALSE)
          # }
          if (is.null(self$options$group)){
            stop("DIF plots require a grouping variable to be supplied")
          }
          
          plotData <- testPlotData$state
          
          p <-
            buildDensityDIF(plotData,
                            ggtheme,
                            theme,
                            axistype = self$options$axisTypeTest,
                            ...)
          
          print(p)
          TRUE
        },
        
        .testPlotSD = function(testPlotData, ggtheme, theme, ...) {
          #
          # if (is.null(testPlotData$state)) {
          #   return(FALSE)
          # }
          
          plotData <- testPlotData$state
          
          p <-
            buildSD(plotData,
                    ggtheme,
                    theme,
                    axistype = self$options$axisTypeTest,
                    ...)
          
          print(p)
          TRUE
        },
        
        .testPlotSDDIF = function(testPlotData, ggtheme, theme, ...) {
          #
          # if (is.null(testPlotData$state)) {
          #   return(FALSE)
          # }
          if (is.null(self$options$group)){
            stop("DIF plots require a grouping variable to be supplied")
          }
          plotData <- testPlotData$state
          
          p <-
            buildSDDIF(plotData,
                       ggtheme,
                       theme,
                       axistype = self$options$axisTypeTest,
                       ...)
          
          print(p)
          TRUE
        },
        
        # Item-level plots ----
        
        .occPlot = function(itemPlotData, ggtheme, theme, ...) {
          if (is.null(itemPlotData$state)) {
            return(FALSE)
          }
          
          plotData <- itemPlotData$state[[1]]
          item = itemPlotData$state[[2]]
          
          p <-
            buildOCC(
              plotData,
              item = item,
              axisType = self$options$axisTypeItem,
              ggtheme,
              theme,
              ...
            )
          
          print(p)
          TRUE
        },
        
        .occPlotDIF = function(itemPlotData, ggtheme, theme, ...) {
          if (is.null(itemPlotData$state)) {
            return(FALSE)
          }
          
          plotData <- itemPlotData$state[[1]]
          item = itemPlotData$state[[2]]
          option = itemPlotData$state[[3]]
          
          p <-
            buildOCCDIF(
              plotData,
              item = item,
              option = option,
              axisType = self$options$axisTypeItem,
              ggtheme,
              theme,
              ...
            )
          
          print(p)
          TRUE
        },
        
        .eisPlot = function(itemPlotData, ggtheme, theme, ...) {
          if (is.null(itemPlotData$state)) {
            return(FALSE)
          }
          
          plotData <- itemPlotData$state[[1]]
          item = itemPlotData$state[[2]]
          
          p <-
            buildEIS(plotData,
                     item = item,
                     alpha = NULL,
                     ggtheme,
                     theme,
                     ...)
          
          print(p)
          TRUE
        },
        
        .eisPlotDIF = function(itemPlotData, ggtheme, theme, ...) {
          if (is.null(itemPlotData$state)) {
            return(FALSE)
          }
          
          plotData <- itemPlotData$state[[1]]
          item = itemPlotData$state[[2]]
          
          p <-
            buildEISDIF(plotData,
                        item = item,
                        alpha = NULL,
                        ggtheme,
                        theme,
                        ...)
          
          print(p)
          TRUE
        },
        
        .pairwisePlotsDIF = function(itemPlotData, ggtheme, theme, ...) {
          if (is.null(itemPlotData$state)) {
            return(FALSE)
          }
          
          plotData <- itemPlotData$state[[1]]
          item = itemPlotData$state[[2]]
          
          p <-
            buildPairwiseDIF(plotData,
                             item = item,
                             alpha = NULL,
                             ggtheme,
                             theme,
                             ...)
          
          print(p)
          TRUE
        }
        
      )
    )
