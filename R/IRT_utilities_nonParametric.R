
# Data ----  

# Plot ----
  
# Test-level plots ----

buildExpected <- function (data, ggtheme, theme, ...) 
{

  p <- ggplot() +
    geom_line(aes(x = data$evalpoints, y = data$expectedscores)) +
    geom_vline(xintercept = data$subjthetasummary, linetype = "dashed", colour = "blue") +
    geom_text(mapping = aes(x = data$subjthetasummary,
                            y = min(data$expectedscores),
                            label = labels(data$subjthetasummary),
                            hjust = -.1,
                            vjust = -1)
              ) +
    labs(title = "Expected Total Score",
         x = paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3]),
         y = "Expected Score") +
    ggtheme + theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
  
  return(p)
}

buildDensity <- function(data, ggtheme, theme, ...){

  p <- ggplot() +
    geom_density(aes(x = data$subjscore)) +
    geom_vline(xintercept = data$subjscoresummary, linetype = "dashed", colour = "blue") + 
    geom_text(aes(x = data$subjscoresummary,
                            y = 0,
                            label = labels(data$subjscoresummary),
                            hjust = -.1,
                            vjust = -1)
    ) +
    labs(title = "Observed Score Distribution",
         x = "Density of Score",
         y = "Scores") +
    ggtheme + theme(
    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
    plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
    )
  
  return(p)
}
  
# Item-level plots ----
  
buildOCC <- function (data, item, ggtheme, theme, ...) {
  # {
  #   if (missing(ylim)) {
  #     ylim = c(0, 1)
  #   }
  #   if (missing(xlim)) {
  #     xlim = c(min(axis), max(axis))
  #   }
  #   if (missing(ylab)) {
  #     ylab = "Probability"
  #   }
  #   if (missing(main)) {
  #     main = -1
  #   }
  #   if (missing(alpha)) {
  #     alpha <- FALSE
  #   }
  
  IRFlines <- tidyr::pivot_longer(data.frame(data$OCC[which(data$OCC[, 1] == which(data$itemlabels == item)), ]),
                                  !c(X1, X2, X3),
                                  names_to = "evalPoint",
                                  values_to = "Probability")
  expectedScores <- rep(data$expectedscores, length.out = nrow(IRFlines))
  
  IRFlines <- cbind(IRFlines, expectedScores)
  
  colnames(IRFlines) = c("Item", "Option", "Key", "evalPoint", "Probability", "Expected Score")
  
p <- ggplot() +
  geom_line(aes(x = IRFlines$`Expected Score`, y = IRFlines$Probability, linetype = as.factor(IRFlines$Option), 
                colour = as.factor(IRFlines$Key))) +
  labs(Title = "Option Characteristic Curves",
         x = "Expected Score",
         y = "Probability",
         linetype = "Option",
       colour = "Key") +
  ggtheme + theme(
    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
    plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
return(p)
  
  #   plotit <- function(x, OBJ, alpha, axis, quants, main, xlim, 
  #                      ylim, xlab, ylab, ...) {
  #     if (main == -1) {
  #       main = paste("Item: ", OBJ$itemlabels[x], "\n")
  #     }
  #     plot(1, ylim = ylim, main = main, xlim = xlim, type = "l", 
  #          ylab = ylab, xlab = xlab, ...)
  #     IRFlines <- OBJ$OCC[which(OBJ$OCC[, 1] == x), ]
  #     SE <- OBJ$stderrs[which(OBJ$OCC[, 1] == x), ]
  #     for (i in 1:nrow(IRFlines)) {
  #       if (OBJ$scale[x] == 1) {
  #         if (IRFlines[i, 3] == 1) {
  #           colortouse <- "blue"
  #           lwidth <- 2
  #         }
  #         else {
  #           colortouse <- "red"
  #           lwidth <- 1
  #         }
  #       }
  #       else {
  #         colortouse <- "black"
  #         lwidth <- 1
  #       }
  #       lines(axis, IRFlines[i, -c(1:3)], col = colortouse, 
  #             lwd = lwidth)
  #       word <- ifelse(IRFlines[i, 2] == -1, "NA", 
  #                      as.character(IRFlines[i, 2]))
  #       wordloc <- round(runif(1, min = 10, max = OBJ$nevalpoints - 
  #                                10))
  #       text(axis[wordloc], IRFlines[i, wordloc], word, cex = 0.7)
  #       if (alpha) {
  #         ME <- qnorm(1 - alpha/2) * SE[i, -c(1:3)]
  #         confhigh <- sapply(IRFlines[i, -c(1:3)] + ME, 
  #                            function(x) min(x, 1))
  #         conflow <- sapply(IRFlines[i, -c(1:3)] - ME, 
  #                           function(x) max(x, 0))
  #         lines(axis, confhigh, lty = 2, col = colortouse)
  #         lines(axis, conflow, lty = 2, col = colortouse)
  #       }
  #     }
  #     axis(3, at = quants, lab = labels(quants), tck = 0)
  #     abline(v = quants, col = "blue", lty = 2)
  #     box()
  #   }
  #   par(ask = TRUE)
  #   nada <- sapply(items, plotit, OBJ = OBJ, alpha = alpha, axis = axis, 
  #                  quants = quants, main, xlim, ylim, xlab, ylab, ...)
  # }
  
}
