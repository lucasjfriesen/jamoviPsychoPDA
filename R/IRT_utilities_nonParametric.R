



buildPlots <- function(data,
                       ggtheme, 
                       theme,
                       plottype = c(
                         "OCC",
                         "EIS",
                         "density",
                         "expected",
                         "sd",
                         "triangle",
                         "tetrahedron",
                         "RCC",
                         "EISDIF",
                         "OCCDIF",
                         "PCA",
                         "expectedDIF",
                         "densityDIF"
                       ),
                       items = "all",
                       subjects,
                       axistype = c("scores", "distribution"),
                       alpha,
                       main,
                       xlab,
                       ylab,
                       xlim,
                       ylim,
                       cex, ...) {
  plottype <- match.arg(
    arg = plottype,
    choices = c(
      "OCC",
      "EIS",
      "density",
      "expected",
      "sd",
      "triangle",
      "tetrahedron",
      "RCC",
      "EISDIF",
      "OCCDIF",
      "PCA",
      "expectedDIF",
      "densityDIF"
    )
  )
  
  axistype <- match.arg(arg = axistype,
                        choices = c("scores",
                                    "distribution"))
  items <- 1:data$nitem
  xlab0 <- "Scores"
  
  if (plottype == "expected") {
    axistype = "distribution"
  }
  if (plottype == "expectedDIF") {
    axistype = "scores"
  }
  if (axistype == "distribution") {
    axis <- data$evalpoints
    quants <- data$subjthetasummary
    if (missing(xlab)) {
      xlab <-
        paste(simpleCap(data$thetadist[[1]]),
              paste(unlist(data$thetadist[-1]),
                    collapse = " "),
              "Quantiles",
              sep = " ")
    }
  } else {
    axis <- data$expectedscores
    quants <- data$subjscoresummary
    if (missing(xlab)) {
      xlab <- "Expected Score"
    }
  }
  
  plotData <-
    data.frame(subjscore = data$subjscore
               )

  p <- buildDensity(plotData, ggtheme, theme, ...)
  
  return(p)
  
}

buildDensity <- function(data, ggtheme, theme, ...){
  plotData = data
  ggplot(data = plotData, aes(x = subjscore)) +
    geom_density() +
    geom_abline(colour = "blue") +
    labs(title = "Observed Score Distribution",
         x = "Density of Score",
         y = "Scores") +
    ggtheme + theme(
    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
    plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
    )
}
