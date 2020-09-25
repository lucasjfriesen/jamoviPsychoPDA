
# Data ----  

# Plot ----
  
# Test-level plots ----

# SD ----

buildSD <- function (data, ggtheme, theme, ...) 
{
Testvar <- apply(data$OCC[,-c(1:3)],2,function(x)sum(x*data$OCC[,3]**2 - (x*data$OCC[,3])**2))

Testsd<-sqrt(Testvar)

p <- ggplot() +
  geom_line(aes(x = data$expectedscores, y = Testsd)) +
  geom_vline(xintercept = data$subjscoresummary, linetype = "dashed", colour = "blue") +
  geom_text(mapping = aes(x = data$subjscoresummary,
                          y = min(Testsd),
                          label = labels(data$subjscoresummary),
                          hjust = -.1,
                          vjust = -1)
  ) +
  labs(title = "Test Standard Deviation",
       x = "Expected Score",
       y = "Standard Deviations") +
  ggtheme + theme(
    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
    plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))

return(p)
}

buildSDDIF <- function (data, ggtheme, theme, ...) 
{
  
  Testvar <- apply(data$OCC[,-c(1:3)],2,function(x)sum(x*data$OCC[,3]**2 - (x*data$OCC[,3])**2))
  Testsd <- sqrt(Testvar)
  newData <- as.data.frame(cbind(Testsd = Testsd, model = "Full"))
  newData$expectedscores <- t(data$expectedscores)
  
  for (group in data$groups){
    Testvar <- apply(data$DIF[[which(data$groups == group)]]$OCC[,-c(1:3)],
                     2,
                     function(x)sum(x*data$DIF[[which(data$groups == group)]]$OCC[,3]**2 - (x*data$DIF[[which(data$groups == group)]]$OCC[,3])**2))
    Testsd <- sqrt(Testvar)
    Testsd <- data.frame(cbind(Testsd = Testsd, model = group))
    Testsd$expectedscores <- t(data$expectedscores)
    
    newData = rbind(newData, Testsd)
  }
  newData$Testsd <- as.numeric(newData$Testsd)

  p <- ggplot() +
    geom_line(aes(x = newData$expectedscores, y = newData$Testsd, colour = newData$model)) +
    geom_vline(xintercept = data$subjscoresummary, linetype = "dashed", colour = "blue") +
    geom_text(mapping = aes(x = data$subjscoresummary,
                            y = min(newData$Testsd),
                            label = labels(data$subjscoresummary),
                            hjust = -.1,
                            vjust = -1)
    ) +
    labs(title = "Test Standard Deviation",
         x = "Expected Score",
         y = "Standard Deviations",
         colour = "Model") +
    ggtheme + theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
  
  return(p)
}

# Expected ----

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

buildExpectedDIF <- function (data, ggtheme, theme, ...) 
{
  newData <- data.frame(evalpoints = data$evalpoints, expectedscores = t(data$expectedscores), model = "Full")
  for (group in data$groups){
    newData = rbind(newData, data.frame(evalpoints = data$evalpoints, expectedscores = t(data$DIF[[which(data$groups == group)]]$expectedscores), model = group))
  }

  p <- ggplot() +
    geom_line(aes(x = newData$evalpoints, y = newData$expectedscores, colour = newData$model)) +
    geom_vline(xintercept = data$subjthetasummary, linetype = "dashed", colour = "blue") +
    geom_text(mapping = aes(x = data$subjthetasummary,
                            y = min(data$expectedscores),
                            label = labels(data$subjthetasummary),
                            hjust = -.1,
                            vjust = -1)
    ) +
    labs(title = "Expected Total Score",
         x = paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3]),
         y = "Expected Score",
         colour = "Model") +
    ggtheme + theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
  
  return(p)
}

# Density ----

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

buildDensityDIF <- function(data, ggtheme, theme, ...){
  
  newData <- data.frame(cbind(subjscore = data$subjscore, model = "Full"))
  for (group in data$groups){
    newData = rbind(newData, cbind(subjscore = data$DIF[[which(data$groups == group)]]$subjscore, model = group))
  }
  newData <- newData[newData$subjscore != 0,]
  
  p <- ggplot() +
    geom_density(aes(x = as.numeric(newData$subjscore), colour = newData$model)) +
    geom_vline(xintercept = data$subjscoresummary, linetype = "dashed", colour = "blue") + 
    geom_text(aes(x = data$subjscoresummary,
                  y = 0,
                  label = labels(data$subjscoresummary),
                  hjust = -.1,
                  vjust = -1)
    ) +
    labs(title = "Observed Score Distribution",
         x = "Density of Score",
         y = "Scores",
         colour = "Model") +
    ggtheme + theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
    )

  return(p)
}
  
# Item-level plots ----
  
buildOCC <- function (data, item, ggtheme, theme, ...) {
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
  

  #     SE <- data$stderrs[which(data$OCC[, 1] == x), ]
 
  #       if (alpha) {
  #         ME <- qnorm(1 - alpha/2) * SE[i, -c(1:3)]
  #         confhigh <- sapply(IRFlines[i, -c(1:3)] + ME, 
  #                            function(x) min(x, 1))
  #         conflow <- sapply(IRFlines[i, -c(1:3)] - ME, 
  #                           function(x) max(x, 0))

}

buildOCCDIF <- function (data, item, ggtheme, theme, ...) {
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
  
  
  #     SE <- data$stderrs[which(data$OCC[, 1] == x), ]
  
  #       if (alpha) {
  #         ME <- qnorm(1 - alpha/2) * SE[i, -c(1:3)]
  #         confhigh <- sapply(IRFlines[i, -c(1:3)] + ME, 
  #                            function(x) min(x, 1))
  #         conflow <- sapply(IRFlines[i, -c(1:3)] - ME, 
  #                           function(x) max(x, 0))
  
}
  
buildEIS <- function(data, item, alpha, ggtheme, theme, ...){
  dbins<-cut(data$subjtheta,breaks=c(-999,data$evalpoints[-length(data$evalpoints)],999),labels=FALSE)
  
  Estimate0<-data$OCC[which(data$OCC[, 1] == which(data$itemlabels == item)),]
  maxitem<-max(Estimate0[,3])
  minitem<-min(Estimate0[,3])
  Stderr0<-data$stderrs[which(data$OCC[, 1] == which(data$itemlabels == item)),]
  resp0<-data$binaryresp[which(data$binaryresp[,1]== which(data$itemlabels == item)),]
  
  Estimate1<-apply(Estimate0[,-c(1:3)],2,function(x)x*Estimate0[,3])
  Estimate<-apply(Estimate1,2,sum)

  Stderr1<-apply(Stderr0[,-c(1:3)],2,function(x)x*Stderr0[,3])
  Stderr<-apply(Stderr1,2,sum)
  
  respit1<-apply(resp0[,-c(1:3)],2,function(x)x*resp0[,3])
  respit<-apply(respit1,2,sum)
  
  propevalpoints<-numeric()
  if (is.null(alpha)){
    alpha = 0.05
  }
  SE<-qnorm(1-alpha/2)*Stderr
  
  
  confhigh<-data.frame(conf = sapply(Estimate+SE,function(x)min(x,maxitem)), level = factor("high"))
  conflow<-data.frame(conf = sapply(Estimate-SE,function(x)max(x,minitem)), level = factor("low"))
seData <- rbind(confhigh, conflow)

  
  for (i in 1:data$nevalpoints){
    binaryrespp<-respit[which(dbins==i)]
    propevalpoints[i]<-sum(binaryrespp)/length(binaryrespp)
  }
  
  p <- ggplot() +
    geom_line(aes(x = data$expectedscores, y = Estimate)) +
    geom_line(aes(x = rep(data$expectedscores, length.out = nrow(seData)), y = seData$conf, colour = seData$level)) +
    geom_point(aes(x = data$expectedscores, y = propevalpoints), size = .5, alpha = .5) +
    labs(Title = paste("Item: ",data$itemlabels[item]),
         subtitle = "Expected Item Score (Item Characteristic Curve)",
         x = "Expected Score",
         y = "Expected Item Score",
         colour = "Conf. Interval") +
    ggtheme + theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
  return(p)
}

buildEISDIF <- function(data, item, alpha, ggtheme, theme, ...){
  dbins<-cut(data$subjtheta,breaks=c(-999,data$evalpoints[-length(data$evalpoints)],999),labels=FALSE)
  
  Estimate0<-data$OCC[which(data$OCC[, 1] == which(data$itemlabels == item)),]
  maxitem<-max(Estimate0[,3])
  minitem<-min(Estimate0[,3])
  Stderr0<-data$stderrs[which(data$OCC[, 1] == which(data$itemlabels == item)),]
  resp0<-data$binaryresp[which(data$binaryresp[,1]== which(data$itemlabels == item)),]
  
  Estimate1<-apply(Estimate0[,-c(1:3)],2,function(x)x*Estimate0[,3])
  Estimate<-apply(Estimate1,2,sum)
  
  Stderr1<-apply(Stderr0[,-c(1:3)],2,function(x)x*Stderr0[,3])
  Stderr<-apply(Stderr1,2,sum)
  
  respit1<-apply(resp0[,-c(1:3)],2,function(x)x*resp0[,3])
  respit<-apply(respit1,2,sum)
  
  propevalpoints<-numeric()
  if (is.null(alpha)){
    alpha = 0.05
  }
  SE<-qnorm(1-alpha/2)*Stderr
  
  
  confhigh<-data.frame(conf = sapply(Estimate+SE,function(x)min(x,maxitem)), level = factor("high"))
  conflow<-data.frame(conf = sapply(Estimate-SE,function(x)max(x,minitem)), level = factor("low"))
  seData <- rbind(confhigh, conflow)
  
  
  for (i in 1:data$nevalpoints){
    binaryrespp<-respit[which(dbins==i)]
    propevalpoints[i]<-sum(binaryrespp)/length(binaryrespp)
  }
  
  p <- ggplot() +
    geom_line(aes(x = data$expectedscores, y = Estimate)) +
    geom_line(aes(x = rep(data$expectedscores, length.out = nrow(seData)), y = seData$conf, colour = seData$level)) +
    geom_point(aes(x = data$expectedscores, y = propevalpoints), size = .5, alpha = .5) +
    labs(Title = paste("Item: ",data$itemlabels[item]),
         subtitle = "Expected Item Score (Item Characteristic Curve)",
         x = "Expected Score",
         y = "Expected Item Score",
         colour = "Conf. Interval") +
    ggtheme + theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
  return(p)
}
