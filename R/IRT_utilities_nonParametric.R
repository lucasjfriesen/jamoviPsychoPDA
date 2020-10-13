
# Data ----  

# Plot ----
  
# Test-level plots ----

# SD ----

buildSD <- function (data, ggtheme, theme, axistype, ...) {
  
  if (axistype == 'distribution') {
    axis <- data$evalpoints
    quants <- data$subjthetasummary
    xlab <- paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
  }
  else{
    axis <- data$expectedscores
    quants <- data$subjscoresummary
    xlab <- "Expected Score"
  }

Testvar <- apply(data$OCC[,-c(1:3)],2,function(x)sum(x*data$OCC[,3]**2 - (x*data$OCC[,3])**2))

Testsd<-sqrt(Testvar)

p <- ggplot() +
  geom_line(aes(x = axis, y = Testsd)) +
  geom_vline(xintercept = quants, linetype = "dashed", colour = "blue") +
  geom_text(mapping = aes(x = quants,
                          y = min(Testsd),
                          label = labels(quants),
                          hjust = -.1,
                          vjust = -1)
  ) +
  labs(title = "Test Standard Deviation",
       x = xlab,
       y = "Standard Deviations") +
  ggtheme + theme(
    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
    plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))

return(p)
}

buildSDDIF <- function (data, ggtheme, theme, axistype, ...) {
  Testvar <- apply(data$OCC[,-c(1:3)],2,function(x)sum(x*data$OCC[,3]**2 - (x*data$OCC[,3])**2))
  Testsd <- sqrt(Testvar)
  newData <- as.data.frame(cbind(Testsd = Testsd, model = "Full"))
  
  if (axistype == 'distribution') {
    newData$axis <- data$evalpoints
    quants <- data$subjthetasummary
    xlab <- paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
  }
  else{
    newData$axis <- t(data$expectedscores)
    quants <- data$subjscoresummary
    xlab <- "Expected Score"
  }
  
  for (group in data$groups){
    Testvar <- apply(data$DIF[[which(data$groups == group)]]$OCC[,-c(1:3)],
                     2,
                     function(x)sum(x*data$DIF[[which(data$groups == group)]]$OCC[,3]**2 - (x*data$DIF[[which(data$groups == group)]]$OCC[,3])**2))
    Testsd <- sqrt(Testvar)
    Testsd <- data.frame(cbind(Testsd = Testsd, model = group))
    
    if (axistype == 'distribution') {
      Testsd$axis <- data$evalpoints
    }
    else{
      Testsd$axis <- t(data$expectedscores)
    }
    
    
    newData = rbind(newData, Testsd)
  }
  newData$Testsd <- as.numeric(newData$Testsd)

  p <- ggplot() +
    geom_line(aes(x = newData$axis, y = newData$Testsd, colour = newData$model)) +
    geom_vline(xintercept = quants, linetype = "dashed", colour = "blue") +
    geom_text(mapping = aes(x = quants,
                            y = min(newData$Testsd),
                            label = labels(quants),
                            hjust = -.1,
                            vjust = -1)
    ) +
    labs(title = "Test Standard Deviation",
         x = xlab,
         y = "Standard Deviations",
         colour = "Model") +
    ggtheme + theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
  
  return(p)
}

# Expected ----

buildExpected <- function (data, ggtheme, theme, axistype, ...) 
{

  if (axistype == 'distribution') {
    axis <- data$evalpoints
    yaxis <- t(data$expectedscores)
    quants <- data$subjthetasummary
    xlab <- paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
    ylab <- "Expected Score"
  }
  else{
    axis <- t(data$expectedscores)
    yaxis <- data$evalpoints
    quants <- data$subjscoresummary
    xlab <- "Expected Score"
    ylab <- paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
  }
  
  p <- ggplot() +
    geom_line(aes(x = axis, y = yaxis)) +
    geom_vline(xintercept = quants, linetype = "dashed", colour = "blue") +
    geom_text(mapping = aes(x = quants,
                            y = min(yaxis),
                            label = labels(quants),
                            hjust = -.1,
                            vjust = -1)
              ) +
    labs(title = "Expected Total Score",
         x = xlab,
         y = ylab) +
    ggtheme + theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
  
  return(p)
}

buildExpectedDIF <- function (data, ggtheme, theme, axistype, ...) 
{
  
  if (axistype == 'distribution') {
    quants <- data$subjthetasummary
    xlab <- paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
    ylab <- "Expected Score"
  } else{
    quants <- data$subjscoresummary
    xlab <- "Expected Score"
    ylab <- paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
  }
  
  if (axistype == 'distribution') {
    newData = data.frame(axis = data$evalpoints,
                                        yaxis = t(data$expectedscores),
                                        model = "Full")
  } else{
    newData = data.frame(axis = t(data$DIF$expectedscores),
                                        yaxis = data$evalpoints,
                                        model = "Full")
  }
  
  for (group in data$groups){
    if (axistype == 'distribution') {
      newData = rbind(newData, data.frame(axis = data$evalpoints,
                                          yaxis = t(data$DIF[[which(data$groups == group)]]$expectedscores),
                                          model = group))
    }
    else{
      newData = rbind(newData, data.frame(axis = t(data$DIF[[which(data$groups == group)]]$expectedscores),
                                          yaxis = data$evalpoints,
                                          model = group))
    }
  }

  p <- ggplot() +
    geom_line(aes(x = newData$axis, y = newData$yaxis, colour = newData$model)) +
    geom_vline(xintercept = quants, linetype = "dashed", colour = "blue") +
    geom_text(mapping = aes(x = quants,
                            y = min(newData$yaxis),
                            label = labels(quants),
                            hjust = -.1,
                            vjust = -1)
    ) +
    labs(title = "Expected Total Score",
         subtitle = "Differential Item Functioning",
         x = xlab,
         y = ylab,
         colour = "Model") +
    ggtheme + theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
  
  return(p)
}

# Density ----

buildDensity <- function(data, ggtheme, theme, axistype, ...){

  if (axistype == 'distribution') {
    axis <- data$subjtheta
    quants <- data$subjthetasummary
    xlab <- paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
    ylab <- "Density of Theta"
  } else{
    axis <- data$subjscore
    quants <- data$subjscoresummary
    xlab <- "Density of Scores"
    ylab <- paste("Quantiles of Expected Scores:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
  }
  
  p <- ggplot() +
    geom_density(aes(x = axis)) +
    geom_vline(xintercept = quants, linetype = "dashed", colour = "blue") + 
    geom_text(aes(x = quants,
                            y = 0,
                            label = labels(quants),
                            hjust = -.1,
                            vjust = -1)
    ) +
    labs(title = "Observed Score Distribution",
         x = xlab,
         y = ylab) +
    ggtheme + theme(
    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
    plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
    )
  
  return(p)
}

buildDensityDIF <- function(data, ggtheme, theme, axistype, ...){
  
  if (axistype == 'distribution') {
    
    newData <- data.frame(cbind(axis = data$subjtheta, model = "Full"))
    for (group in data$groups){
      newData = rbind(newData, cbind(axis = data$DIF[[which(data$groups == group)]]$subjtheta, model = group))
    }
    
    quants <- data$subjthetasummary
    xlab <- paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
    ylab <- "Density of Scores"
  } else{
    
    newData <- data.frame(cbind(axis = data$subjscore, model = "Full"))
    for (group in data$groups){
      newData = rbind(newData, cbind(axis = data$DIF[[which(data$groups == group)]]$subjscore, model = group))
    }
    
    quants <- data$subjscoresummary
    xlab <- "Density of Theta"
    ylab <- paste("Quantiles of Expected Scores:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
  }
  

  newData <- newData[newData$axis != 0,]
  
  p <- ggplot() +
    geom_density(aes(x = as.numeric(newData$axis), colour = newData$model)) +
    geom_vline(xintercept = quants, linetype = "dashed", colour = "blue") + 
    geom_text(aes(x = quants,
                  y = 0,
                  label = labels(quants),
                  hjust = -.1,
                  vjust = -1)
    ) +
    labs(title = "Observed Score Distribution",
         subtitle = "Differential Item Functioning",
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
  
buildOCC <- function (data, item, ggtheme, theme, axistype, ...) {
  
    IRFlines <- tidyr::pivot_longer(data.frame(data$OCC[which(data$OCC[, 1] == which(data$itemlabels == item)), ]),
                                    !c(X1, X2, X3),
                                    names_to = "evalPoint",
                                    values_to = "Probability")
    expectedScores <- rep(data$expectedscores, length.out = nrow(IRFlines))
    IRFlines$evalPoint <- rep(data$evalpoints, length.out = nrow(IRFlines))
    IRFlines <- cbind(IRFlines, expectedScores)
    
    if (axistype == "distribution"){
      colnames(IRFlines) = c("Item", "Option", "Key", "axis", "Probability", "Expected Score")
      xlab <- paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
    } else {
      colnames(IRFlines) = c("Item", "Option", "Key", "evalPoint", "Probability", "axis")
      xlab = "Expected Score"
    }
  
p <- ggplot() +
  geom_line(aes(x = IRFlines$axis, y = IRFlines$Probability, linetype = as.factor(IRFlines$Option), 
                colour = as.factor(IRFlines$Key))) +
  labs(title = "Option Characteristic Curves", 
  subtitle = paste0("Item: ", item),
         x = xlab,
         y = "Probability",
         linetype = "Option",
       colour = "Key") +
  ggtheme + theme(
    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
    plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
return(p)
  



}

buildOCCDIF <- function (data, item, option, ggtheme, theme, ...) {

  IRFlines <- tidyr::pivot_longer(data.frame(data$OCC[which(data$OCC[, 1] == which(data$itemlabels == item)), ]),
                                  !c(X1, X2, X3),
                                  names_to = "evalPoint",
                                  values_to = "Probability")
  expectedScores <- rep(data$expectedscores, length.out = nrow(IRFlines))
  IRFlines$evalPoint <- rep(data$evalpoints, length.out = nrow(IRFlines))
  IRFlines <- cbind(IRFlines, expectedScores, model = "Full")

    for (group in data$groups){
    newData <- data$DIF[[which(data$groups == group)]]
    newData <- tidyr::pivot_longer(data.frame(newData$OCC[which(newData$OCC[, 1] == which(newData$itemlabels == item)), ]),
                                    !c(X1, X2, X3),
                                    names_to = "evalPoint",
                                    values_to = "Probability")
    expectedScores <- rep(data$expectedscores, length.out = nrow(newData))
    IRFlines$evalPoint <- rep(data$evalpoints, length.out = nrow(IRFlines))
    newData <- cbind(newData, expectedScores, model = group)
    
    IRFlines <- rbind(IRFlines, newData)
  }
  
  if (axistype == "distribution"){
    colnames(IRFlines) = c("Item", "Option", "Key", "axis", "Probability", "Expected Score", "Model")
    xlab <- paste("Quantiles of Distribution:", data$thetadist[1], ", Mean:", data$thetadist[2], ", SD:", data$thetadist[3])
  } else {
    colnames(IRFlines) = c("Item", "Option", "Key", "evalPoint", "Probability", "axis", "Model")
    xlab = "Expected Score"
  }

  
  IRFlines <- IRFlines[IRFlines$Option == option,]
  
  p <- ggplot() +
    geom_line(aes(x = IRFlines$`Expected Score`, y = IRFlines$Probability, colour = IRFlines$Model)) +
    labs(title = "Option Characteristic Curves",
         subtitle = paste0("Item: ", item, "    Option: ", option),
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
    geom_ribbon(aes(x = data$expectedscores, ymin = conflow$conf, ymax = confhigh$conf), alpha = .5, fill = "grey70") +
    geom_point(aes(x = data$expectedscores, y = propevalpoints), size = .5, alpha = .5) +
    labs(title = "Expected Item Score (Item Characteristic Curve)",
         subtitle = paste0("Item: ", item),
         x = "Expected Score",
         y = "Expected Item Score") +
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
  
  newData <-
    data.frame(
      expectedScores = t(data$expectedscores),
      Estimate,
      confhigh = confhigh$conf,
      conflow = conflow$conf,
      level = confhigh$level,
      propevalpoints,
      model = "Full"
    )
  
  for (group in data$groups){
    subdata <- data$DIF[[which(data$groups == group)]]
    dbins<-cut(subdata$subjtheta,breaks=c(-999,subdata$evalpoints[-length(subdata$evalpoints)],999),labels=FALSE)
    
    Estimate0<-subdata$OCC[which(subdata$OCC[, 1] == which(subdata$itemlabels == item)),]
    maxitem<-max(Estimate0[,3])
    minitem<-min(Estimate0[,3])
    Stderr0<-subdata$stderrs[which(subdata$OCC[, 1] == which(subdata$itemlabels == item)),]
    resp0<-subdata$binaryresp[which(subdata$binaryresp[,1]== which(subdata$itemlabels == item)),]
    
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
    
    
    
    for (i in 1:subdata$nevalpoints){
      binaryrespp<-respit[which(dbins==i)]
      propevalpoints[i]<-sum(binaryrespp)/length(binaryrespp)
    }
    
    newData <-
      rbind(newData, data.frame(
        expectedScores = t(data$expectedscores),
        Estimate,
        confhigh = confhigh$conf,
        conflow = conflow$conf,
        level = confhigh$level,
        propevalpoints,
        model = group
      ))
  }
  
  p <- ggplot() +
    geom_line(aes(x = newData$expectedScores, y = newData$Estimate, colour = newData$model)) +
    geom_point(aes(x = newData$expectedScores, y = newData$propevalpoints, colour = newData$model), size = 1, alpha = .5) +
    labs(title = "Expected Item Score (Item Characteristic Curve)",
         subtitle = paste("Item: ", item),
         x = "Expected Score",
         y = "Expected Item Score") +
    ggtheme + theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
  return(p)
}

buildPairwiseDIF <- function(data, item, alpha, ggtheme, theme, ...){
  grps <- data$groups
  ngrps <- length(grps)
  
  newData <- data.frame()
  
  for (i in 1:(ngrps - 1)) {
    for (j in (i + 1):ngrps) {
      grp1 <- data$DIF[[i]]
      grp2 <- data$DIF[[j]]
      
      plotData <-
        data.frame(
          xvalue = apply(grp1$OCC[, -c(1:3)], 2, function(x)
            sum(x * grp1$OCC[, 3])),
          yvalue = apply(grp2$OCC[, -c(1:3)], 2, function(x)
            sum(x * grp2$OCC[, 3])),
          xgroup = grps[i],
          ygroup = grps[j],
          facet = paste0("x = ",grps[i], ", y = ", grps[j])
        )
      newData <- rbind(newData, plotData)
      
    }
  }
  
  
  p <- ggplot(data = newData, aes(x = xvalue, y = yvalue, colour = facet)) +
    geom_line(size = 1.5) +
    geom_hline(yintercept = (quantile(newData$yvalue, c(.05, .25, .5 , .75, .95))), lty = 2, colour = "blue") +
    geom_vline(xintercept = (quantile(newData$xvalue, c(.05, .25, .5 , .75, .95))), lty = 2, colour = "blue") +
    labs(title = "Pairwise Differential Item Functioning",
         subtitle = paste0("Item: ", item),
         x = "",
         y = "") +
    facet_wrap(~ facet,
               dir = "v") +
    ggtheme + theme(
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5.5 * 1.2)),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
  
  return(p)
}
