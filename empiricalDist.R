library(difR)
library(boot)
data("verbal")
DATA <- cbind(verbal$S1wantCurse, verbal$Gender, verbal$Anger)

print.bootSE <- function(x, digits = getOption("digits"),
                          index = 1L:ncol(boot.out$t), ...)
{
#
# Print the output of a bootstrap
#
    boot.out <- x
    sim <- boot.out$sim
    cl <- boot.out$call
    t <- matrix(boot.out$t[, index], nrow = nrow(boot.out$t))
    allNA <- apply(t,2L,function(t) all(is.na(t)))
    ind1 <- index[allNA]
    index <- index[!allNA]
    t <- matrix(t[, !allNA], nrow = nrow(t))
    rn <- paste("t",index,"*",sep="")
    if (length(index) == 0L)
        op <- NULL
    else if (is.null(t0 <- boot.out$t0)) {
        if (is.null(boot.out$call$weights))
            op <- cbind(apply(t,2L,mean,na.rm=TRUE),
                        sqrt(apply(t,2L,function(t.st) var(t.st[!is.na(t.st)]))))
        else {
            op <- NULL
            for (i in index)
                op <- rbind(op, imp.moments(boot.out,index=i)$rat)
            op[,2L] <- sqrt(op[,2])
        }
        dimnames(op) <- list(rn,c("mean", "std. error"))
        return(op)
    }
    else {
        t0 <- boot.out$t0[index]
        if (is.null(boot.out$call$weights)) {
            op <- cbind(t0,apply(t,2L,mean,na.rm=TRUE)-t0,
                        sqrt(apply(t,2L,function(t.st) var(t.st[!is.na(t.st)]))))
            dimnames(op) <- list(rn, c("original"," bias  "," std. error"))
            return(op)
        }
        else {
            op <- NULL
            for (i in index)
                op <- rbind(op, imp.moments(boot.out,index=i)$rat)
            op <- cbind(t0,op[,1L]-t0,sqrt(op[,2L]),
                        apply(t,2L,mean,na.rm=TRUE))
            dimnames(op) <- list(rn,c("original", " bias  ",
                                      " std. error", " mean(t*)"))
            return(op)
        }
    }
}


 NagR2 <- function(DATA, ind){
    n <- nrow(DATA)
    m0 <- glm(DATA[,1][ind] ~ DATA[,2] * DATA[,3],
                      family = binomial("logit"))
  
    m1 <- glm(DATA[,1][ind] ~ DATA[,2],
                      family = binomial("logit"))
    
    R2cox0 <-  1- exp((m0$deviance - m0$null.deviance)/n)
    R2nag0 <-  R2cox0/(1-exp((-m0$null.deviance)/n))
    R2nag0
    
    R2cox1 <-  1- exp((m1$deviance - m1$null.deviance)/n)
    R2nag1 <-  R2cox1/(1-exp((-m1$null.deviance)/n))
    R2nag1
    deltaNagR2 <- R2nag0 - R2nag1
    return(deltaNagR2)
 }
 
empDist <- function(DATA, alpha, hypTrueEff) {
  # Get bootstrapped distribution
  myBoot <- boot(DATA, NagR2, R = 1000) 
  # se of emp. dist.
  se <- print.bootSE(myBoot)[[3]]
  # Density values for use below shifted by the value of our hypothesised True Effect
  myBoot$t <- (myBoot$t-hypTrueEff)
  # I'm dividing by SE to put it on the right scale?
  D <- density(myBoot$t, n = 1024)
  # `pemp` probability of observed deltaNagR2 under the above emp. dist. 
  #pObsD <- approx(D$x, D$y, xout = myBoot$t0)[[1]]
  # length 2 vector of quantiles matching bottom alpha/2 and upper alpha/2 in the emp. dist.
  quant <- quantile(D$x, c(alpha/2, 1-(alpha/2)))
  # points in the probability distribution matching the Lower and Upper quantiles
  qLower <- quant[[1]]
  qUpper <- quant[[2]]
  # Here there be monsters
  p.hi <- qUpper - hypTrueEff
  p.lo <- qLower - hypTrueEff
  p.hi <- 1 - approx(D$x, D$y, xout = p.hi)[[1]]
  p.lo <- approx(D$x, D$y, xout = -p.lo)[[1]]
  # Does this math still hold in the emp. dist.?
  power <- p.hi + p.lo
  # Type-S error rate
  typeS <- p.lo / power
  # typeM
  estimate <- hypTrueEff + se * sample(D$x, replace = T, size = 5000)
  significant <- estimate < se * qUpper  | estimate > se * qUpper
  typeM <- mean(abs(estimate)[significant]) / hypTrueEff
  empRes <- list(typeM = typeM, typeS = typeS, power = power)
  return(empRes)
}

# A-B threshold in ZT scale (I think)
hypTrueEff <- 0.013

empDist(DATA, alpha = 0.05, hypTrueEff = hypTrueEff)


# Tjurs shit ----

        FisherZ <- function(rho) {
          0.5 * log((1 + rho) / (1 - rho))
        }
        
        FisherZInv <- function(z) {
          (exp(2 * z) - 1) / (1 + exp(2 * z))
        }
        
        CorCI <-
          function (rho,
                    n,
                    conf.level = 0.95,
                    alternative = c("two.sided",
                                    "less", "greater"))
          {
            if (n < 3L)
              stop("not enough finite observations")
            if (!missing(conf.level) &&
                (length(conf.level) != 1 || !is.finite(conf.level) ||
                 conf.level < 0 || conf.level > 1))
              stop("'conf.level' must be a single number between 0 and 1")
            alternative <- match.arg(alternative)
            z <- FisherZ(rho)
            sigma <- 1 / sqrt(n - 3)
            ci <-
              switch(
                alternative,
                less = c(-Inf, z + sigma * qnorm(conf.level)),
                greater = c(z - sigma * qnorm(conf.level), Inf),
                two.sided = z +
                  c(-1, 1) * sigma * qnorm((1 + conf.level) /
                                             2)
              )
            ci <- FisherZInv(ci)
            return(c(
              cor = rho,
              lwr.ci = ci[1],
              upr.ci = ci[2]
            ))
          }
        
        tjursD <-
          function (fit) {
            fittedValues <- stats::fitted(fit)
            predictedValues <-
              stats::predict(fit, type = "response", re.form = NULL)
            if (anyNA(stats::residuals(fit))) {
              predictedValues <- predictedValues[!is.na(stats::residuals(fit))]
            }
            categories <- unique(fittedValues)
            mean1 <-
              mean(predictedValues[which(fittedValues == categories[1])], na.rm = T)
            mean2 <-
              mean(predictedValues[which(fittedValues == categories[2])], na.rm = T)
            D <- abs(mean2 - mean1)
            return(CorCI(
              D,
              n = length(fittedValues),
              conf.level = 1 - alpha
            ))
          }