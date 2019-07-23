retroDesign <- function(D,
                        observedSE,
                        alpha,
                        df,
                        nSims) {
  z <- qt(1 - alpha / 2, df)
  p.hi <- 1 - pt(z - D / observedSE, df)
  p.lo <- pt(-z - D / observedSE, df)
  power <- p.hi + p.lo
  typeS <- p.lo / power
  lambda <- D / observedSE
  typeM <-
    (dt(lambda + z, df = df) + dt(lambda - z, df = df) +
       lambda * (pt(lambda + z, df = df) + pt(lambda - z, df = df) - 1)) /
    (lambda * (1 - pt(lambda + z, df = df) + pt(lambda - z, df = df)))
  return(list(
    power = power,
    typeS = typeS,
    typeM = typeM
  ))
}

retroDesignEmp <- function(D,
                           observedSE,
                           alpha,
                           df,
                           nSims) {
  z <- qt(1 - alpha / 2, df)
  estimate <- D + observedSE * rt(nSims, df)
  return(list(
    estimate,
    rep(D, times = nSims),
    rep(z, times = nSims),
    rep(observedSE, times = nSims)
  ))
}