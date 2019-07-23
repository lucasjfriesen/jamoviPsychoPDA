
# This file is a generated template, your changes will not be overwritten

ordinalReliabilityClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ordinalReliabilityClass",
    inherit = ordinalReliabilityBase,
    private = list(
        .run = function() {
          
          data = self$data
          groups = self$data$groups
          
          ordinalRhos <- function(data){
            polychoricRho <- psych::polychoric(data)$rho
            ordinalAlpha <- psych::alpha(polychoricRho)
            ordinalOmega <- psych::omega(polychoricRho, plot = FALSE)
            ordinalGuttman <- psych::splitHalf(polychoricRho)
            
            # Theta coefficient
            # Θ = [p/(p-1)]*[1-(1/λ1)]
            # Zumbo et al 2007
            
            numFactors <- dim(ordinalOmega$schmid$sl)[2] - 3
            eigenValues <- diag(t(ordinalOmega$schmid$sl[, 1:numFactors]) %*% ordinalOmega$schmid$sl[, 1:numFactors])
            ordinalTheta <- (ncol(data)/(ncol(data)-1)) * (1-(1/max(eigenValues)))
            
            polychoricRho[upper.tri(polychoricRho)] <- NA
            
            return(list("ordinalAlpha" = ordinalAlpha,
                        "ordinalOmega" = ordinalOmega,
                        "ordinalGuttman" = ordinalGuttman,
                        "ordinalTheta" = ordinalTheta,
                        "polychoricRho" = polychoricRho
                        ))
          }
          
          rhos <- ordinalRhos(data)
          
          self$results$summaryTableAlpha$setContent(value = list(
            alpha = rhos$ordinalAlpha$total
          ))
          
          self$results$summaryTableOmega$setContent(value = list(
            omega = data.frame(rhos$ordinalOmega[1:5])
          ))
          
          self$results$summaryTableTheta$setContent(value = list(
            alpha = rhos$ordinalTheta
          ))

        })
)
