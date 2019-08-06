
# This file is a generated template, your changes will not be overwritten

ordinalReliabilityClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ordinalReliabilityClass",
    inherit = ordinalReliabilityBase,
    private = list(
        .run = function() {
          
            if (is.null(self$options$items)){
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                    
                    div.instructions {
                    width: 500px;
                    height: 225px;
                    display: flex;
                    flex-wrap: wrap;
                    align-content: center;
                    }
                    </style>
                    </head>
                    <body>
                    <div class='instructions'>
                    <p><b>This analysis is still in testing Please report any errors or requests <a href='https://github.com/lucasjfriesen/jamoviPsychoPDA/issues' target = '_blank'>here</a></b></p>
                    <p>Welcome to PsychoPDA's Ordinal Reliability analysis To get started:</p>
                    <ol>
                    <li>Input the 'Items'<br /><br /></li>
                    <li>(Optional)Input the 'Grouping Variable'<br /><br /></li>
                    </ol>
                    <p>If you encounter any errors, or have questions, please see the <a href='https://lucasjfriesen.github.io/jamoviPsychoPDA_docs/ordinalReliability.html' target = '_blank'>documentation</a>.</p>
                    </div>
                    </body>
                    </html>"
                  )
          return()
        } else {
          self$results$instructions$setVisible(visible = FALSE)
        }
            
          data = self$data
          
          items = data[, self$options$items]
          groups = data[, self$options$groups]
          
          ordinalRhos <- function(data){
            polychoricRho <- psych::polychoric(data)$rho
            ordinalAlpha <- psych::alpha(polychoricRho)
            ordinalOmega <- psych::omega(polychoricRho, plot = FALSE)
            ordinalGuttman <- psych::splitHalf(polychoricRho)
            
            # Theta coefficient
            # theta = [p/(p-1)]*[1-(1/theta[1])]
            # Zumbo et al 2007
            
            numFactors <- dim(ordinalOmega$schmid$sl)[2] - 3
            eigenValues <- diag(t(ordinalOmega$schmid$sl[, 1:numFactors]) %*% ordinalOmega$schmid$sl[, 1:numFactors])
            ordinalTheta <- data.frame(ordinalTheta = (ncol(data)/(ncol(data)-1)) * (1-(1/max(eigenValues))))
            
            polychoricRho[upper.tri(polychoricRho)] <- NA
            
            return(list("ordinalAlpha" = ordinalAlpha,
                        "ordinalOmega" = ordinalOmega,
                        "ordinalGuttman" = ordinalGuttman,
                        "ordinalTheta" = ordinalTheta,
                        "polychoricRho" = polychoricRho
                        ))
          }
          
          rhos <- ordinalRhos(items)
          
          # self$results$text$setContent
          
          self$results$summaryTableAlpha$setRow(rowNo = 1, value = list(
            raw_alpha = rhos$ordinalAlpha$total$raw_alpha,
            std.alpha = rhos$ordinalAlpha$total$std.alpha,
            G6 = rhos$ordinalAlpha$total$`G6(smc)`,
            average_r = rhos$ordinalAlpha$total$average_r,
            SN = rhos$ordinalAlpha$total$`S/N`,
            median_r = rhos$ordinalAlpha$total$median_r
          ))
          
          self$results$summaryTableGuttman$setRow(rowNo = 1, value = list(
              maxSHR = rhos$ordinalGuttman$maxrb,
              guttmanL6 = rhos$ordinalGuttman$lambda6,
              avgSHR = rhos$ordinalGuttman$meanr,
              alpha = rhos$ordinalGuttman$alpha,
              minSHR = rhos$ordinalGuttman$minrb
          ))

          self$results$summaryTableOmega$setRow(rowNo = 1, value = list(
            omega_h = rhos$ordinalOmega$omega_h,
            omega.lim = rhos$ordinalOmega$omega.lim,
            alpha = rhos$ordinalOmega$alpha,
            omega.tot = rhos$ordinalOmega$omega.tot,
            G6 = rhos$ordinalOmega$G6
          ))
          
          self$results$summaryTableTheta$setRow(rowNo = 1, value = list(
            ordinalTheta = rhos$ordinalTheta[[1]]
          ))
          
          self$results$polychoricRho$setContent(rhos$polychoricRho)

        })
)
