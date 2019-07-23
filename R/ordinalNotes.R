library(psych) # This activates the R package8 ‘psych’ (Revelle, 2011) for the
# current session in R.
data(bfi) # This loads the dataset bfi contained in the R package psych.
attach(bfi) # This attaches the dataset bfi to the current session in R.
bfi5items<-data.frame(N1,N2,N3,N4,N5) # This creates a new dataset, labeled bfi5items, containing only five
# (ordinal) variables, N1 to N5, of the original 15-item dataset bfi.
describe(bfi5items) # This describes the dataset bfi5items, providing descriptives, such
# as n, mean, sd, min, max, range, skew, and kurtosis.
bfi5items # This displays the object/dataset called bfi5items.
polychoric(bfi5items) # This provides the polychoric correlation matrix for the dataset bfi5items.

# This calculates the Pearson correlation matrix for the dataset
# bfi5items, only taking into account cases with complete data
# (“complete.obs”).
cor(bfi5items, y=NULL,use="complete.obs", method=c("pearson"))

cov(bfi5items, y=NULL, use="complete.obs", method=c("pearson"))
# This calculates the Pearson method covariance matrix for the
# dataset bfi5items, only taking into account cases with complete data
# (“complete.obs”).

skew(bfi5items) # This provides the skewness for all items in the bfi5items dataset.
kurtosi(bfi5items) # This provides the kurtosis9 for all items in the bfi5items dataset.
scree(bfi5items) # This provides the scree plots of the eigenvalues for a factor
# analysis and a principal component analysis for the dataset
# bfi5items.
examplename<-polychoric(bfi5items) # This saves the polychoric correlation matrix, and
# corresponding tau values, under the name examplename. You may
# choose any name to save the matrix. (Note: R will not produce
# any output for this step.) 
alpha(examplename$rho) # This provides (raw and standardized) alpha, and corresponding
# item statistics, based on the data set or matrix that is specified in
# brackets. (The $rho command specifies that only the correlation
# matrix is used for the calculation, disregarding the tau values that
# are saved in conjunction with the matrix.) In the output of this
# calculation, alpha represents ordinal alpha, because it is based on
# the polychoric correlation matrix for the bfi5items dataset saved
# under the name examplename. One should obtain the following
# results as part of the R output: raw_alpha = .84; std.alpha = .84;
# average_r = .51. (Please note that raw alpha and standardized alpha
# are the same when they are calculated from a correlation matrix.)
alpha(bfi5items) # This provides raw/Cronbach’s and standardized alpha of the
# object specified in brackets. In this case, the object is a datamatrix (bfi5items), and R therefore calculates raw/Cronbach’s and
# standardized alpha, respectively, from the Pearson covariance and
# the Pearson correlation matrices of the data set. This step, in
# combination with the previous one, will allow one to compare
# ordinal alpha with raw/Cronbach’s alpha. One should obtain the
# following results as part of the R output: raw_alpha = .81; std.alpha
# = .81; average_r = .47.
fa(bfi5items) # This provides the factor loadings (MR1), communalities (h2),
# and uniquenesses (u2) for a 1-factor solution of the bfi5items data
# matrix.
fa(examplename$rho) # This provides the factor loadings (MR1), communalities (h2),
# and uniquenesses (u2) for a 1-factor solution of the polychoric
# correlation matrix that was saved under the name examplename.
guttman(examplename$rho) # This provides alternative estimates of reliability for the data
# matrix that is specified in brackets (i.e., examplename$rho). In the
# R output, these estimates are labeled as beta, Guttman bounds
# L1, L2, L3 (alpha), L4 (max), L5, L6 (smc), TenBerge bounds
# mu0, mu1, mu2, mu3, alpha of the first PC (=principal
# component), and the “estimated greatest lower bound based upon
# communalities”. Since the specified data matrix is, in this case, a
# polychoric correlation matrix, all the reliability estimates represent
# ordinal versions. (We note that the guttman syntax command
# includes alpha (=L3) as one of the reliability estimates—however,
# the alpha syntax command provides additional item
# characteristics, such as the item-total correlations, that may be of
# interest to the user.)
#  [Further details and references with regard to the different
# reliability coefficients featured in the guttman command can be
# found in Revelle, 2011.]
guttman(bfi5items) # Equivalent to the command above, this provides a list of
# alternative estimates of reliability for the data matrix specified in
# brackets. Since bfi5items is a raw data matrix, the reliability
# estimates represent, in this case, Pearson correlation based
# reliability estimates.
omega(examplename$rho) # This provides the ordinal versions of the reliability coefficients
# omega (hierarchical, asymptotic, and total), because their
# calculation is based on the polychoric correlation matrix
# ‘examplename’.
omega(bfi5items) # This provides omega coefficients for the data matrix bfi5items.
# (For details, see Revelle, 2011.) 