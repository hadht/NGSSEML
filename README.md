# The R-Package NGSSEML 

Non-Gaussian State Space with Exact Marginal Likelihood R Package (Package NGSSEML)

R Code.

R Package ‘NGSSEML’

Type: R Package.

Title Non-Gaussian state space models with exact marginal likelihood.

Version 2.2

Date 2021-09-02.

Authors: T. R. Santos, G. C. Franco, D. Gamerman.

Maintainer: T. R. Santos thiagords@est.ufmg.br

 Due to a large quantity of non-Gaussian time series and reliability data, the R-package non-Gaussian state-space with exact marginal likelihood is useful for modeling and forecasting non-Gaussian time series and reliability data via non-Gaussian state-space models with the exact marginal likelihood easily, see Gamerman, Santos and Franco (2013) <doi:10.1111/jtsa.12039> and Santos, Gamerman and Franco (2017) <doi:10.1109/TR.2017.2670142>. The package gives codes for formulating and specifying the non-Gaussian state-space models in the R language. Inferences for the parameters of the model can be made under the classical and Bayesian. Furthermore, prediction, filtering, and smoothing procedures can be used to perform inferences for the latent parameters. Applications include, e.g., count, volatility, piecewise exponential, and software reliability data.

Depends: R (>= 3.5.1).

URL: https://github.com/hadht/NGSSEML
Imports: 
    mvtnorm,
    fields,
    compiler,
    dlm,
    car, interp

Installation: 

From CRAN: 

install.packages("NGSSEML")

Or

install.packages("devtools")

devtools::install_github("hadht/NGSSMEL")

Or

Essentially, you can install packages using utils by unzipping a local zipfile downloaded from github, 
and then running  utils:::menuInstallLocal()

and searching where the zip file  NGSSEML_2.2.zip  have been saved.



Disclaimer: Use of any code from this package is at own risk!

