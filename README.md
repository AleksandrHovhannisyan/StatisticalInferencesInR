# Statistical Inferences In R
Open-source package for performing statistical inferences in the R programming language.

## Motivation
The R programming language contains a wide variety of built-in functions for calculating distributions and calculating
values used in common statistical inference tests. However, there is currently no package that offers uniform and easy-to-understand
functions for performing such inferences. The aim of this project is to publish such a package for the R programming community.

### Clear, uniform naming
Cryptic naming is all too common in programming, and it's the bane of end users. The functions in this package promote clear naming conventions to help eliminate ambiguity and make it easier for users to quickly
find the functions they are looking for. Consider the functions `Mean.1Pop.CI` and `Mean.1Pop.Hypothesis` as two guiding examples.
- The first part of the function name tells the reader what population parameter they wish to perform an inference on (`Mean`).
- The second part of the function name denotes whether this is for a single population (`1Pop`) or two populations (`2Pop`). In the case of linear regression, the alternatives of `1Regressor` and `2Regressor` are used.
- The third part of the function name tells the reader what type of test will be performed. `CI` denotes Confidence Interval, whereas `Hypothesis` denotes a formal hypothesis test.