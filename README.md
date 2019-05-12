# Statistical Inferences In R
Package for performing statistical inferences in the R programming language.

## Motivation
Though the R programming language contains a wide variety of built-in functions for calculating distributions and
values used in common inference tests, I couldn't be bothered to remember them all because they don't have any consistent naming conventions.

For my stats course at the University of Florida, we were allowed to use the R language (and R Studio) on homeworks, quizzes, and exams. I developed this package to make my life MUCH easier so that I could more easily recall the functions I needed for certain problems. To that end, I used the following naming conventions so that I could find functions more quickly (consider the functions `Mean.1Pop.CI` and `Mean.1Pop.Hypothesis` as two guiding examples):

- The first part of the function name tells you what population parameter you'll be performing an inference on (`Mean`). So all I had to do was simply identify what statistic the question was looking for.
- The second part of the function name denotes whether this is for a single population (`1Pop`) or two populations (`2Pop`). In the case of linear regression, I used the alternatives of `1Regressor` and `2Regressor`.
- The third part of the function name tells you what type of test will be performed. `CI` denotes Confidence Interval, whereas `Hypothesis` denotes a formal hypothesis test.

(Translation: I got lazy when taking stats and found a creative way to reduce my workload. Sue me.)

I'll leave you with a quote from Bill Gates:

"A lazy person will find an easy way to do [the job]."

Look, stats is cool and all, but I'm a programmer at heart :)
