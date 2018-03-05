####################################
## Simple chain example: x -> y -> z
####################################
x = createCPT(list("x"), probs = c(0.3, 0.7), levelsList = list(c("T", "F")))
yx = createCPT(list("y", "x"), probs = c(0.8, 0.4, 0.2, 0.6),
               levelsList = list(c("T", "F"), c("T", "F")))
zy = createCPT(list("z", "y"), probs = c(0.5, 0.6, 0.5, 0.4),
               levelsList = list(c("T", "F"), c("T", "F")))

(xyzNet = list("x" = x, "y" = yx, "z" = zy))

## Some simple operations you might try to check your code
productFactor(x, yx)
productFactor(productFactor(x, yx), zy)
marginalizeFactor(productFactor(x, yx), "x")
marginalizeFactor(productFactor(yx, zy), "z")

## Notice in the observe function, you just need to delete rows that are
## inconsistent with the given observations. Factors do not need to be combined
## or normalized in this step.
observe(xyzNet, "x", "T")
observe(xyzNet, c("x", "y"), c("T", "T"))

## Marginalize must first combine all factors involving the variable to
## marginalize. Again, this operation may lead to factors that aren't
## probabilities.
marginalize(xyzNet, "x")
marginalize(xyzNet, "y")
marginalize(xyzNet, "z")
marginalize(xyzNet, c("x", "z"))

#############################
## Bishop book (Ch 8) example
#############################
b = createCPT(list("battery"), probs = c(0.9, 0.1), levelsList = list(c(1, 0)))
f = createCPT(list("fuel"), probs = c(0.9, 0.1), levelsList = list(c(1, 0)))
gbf = createCPT(list("gauge", "battery", "fuel"),
                probs = c(0.8, 0.2, 0.2, 0.1, 0.2, 0.8, 0.8, 0.9),
                levelsList = list(c(1, 0), c(1, 0), c(1, 0)))

carNet = list("battery" = b, "fuel" = f, "gauge" = gbf)

## Some examples:
## Notice that different order of operations give the same answer
## (rows/columns may be permuted)
productFactor(productFactor(b, f), gbf)
productFactor(productFactor(gbf, f), b)

marginalizeFactor(productFactor(gbf, b), "gauge")
productFactor(marginalizeFactor(gbf, "gauge"), b)

productFactor(marginalizeFactor(productFactor(gbf, b), "battery"), f)
marginalizeFactor(productFactor(productFactor(gbf, f), b), "battery")

marginalizeFactor(productFactor(marginalizeFactor(productFactor(gbf, b), "battery"), f), "gauge")
marginalizeFactor(productFactor(marginalizeFactor(productFactor(gbf, b), "battery"), f), "fuel")

## Examples computed in book (see pg. 377)
infer(carNet, c("battery", "fuel"), NULL, NULL)     ## (8.30)
infer(carNet, c("battery"), "fuel", 0)              ## (8.31)
infer(carNet, c("battery"), "gauge", 0)             ## (8.32)
infer(carNet, NULL, c("gauge", "battery"), c(0, 0)) ## (8.33)


###########################################################################
## Kevin Murphy's Example: http://www.cs.ubc.ca/~murphyk/Bayes/bnintro.html
###########################################################################
c = createCPT(list("cloudy"), probs = c(0.5, 0.5),
              levelsList = list(c("F", "T")))
rc = createCPT(list("rain", "cloudy"), probs = c(0.8, 0.2, 0.2, 0.8),
               levelsList = list(c("F", "T"), c("F", "T")))
sc = createCPT(c("sprinkler", "cloudy"), probs = c(0.5, 0.9, 0.5, 0.1),
               levelsList = list(c("F", "T"), c("F", "T")))
wsr = createCPT(list("wet", "sprinkler", "rain"),
                probs = c(1, 0.1, 0.1, 0.01, 0, 0.9, 0.9, 0.99),
                levelsList = list(c("F", "T"), c("F", "T"), c("F", "T")))

grassNet = list("cloudy" = c, "rain" = rc, "sprinkler" = sc, "wet" = wsr)

## Test your infer() method by replicating the computations on the website!!
