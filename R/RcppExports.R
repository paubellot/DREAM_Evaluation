# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

e2l <- function(gold_positives, regulators, targets, nrow, ncol) {
    .Call('DREAM_e2l', PACKAGE = 'DREAM', gold_positives, regulators, targets, nrow, ncol)
}

rm_not_gs <- function(prediction, cleaned, G, regulators, targets) {
    .Call('DREAM_rm_not_gs', PACKAGE = 'DREAM', prediction, cleaned, G, regulators, targets)
}

