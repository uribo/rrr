fit_model <- function(formula, data) {
  lm(formula, data = data) |> 
    coefficients()
}
