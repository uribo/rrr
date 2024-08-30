library(targets)
source(here::here("src/functions.R"))
tar_option_set(packages = c("tibble", "ggplot2"))
list(
  tar_target(
    mtcars_mod,
    mtcars |> 
      tibble::rowid_to_column(var = "car") |> 
      tibble::as_tibble()
  ),
  tar_target(
    lm_coef_mtcars,
    fit_model(mpg ~ wt, data = mtcars_mod)
  ),
  tar_target(
    plot_mtcats,
    ggplot(data = mtcars_mod) +
      aes(wt, mpg) +
      geom_point(color = "red") +
      geom_abline(intercept = lm_coef_mtcars[1], slope = lm_coef_mtcars[2])
  )
)
