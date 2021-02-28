library(tidyverse)
library(tidymodels)


source("functions.R")

log_lasso <- function(mode = "classification", penalty) {
  ## log_lasso is the function for the model. There is no data 
  ## exposed to this function.
  ##
  ## Input:
  ## - mode: 
  ##        Since it is logistic lasso regression model, the mode
  ##        need to be 'classification'.
  ## - penalty:
  ##        The penalty term used in the lasso algorithm.
  ##
  ## Warning:
  ## "stop when mode is not classification."
  ##
  ## Example:
  ## library(tidyverse)
  ## library(tidymodels)
  ##
  ## n = 1000
  ## dat <- tibble(x = seq(-3,3, length.out = n),
  ##               w = 3*cos(3*seq(-pi,pi, length.out = n)),
  ##               y = rbinom(n,size = 1, prob = 1/(1 + exp(-w+2*x)) )%>% as.numeric %>% factor,
  ##               cat = sample(c("a","b","c"), n, replace = TRUE))
  ##
  ## rec <- recipe(y ~ . , data = dat) %>%
  ##   step_dummy(all_nominal(), -y) %>% step_zv(all_outcomes()) %>%
  ##   step_normalize(all_numeric(), -y) %>% 
  ##   step_intercept()
  ## 
  ## lambda = 1.5
  ## logistic_lasso(penalty=lambda) %>% set_engine("fit_logistic_lasso")
  
  
  # # Check for correct mode
  # if (mode  != "classification") {
  #   stop("`mode` should be 'classification'", call. = FALSE)
  # }
  
  # Capture the arguments in quosures
  args <- list(penalty = rlang::enquo(penalty))
  new_model_spec("log_lasso",
                 args = args,
                 mode = mode, 
                 eng_args = NULL, 
                 method = NULL, 
                 engine = NULL)
}


set_new_model("log_lasso")

set_model_mode(model = "log_lasso", 
               mode = "classification")

set_model_engine("log_lasso", 
                 mode = "classification", 
                 eng = "fit_logistic_lasso")


set_dependency("log_lasso", 
               eng = "fit_logistic_lasso", 
               pkg = "base")



set_model_arg(
  model = "log_lasso",
  eng = "fit_logistic_lasso",
  parsnip = "penalty",
  original = "lambda",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE)



set_encoding(
  model = "log_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification", 
  options = list(
    predictor_indicators = "traditional", 
    compute_intercept = TRUE, 
    remove_intercept = TRUE, 
    allow_sparse_x = FALSE))



set_fit(
  model = "log_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(fun = "fit_logistic_lasso"),
    defaults = list()
  )
)


set_pred(
  model = "log_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict_logistic_lasso"), 
    args = list(
      object = expr(object$fit),
      new_x = expr(as.matrix(new_data[, names(object$fit$beta)])) )
  ) )



