#####################################################
# Primary function
#####################################################
#' RDD Policy learning
#'
#' @param y outcome variable
#' @param x running variable
#' @param c cutoff variable
#' @param data data frame containing all variables
#' @param fold number of folds
#' @param M multiplicative smoothness factor
#' @param cost cost for calculating regret
#' @param groupname
#'
#' @return
#'  call = cl, #the matched call to the rdlearn function.
#'  variables = varnames, #outcome, running variable, cutoff, pretreatment covariates (for calculating propensity score)
#'  sample = n, #sample sizes withnisn baseline cutoffs(like the left side of Table 1)
#'  numgroup = q, # the number of groups
#'  groupname = groupname, # the name of group in the order of cutoffs from low to high (default is Group1, Group2,... from low to high)
#'  M = M, #multiplicative smoothness factor
#'  cost = cost, #cost for calculating regret
#'  rdestimates: and standard error within conventional RD framework(like the right side of Table 1) # todo
#'  basecut = c.vec, #baseline cutoffs
#'  safecut = safecut_all, #learned optimal cutoffs
#'  regret = regret_sum, #regret of optimal policy
#'  #### just for sensitivity analysis ###
#'  Lip_1 = Lip_1_sens,
#'  Lip_0 = Lip_0_sens,
#'  B.1m = B.1m_sens,
#'  B.0m = B.0m_sens,
#'  data_all = data_all_sens
#'
#' @importFrom nprobust lprobust
#' @importFrom nnet multinom
#' @import tidyverse
#'
#' @references Yi Zhang ...
#'
#' @examples
#'
#' @export
rdlearn <- function(
    y,
    x,
    c,
    groupname = NULL,
    data,
    fold = 20,
    M = 1,
    cost = 0
      # Cost should be scaled by the range of the outcome Y.
      # automatically scale the cost according to the range of Y?
      # In our application, Y is an indicator, so C is within [0,1].
      ### models ###
      # ps_model, # nnet
      # psout_model, # local linear/polynomial regressionzl, lprobust
      # lip_model # fix this lprobust(... ,deriv = 1, p=2, bwselect="mse-dpi")
      # b_model, # fix this lprobust(... ,deriv = 1, p=2, bwselect="mse-dpi")
    )
{
  #######################################################################

  # Get function call
  cl <- match.call()

  # Check argument missingness and type
  if(missing(y) || !is.character(y) || length(y) > 1)
    stop("'y' must be a character string of length one.")
  if(missing(x) || !is.character(x) || length(x) > 1)
    stop("'x' must be a character string of length one.")
  if(missing(c) || !is.character(c) || length(c) > 1)
    stop("'c' must be a character string of length one.")

  # Store all variable names in 'varnames'
  varnames <- list(y, x, c)

  # Check if all variables are in 'data'
  if (!all(varnames %in% names(data))) {
    stop("all variables must be in 'data'.")
  }

  # check NA
  if (anyNA(data[[y]]))
    stop("the column 'y' contains NA.")
  if (anyNA(data[[x]]))
    stop("the column 'x' contains NA.")
  if (anyNA(data[[c]]))
    stop("the column 'c' contains NA.")

  ########### cleaning data #############

  Y = data[[y]] ; X = data[[x]] ; C = data[[c]]

  c.vec = sort(unique(C)) #cutoffs from min to max
  n = length(Y) # sample size
  q = length(unique(C)) # number of groups

  G = match(C,c.vec)  # Group index
  D = as.numeric(X>=C) # Treatment index

  # make groupname
  if(is.null(groupname)) {
    groupname <- character(q)
    for (k in 1:q) {
      groupname[k] <- paste0("Group", k)
    }
  }
  else{
    grouplist <- data[[groupname]]
    dict <- setNames(grouplist, C)
    groupname <- sapply(c.vec, function(x) dict[[as.character(x)]])
  }

  set.seed(1) #一旦確認のためにシードを設定する

  tempdata = data.frame(Y=Y,X=X,C=C,D=D,G=G)
  data_split <- tempdata %>%
    mutate(fold_id = sample(1:fold, size = dim(tempdata)[1], replace = TRUE)) %>%
    group_by(fold_id) %>%
    nest() %>%
    arrange(fold_id)
  data_all = data_split %>% unnest(data) %>% ungroup()
  data_all = as.data.frame(data_all)

  #これまで定義した変数
  # Y
  # X
  # C
  # c.vec
  # n
  # q
  # G
  # D
  # fold
  # data_split
  # data_all
  # これらを変数とするのは汚くないか？
  # まとめたほうがいいの？

  pseudoout_and_ps <- pseudoout_ps(Y = Y,
                           X = X,
                           C = C,
                           c.vec = c.vec,
                           n = n,
                           q = q,
                           G = G,
                           D = D,
                           fold = fold,
                           data_split = data_split,
                           data_all = data_all)
  data_all <- pseudoout_and_ps$data_all_temp
  Lip_1 <- pseudoout_and_ps$Lip_1_temp
  Lip_0 <- pseudoout_and_ps$Lip_0_temp
  B.1m <- pseudoout_and_ps$B.1m_temp
  B.0m <- pseudoout_and_ps$B.0m_temp

  ##ここまで計算して結果としてLip_1, Lip_0, B.1m, B.0m, data_all を出しておけばOKって感じかな?
  data_all_sens = data_all
  Lip_1_sens = Lip_1
  Lip_0_sens = Lip_0
  B.1m_sens = B.1m
  B.0m_sens = B.0m

  safecut_all = cut_learn(
    cost,
    M,
    q,
    c.vec,
    groupname,
    Lip_1,
    Lip_0,
    B.1m,
    B.0m,
    data_all
  )

  out <- list(
    call = cl,
    variables = varnames,
    sample = n,
    numgroup = q,
    groupname = groupname,
    M = M,
    cost = cost,
    basecut = c.vec,
    safecut = safecut_all,
    #### just for sensitivity analysis ###
    Lip_1 = Lip_1_sens,
    Lip_0 = Lip_0_sens,
    B.1m = B.1m_sens,
    B.0m = B.0m_sens,
    data_all = data_all_sens,
    pseudoout_and_ps = pseudoout_and_ps #今これと上のSensitivity analysisのために用意された変数は同じになっている
  )

 class(out) <- "rdlearn"
 out
}
