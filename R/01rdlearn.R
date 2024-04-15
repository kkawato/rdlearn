#####################################################
# Primary function
#####################################################
#'
#' @param y column name of outcome variable.
#' @param x column name of running variable.
#' @param c column name of cutoff.
#' @param groupname column name of each cutoff group's name (e.g. department name).
#' If no argument is entered, the names "Group 1", "Group 2", ... are automatically assigned from the group with most smallest cutoff.
#' @param data data frame containing all variables.
#' @param fold number of folds for cross-fitting. Default is 10.
#' @param M multiplicative smoothness factor. Default is 1.
#' @param cost cost for calculating regret. Default is 0.
#'
#' @return An object
#'
#' @importFrom nprobust lprobust
#' @importFrom nnet multinom
#' @import tidyverse
#'
#' @references Yi Zhang ...
#'
#' @examples
#' result <- rdlearn(y = "acces", x = "saber11", c = "cutoff", groupname = "department", data = colombia_acces, fold = 20, M = c(0, 1), cost = 0)
#' plot.rdlearn(result)
#' # use "plot.rdlearn" to visualize the result.
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

  # check M and cost
  if (length(M) > 1 && length(cost) > 1) {
    stop("Both M and cost are vectors.")
  }

  ########### cleaning data #############

  # prepare important variables
  Y <- data[[y]] ; X <- data[[x]] ; C <- data[[c]]
  c.vec <- sort(unique(C)) #cutoffs from min to max
  n <- length(Y) # sample size
  q <- length(unique(C)) # number of groups
  G <- match(C,c.vec)  # Group index, from min cutoff to max cutoff
  D <- as.numeric(X>=C) # Treatment index

  # make groupname
  if (is.null(groupname)) {
    groupname = character(q)
    for (k in 1:q) {
      groupname[k] = paste0("Group", k)
    }
  } else {
    grouplist = data[[groupname]]
    dict = setNames(grouplist, C)
    groupname = sapply(c.vec, function(x) dict[[as.character(x)]])
  }

  # add fold_id to data
  tempdata <- data.frame(Y = Y, X = X, C = C, D = D, G = G)
  data_split <- tempdata %>%
    mutate(fold_id = sample(1:fold, size = dim(tempdata)[1], replace = TRUE)) %>%
    group_by(fold_id) %>%
    nest() %>%
    arrange(fold_id)

  data_all <- data_split %>%
    unnest(data) %>%
    ungroup()

  # cross fitting
  temp_result <- crossfit(
    c.vec = c.vec,
    q = q,
    fold = fold,
    data_split = data_split,
    data_all = data_all
  )

  safecut_all <- safelearn(
    c.vec = c.vec,
    n = n,
    q = q,
    cost = cost,
    M = M,
    groupname = groupname,
    temp_result = temp_result
  )

  out <- list(
    call = cl,
    variables = varnames,
    basecut = c.vec,
    sample = n,
    numgroup = q,

    Y = Y,
    X = X,
    C = C,
    G = G,
    D = D,

    M = M,
    cost = cost,
    groupname = groupname,

    safecut = safecut_all,
    data_all = data_all,
    temp_result = temp_result
  )

 class(out) <- "rdlearn"
 out
}


