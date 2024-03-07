rdlearn_a1 <- function(
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

  # # #check M and cost
  # if (length(M) > 1 & length(cost) > 1)
  #   stop("both M and cost are vectors.")

  ########## cleaning data #############
  Y <- data[[y]]
  X <- data[[x]]
  C <- data[[c]]

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

  K = fold

  datall = data.frame(Y=Y,X=X,C=C,D=D,G=G)
  data_split <- datall %>%
    mutate(fold_id = sample(1:K, size = dim(datall)[1], replace = TRUE)) %>%
    group_by(fold_id) %>%
    nest() %>%
    arrange(fold_id) #split data
  # `[`(., order(names(.)$fold_id))　# Order the resulting list based on fold_id names (numerically)
  # Error in `arrange(., fold_id)`: could not find function "arrange"
  # Error in `nest(.)`: could not find function "nest"
  # Why?

  data_all = data_split %>% unnest(data) %>% ungroup()

  #################################################################################
  ########## Choosing the smoothness parameter (Section 4.3 & Appendix A.2)
  #################################################################################

  mu.fit=NULL

  #################################################################################
  ######### Appendix A.2. Step.1(a)(b)
  #################################################################################

  for(k in 1:K){
    data_train = data_split %>% filter(fold_id!=k) %>% unnest(data) %>% ungroup() %>% select(-fold_id)
    data_test = data_split %>% filter(fold_id==k) %>% unnest(data) %>% ungroup() %>% select(-fold_id)

    # conditional prob
    # for now fix this model as nnet
    gamfit = multinom(formula = G ~ X, data = data_train)

    for(g in seq(1,q,1)){
      eval.dat1.m = c(data_test %>% filter(X>=c.vec[g],X<c.vec[min(g+1,q)],D==0) %>% select(X))$X
      eval.dat1.aug = c(data_test %>% filter(X>=c.vec[g],X<c.vec[min(g+1,q)],G==g) %>% select(X))$X
      eval.dat1.pseudo = c(data_test %>% filter(D==1, X>=c.vec[g]) %>% select(X))$X
      eval.dat1.all = c(eval.dat1.m,eval.dat1.aug, eval.dat1.pseudo)

      # psout_model
      # fix this model for now
      # refere to (14) \hat\tilda m ^ {-k[i]}

      # Error in `seq.default(x.min, x.max, length.out = imsegrid)`: 'from' must be a finite number
      mu.fit1 = lprobust(data_train$Y[data_train$D==1 & data_train$G==g], data_train$X[data_train$D==1 & data_train$G==g],eval = eval.dat1.all,bwselect="imse-dpi")$Estimate[,5]

      tryCatch( { data_all[  data_all$fold_id==k & data_all$X>=c.vec[g] & data_all$X<c.vec[min(g+1,q)] & data_all$D==0, paste0("mu",".m")]= mu.fit1[1:length(eval.dat1.m)] },error=function(e) return(0))
      tryCatch( { data_all[  data_all$fold_id==k & data_all$X>=c.vec[g] & data_all$X<c.vec[min(g+1,q)] & data_all$G==g, paste0("mu",".aug")]= mu.fit1[(length(eval.dat1.m)+1):(length(eval.dat1.m)+length(eval.dat1.aug))] },error=function(e) return(0))
      tryCatch( { data_all[  data_all$fold_id==k & data_all$D==1 & data_all$X>=c.vec[g], paste0("pseudo.",g)]= mu.fit1[c((length(eval.dat1.m)+length(eval.dat1.aug)+1):length(eval.dat1.all))]} ,error=function(e) return(0))

      eval.dat0.m = c( data_test %>% filter(X>=c.vec[max(g-1,1)],X<c.vec[g],D==1) %>% select(X))$X
      eval.dat0.aug = c( data_test %>% filter(X>=c.vec[max(g-1,1)],X<c.vec[g],G==g) %>% select(X))$X
      eval.dat0.pseudo = c( data_test %>% filter(D==0, X<c.vec[g]) %>% select(X))$X
      eval.dat0.all = c(eval.dat0.m,eval.dat0.aug, eval.dat0.pseudo)

      # psout_model
      # fix this model for now
      # refere to (14) \hat\tilda m ^ {-k[i]}
      mu.fit0 = lprobust(data_train$Y[data_train$D==0 & data_train$G==g], data_train$X[data_train$D==0 & data_train$G==g], eval = eval.dat0.all, bwselect="imse-dpi")$Estimate[,5]

      tryCatch( { data_all[data_all$fold_id==k & data_all$X>=c.vec[max(g-1,1)] & data_all$X<c.vec[g] & data_all$D==1, paste0("mu",".m")]= mu.fit0[1:length(eval.dat0.m)] },error=function(e) return(0))
      tryCatch( { data_all[data_all$fold_id==k & data_all$X>=c.vec[max(g-1,1)] & data_all$X<c.vec[g] & data_all$G==g, paste0("mu",".aug")]= mu.fit0[(length(eval.dat0.m)+1):(length(eval.dat0.m)+length(eval.dat0.aug))] },error=function(e) return(0))
      tryCatch( { data_all[data_all$fold_id==k & data_all$D==0 & data_all$X<c.vec[g], paste0("pseudo.",g)]= mu.fit0[c((length(eval.dat0.m)+length(eval.dat0.aug)+1):length(eval.dat0.all))]} ,error=function(e) return(0))
    }

    ############################################################################
    eval.dat1.p = data_test %>% filter(D==1)
    pred = predict(gamfit, newdata = eval.dat1.p, "probs")
    print(dim(pred)[1])
    print(length(pred))
    print(is.null(pred))
    print(data_all[data_all$fold_id==k & data_all$D==1,])
    print(class(data_all))
    data_all <- as.data.frame(data_all)
    print(class(pred))
    pred_df <- data.frame(pred)
    print(class(pred_df))
    print(dim(pred_df)[1])
    data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = pred_df

    # data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = t(as.matrix(pred, byrow=F))
    # data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = t(as.matrix(pred))
    # data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = as.matrix(pred)
    # data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = as.matrix(pred,byrow=F)
    # data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = pred
    # data_all[data_all$fold_id == k & data_all$D == 1, paste0("pseudo.ps", seq(1, q, 1))] <- pred
    # data_all <- data_all %>%
    # filter(fold_id == k & D == 1) %>%
    # mutate(new_column = paste0("pseudo.ps", seq(1, q, 1)))


    eval.dat0.p = data_test %>% filter(D==0)
    pred = predict(gamfit, newdata = eval.dat0.p, "probs")
    print(dim(pred)[1])
    print(is.null(pred))
    print(data_all)
    pred_df <- data.frame(pred)
    # data_all[data_all$fold_id==k & data_all$D==0, paste0("pseudo.ps",seq(1,q,1))] = t(as.matrix(pred, byrow=F))
    data_all[data_all$fold_id==k & data_all$D==0, paste0("pseudo.ps",seq(1,q,1))] = pred_df

    ############################################################################

    # eval.dat1.p = data_test %>% filter(D==1)
    # pred = predict(gamfit, newdata = eval.dat1.p, "probs")
    # print(pred)
    # # if(dim(pred)[1]==1){
    # if(is.null(dim(pred)[1])){
    #   # if (dim(pred)[1] == 1 || is.null(dim(pred)[1])) {
    #   data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = t(as.matrix(pred, byrow=F))
    # }
    #
    # if(dim(pred)[1]!=1){
    # # if (dim(pred)[1] != 1 || is.null(dim(pred)[1])) {
    # # if(is.null(dim(pred)[1])){
    #   data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = pred
    # }
    # eval.dat0.p = data_test %>% filter(D==0)
    # pred = predict(gamfit, newdata = eval.dat0.p, "probs")
    # # if(dim(pred)[1] == 1){
    # if(is.null(dim(pred)[1])){
    #   # if (dim(pred)[1] == 1 || is.null(dim(pred)[1])) {
    #   data_all[data_all$fold_id==k & data_all$D==0,paste0("pseudo.ps",seq(1,q,1))] = t(as.matrix(pred, byrow=F))
    # }
    #
    # if(dim(pred)[1]!=1){
    # # if (dim(pred)[1] != 1 || is.null(dim(pred)[1])) {
    # # if(is.null(dim(pred)[1])){
    # data_all[data_all$fold_id==k & data_all$D==0,paste0("pseudo.ps",seq(1,q,1))] = pred
    # }


    ############################################################################

    # eval.dat1.p = data_test %>% filter(D==1)
    # tryCatch(
    #   {pred = predict(gamfit, newdata = eval.dat1.p, "probs")
    #   print(pred)
    #   print(data_all[data_all$fold_id==k & data_all$D==1,])
    #
    #   if(is.null(dim(pred)[1])){
    #     stop("help!")
    #   }
    #
    #   # if(is.null(dim(pred)[1])){
    #   #   data_all <- data_all %>%
    #   #   filter(fold_id == k & D == 1) %>%
    #   #   mutate_at(vars(paste0("pseudo.ps", seq(1, q, 1))), ~ pred)
    #   # }
    #
    #   # if(is.null(dim(pred)[1])){
    #   #   data_all[data_all$fold_id==k & data_all$D==0, paste0("pseudo.ps",seq(1,q,1))] <- pred
    #   # }
    #
    #   if(dim(pred)[1]==1){
    #   # if (dim(pred)[1] == 1 || is.null(dim(pred)[1])) {
    #
    #     data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = t(as.matrix(pred, byrow=F))
    #
    #     data_all <- data_all %>%
    #       filter(fold_id == k & D == 1) %>%
    #       mutate_at(vars(paste0("pseudo.ps", seq(1, q, 1))), ~ pred)
    #   }
    #
    #   if(dim(pred)[1]!=1){
    #     data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = pred
    #   }},error=function(e) return(0))
    #
    # eval.dat0.p = data_test %>% filter(D==0)
    # tryCatch(
    #   {pred = predict(gamfit, newdata = eval.dat0.p, "probs")
    #
    #   # print(pred)
    #   # print(data_all[data_all$fold_id==k & data_all$D==0,])
    #
    #   # if(is.null(dim(pred)[1])){
    #   #  data_all[data_all$fold_id==k & data_all$D==0, paste0("pseudo.ps",seq(1,q,1))] <- pred
    #   # }
    #
    #   # if(is.null(dim(pred)[1])){
    #   #  data_all[data_all$fold_id==k & data_all$D==0, paste0("pseudo.ps",seq(1,q,1))] <- pred
    #   # }
    #
    #   if(is.null(dim(pred)[1])){
    #     stop("help!")
    #     # data_all <- data_all %>%
    #     #   filter(fold_id == k & D == 0) %>%
    #     #   mutate_at(vars(paste0("pseudo.ps", seq(1, q, 1))), ~ pred)
    #   }
    #
    #   if(dim(pred)[1]==1){
    #     stop("why?")
    #   # if (dim(pred)[1] == 1 || is.null(dim(pred)[1])) {
    #
    #   # data_all[data_all$fold_id==k & data_all$D==0, paste0("pseudo.ps",seq(1,q,1))]= t(as.matrix(pred, byrow=F))
    #
    #   data_all <- data_all %>%
    #     filter(fold_id == k & D == 0) %>%
    #     mutate_at(vars(paste0("pseudo.ps", seq(1, q, 1))), ~ pred)
    #
    #   }
    #
    #   if(dim(pred)[1]!=1){
    #     stop("why?!?!?!?!")
    #     data_all[data_all$fold_id==k & data_all$D==0, paste0("pseudo.ps",seq(1,q,1))] = pred
    #   }},error=function(e) return(0))
    # # print(data_all)
    #

  }
  data_all
}

# data_all <- rdlearn_a1(y="Out",x="Run",c="Cut", data = input, fold = 10)

#   #Error in `if (dim(pred)[1] == 1) {
#   # data_all[data_all$fold_id == k & data_all$D == 1, paste0("pseudo.ps", seq(1, q, 1))] = t(as.matrix(pred, byrow = F))
#   # }`: argument is of length zero

# Error in `[<-` at RDDPackage/R/rdlearn_a1.R:151:9:
#   ! Can't recycle input of size 84 to size 2.


