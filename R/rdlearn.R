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
      eval.dat1.m = c( data_test %>% filter(X>=c.vec[g],X<c.vec[min(g+1,q)],D==0) %>% select(X))$X
      eval.dat1.aug = c( data_test %>% filter(X>=c.vec[g],X<c.vec[min(g+1,q)],G==g) %>% select(X))$X
      eval.dat1.pseudo = c( data_test %>% filter(D==1, X>=c.vec[g]) %>% select(X))$X
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

      tryCatch( { data_all[  data_all$fold_id==k & data_all$X>=c.vec[max(g-1,1)] & data_all$X<c.vec[g] & data_all$D==1, paste0("mu",".m")]= mu.fit0[1:length(eval.dat0.m)] },error=function(e) return(0))
      tryCatch( { data_all[  data_all$fold_id==k & data_all$X>=c.vec[max(g-1,1)] & data_all$X<c.vec[g] & data_all$G==g, paste0("mu",".aug")]= mu.fit0[(length(eval.dat0.m)+1):(length(eval.dat0.m)+length(eval.dat0.aug))] },error=function(e) return(0))
      tryCatch( { data_all[  data_all$fold_id==k & data_all$D==0 & data_all$X<c.vec[g], paste0("pseudo.",g)]= mu.fit0[c((length(eval.dat0.m)+length(eval.dat0.aug)+1):length(eval.dat0.all))]} ,error=function(e) return(0))
    }

    # propensity score
    # gamfit = multinom(formula = G ~ X, data = data_train)

    eval.dat1.p = data_test %>% filter(D==1)
    tryCatch(
      { pred = predict(gamfit, newdata = eval.dat1.p, "probs")

      if(dim( pred)[1]==1){
        data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = t(as.matrix(pred, byrow=F))
      }
      if(dim( pred)[1]!=1){
        data_all[data_all$fold_id==k & data_all$D==1, paste0("pseudo.ps",seq(1,q,1))] = pred
      }},error=function(e) return(0))
    eval.dat0.p = data_test %>% filter(D==0)
    tryCatch(
      { pred = predict(gamfit, newdata = eval.dat0.p, "probs")

      if(dim( pred)[1]==1){
        data_all[data_all$fold_id==k & data_all$D==0,paste0("pseudo.ps",seq(1,q,1))]= t(as.matrix(pred, byrow=F))
      }
      if(dim( pred)[1]!=1){
        data_all[data_all$fold_id==k & data_all$D==0,paste0("pseudo.ps",seq(1,q,1))]=pred
      }},error=function(e) return(0))

  }

  print("Cross-fitting finished")
  print("Extrapolation Started")

  #################################################################################
  ## Second, (1) estimate cross-group differences B
  ## (2) choose the value of smoothness parameter Lip
  #################################################################################
  psd_dat1 = psd_dat0 =NULL;
  Lip_1 = Lip_0=matrix(0,q,q) # storing the value of smoothness parameter;  1/0: treatment/control
  B.1m = B.0m = matrix(0,nrow=q,ncol=q) # storing the value of estimated cross-group differences at cutoff point

  # Error in `eval(parse(text = paste0("pseudo.ps", g)))`: object 'pseudo.ps1' not found
  # for the case where q = 2 (two cutoffs) this part should be modified

  for(g in seq(1,q-1,1))
    for(g.pr in seq(g+1,q,1)){

      #########################
      # for D==1
      #########################
      temp.dat = data_all %>% filter(D==1 & X>=max(c.vec[g.pr],c.vec[g]))
      # Construct the pseudo-outcome
      temp.vc = data.frame(temp.dat[ ,paste0("pseudo.", g)] - temp.dat[, paste0("pseudo.", g.pr)] +
                      with(temp.dat,I(G==g)*(Y-eval(parse(text =paste0("pseudo.", g))))/eval(parse(text =paste0("pseudo.ps", g))) )-
                      with(temp.dat,I(G==g.pr)*(Y-eval(parse(text =paste0("pseudo.", g.pr))))/eval(parse(text =paste0("pseudo.ps", g.pr))) )
                    ,temp.dat $X,g,g.pr)

      names(temp.vc)[1:2] = c("psout","X")
      psd_dat1 = rbind(psd_dat1, temp.vc )


      # Section 4.3
      # compute the absolute value of the estimated first-order derivative
      # at a grid of points in the region of overlapping policies between the two groups
      Lip_1[g,g.pr]=abs(lprobust(temp.vc[,"psout"],temp.vc[,"X"],
                                 eval = max(c.vec[g.pr],c.vec[g]), # it should be quantiles
                                 deriv=1, p=2, bwselect="mse-dpi")$Estimate[,5])

      # Algorithm 1 (Appendix.A.2.)
      # regress pseudo-outcome on covariates X
      B.1m[g,g.pr]=lprobust(temp.vc[,"psout"],temp.vc[,"X"],
                            eval = max(c.vec[g.pr],c.vec[g]), # it should be quantiles
                            deriv=0, p=1, bwselect="mse-dpi")$Estimate[,5]

      #########################
      # same procedure for D==0
      #########################
      temp.dat = data_all %>% filter(D==0 & X<min(c.vec[g.pr],c.vec[g]))

      temp.vc = data.frame("psout"=temp.dat[,paste0("pseudo.",g)]-temp.dat[,paste0("pseudo.",g.pr)]+
                             with(temp.dat,I(G==g)*(Y-eval(parse(text =paste0("pseudo.",g))))/eval(parse(text = paste0("pseudo.ps",g))) )-
                             with(temp.dat,I(G==g.pr)*(Y-eval(parse(text =paste0("pseudo.",g.pr))))/eval(parse(text = paste0("pseudo.ps",g.pr))) ),   temp.dat $X,g,g.pr)
      names(temp.vc)[1:2]=c("psout","X")
      psd_dat0=rbind(psd_dat0, temp.vc )

      Lip_0[g,g.pr]=abs(lprobust(temp.vc[,"psout"],temp.vc[,"X"],eval = min(c.vec[g.pr],c.vec[g]), deriv = 1,p=2, bwselect="mse-dpi")$Estimate[,5])
      B.0m[g,g.pr]=lprobust(temp.vc[,"psout"],temp.vc[,"X"],eval = min(c.vec[g.pr],c.vec[g]), bwselect="mse-dpi")$Estimate[,5]
}

  Lip_1 = Lip_1 + t(Lip_1) ; Lip_0 = Lip_0 + t(Lip_0)
  B.1m = B.1m + t(-B.1m) ; B.0m = B.0m + t(-B.0m)

  ############## initial values for smoothness parameter ##############
  Lip_1temp = Lip_1 ; Lip_0temp = Lip_0

  ############### extrapolation function which return upper bound, lower bound ###############
  lip_extra = function(x.train,group,g,g.prim){ # extrapolation function

    if(group=="B1"){ #B1 G=1
      d=1;Lip=Lip_1[g,g.prim]
      B.m=B.1m[g,g.prim];
      eval.main = unique(C[G == max(g,g.prim)])
    }
    if(group=="B0"){ #B1 G=1
      d=0;Lip=Lip_0[g,g.prim]
      B.m=B.0m[g,g.prim];
      eval.main = unique(C[G == min(g,g.prim)])
    }

    B.up=  B.m
    B.low= B.m

    upper = sapply(x.train,function(x_prime) min(1, min( B.m + Lip * abs(x_prime - eval.main)) ))
    lower= sapply(x.train,function(x_prime) max(-1, max( B.m - Lip * abs(x_prime - eval.main) )) )
    return(list(upper = upper, lower = lower ))
  }

  print("Alorithm 1 finished")
  print("Calculating Regret Started")

  #############################################################################
  ############### calculating regret
  #############################################################################

  group <- groupname
  safecut_all <- data.frame(group)
  M_vec <- M
  cost_vec <- cost
  for(ll in cost_vec){
    for(kk in M_vec){
      Lip_1 = kk*Lip_1temp ; Lip_0 = kk*Lip_0temp
      c.all = rep(0,length(c.vec))
      for(g in seq(1,q,1)){

        eval.dat1 = c(data_all %>% filter(G==g, X>=c.vec[1], X<c.vec[q],X<c.vec[g]) %>% select(X))$X #d(1)
        IND.1 = sapply(eval.dat1, function(x) sum(c.vec<x))
        eval.dat0 = c(data_all %>% filter(G==g,  X>=c.vec[1], X<c.vec[q],X>=c.vec[g]) %>% select(X))$X #d(0)
        IND.0 = sapply(eval.dat0, function(x) sum(c.vec<x))

        tryCatch(
          {  data_all[data_all$G==g &  data_all$X>=c.vec[1] & data_all$X<c.vec[q] & data_all$X<c.vec[g],paste0("d",1)]=
            apply( cbind(eval.dat1,IND.1),1, function(x) sum(unlist(sapply(x[2]:x[2],function(g.temp) lip_extra(x.train=x[1],group="B1",g=g,g.prim = g.temp))[2,])))
          },error=function(e) return(0))
        tryCatch(
          {  data_all[data_all$G==g &  data_all$X>=c.vec[1] & data_all$X<c.vec[q] & data_all$X>=c.vec[g],paste0("d",0)]=
            apply( cbind(eval.dat0,IND.0),1, function(x) sum(unlist(sapply((x[2]+1):(x[2]+1),function(g.temp) lip_extra(x.train=x[1],group="B0",g=g,g.prim = g.temp))[2,])) )
          },error=function(e) return(0))

      }
      data_mid = data_all %>% filter(X>=min(c.vec),X<max(c.vec))
      regret_sum=NULL
      for(g in seq(1,q,1)){
        regret=NULL
        for(c.alt in unique(X[X>=c.vec[1]&X<c.vec[q]]) ){
          if(c.alt>=c.vec[g]){
            temp1= tryCatch(-sum(data_mid[data_mid $X>=c.vec[g] & data_mid $X<c.alt & data_mid $G==g,"Y"])/n, error=function(e) return(0))
            ###########
            dat.temp = data_mid %>% filter(G==g, X<c.alt, X>=c.vec[g])

            tempDB1=
              tryCatch( sum( dat.temp[,"mu.m"] )  /n, error=function(e) return(0))

            tempd =   tryCatch( sum( dat.temp[,paste0("d",0)])/n, error=function(e) return(0))
            ###########
            dat.temp = data_mid %>% filter( X<c.alt, X>=c.vec[g], X>=c.vec[ifelse(G==1,1,G-1)],X<c.vec[G] )  #& X>=c.vec[G-1] & X<c.vec[G]

            tempDB2=
              tryCatch( sum( with(dat.temp, eval(parse(text =paste0("pseudo.ps",g)))/eval(parse(text =paste0("pseudo.ps",G)))*(Y-eval(parse(text ="mu.aug"))) )
              )/n, error=function(e) return(0))

            tempcost = tryCatch(ll*dim(data_mid[data_mid $X>=c.vec[g] & data_mid $X<c.alt & data_mid $G==g,"Y"])[1]/n, error=function(e) return(0))

            temp.reg = temp1 + tempDB1 + tempd + tempDB2 + tempcost
          }
          if(c.alt<c.vec[g]){
            temp1= tryCatch(-sum(data_mid[data_mid $X<c.vec[g] & data_mid $X>=c.alt & data_mid $G==g,"Y"])/n, error=function(e) return(0))
            ###########
            dat.temp = data_mid %>% filter(G==g, X>=c.alt, X<c.vec[g])

            tempDB1= tryCatch( sum( dat.temp[,"mu.m"] )/n, error=function(e) return(0))

            tempd = tryCatch( sum( dat.temp[,paste0("d",1)])/n, error=function(e) return(0))

            dat.temp = data_mid %>% filter( X>=c.alt, X<c.vec[g], X>=c.vec[G],X<c.vec[ifelse(G==q,q,G+1)] )

            tempDB2 = tryCatch( sum( with(dat.temp, eval(parse(text =paste0("pseudo.ps",g)))/eval(parse(text =paste0("pseudo.ps",G)))*(Y-eval(parse(text ="mu.aug"))) ) )/n, error=function(e) return(0))

            tempcost = tryCatch(ll*dim(data_mid[data_mid $X<c.vec[g] & data_mid $X>=c.alt & data_mid $G==g,"Y"])[1]/n, error=function(e) return(0))

            temp.reg = temp1 + tempDB1 + tempd + tempDB2 - tempcost

          }
          regret=c(regret,temp.reg)
        }

        if(max(regret)==0){
          c.all[g]=c.vec[g]
        }else{
          c.all[g]= unique(X[X>=c.vec[1]&X<c.vec[q]])[which(regret==max(regret))[1]]
        }
        regret_sum=c(regret_sum, max(regret))
      }
      # create dataframe
      group <- groupname
      c.all_df <- data.frame(c.all, group)
      colname <- paste0("M=",kk,",","C=",ll)
      names(c.all_df)[1] <- colname
      safecut_all <- full_join(safecut_all, c.all_df, by=("group" = "group"))
    }
  }

  out <- list(
    call = cl, #the matched call to the rdlearn function.
    variables = varnames, #outcome, running variable, cutoff, pretreatment covariates (for calculating propensity score)
    sample = n, #sample sizes withnisn baseline cutoffs(like the left side of Table 1)
    numgroup = q, # the number of groups
    groupname = groupname, # the name of group in the order of cutoffs from low to high (default is Group1, Group2,... from low to high)
    # ps_model = #model for estimating propensity score
    # psout_model: #model for group specific regression
    # lip_model: #Any generic nonparametric regression methods
    # b_model: #Any generic nonparametric regression methods with the running variable as the only predictor to construct the DR pseudo outcome for the actual observed difference
    M = M, #multiplicative smoothness factor
    cost = cost, #cost for calculating regret
    #rdestimates: and standard error within conventional RD framework(like the right side of Table 1) # todo
    basecut = c.vec, #baseline cutoffs
    safecut = safecut_all, #learned optimal cutoffs
    regret = regret_sum #regret of optimal policy
  )

 class(out) <- "rdlearn"
 out
}


# if (length(M) > 1 & length(cost) == 1){
# M_vec <- M
# for(kk in M_vec){
#   Lip_1 = kk*Lip_1temp ; Lip_0 = kk*Lip_0temp
#   c.all = rep(0,length(c.vec))
#   for(g in seq(1,q,1)){
#
#     eval.dat1 = c(data_all %>% filter(G==g, X>=c.vec[1], X<c.vec[q],X<c.vec[g]) %>% select(X))$X #d(1)
#     IND.1 = sapply(eval.dat1, function(x) sum(c.vec<x))
#     eval.dat0 = c(data_all %>% filter(G==g,  X>=c.vec[1], X<c.vec[q],X>=c.vec[g]) %>% select(X))$X #d(0)
#     IND.0 = sapply(eval.dat0, function(x) sum(c.vec<x))
#
#     tryCatch(
#       {  data_all[data_all$G==g &  data_all$X>=c.vec[1] & data_all$X<c.vec[q] & data_all$X<c.vec[g],paste0("d",1)]=
#         apply( cbind(eval.dat1,IND.1),1, function(x) sum(unlist(sapply(x[2]:x[2],function(g.temp) lip_extra(x.train=x[1],group="B1",g=g,g.prim = g.temp))[2,])))
#       },error=function(e) return(0))
#     tryCatch(
#       {  data_all[data_all$G==g &  data_all$X>=c.vec[1] & data_all$X<c.vec[q] & data_all$X>=c.vec[g],paste0("d",0)]=
#         apply( cbind(eval.dat0,IND.0),1, function(x) sum(unlist(sapply((x[2]+1):(x[2]+1),function(g.temp) lip_extra(x.train=x[1],group="B0",g=g,g.prim = g.temp))[2,])) )
#       },error=function(e) return(0))
#
#   }
#   data_mid = data_all %>% filter(X>=min(c.vec),X<max(c.vec))
#   regret_sum=NULL
#   for(g in seq(1,q,1)){
#     regret=NULL
#     for(c.alt in unique(X[X>=c.vec[1]&X<c.vec[q]]) ){
#       if(c.alt>=c.vec[g]){
#         temp1= tryCatch(-sum(data_mid[data_mid $X>=c.vec[g] & data_mid $X<c.alt & data_mid $G==g,"Y"])/n, error=function(e) return(0))
#         ###########
#         dat.temp = data_mid %>% filter(G==g, X<c.alt, X>=c.vec[g])
#
#         tempDB1=
#           tryCatch( sum( dat.temp[,"mu.m"] )  /n, error=function(e) return(0))
#
#         tempd =   tryCatch( sum( dat.temp[,paste0("d",0)])/n, error=function(e) return(0))
#         ###########
#         dat.temp = data_mid %>% filter( X<c.alt, X>=c.vec[g], X>=c.vec[ifelse(G==1,1,G-1)],X<c.vec[G] )  #& X>=c.vec[G-1] & X<c.vec[G]
#
#         tempDB2=
#           tryCatch( sum( with(dat.temp, eval(parse(text =paste0("pseudo.ps",g)))/eval(parse(text =paste0("pseudo.ps",G)))*(Y-eval(parse(text ="mu.aug"))) )
#           )/n, error=function(e) return(0))
#
#         tempcost = tryCatch(cost*dim(data_mid[data_mid $X>=c.vec[g] & data_mid $X<c.alt & data_mid $G==g,"Y"])[1]/n, error=function(e) return(0))
#
#         temp.reg = temp1 + tempDB1 + tempd + tempDB2 + tempcost
#       }
#       if(c.alt<c.vec[g]){
#         temp1= tryCatch(-sum(data_mid[data_mid $X<c.vec[g] & data_mid $X>=c.alt & data_mid $G==g,"Y"])/n, error=function(e) return(0))
#         ###########
#         dat.temp = data_mid %>% filter(G==g, X>=c.alt, X<c.vec[g])
#
#
#         tempDB1=
#           tryCatch( sum( dat.temp[,"mu.m"] )/n, error=function(e) return(0))
#
#         tempd = tryCatch( sum( dat.temp[,paste0("d",1)])/n, error=function(e) return(0))
#
#         dat.temp = data_mid %>% filter( X>=c.alt, X<c.vec[g], X>=c.vec[G],X<c.vec[ifelse(G==q,q,G+1)] )
#
#         tempDB2 = tryCatch( sum( with(dat.temp, eval(parse(text =paste0("pseudo.ps",g)))/eval(parse(text =paste0("pseudo.ps",G)))*(Y-eval(parse(text ="mu.aug"))) ) )/n, error=function(e) return(0))
#
#         tempcost = tryCatch(cost*dim(data_mid[data_mid $X<c.vec[g] & data_mid $X>=c.alt & data_mid $G==g,"Y"])[1]/n, error=function(e) return(0))
#
#         temp.reg = temp1 + tempDB1 + tempd + tempDB2 - tempcost
#
#       }
#       regret=c(regret,temp.reg)
#
#     }
#
#     if(max(regret)==0){
#       c.all[g]=c.vec[g]
#     }else{
#       c.all[g]= unique(X[X>=c.vec[1]&X<c.vec[q]])[which(regret==max(regret))[1]]
#     }
#     regret_sum=c(regret_sum,max(regret))
# }
#
# # create dataframe
# group <- groupname
# c.all_df <- data.frame(c.all, group)
# colname <- paste0("M=",kk,",","C=",cost)
# names(c.all_df)[1] <- colname
# safecut_all <- full_join(safecut_all, c.all_df, by=("group" = "group"))
# }
# }
# if(length(M) == 1 & length(cost) > 1){
# cost_vec <- cost
# for(l in cost_vec){
#   Lip_1 = M * Lip_1temp ; Lip_0 = M * Lip_0temp
#   c.all= rep(0,length(c.vec))
#   for(g in seq(1,q,1)){
#
#     eval.dat1 = c(data_all %>% filter(G==g, X>=c.vec[1], X<c.vec[q],X<c.vec[g]) %>% select(X))$X #d(1)
#     IND.1 = sapply(eval.dat1, function(x) sum(c.vec<x))
#     eval.dat0 = c(data_all %>% filter(G==g,  X>=c.vec[1], X<c.vec[q],X>=c.vec[g]) %>% select(X))$X #d(0)
#     IND.0 = sapply(eval.dat0, function(x) sum(c.vec<x))
#
#     tryCatch(
#       { data_all[data_all$G==g &  data_all$X>=c.vec[1] & data_all$X<c.vec[q] & data_all$X<c.vec[g],paste0("d",1)]=
#         apply( cbind(eval.dat1,IND.1),1, function(x) sum(unlist(sapply(x[2]:x[2],function(g.temp) lip_extra(x.train=x[1],group="B1",g=g,g.prim = g.temp))[2,])))
#       },error=function(e) return(0))
#     tryCatch(
#       { data_all[data_all$G==g &  data_all$X>=c.vec[1] & data_all$X<c.vec[q] & data_all$X>=c.vec[g],paste0("d",0)]=
#         apply( cbind(eval.dat0,IND.0),1, function(x) sum(unlist(sapply((x[2]+1):(x[2]+1),function(g.temp) lip_extra(x.train=x[1],group="B0",g=g,g.prim = g.temp))[2,])) )
#       },error=function(e) return(0))
#
#   }
#   data_mid = data_all %>% filter(X>=min(c.vec),X<max(c.vec))
#   regret_sum=NULL
#   for(g in seq(1,q,1)){
#     regret=NULL
#     for(c.alt in unique(X[X>=c.vec[1]&X<c.vec[q]]) ){
#       if(c.alt>=c.vec[g]){
#         temp1= tryCatch(-sum(data_mid[data_mid $X>=c.vec[g] & data_mid $X<c.alt & data_mid $G==g,"Y"])/n, error=function(e) return(0))
#         ###########
#         dat.temp = data_mid %>% filter(G==g, X<c.alt, X>=c.vec[g])
#
#         tempDB1 = tryCatch( sum( dat.temp[,"mu.m"] )  /n, error=function(e) return(0))
#
#         tempd = tryCatch( sum( dat.temp[,paste0("d",0)])/n, error=function(e) return(0))
#         ###########
#         dat.temp = data_mid %>% filter( X<c.alt, X>=c.vec[g], X>=c.vec[ifelse(G==1,1,G-1)],X<c.vec[G] )  #& X>=c.vec[G-1] & X<c.vec[G]
#
#         tempDB2=
#           tryCatch( sum( with(dat.temp, eval(parse(text =paste0("pseudo.ps",g)))/eval(parse(text =paste0("pseudo.ps",G)))*(Y-eval(parse(text ="mu.aug"))) )
#           )/n, error=function(e) return(0))
#
#         tempcost = tryCatch(l * dim(data_mid[data_mid $X>=c.vec[g] & data_mid $X<c.alt & data_mid $G==g,"Y"])[1]/n, error=function(e) return(0))
#
#         temp.reg = temp1 + tempDB1 + tempd + tempDB2 + tempcost
#       }
#       if(c.alt<c.vec[g]){
#         temp1= tryCatch(-sum(data_mid[data_mid $X<c.vec[g] & data_mid $X>=c.alt & data_mid $G==g,"Y"])/n, error=function(e) return(0))
#         ###########
#         dat.temp = data_mid %>% filter(G==g, X>=c.alt, X<c.vec[g])
#
#
#         tempDB1 = tryCatch( sum( dat.temp[,"mu.m"] )/n, error=function(e) return(0))
#
#         tempd = tryCatch( sum( dat.temp[,paste0("d",1)])/n, error=function(e) return(0))
#
#         dat.temp = data_mid %>% filter( X>=c.alt, X<c.vec[g], X>=c.vec[G],X<c.vec[ifelse(G==q,q,G+1)] )
#
#         tempDB2 = tryCatch( sum( with(dat.temp, eval(parse(text =paste0("pseudo.ps",g)))/eval(parse(text =paste0("pseudo.ps",G)))*(Y-eval(parse(text ="mu.aug"))) ) )/n, error=function(e) return(0))
#
#         tempcost = tryCatch(l * dim(data_mid[data_mid $X<c.vec[g] & data_mid $X>=c.alt & data_mid $G==g,"Y"])[1]/n, error=function(e) return(0))
#
#         temp.reg = temp1 + tempDB1 + tempd + tempDB2 - tempcost
#
#       }
#
#       regret=c(regret,temp.reg)
#
#     }
#
#     if(max(regret)==0){
#       c.all[g]=c.vec[g]
#     }else{
#       c.all[g]= unique(X[X>=c.vec[1]&X<c.vec[q]])[which(regret==max(regret))[1]]
#     }
#     regret_sum=c(regret_sum,max(regret))
#   }
#
# # create dataframe
# group <- groupname
# c.all_df <- data.frame(c.all, group)
# colname <- paste0("M=",M,",","C=",l)
# names(c.all_df)[1] <- colname
# safecut_all <- full_join(safecut_all, c.all_df, by=("group" = "group"))
# }
# }
