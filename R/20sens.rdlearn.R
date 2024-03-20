sens.rdlearn <- function (
    result,
    M = 1,
    cost = 0,
    xlab="",
    ylab=""
){
    if(length(M) > 1){
      print(M)
      print("use new M")
    }
    else{
      M = result$M
      print(M)
      print("inherit old M")
    }
    if(length(cost) > 1){
      print(cost)
      print("use new cost")
    }
    else{
      cost = result$cost
      print(cost)
      print("inherit old cost")
    }

  safecut_new = cut_learn(
    cost = cost,
    M = M,
    q = result$numgroup, # change it later
    c.vec = result$basecut,
    groupname = result$groupname,
    Lip_1 = result$pseudoout_and_ps$Lip_1_temp,
    Lip_0 = result$pseudoout_and_ps$Lip_0_temp,
    B.1m = result$pseudoout_and_ps$B.1m_temp,
    B.0m = result$pseudoout_and_ps$B.0m_temp,
    data_all = result$pseudoout_and_ps$data_all_temp
  )

  ########### create a new plot #############
  plot.rdlearn(result, safecut = safecut_new)
}


# sens.rdlearn <- function(
#     result,
#     M = 1,
#     cost = 0,
#     xlab="",
#     ylab=""
#     ){
#
#   if(length(M) > 1){
#     print(M)
#   }
#   else{
#     M = result$M
#     print(M)
#   }
#
#   if(length(cost) > 1){
#     print(cost)
#   }
#   else{
#     cost = result$cost
#     print(cost)
#   }
#
#   q = result$numgroup
#   c.vec = result$basecut
#   groupname = result$groupname
#   Lip_1 = result$Lip_1
#   Lip_0 = result$Lip_0
#   B.1m = result$B.1m
#   B.0m = result$B.0m
#   data_all = result$data_all
#
#   ############## initial values for smoothness parameter ##############
#   Lip_1temp = Lip_1 ; Lip_0temp = Lip_0
#
#   ############### extrapolation function which return upper bound, lower bound ###############
#   lip_extra = function(x.train,group,g,g.prim){ # extrapolation function
#
#     if(group == "B1"){ #B1 G=1
#       d=1 ; Lip = Lip_1[g,g.prim]
#       B.m = B.1m[g,g.prim];
#       eval.main = unique(C[G == max(g,g.prim)])
#     }
#
#     if(group == "B0"){ #B1 G=1
#       d=0 ; Lip = Lip_0[g,g.prim]
#       B.m = B.0m[g,g.prim];
#       eval.main = unique(C[G == min(g,g.prim)])
#     }
#
#     upper = sapply(x.train, function(x_prime) min(1, min(B.m + Lip * abs(x_prime - eval.main))))
#     lower = sapply(x.train, function(x_prime) max(-1, max(B.m - Lip * abs(x_prime - eval.main))))
#     return(list(upper = upper, lower = lower ))
#   }
#
#   #############################################################################
#   ############### calculating regret ##########################################
#   #############################################################################
#
#   group <- groupname
#   safecut_all <- data.frame(group)
#
#   for(ll in cost){
#     for(kk in M){
#       print(paste("Caluculating the case of M =",kk,",C =",ll))
#       Lip_1 = kk * Lip_1temp ; Lip_0 = kk * Lip_0temp
#       c.all = rep(0,length(c.vec))
#       for(g in seq(1,q,1)){
#
#         eval.dat1 = c(data_all %>% filter(G==g, X>=c.vec[1], X<c.vec[q],X<c.vec[g]) %>% select(X))$X #d(1)
#         IND.1 = sapply(eval.dat1, function(x) sum(c.vec<x))
#
#         eval.dat0 = c(data_all %>% filter(G==g,  X>=c.vec[1], X<c.vec[q],X>=c.vec[g]) %>% select(X))$X #d(0)
#         IND.0 = sapply(eval.dat0, function(x) sum(c.vec<x))
#
#         tryCatch(
#           {  data_all[data_all$G==g &  data_all$X>=c.vec[1] & data_all$X<c.vec[q] & data_all$X<c.vec[g],paste0("d",1)]=
#             apply( cbind(eval.dat1,IND.1),1, function(x) sum(unlist(sapply(x[2]:x[2], function(g.temp) lip_extra(x.train=x[1], group="B1", g=g, g.prim = g.temp))[2,])))
#           },error=function(e) return(0))
#         tryCatch(
#           {  data_all[data_all$G==g &  data_all$X>=c.vec[1] & data_all$X<c.vec[q] & data_all$X>=c.vec[g],paste0("d",0)]=
#             apply( cbind(eval.dat0,IND.0),1, function(x) sum(unlist(sapply((x[2]+1):(x[2]+1), function(g.temp) lip_extra(x.train=x[1], group="B0", g=g, g.prim = g.temp))[2,])))
#           },error=function(e) return(0))
#       }
#
#       data_mid = data_all %>% filter(X>=min(c.vec),X<max(c.vec))
#       regret_sum=NULL
#       for(g in seq(1,q,1)){
#         regret=NULL
#         for(c.alt in unique(X[X>=c.vec[1]&X<c.vec[q]]) ){
#           if(c.alt>=c.vec[g]){
#             temp1= tryCatch(-sum(data_mid[data_mid $X>=c.vec[g] & data_mid $X<c.alt & data_mid $G==g,"Y"])/n, error=function(e) return(0))
#             ###########
#             dat.temp = data_mid %>% filter(G==g, X<c.alt, X>=c.vec[g])
#
#             tempDB1=
#               tryCatch( sum( dat.temp[,"mu.m"] )  /n, error=function(e) return(0))
#
#             tempd =   tryCatch( sum( dat.temp[,paste0("d",0)])/n, error=function(e) return(0))
#             ###########
#             dat.temp = data_mid %>% filter( X<c.alt, X>=c.vec[g], X>=c.vec[ifelse(G==1,1,G-1)],X<c.vec[G] )  #& X>=c.vec[G-1] & X<c.vec[G]
#
#             tempDB2=
#               tryCatch( sum( with(dat.temp, eval(parse(text =paste0("pseudo.ps",g)))/eval(parse(text =paste0("pseudo.ps",G)))*(Y-eval(parse(text ="mu.aug"))) )
#               )/n, error=function(e) return(0))
#
#             tempcost = tryCatch(ll*dim(data_mid[data_mid $X>=c.vec[g] & data_mid $X<c.alt & data_mid $G==g,"Y"])[1]/n, error=function(e) return(0))
#
#             temp.reg = temp1 + tempDB1 + tempd + tempDB2 + tempcost
#           }
#           if(c.alt<c.vec[g]){
#             temp1= tryCatch(-sum(data_mid[data_mid $X<c.vec[g] & data_mid $X>=c.alt & data_mid $G==g,"Y"])/n, error=function(e) return(0))
#             ###########
#             dat.temp = data_mid %>% filter(G==g, X>=c.alt, X<c.vec[g])
#
#             tempDB1= tryCatch( sum( dat.temp[,"mu.m"] )/n, error=function(e) return(0))
#
#             tempd = tryCatch( sum( dat.temp[,paste0("d",1)])/n, error=function(e) return(0))
#
#             dat.temp = data_mid %>% filter( X>=c.alt, X<c.vec[g], X>=c.vec[G],X<c.vec[ifelse(G==q,q,G+1)] )
#
#             tempDB2 = tryCatch( sum( with(dat.temp, eval(parse(text =paste0("pseudo.ps",g)))/eval(parse(text =paste0("pseudo.ps",G)))*(Y-eval(parse(text ="mu.aug"))) ) )/n, error=function(e) return(0))
#
#             tempcost = tryCatch(ll*dim(data_mid[data_mid $X<c.vec[g] & data_mid $X>=c.alt & data_mid $G==g,"Y"])[1]/n, error=function(e) return(0))
#
#             temp.reg = temp1 + tempDB1 + tempd + tempDB2 - tempcost
#           }
#           regret=c(regret,temp.reg)
#         }
#
#         if(max(regret)==0){
#           c.all[g]=c.vec[g]
#         }else{
#           c.all[g]= unique(X[X>=c.vec[1]&X<c.vec[q]])[which(regret==max(regret))[1]]
#         }
#         regret_sum=c(regret_sum, max(regret))
#       }
#
#       # create dataframe
#       group <- groupname
#       c.all_df <- data.frame(c.all, group)
#       colname <- paste0("M=",kk,",","C=",ll)
#       names(c.all_df)[1] <- colname
#       safecut_all <- full_join(safecut_all, c.all_df, by=("group" = "group"))
#     }
#   }
#
#   out <- list(
#     numgroup = q, # the number of groups
#     groupname = groupname, # the name of group in the order of cutoffs from low to high (default is Group1, Group2,... from low to high)
#     M = M, #multiplicative smoothness factor
#     cost = cost, #cost for calculating regret
#     basecut = c.vec, #baseline cutoffs
#     safecut = safecut_all #learned optimal cutoffs
#   )
#
#   plot.rdlearn(out)
# }
