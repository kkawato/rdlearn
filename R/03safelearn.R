safelearn = function(
    c.vec,
    n,
    q,
    cost,
    M,
    groupname,
    dif.1m,
    dif.0m,
    Lip_1,
    Lip_0,
    data_all)
{
  ################################################################################
  # please refer to
  # Section 4.1. Doubly robust estimation
  # Section 4.2. Estimating the bounds
  ################################################################################
  Y = data_all['Y']
  X = data_all['X']
  C = data_all['C']
  G = data_all['G']

  # ====================================================================== #
  # Section 4.2. Estimating the bounds

  lip_extra = function(
    x.train,
    group,
    g,
    g.pr){

    if(group == "dif1"){ #B1 G=1
      d = 1
      Lip = Lip_1[g, g.pr]
      dif.m = dif.1m[g, g.pr]
      eval.main = unique(C[G == max(g, g.pr)])
    }

    if(group == "dif0"){ #B1 G=1
      d = 0
      Lip = Lip_0[g, g.pr]
      dif.m = dif.0m[g, g.pr]
      eval.main = unique(C[G == min(g, g.pr)])
    }

    upper = sapply(x.train, function(x_prime) min(1, min(dif.m + Lip * abs(x_prime - eval.main))))
    lower = sapply(x.train, function(x_prime) max(-1, max(dif.m - Lip * abs(x_prime - eval.main))))
    return(list(upper = upper, lower = lower ))
  }
  # ====================================================================== #

  group = groupname #------------------------------------------- - - - -- - - --- - - -- - - - -- - - -fix this later
  safecut_all = data.frame(group)

  Lip_1temp = Lip_1
  Lip_0temp = Lip_0

  for(temp_cost in cost){
  for(temp_M in M){
    print(paste("Caluculating the case of M =", temp_M, "C =", temp_cost))

    Lip_1 = temp_M * Lip_1temp
    Lip_0 = temp_M * Lip_0temp

    c.all = rep(0, length(c.vec))

    for(g in seq(1,q,1)){
      eval.dat1 = c(data_all %>%
                      filter(G == g,
                             X >= c.vec[1],
                             X < c.vec[q],
                             X < c.vec[g]) %>%
                      select(X)) $ X #d(1)

      IND.1 = sapply(eval.dat1, function(x) sum(c.vec < x))

      eval.dat0 = c(data_all %>%
                      filter(G == g, X >= c.vec[1], X < c.vec[q], X >= c.vec[g]) %>%
                      select(X)) $ X #d(0)

      IND.0 = sapply(eval.dat0, function(x) sum(c.vec<x))

      tryCatch(
        { data_all[data_all $ G == g
                   & data_all $ X >= c.vec[1]
                   & data_all $ X < c.vec[q]
                   & data_all $ X < c.vec[g],
                   paste0("d", 1)] =
          apply(cbind(eval.dat1,IND.1), 1, function(x) sum(unlist(sapply(x[2]:x[2], function(g.temp) lip_extra(x.train=x[1], group = "dif1", g = g, g.pr = g.temp))[2,])))
        },error=function(e) return(0))

      tryCatch(
        { data_all[data_all $ G == g
                   & data_all $ X >= c.vec[1]
                   & data_all $ X < c.vec[q]
                   & data_all $ X >= c.vec[g],
                   paste0("d", 0)] =
          apply(cbind(eval.dat0,IND.0), 1, function(x) sum(unlist(sapply((x[2]+1):(x[2]+1), function(g.temp) lip_extra(x.train=x[1], group = "dif0", g = g, g.pr = g.temp))[2,])))
        },error=function(e) return(0))
    }

    data_mid = data_all %>% filter( X >= min(c.vec), X < max(c.vec))
    regret_sum = NULL

    for(g in seq(1,q,1)){

      regret = NULL

      for(c.alt in unique(X[X>=c.vec[1]&X<c.vec[q]])){
        if(c.alt >= c.vec[g]){
          temp1 = tryCatch(-sum(data_mid[data_mid$X>=c.vec[g] & data_mid$X<c.alt & data_mid$G==g, "Y"]) / n, error = function(e) return(0))

          ###########
          dat.temp = data_mid %>% filter(G == g, X < c.alt, X >= c.vec[g])

          tempDB1 = tryCatch(sum(dat.temp[, "mu.m"])/n, error=function(e) return(0))

          tempd = tryCatch(sum(dat.temp[, paste0("d",0)])/n, error=function(e) return(0))

          ###########
          dat.temp = data_mid %>% filter(X<c.alt, X>=c.vec[g], X>=c.vec[ifelse(G==1,1,G-1)],X<c.vec[G])  #& X>=c.vec[G-1] & X<c.vec[G]

          tempDB2 = tryCatch(sum(with(dat.temp, eval(parse(text=paste0("pseudo.ps",g))) /
                                      eval(parse(text = paste0("pseudo.ps",G))) * (Y - eval(parse(text = "mu.aug")))))/n,
                                      error=function(e) return(0))

          tempcost = tryCatch(temp_cost * dim(data_mid[data_mid$X>=c.vec[g] & data_mid$X<c.alt & data_mid$G==g, "Y"])[1]/n,
                              error=function(e) return(0))

          temp.reg = temp1 + tempDB1 + tempd + tempDB2 + tempcost
          # print(temp.reg)
          # print(temp1)
          # print(tempDB1)
          # print(tempd)
          # print(tempDB2)
        }

        if(c.alt < c.vec[g]){
          temp1 = tryCatch(-sum(data_mid[data_mid$X<c.vec[g] & data_mid$X>=c.alt & data_mid$G==g,"Y"])/n, error=function(e) return(0))

          dat.temp = data_mid %>% filter(G == g, X >= c.alt, X < c.vec[g])

          tempDB1 = tryCatch(sum(dat.temp[,"mu.m"] )/n, error = function(e) return(0))

          tempd = tryCatch(sum(dat.temp[,paste0("d",1)])/n, error = function(e) return(0))

          dat.temp = data_mid %>% filter(X >= c.alt, X < c.vec[g], X >= c.vec[G], X < c.vec[ifelse(G == q, q, G + 1)])

          tempDB2 = tryCatch(sum(with(dat.temp, eval(parse(text = paste0("pseudo.ps",g))) /
                                        eval(parse(text = paste0("pseudo.ps", G))) * (Y - eval(parse(text = "mu.aug")))))/n, error = function(e) return(0))

          tempcost = tryCatch(temp_cost * dim(data_mid[data_mid$X<c.vec[g] & data_mid$X>=c.alt & data_mid$G==g, "Y"])[1]/n, error = function(e) return(0))

          temp.reg = temp1 + tempDB1 + tempd + tempDB2 - tempcost

        }
        regret = c(regret, temp.reg)
        # print(regret)
      }

      if (max(regret) == 0) { #if (max(regret) == 0) max(regret, na.rm = TRUE) == 0
        c.all[g] = c.vec[g]
      } else {
        c.all[g] = unique(X[X>=c.vec[1]&X<c.vec[q]])[which(regret==max(regret))[1]]
      }
      regret_sum = c(regret_sum, max(regret))
    }
    group = groupname #-------------------------------------------fix this later
    colname = paste0("M=", temp_M, ",", "C=", temp_cost)
    c.all_df = data.frame(c.all, group)
    names(c.all_df)[1] = colname
    safecut_all = full_join(safecut_all, c.all_df, by=("group" = "group"))
  }
  }
  safecut_all
}
