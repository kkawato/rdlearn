# ############### extrapolation function which return upper bound, lower bound ###############
# lip_extra = function(
#                     c.vec = c.vec,
#                     Y = Y,
#                     X = X,
#                     C = C,
#                     G = G,
#                     D = D,
#                     n = n,
#                     q = q,
#                     Lip_1 = Lip_1,
#                     Lip_0 = Lip_0,
#                     B.1m = B.1m,
#                     B.0m = B.0m,
#                     x.train,
#                     group,
#                     g,
#                     g.prim){ # extrapolation function
#
#   if(group == "B1"){ #B1 G=1
#     d=1
#     Lip = Lip_1[g,g.prim]
#     B.m = B.1m[g,g.prim]
#     eval.main = unique(C[G == max(g,g.prim)])
#     print(Lip)
#     print(B.m)
#   }
#
#   if(group == "B0"){ #B1 G=1
#     d=0
#     Lip = Lip_0[g,g.prim]
#     B.m = B.0m[g,g.prim]
#     eval.main = unique(C[G == min(g,g.prim)])
#     print(Lip)
#     print(B.m)
#   }
#
#   upper = sapply(x.train, function(x_prime) min(1, min( B.m + Lip * abs(x_prime - eval.main))))
#   lower = sapply(x.train, function(x_prime) max(-1, max( B.m - Lip * abs(x_prime - eval.main))))
#   return(list(upper = upper, lower = lower ))
# }

############### extrapolation function which return upper bound, lower bound ###############
lip_extra = function(
    x.train,
    group,
    g,
    g.prim){ # extrapolation function

  if(group == "B1"){ #B1 G=1
    d=1
    Lip = Lip_1[g,g.prim]
    B.m = B.1m[g,g.prim]
    eval.main = unique(C[G == max(g,g.prim)])
    print("Lip", Lip)
    print("B.m",B.m)
  }

  if(group == "B0"){ #B1 G=1
    d=0
    Lip = Lip_0[g,g.prim]
    B.m = B.0m[g,g.prim]
    eval.main = unique(C[G == min(g,g.prim)])
    print("Lip",Lip)
    print("B.m",B.m)
  }

  upper = sapply(x.train, function(x_prime) min(1, min( B.m + Lip * abs(x_prime - eval.main))))
  lower = sapply(x.train, function(x_prime) max(-1, max( B.m - Lip * abs(x_prime - eval.main))))
  return(list(upper = upper, lower = lower ))
}

