cleaned_data <- function(
  y = "acces",
  x = "saber11",
  c = "cutoff",
  data = colombia_acces,
  groupname = "department",
  fold = 20
){
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

  datall = data.frame(Y=Y,X=X,C=C,D=D,G=G)
  data_split <- datall %>%
    mutate(fold_id = sample(1:fold, size = dim(datall)[1], replace = TRUE)) %>%
    group_by(fold_id) %>%
    nest() %>%
    arrange(fold_id)
  data_all = data_split %>% unnest(data) %>% ungroup()
  data_all = as.data.frame(data_all)

  out<-list(
    data_all = data_all,
    data_split = data_split,
    numgroup = q,
    basecut = c.vec
  )
}
