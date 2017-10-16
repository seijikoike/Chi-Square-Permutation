chi.perm <- function(data, trials){
  library(gtools)
  N <- sum(data)
  r <- nrow(data)
  c <- ncol(data)
  
  expected.counts.obs <- t(apply(data, 1, function(x){sum(x)*colSums(data)/N}))
  
  chi.obs <- sum((data-expected.counts.obs)^2 / expected.counts.obs)
  #Calculating the observed chi-square statistic
  
  
  chis <- rep(NA,trials)
  all.obs.list <- sapply(1:c, function(x){rep(colnames(data)[x], colSums(data)[x])})
  for(i in 1:trials){
    perm.all.obs <- unlist(all.obs.list)[permute(1:N)] 
    
    split.list <- split(perm.all.obs, rep(1:r, rowSums(data)))
  
    p.data <- t(sapply(split.list, function(x){table(factor(x, levels = colnames(data)))}))
    p.data <- as.data.frame(p.data, row.names = rownames(data))
    
    #This ensures the integrity of row and column sums are same throughout
    
    chis[i] <- sum(((p.data-expected.counts.obs)^2) / expected.counts.obs)
  }
  p <- sum(chis >= chi.obs)/trials
  return(p)
}

library(titanic)
data("titanic_train") 

surv.class <-titanic_train[,2:3]


first.surv <- nrow(subset(surv.class, Pclass == 1 & Survived == 1))
first.dead <- nrow(subset(surv.class, Pclass == 1 & Survived == 0))

sec.surv <- nrow(subset(surv.class, Pclass == 2 & Survived == 1))
sec.dead <- nrow(subset(surv.class, Pclass == 2 & Survived == 0))

third.surv <- nrow(subset(surv.class, Pclass == 3 & Survived == 1))
third.dead <- nrow(subset(surv.class, Pclass == 3 & Survived == 0))

survival.df <- as.data.frame(matrix(c(first.surv, sec.surv, third.surv,
                                      first.dead, sec.dead, third.dead),
                                    nrow = 2, ncol = 3, byrow = TRUE))

colnames(survival.df) <- c("First Class", "Second Class", "Third Class")
rownames(survival.df) <- c("Survived", "Died")

survival.df
chi.perm(survival.df, 5000)
system.time(chi.perm(survival.df, 5000))
