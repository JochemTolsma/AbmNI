############testing#############

### init
library(plotly)
seeds <- sample(2345:34567, 1000, replace=FALSE) #CAUTION, DO NOT OVERWRITE IF YOU WANT TO REPLICATE RUNS
nagents <- 10
#opinions <- seq(-1 ,1, length.out=10)
#opinions <- round(runif(10, min=-1, max=1), 1)
opinions <- c( -0.1 , 1.0 , 0.4 , 0.4 ,-0.5 , 0.4 , 0.7 , 0.7 ,-0.3,  0.6)
groups <- rep(c(-1,1), 5 )
groups <- c(-1,  1, -1,  1, -1,  1, -1,  1, -1,  1)
cor(opinions, groups) #moderate to strong correlation
set.seed(3456)
net1 <- matrix(sample(c(0,1),100, replace=TRUE), nrow=10, ncol =10)
diag(net1) <- 0
net1
###


#only opinions matter
opinions <- c( -0.1 , 1.0 , 0.4 , 0.4 ,-0.5 , 0.4 , 0.7 , 0.7 ,-0.3,  0.6)
nsim <- 50
results <- list()
for (i in 1:nsim) {
  #NOTE: IF MANY RUNS SET KEEP TO FALSE AND VERBOSE TO FALSE
  results[[i]] <- ABM_NI(opinions=opinions, groups=groups, iter=1000, keep=FALSE, verbose=FALSE, seed = seeds[i], H = 1)
}

df_sd <- do.call("rbind", lapply(results, FUN = function(x) x[[1]]$sd))
table(round(df_sd,1))
df_opinions <- do.call("rbind", lapply(results, FUN = function(x) x[[1]]$opinions))
df_opinions[round(df_sd,1) == 0, ] #consensus (no consensus at extreme!)
df_opinions[round(df_sd,1) == 0.6, ] # all extremes, 1 outlier extremist
df_opinions[round(df_sd,1) == 0.8, ] # all extremes, 2 outlier extremist
df_opinions[round(df_sd,1) == 1, ] # all extremes, 3 outlier
#other runs demonstrate that all stuff can happen, polarization, nuanced consensus and extreme consensus.

#check out run 1 and 7
#thus rerun with same seed
opinions <- c( -0.1 , 1.0 , 0.4 , 0.4 ,-0.5 , 0.4 , 0.7 , 0.7 ,-0.3,  0.6)
results <- ABM_NI(opinions=opinions, groups=groups, iter=200, verbose=FALSE, seed = seeds[1], H = 1, keep=TRUE)
opinions <- do.call("rbind", lapply(results, FUN = function(x) x$opinions))
opinions <- round(opinions, 1)

#plot development
matres <- matrix(NA, nrow=200, ncol=length(seq(-1, 1, 0.1)))
for (i in 1:200) {
  op <- factor(opinions[i,], levels = seq(-1, 1, 0.1))
  prop <- prop.table(table(op)) #relative frequency across all possible values
  matres[i,] <- prop
}

#matres is the relative frequency of the opinion. thus ranges from 0 to 1.
time <- 1:200
opinion <- seq(-1, 1, 0.1)

# Create surface plot
plot_ly(x = time, y = opinion, z = t(matres), type = "surface")
#normally i work in rmarkdown, works as a charm. now working in .r and viewer does not work. solution is to export to html. than it will show up.

# and 7

opinions <- c( -0.1 , 1.0 , 0.4 , 0.4 ,-0.5 , 0.4 , 0.7 , 0.7 ,-0.3,  0.6)
results <- ABM_NI(opinions=opinions, groups=groups, iter=200, verbose=FALSE, seed = seeds[7], H = 1, keep=TRUE)
opinions <- do.call("rbind", lapply(results, FUN = function(x) x$opinions))

opinions <- round(opinions, 1)

matres <- matrix(NA, nrow=200, ncol=length(seq(-1, 1, 0.1)))

for (i in 1:200) {
  op <- factor(opinions[i,], levels = seq(-1, 1, 0.1))
  prop <- prop.table(table(op))
  matres[i,] <- prop
}

matres
time <- 1:200
opinion <- seq(-1, 1, 0.1)

# Create surface plot
plot_ly(x = time, y = opinion, z = t(matres), type = "surface")

########standard model seems to behave as expected. some more testing necessary.


