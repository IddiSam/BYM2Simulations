# CODES FOR GENERATING SIMULATED OUTCOMES FOR EACH SPATIAL MATRIX

# user   system  elapsed (for 2 samples)
# 7469.686 1008.434 2957.142 (Approximately, 24 mins for the one sample of estimates)
#452.960 448.793 268.802
# For each of the different weights (W1, W2, and W3) we would have 4 different scenarios making a total of 12 combinations

# Simulation codes below
library(parallel)
#########################################################################################
# Here, $\phi = 0.3 < 0.5$ and $\tau_\epsilon = 4/9 < 1$
bym2.sim.result_1 <- function (j, n.sim) 
  
{ # - j: (integer), gives the particular combination to consider
  # - n.sim (integer>1) denotes the number of simulations 
  
  # Load file containing the list of parameters and weights
  load("/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Simulation/Param.RData")
  
  # The number of possible combinations 
  n.comb <- prod(unlist(lapply(list.sim.par , length)))
  
  # We create an array for all the different possibilities
  arr.comb <- array(1:n.comb, unlist(lapply(list.sim.par, length)))
  
  n <- nrow(X)
  ind.par <- as.numeric(which(arr.comb==j, arr.ind=T))
  print(ind.par)
  phi <- list.sim.par[[1]][ind.par[1]]
  W.nb <- list.sim.par[[2]][[ind.par[2]]]
  tau <- list.sim.par[[3]][ind.par[3]]
  
  W1.nb <- list.sim.par[["W"]][[1]]
  W1.listw <- spdep::nb2listw(W1.nb, style = "W")
  spdep::nb2INLA("W1.nb_graph",W1.nb)
  W1.nb_graph <- INLA::inla.read.graph(filename="W1.nb_graph")
  
  W2.nb <- list.sim.par[["W"]][[2]]
  W2.listw <- spdep::nb2listw(W2.nb, style = "W")
  spdep::nb2INLA("W2.nb_graph",W2.nb)
  W2.nb_graph <- INLA::inla.read.graph(filename="W2.nb_graph")
  
  W3.nb <- list.sim.par[["W"]][[3]]
  W3.listw <- spdep::nb2listw(W3.nb, style = "W")
  spdep::nb2INLA("W3.nb_graph",W3.nb)
  W3.nb_graph <- INLA::inla.read.graph(filename="W3.nb_graph")
  
  # Matrix to store estimates
  bym2_df=matrix(NA, nrow=n.sim, ncol=193)
  colnames(bym2_df) <- c("sample", "para.comb",
                         
                         "True.b0.W1.Pcs", "b0.W1.Pcs", "b0.W1.Pcs.sd", "b0.W1.Pcs_0.025CI", "b0.W1.Pcs_0.975CI", 
                         "True.b1.W1.Pcs", "b1.W1.Pcs", "b1.W1.Pcs.sd", "b1.W1.Pcs_0.025CI", "b1.W1.Pcs_0.975CI", 
                         "True.b2.W1.Pcs", "b2.W1.Pcs", "b2.W1.Pcs.sd", "b2.W1.Pcs_0.025CI", "b2.W1.Pcs_0.975CI",
                         "True.tau.W1.Pcs", "tau.W1.Pcs", "tau.W1.Pcs.sd", "tau.W1.Pcs_0.025CI", "tau.W1.Pcs_0.975CI",
                         "True.phi.W1.Pcs", "phi.W1.Pcs", "phi.W1.Pcs.sd", "phi.W1.Pcs_0.025CI", "phi.W1.Pcs_0.975CI",
                         
                         "True.b0.W2.Pcs", "b0.W2.Pcs", "b0.W2.Pcs.sd", "b0.W2.Pcs_0.025CI", "b0.W2.Pcs_0.975CI", 
                         "True.b1.W2.Pcs", "b1.W2.Pcs", "b1.W2.Pcs.sd", "b1.W2.Pcs_0.025CI", "b1.W2.Pcs_0.975CI", 
                         "True.b2.W2.Pcs", "b2.W2.Pcs", "b2.W2.Pcs.sd", "b2.W2.Pcs_0.025CI", "b2.W2.Pcs_0.975CI",
                         "True.tau.W2.Pcs", "tau.W2.Pcs", "tau.W2.Pcs.sd", "tau.W2.Pcs_0.025CI", "tau.W2.Pcs_0.975CI",
                         "True.phi.W2.Pcs", "phi.W2.Pcs", "phi.W2.Pcs.sd", "phi.W2.Pcs_0.025CI", "phi.W2.Pcs_0.975CI",
                         
                         "True.b0.W3.Pcs", "b0.W3.Pcs", "b0.W3.Pcs.sd", "b0.W3.Pcs_0.025CI", "b0.W3.Pcs_0.975CI", 
                         "True.b1.W3.Pcs", "b1.W3.Pcs", "b1.W3.Pcs.sd", "b1.W3.Pcs_0.025CI", "b1.W3.Pcs_0.975CI", 
                         "True.b2.W3.Pcs", "b2.W3.Pcs", "b2.W3.Pcs.sd", "b2.W3.Pcs_0.025CI", "b2.W3.Pcs_0.975CI",
                         "True.tau.W3.Pcs", "tau.W3.Pcs", "tau.W3.Pcs.sd", "tau.W3.Pcs_0.025CI", "tau.W3.Pcs_0.975CI",
                         "True.phi.W3.Pcs", "phi.W3.Pcs", "phi.W3.Pcs.sd", "phi.W3.Pcs_0.025CI", "phi.W3.Pcs_0.975CI",
                         
                         "True.b0.iid", "b0.iid", "b0.iid.sd", "b0.iid_0.025CI", "b0.iid_0.975CI", 
                         "True.b1.iid", "b1.iid", "b1.iid.sd", "b1.iid_0.025CI", "b1.iid_0.975CI", 
                         "True.b2.iid", "b2.iid", "b2.iid.sd", "b2.iid_0.025CI", "b2.iid_0.975CI",
                         "True.tau.iid", "tau.iid", "tau.iid.sd", "tau.iid_0.025CI", "tau.iid_0.975CI",
                         "True.phi.iid", "phi.iid", "phi.iid.sd", "phi.iid_0.025CI", "phi.iid_0.975CI",
                         
                         "True.b0.glm", "b0.glm", "b0.glm.sd", "b0.glm_0.025CI", "b0.glm_0.975CI", 
                         "True.b1.glm", "b1.glm", "b1.glm.sd", "b1.glm_0.025CI", "b1.glm_0.975CI", 
                         "True.b2.glm", "b2.glm", "b2.glm.sd", "b2.glm_0.025CI", "b2.glm_0.975CI",
                         "True.tau.glm", "tau.glm", "tau.glm.sd", "tau.glm_0.025CI", "tau.glm_0.975CI",
                         "True.phi.glm", "phi.glm", "phi.glm.sd", "phi.glm_0.025CI", "phi.glm_0.975CI",
                         
                         "WAIC.W1.Pcs", "WAIC.P.eff.W1.Pcs", "DIC.W1.Pcs", "Deviance.W1.Pcs", "DIC.P.eff.W1.Pcs", "MLIK.W1.Pcs",
                         "WAIC.W2.Pcs", "WAIC.P.eff.W2.Pcs", "DIC.W2.Pcs", "Deviance.W2.Pcs", "DIC.P.eff.W2.Pcs", "MLIK.W2.Pcs",
                         "WAIC.W3.Pcs", "WAIC.P.eff.W3.Pcs", "DIC.W3.Pcs", "Deviance.W3.Pcs", "DIC.P.eff.W3.Pcs", "MLIK.W3.Pcs",
                         
                         "WAIC.iid", "WAIC.P.eff.iid", "DIC.iid", "Deviance.iid", "DIC.P.eff.iid", "MLIK.iid",
                         "WAIC.glm", "WAIC.P.eff.glm", "DIC.glm", "Deviance.glm", "DIC.P.eff.glm", "MLIK.glm",
                         
                         "CPO.W1.Pcs", "CPO.W2.Pcs", "CPO.W3.Pcs", "CPO.iid", "CPO.glm",
                         
                        "MSE.W1.Pcs", "MSE.W2.Pcs", "MSE.W3.Pcs", "MSE.iid", "MSE.glm",
                         
                         "MI.stat.W1", "MI.p-v.W1", "MI.stat.W2", "MI.p-v.W2", "MI.stat.W3", "MI.p-v.W3",
                         
                         "Pre.Time_W1.Pcs", "Run.Time_W1.Pcs", "Post.Time_W1.Pcs", "Total.Time_W1.Pcs",
                         "Pre.Time_W2.Pcs", "Run.Time_W2.Pcs", "Post.Time_W2.Pcs", "Total.Time_W2.Pcs",
                         "Pre.Time_W3.Pcs", "Run.Time_W3.Pcs", "Post.Time_W3.Pcs", "Total.Time_W3.Pcs",
                         
                         "Pre.Time_iid", "Run.Time_iid", "Post.Time_iid", "Total.Time_iid",
                         "Pre.Time_glm", "Run.Time_glm", "Post.Time_glm", "Total.Time_glm")
  
  W.listw <- spdep::nb2listw(W.nb, style = "W")
  A.mat <- spdep::nb2mat(W.nb)
  A.mat[(A.mat>0)] = 1
  D.mat <- diag(rowSums(A.mat))
  Q = D.mat - A.mat
  Q.scaled = INLA::inla.scale.model(Q, constr = list(A = matrix(1, nrow = 1, ncol = n), e = 1E-40))
  Q.scaled <- as (Q.scaled, "matrix")
  inQ.scaled <- MASS::ginv(Q.scaled)
  mu0 <- cbind(rep(0, n))
  I <- diag(1,n)
  
  set.seed(j)
  u <- MASS::mvrnorm(n = 31*n.sim, mu = mu0, Sigma = inQ.scaled)
  u <- t(u)
  v <- MASS::mvrnorm(n = 31*n.sim, mu = mu0, Sigma = I)
  v <- t(v)
  
  # Set Pc Priors  
  prior.list1 = list(
    PC.prior.cs = list(
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.9)),
      phi = list(
        prior = "pc",
        param = c(0.5, 0.9)))
  )
  
  for (l in 1:n.sim)
  {print(l)
    k = 30*l
    repeat{
      b <- (sqrt(1-phi)*v[,k] + sqrt(phi)*u[,k])/sqrt(tau)
      set.seed(j)
      Y_e.pois <- rpois(n=n, lambda=exp(X%*%beta + b))
      sim_df <- data.frame(district = 1:n, X[,-1], Y=Y_e.pois)
      
      model.test <- INLA::inla(formula = Y ~ X1 + X2, 
                               data = sim_df, 
                               family = "poisson", 
                               control.compute = list(waic=TRUE, dic = TRUE), 
                               control.predictor = list(link=1), 
                               num.threads = 3, verbose = F)
      Y.test <- model.test$summary.fitted.values$mean
      resid.test <- Y_e.pois - Y.test
      
      MI.test <- spdep::moran.mc(resid.test, listw=W.listw, nsim = 1000, alternative="two.sided")
      pv <- MI.test[["p.value"]]
      k = k + 1
      
      if((pv < 0.05) & (max(Y_e.pois) < 5000))
      {
        break
      }
    }
    
    models_w1 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W1.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })   
    bym2_df[l,1] <- l
    bym2_df[l,2] <- j
    bym2_df[l,3] <- beta[1]
    bym2_df[l,4] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,5] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,6] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,7] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,8] <- beta[2]
    bym2_df[l,9] <- round(models_w1$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,10] <- round(models_w1$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,11] <- round(models_w1$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,12] <- round(models_w1$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,13] <- beta[3]
    bym2_df[l,14] <- round(models_w1$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,15] <- round(models_w1$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,16] <- round(models_w1$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,17] <- round(models_w1$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,18] <- tau
    bym2_df[l,19] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,20] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,21] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,22] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,23] <- phi
    bym2_df[l,24] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,25] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,26] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,27] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,128] <- round(models_w1$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,129] <- round(models_w1$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,130] <- round(models_w1$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,131] <- round(models_w1$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,132] <- round(models_w1$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,133] <- round(models_w1$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W1.Pcs <- log(models_w1[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,158] <- -sum(logCPO.W1.Pcs)
    
    sim_df$Y.W1.Pcs <- models_w1$PC.prior.cs$summary.fitted.values$mean
    resid.W1.Pcs <- sim_df$Y - sim_df$Y.W1.Pcs
    bym2_df[l,163] <-  round(mean(resid.W1.Pcs^2),3)
    
    bym2_df[l,168] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,169] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,170] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,171] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    models_w2 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W2.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })
    
    bym2_df[l,28] <- beta[1]
    bym2_df[l,29] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,30] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,31] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,32] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,33] <- beta[2]
    bym2_df[l,34] <- round(models_w2$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,35] <- round(models_w2$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,36] <- round(models_w2$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,37] <- round(models_w2$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,38] <- beta[3]
    bym2_df[l,39] <- round(models_w2$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,40] <- round(models_w2$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,41] <- round(models_w2$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,42] <- round(models_w2$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,43] <- tau
    bym2_df[l,44] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,45] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,46] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,47] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,48] <- phi
    bym2_df[l,49] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,50] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,51] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,52] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,134] <- round(models_w2$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,135] <- round(models_w2$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,136] <- round(models_w2$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,137] <- round(models_w2$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,138] <- round(models_w2$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,139] <- round(models_w2$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W2.Pcs <- log(models_w2[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,159] <- -sum(logCPO.W2.Pcs)
    
    sim_df$Y.W2.Pcs <- models_w2$PC.prior.cs$summary.fitted.values$mean
    resid.W2.Pcs <- sim_df$Y - sim_df$Y.W2.Pcs
    bym2_df[l,164] <-  round(mean(resid.W2.Pcs^2),3)
    
    bym2_df[l,172] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,173] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,174] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,175] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    models_w3 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W3.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })       
    
    
    bym2_df[l,53] <- beta[1]
    bym2_df[l,54] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,55] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,56] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,57] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,58] <- beta[2]
    bym2_df[l,59] <- round(models_w3$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,60] <- round(models_w3$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,61] <- round(models_w3$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,62] <- round(models_w3$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,63] <- beta[3]
    bym2_df[l,64] <- round(models_w3$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,65] <- round(models_w3$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,66] <- round(models_w3$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,67] <- round(models_w3$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,68] <- tau
    bym2_df[l,69] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,70] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,71] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,72] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,73] <- phi
    bym2_df[l,74] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,75] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,76] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,77] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,140] <- round(models_w3$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,141] <- round(models_w3$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,142] <- round(models_w3$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,143] <- round(models_w3$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,144] <- round(models_w3$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,145] <- round(models_w3$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W3.Pcs <- log(models_w3[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,160] <- -sum(logCPO.W3.Pcs)
    
    sim_df$Y.W3.Pcs <- models_w3$PC.prior.cs$summary.fitted.values$mean
    resid.W3.Pcs <- sim_df$Y - sim_df$Y.W3.Pcs
    bym2_df[l,165] <-  round(mean(resid.W3.Pcs^2),3)
    
    bym2_df[l,176] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,177] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,178] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,179] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    model_glm_iid <- INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "iid"), 
                                data = sim_df, 
                                family = "poisson", 
                                control.compute = list(waic=T, dic=T, cpo = TRUE, config = TRUE), 
                                control.predictor = list(link=1), 
                                num.threads = 3, verbose = F)
    
    bym2_df[l,78] <- beta[1]
    bym2_df[l,79] <- round(model_glm_iid$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,80] <- round(model_glm_iid$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,81] <- round(model_glm_iid$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,82] <- round(model_glm_iid$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,83] <- beta[2]
    bym2_df[l,84] <- round(model_glm_iid$summary.fixed['X1', 'mean'],3)
    bym2_df[l,85] <- round(model_glm_iid$summary.fixed['X1', 'sd'],3)
    bym2_df[l,86] <- round(model_glm_iid$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,87] <- round(model_glm_iid$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,88] <- beta[3]
    bym2_df[l,89] <- round(model_glm_iid$summary.fixed['X2', 'mean'],3)
    bym2_df[l,90] <- round(model_glm_iid$summary.fixed['X2', 'sd'],3)
    bym2_df[l,91] <- round(model_glm_iid$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,92] <- round(model_glm_iid$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,93] <- tau
    bym2_df[l,94] <- round(model_glm_iid$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,95] <- round(model_glm_iid$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,96] <- round(model_glm_iid$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,97] <- round(model_glm_iid$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,98] <- phi
    bym2_df[l,99] <- round(model_glm_iid$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,100] <- round(model_glm_iid$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,101] <- round(model_glm_iid$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,102] <- round(model_glm_iid$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,146] <- round(model_glm_iid[["waic"]][["waic"]],3)
    bym2_df[l,147] <- round(model_glm_iid[["waic"]][["p.eff"]],3)
    bym2_df[l,148] <- round(model_glm_iid[["dic"]][["dic"]],3)
    bym2_df[l,149] <- round(model_glm_iid[["dic"]][["p.eff"]],3)
    bym2_df[l,150] <- round(model_glm_iid[["dic"]][["mean.deviance"]],3)
    bym2_df[l,151] <- round(model_glm_iid[["mlik"]][[2,1]],3)
    
    logCPO.iid <- log(model_glm_iid[["cpo"]][["cpo"]])
    bym2_df[l,161] <- -sum(logCPO.iid)
    
    sim_df$Y.iid <- model_glm_iid$summary.fitted.values$mean
    resid.iid <- sim_df$Y - sim_df$Y.iid
    bym2_df[l,166] <-  round(mean(resid.iid^2),3)
    
    bym2_df[l,180] <- model_glm_iid[["cpu.used"]][["Pre"]]
    bym2_df[l,181] <- model_glm_iid[["cpu.used"]][["Running"]]
    bym2_df[l,182] <- model_glm_iid[["cpu.used"]][["Post"]]
    bym2_df[l,183] <- model_glm_iid[["cpu.used"]][["Total"]]
    
    model_glm <- INLA::inla(formula = Y ~ X1 + X2, 
                            data = sim_df, 
                            family = "poisson", 
                            control.compute = list(waic=TRUE, dic = TRUE, cpo = TRUE, config = TRUE), 
                            control.predictor = list(link=1), 
                            num.threads = 3, verbose = FALSE)
    
    bym2_df[l,103] <- beta[1]
    bym2_df[l,104] <- round(model_glm$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,105] <- round(model_glm$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,106] <- round(model_glm$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,107] <- round(model_glm$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,108] <- beta[2]
    bym2_df[l,109] <- round(model_glm$summary.fixed['X1', 'mean'],3)
    bym2_df[l,110] <- round(model_glm$summary.fixed['X1', 'sd'],3)
    bym2_df[l,111] <- round(model_glm$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,112] <- round(model_glm$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,113] <- beta[3]
    bym2_df[l,114] <- round(model_glm$summary.fixed['X2', 'mean'],3)
    bym2_df[l,115] <- round(model_glm$summary.fixed['X2', 'sd'],3)
    bym2_df[l,116] <- round(model_glm$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,117] <- round(model_glm$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,118] <- tau
    bym2_df[l,119] <- NA
    bym2_df[l,120] <- NA
    bym2_df[l,121] <- NA
    bym2_df[l,122] <- NA
    bym2_df[l,123] <- phi
    bym2_df[l,124] <- NA
    bym2_df[l,125] <- NA
    bym2_df[l,126] <- NA
    bym2_df[l,127] <- NA
  
    bym2_df[l,152] <- round(model_glm[["waic"]][["waic"]],3)
    bym2_df[l,153] <- round(model_glm[["waic"]][["p.eff"]],3)
    bym2_df[l,154] <- round(model_glm[["dic"]][["dic"]],3)
    bym2_df[l,155] <- round(model_glm[["dic"]][["p.eff"]],3)
    bym2_df[l,156] <- round(model_glm[["dic"]][["mean.deviance"]],3)
    bym2_df[l,157] <- round(model_glm[["mlik"]][[2,1]],3)
    
    logCPO.glm <- log(model_glm[["cpo"]][["cpo"]])
    bym2_df[l,162] <- -sum(logCPO.glm)
    
    sim_df$Y.glm <- model_glm$summary.fitted.values$mean
    resid.glm <- sim_df$Y - sim_df$Y.glm
    bym2_df[l,167] <-  round(mean(resid.glm^2),3)
    
    MI1 <- spdep::moran.mc(resid.glm, listw=W1.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,184] <- round(MI1[["statistic"]][["statistic"]], 3)
    bym2_df[l,185] <- round(MI1[["p.value"]], 3)
    
    MI2 <- spdep::moran.mc(resid.glm, listw=W2.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,186] <- round(MI2[["statistic"]][["statistic"]], 3)
    bym2_df[l,187] <- round(MI2[["p.value"]], 3)
    
    MI3 <- spdep::moran.mc(resid.glm, listw=W3.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,188] <- round(MI3[["statistic"]][["statistic"]], 3)
    bym2_df[l,189] <- round(MI3[["p.value"]], 3)
    
    bym2_df[l,190] <- model_glm[["cpu.used"]][["Pre"]]
    bym2_df[l,191] <- model_glm[["cpu.used"]][["Running"]]
    bym2_df[l,192] <- model_glm[["cpu.used"]][["Post"]]
    bym2_df[l,193] <- model_glm[["cpu.used"]][["Total"]]
    
  }
  

  return(bym2_df)
}    


#########################################################################################
# Here, $\phi = 0.9 > 0.5$ and $\tau_\epsilon = 4/9 < 1$
bym2.sim.result_2 <- function (j, n.sim) 
  
{ # - j: (integer), gives the particular combination to consider
  # - n.sim (integer>1) denotes the number of simulations 
  
  # Load file containing the list of parameters and weights
  load("/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Simulation/Param.RData")
  
  # The number of possible combinations 
  n.comb <- prod(unlist(lapply(list.sim.par , length)))
  
  # We create an array for all the different possibilities
  arr.comb <- array(1:n.comb, unlist(lapply(list.sim.par, length)))
  
  n <- nrow(X)
  ind.par <- as.numeric(which(arr.comb==j, arr.ind=T))
  print(ind.par)
  phi <- list.sim.par[[1]][ind.par[1]]
  W.nb <- list.sim.par[[2]][[ind.par[2]]]
  tau <- list.sim.par[[3]][ind.par[3]]
  
  W1.nb <- list.sim.par[["W"]][[1]]
  W1.listw <- spdep::nb2listw(W1.nb, style = "W")
  spdep::nb2INLA("W1.nb_graph",W1.nb)
  W1.nb_graph <- INLA::inla.read.graph(filename="W1.nb_graph")
  
  W2.nb <- list.sim.par[["W"]][[2]]
  W2.listw <- spdep::nb2listw(W2.nb, style = "W")
  spdep::nb2INLA("W2.nb_graph",W2.nb)
  W2.nb_graph <- INLA::inla.read.graph(filename="W2.nb_graph")
  
  W3.nb <- list.sim.par[["W"]][[3]]
  W3.listw <- spdep::nb2listw(W3.nb, style = "W")
  spdep::nb2INLA("W3.nb_graph",W3.nb)
  W3.nb_graph <- INLA::inla.read.graph(filename="W3.nb_graph")
  
  # Matrix to store estimates
  bym2_df=matrix(NA, nrow=n.sim, ncol=193)
  colnames(bym2_df) <- c("sample", "para.comb",
                         
                         "True.b0.W1.Pcs", "b0.W1.Pcs", "b0.W1.Pcs.sd", "b0.W1.Pcs_0.025CI", "b0.W1.Pcs_0.975CI", 
                         "True.b1.W1.Pcs", "b1.W1.Pcs", "b1.W1.Pcs.sd", "b1.W1.Pcs_0.025CI", "b1.W1.Pcs_0.975CI", 
                         "True.b2.W1.Pcs", "b2.W1.Pcs", "b2.W1.Pcs.sd", "b2.W1.Pcs_0.025CI", "b2.W1.Pcs_0.975CI",
                         "True.tau.W1.Pcs", "tau.W1.Pcs", "tau.W1.Pcs.sd", "tau.W1.Pcs_0.025CI", "tau.W1.Pcs_0.975CI",
                         "True.phi.W1.Pcs", "phi.W1.Pcs", "phi.W1.Pcs.sd", "phi.W1.Pcs_0.025CI", "phi.W1.Pcs_0.975CI",
                         
                         "True.b0.W2.Pcs", "b0.W2.Pcs", "b0.W2.Pcs.sd", "b0.W2.Pcs_0.025CI", "b0.W2.Pcs_0.975CI", 
                         "True.b1.W2.Pcs", "b1.W2.Pcs", "b1.W2.Pcs.sd", "b1.W2.Pcs_0.025CI", "b1.W2.Pcs_0.975CI", 
                         "True.b2.W2.Pcs", "b2.W2.Pcs", "b2.W2.Pcs.sd", "b2.W2.Pcs_0.025CI", "b2.W2.Pcs_0.975CI",
                         "True.tau.W2.Pcs", "tau.W2.Pcs", "tau.W2.Pcs.sd", "tau.W2.Pcs_0.025CI", "tau.W2.Pcs_0.975CI",
                         "True.phi.W2.Pcs", "phi.W2.Pcs", "phi.W2.Pcs.sd", "phi.W2.Pcs_0.025CI", "phi.W2.Pcs_0.975CI",
                         
                         "True.b0.W3.Pcs", "b0.W3.Pcs", "b0.W3.Pcs.sd", "b0.W3.Pcs_0.025CI", "b0.W3.Pcs_0.975CI", 
                         "True.b1.W3.Pcs", "b1.W3.Pcs", "b1.W3.Pcs.sd", "b1.W3.Pcs_0.025CI", "b1.W3.Pcs_0.975CI", 
                         "True.b2.W3.Pcs", "b2.W3.Pcs", "b2.W3.Pcs.sd", "b2.W3.Pcs_0.025CI", "b2.W3.Pcs_0.975CI",
                         "True.tau.W3.Pcs", "tau.W3.Pcs", "tau.W3.Pcs.sd", "tau.W3.Pcs_0.025CI", "tau.W3.Pcs_0.975CI",
                         "True.phi.W3.Pcs", "phi.W3.Pcs", "phi.W3.Pcs.sd", "phi.W3.Pcs_0.025CI", "phi.W3.Pcs_0.975CI",
                         
                         "True.b0.iid", "b0.iid", "b0.iid.sd", "b0.iid_0.025CI", "b0.iid_0.975CI", 
                         "True.b1.iid", "b1.iid", "b1.iid.sd", "b1.iid_0.025CI", "b1.iid_0.975CI", 
                         "True.b2.iid", "b2.iid", "b2.iid.sd", "b2.iid_0.025CI", "b2.iid_0.975CI",
                         "True.tau.iid", "tau.iid", "tau.iid.sd", "tau.iid_0.025CI", "tau.iid_0.975CI",
                         "True.phi.iid", "phi.iid", "phi.iid.sd", "phi.iid_0.025CI", "phi.iid_0.975CI",
                         
                         "True.b0.glm", "b0.glm", "b0.glm.sd", "b0.glm_0.025CI", "b0.glm_0.975CI", 
                         "True.b1.glm", "b1.glm", "b1.glm.sd", "b1.glm_0.025CI", "b1.glm_0.975CI", 
                         "True.b2.glm", "b2.glm", "b2.glm.sd", "b2.glm_0.025CI", "b2.glm_0.975CI",
                         "True.tau.glm", "tau.glm", "tau.glm.sd", "tau.glm_0.025CI", "tau.glm_0.975CI",
                         "True.phi.glm", "phi.glm", "phi.glm.sd", "phi.glm_0.025CI", "phi.glm_0.975CI",
                         
                         "WAIC.W1.Pcs", "WAIC.P.eff.W1.Pcs", "DIC.W1.Pcs", "Deviance.W1.Pcs", "DIC.P.eff.W1.Pcs", "MLIK.W1.Pcs",
                         "WAIC.W2.Pcs", "WAIC.P.eff.W2.Pcs", "DIC.W2.Pcs", "Deviance.W2.Pcs", "DIC.P.eff.W2.Pcs", "MLIK.W2.Pcs",
                         "WAIC.W3.Pcs", "WAIC.P.eff.W3.Pcs", "DIC.W3.Pcs", "Deviance.W3.Pcs", "DIC.P.eff.W3.Pcs", "MLIK.W3.Pcs",
                         
                         "WAIC.iid", "WAIC.P.eff.iid", "DIC.iid", "Deviance.iid", "DIC.P.eff.iid", "MLIK.iid",
                         "WAIC.glm", "WAIC.P.eff.glm", "DIC.glm", "Deviance.glm", "DIC.P.eff.glm", "MLIK.glm",
                         
                         "CPO.W1.Pcs", "CPO.W2.Pcs", "CPO.W3.Pcs", "CPO.iid", "CPO.glm",
                         
                         "MSE.W1.Pcs", "MSE.W2.Pcs", "MSE.W3.Pcs", "MSE.iid", "MSE.glm",
                         
                         "MI.stat.W1", "MI.p-v.W1", "MI.stat.W2", "MI.p-v.W2", "MI.stat.W3", "MI.p-v.W3",
                         
                         "Pre.Time_W1.Pcs", "Run.Time_W1.Pcs", "Post.Time_W1.Pcs", "Total.Time_W1.Pcs",
                         "Pre.Time_W2.Pcs", "Run.Time_W2.Pcs", "Post.Time_W2.Pcs", "Total.Time_W2.Pcs",
                         "Pre.Time_W3.Pcs", "Run.Time_W3.Pcs", "Post.Time_W3.Pcs", "Total.Time_W3.Pcs",
                         
                         "Pre.Time_iid", "Run.Time_iid", "Post.Time_iid", "Total.Time_iid",
                         "Pre.Time_glm", "Run.Time_glm", "Post.Time_glm", "Total.Time_glm")
  
  W.listw <- spdep::nb2listw(W.nb, style = "W")
  A.mat <- spdep::nb2mat(W.nb)
  A.mat[(A.mat>0)] = 1
  D.mat <- diag(rowSums(A.mat))
  Q = D.mat - A.mat
  Q.scaled = INLA::inla.scale.model(Q, constr = list(A = matrix(1, nrow = 1, ncol = n), e = 1E-40))
  Q.scaled <- as (Q.scaled, "matrix")
  inQ.scaled <- MASS::ginv(Q.scaled)
  mu0 <- cbind(rep(0, n))
  I <- diag(1,n)
  
  set.seed(j)
  u <- MASS::mvrnorm(n = 31*n.sim, mu = mu0, Sigma = inQ.scaled)
  u <- t(u)
  v <- MASS::mvrnorm(n = 31*n.sim, mu = mu0, Sigma = I)
  v <- t(v)
  
  # Set Pc Priors  
  prior.list1 = list(
    PC.prior.cs = list(
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.9)),
      phi = list(
        prior = "pc",
        param = c(0.5, 0.9)))
  )
  
  for (l in 1:n.sim)
  {print(l)
    k = 30*l
    repeat{
      b <- (sqrt(1-phi)*v[,k] + sqrt(phi)*u[,k])/sqrt(tau)
      set.seed(j)
      Y_e.pois <- rpois(n=n, lambda=exp(X%*%beta + b))
      sim_df <- data.frame(district = 1:n, X[,-1], Y=Y_e.pois)
      
      model.test <- INLA::inla(formula = Y ~ X1 + X2, 
                               data = sim_df, 
                               family = "poisson", 
                               control.compute = list(waic=TRUE, dic = TRUE), 
                               control.predictor = list(link=1), 
                               num.threads = 3, verbose = F)
      Y.test <- model.test$summary.fitted.values$mean
      resid.test <- Y_e.pois - Y.test
      
      MI.test <- spdep::moran.mc(resid.test, listw=W.listw, nsim = 1000, alternative="two.sided")
      pv <- MI.test[["p.value"]]
      k = k + 1
      
      if((pv < 0.05) & (max(Y_e.pois) < 5000))
      {
        break
      }
    }
    
    models_w1 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W1.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })   
    bym2_df[l,1] <- l
    bym2_df[l,2] <- j
    bym2_df[l,3] <- beta[1]
    bym2_df[l,4] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,5] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,6] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,7] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,8] <- beta[2]
    bym2_df[l,9] <- round(models_w1$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,10] <- round(models_w1$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,11] <- round(models_w1$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,12] <- round(models_w1$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,13] <- beta[3]
    bym2_df[l,14] <- round(models_w1$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,15] <- round(models_w1$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,16] <- round(models_w1$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,17] <- round(models_w1$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,18] <- tau
    bym2_df[l,19] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,20] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,21] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,22] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,23] <- phi
    bym2_df[l,24] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,25] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,26] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,27] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,128] <- round(models_w1$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,129] <- round(models_w1$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,130] <- round(models_w1$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,131] <- round(models_w1$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,132] <- round(models_w1$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,133] <- round(models_w1$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W1.Pcs <- log(models_w1[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,158] <- -sum(logCPO.W1.Pcs)
    
    sim_df$Y.W1.Pcs <- models_w1$PC.prior.cs$summary.fitted.values$mean
    resid.W1.Pcs <- sim_df$Y - sim_df$Y.W1.Pcs
    bym2_df[l,163] <-  round(mean(resid.W1.Pcs^2),3)
    
    bym2_df[l,168] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,169] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,170] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,171] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    models_w2 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W2.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })
    
    bym2_df[l,28] <- beta[1]
    bym2_df[l,29] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,30] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,31] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,32] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,33] <- beta[2]
    bym2_df[l,34] <- round(models_w2$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,35] <- round(models_w2$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,36] <- round(models_w2$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,37] <- round(models_w2$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,38] <- beta[3]
    bym2_df[l,39] <- round(models_w2$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,40] <- round(models_w2$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,41] <- round(models_w2$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,42] <- round(models_w2$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,43] <- tau
    bym2_df[l,44] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,45] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,46] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,47] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,48] <- phi
    bym2_df[l,49] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,50] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,51] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,52] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,134] <- round(models_w2$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,135] <- round(models_w2$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,136] <- round(models_w2$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,137] <- round(models_w2$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,138] <- round(models_w2$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,139] <- round(models_w2$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W2.Pcs <- log(models_w2[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,159] <- -sum(logCPO.W2.Pcs)
    
    sim_df$Y.W2.Pcs <- models_w2$PC.prior.cs$summary.fitted.values$mean
    resid.W2.Pcs <- sim_df$Y - sim_df$Y.W2.Pcs
    bym2_df[l,164] <-  round(mean(resid.W2.Pcs^2),3)
    
    bym2_df[l,172] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,173] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,174] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,175] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    models_w3 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W3.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })       
    
    
    bym2_df[l,53] <- beta[1]
    bym2_df[l,54] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,55] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,56] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,57] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,58] <- beta[2]
    bym2_df[l,59] <- round(models_w3$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,60] <- round(models_w3$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,61] <- round(models_w3$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,62] <- round(models_w3$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,63] <- beta[3]
    bym2_df[l,64] <- round(models_w3$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,65] <- round(models_w3$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,66] <- round(models_w3$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,67] <- round(models_w3$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,68] <- tau
    bym2_df[l,69] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,70] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,71] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,72] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,73] <- phi
    bym2_df[l,74] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,75] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,76] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,77] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,140] <- round(models_w3$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,141] <- round(models_w3$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,142] <- round(models_w3$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,143] <- round(models_w3$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,144] <- round(models_w3$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,145] <- round(models_w3$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W3.Pcs <- log(models_w3[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,160] <- -sum(logCPO.W3.Pcs)
    
    sim_df$Y.W3.Pcs <- models_w3$PC.prior.cs$summary.fitted.values$mean
    resid.W3.Pcs <- sim_df$Y - sim_df$Y.W3.Pcs
    bym2_df[l,165] <-  round(mean(resid.W3.Pcs^2),3)
    
    bym2_df[l,176] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,177] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,178] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,179] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    model_glm_iid <- INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "iid"), 
                                data = sim_df, 
                                family = "poisson", 
                                control.compute = list(waic=T, dic=T, cpo = TRUE, config = TRUE), 
                                control.predictor = list(link=1), 
                                num.threads = 3, verbose = F)
    
    bym2_df[l,78] <- beta[1]
    bym2_df[l,79] <- round(model_glm_iid$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,80] <- round(model_glm_iid$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,81] <- round(model_glm_iid$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,82] <- round(model_glm_iid$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,83] <- beta[2]
    bym2_df[l,84] <- round(model_glm_iid$summary.fixed['X1', 'mean'],3)
    bym2_df[l,85] <- round(model_glm_iid$summary.fixed['X1', 'sd'],3)
    bym2_df[l,86] <- round(model_glm_iid$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,87] <- round(model_glm_iid$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,88] <- beta[3]
    bym2_df[l,89] <- round(model_glm_iid$summary.fixed['X2', 'mean'],3)
    bym2_df[l,90] <- round(model_glm_iid$summary.fixed['X2', 'sd'],3)
    bym2_df[l,91] <- round(model_glm_iid$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,92] <- round(model_glm_iid$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,93] <- tau
    bym2_df[l,94] <- round(model_glm_iid$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,95] <- round(model_glm_iid$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,96] <- round(model_glm_iid$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,97] <- round(model_glm_iid$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,98] <- phi
    bym2_df[l,99] <- round(model_glm_iid$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,100] <- round(model_glm_iid$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,101] <- round(model_glm_iid$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,102] <- round(model_glm_iid$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,146] <- round(model_glm_iid[["waic"]][["waic"]],3)
    bym2_df[l,147] <- round(model_glm_iid[["waic"]][["p.eff"]],3)
    bym2_df[l,148] <- round(model_glm_iid[["dic"]][["dic"]],3)
    bym2_df[l,149] <- round(model_glm_iid[["dic"]][["p.eff"]],3)
    bym2_df[l,150] <- round(model_glm_iid[["dic"]][["mean.deviance"]],3)
    bym2_df[l,151] <- round(model_glm_iid[["mlik"]][[2,1]],3)
    
    logCPO.iid <- log(model_glm_iid[["cpo"]][["cpo"]])
    bym2_df[l,161] <- -sum(logCPO.iid)
    
    sim_df$Y.iid <- model_glm_iid$summary.fitted.values$mean
    resid.iid <- sim_df$Y - sim_df$Y.iid
    bym2_df[l,166] <-  round(mean(resid.iid^2),3)
    
    bym2_df[l,180] <- model_glm_iid[["cpu.used"]][["Pre"]]
    bym2_df[l,181] <- model_glm_iid[["cpu.used"]][["Running"]]
    bym2_df[l,182] <- model_glm_iid[["cpu.used"]][["Post"]]
    bym2_df[l,183] <- model_glm_iid[["cpu.used"]][["Total"]]
    
    model_glm <- INLA::inla(formula = Y ~ X1 + X2, 
                            data = sim_df, 
                            family = "poisson", 
                            control.compute = list(waic=TRUE, dic = TRUE, cpo = TRUE, config = TRUE), 
                            control.predictor = list(link=1), 
                            num.threads = 3, verbose = FALSE)
    
    bym2_df[l,103] <- beta[1]
    bym2_df[l,104] <- round(model_glm$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,105] <- round(model_glm$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,106] <- round(model_glm$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,107] <- round(model_glm$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,108] <- beta[2]
    bym2_df[l,109] <- round(model_glm$summary.fixed['X1', 'mean'],3)
    bym2_df[l,110] <- round(model_glm$summary.fixed['X1', 'sd'],3)
    bym2_df[l,111] <- round(model_glm$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,112] <- round(model_glm$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,113] <- beta[3]
    bym2_df[l,114] <- round(model_glm$summary.fixed['X2', 'mean'],3)
    bym2_df[l,115] <- round(model_glm$summary.fixed['X2', 'sd'],3)
    bym2_df[l,116] <- round(model_glm$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,117] <- round(model_glm$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,118] <- tau
    bym2_df[l,119] <- NA
    bym2_df[l,120] <- NA
    bym2_df[l,121] <- NA
    bym2_df[l,122] <- NA
    bym2_df[l,123] <- phi
    bym2_df[l,124] <- NA
    bym2_df[l,125] <- NA
    bym2_df[l,126] <- NA
    bym2_df[l,127] <- NA
    
    bym2_df[l,152] <- round(model_glm[["waic"]][["waic"]],3)
    bym2_df[l,153] <- round(model_glm[["waic"]][["p.eff"]],3)
    bym2_df[l,154] <- round(model_glm[["dic"]][["dic"]],3)
    bym2_df[l,155] <- round(model_glm[["dic"]][["p.eff"]],3)
    bym2_df[l,156] <- round(model_glm[["dic"]][["mean.deviance"]],3)
    bym2_df[l,157] <- round(model_glm[["mlik"]][[2,1]],3)
    
    logCPO.glm <- log(model_glm[["cpo"]][["cpo"]])
    bym2_df[l,162] <- -sum(logCPO.glm)
    
    sim_df$Y.glm <- model_glm$summary.fitted.values$mean
    resid.glm <- sim_df$Y - sim_df$Y.glm
    bym2_df[l,167] <-  round(mean(resid.glm^2),3)
    
    MI1 <- spdep::moran.mc(resid.glm, listw=W1.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,184] <- round(MI1[["statistic"]][["statistic"]], 3)
    bym2_df[l,185] <- round(MI1[["p.value"]], 3)
    
    MI2 <- spdep::moran.mc(resid.glm, listw=W2.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,186] <- round(MI2[["statistic"]][["statistic"]], 3)
    bym2_df[l,187] <- round(MI2[["p.value"]], 3)
    
    MI3 <- spdep::moran.mc(resid.glm, listw=W3.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,188] <- round(MI3[["statistic"]][["statistic"]], 3)
    bym2_df[l,189] <- round(MI3[["p.value"]], 3)
    
    bym2_df[l,190] <- model_glm[["cpu.used"]][["Pre"]]
    bym2_df[l,191] <- model_glm[["cpu.used"]][["Running"]]
    bym2_df[l,192] <- model_glm[["cpu.used"]][["Post"]]
    bym2_df[l,193] <- model_glm[["cpu.used"]][["Total"]]
    
  }
  
  
  return(bym2_df)
}    



#########################################################################################
# Here, $\phi = 0.3 < 0.5$ and $\tau_\epsilon = 4 > 1$
bym2.sim.result_3 <- function (j, n.sim) 
  
{ # - j: (integer), gives the particular combination to consider
  # - n.sim (integer>1) denotes the number of simulations 
  
  # Load file containing the list of parameters and weights
  load("/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Simulation/Param.RData")
  
  # The number of possible combinations 
  n.comb <- prod(unlist(lapply(list.sim.par , length)))
  
  # We create an array for all the different possibilities
  arr.comb <- array(1:n.comb, unlist(lapply(list.sim.par, length)))
  
  n <- nrow(X)
  ind.par <- as.numeric(which(arr.comb==j, arr.ind=T))
  print(ind.par)
  phi <- list.sim.par[[1]][ind.par[1]]
  W.nb <- list.sim.par[[2]][[ind.par[2]]]
  tau <- list.sim.par[[3]][ind.par[3]]
  
  W1.nb <- list.sim.par[["W"]][[1]]
  W1.listw <- spdep::nb2listw(W1.nb, style = "W")
  spdep::nb2INLA("W1.nb_graph",W1.nb)
  W1.nb_graph <- INLA::inla.read.graph(filename="W1.nb_graph")
  
  W2.nb <- list.sim.par[["W"]][[2]]
  W2.listw <- spdep::nb2listw(W2.nb, style = "W")
  spdep::nb2INLA("W2.nb_graph",W2.nb)
  W2.nb_graph <- INLA::inla.read.graph(filename="W2.nb_graph")
  
  W3.nb <- list.sim.par[["W"]][[3]]
  W3.listw <- spdep::nb2listw(W3.nb, style = "W")
  spdep::nb2INLA("W3.nb_graph",W3.nb)
  W3.nb_graph <- INLA::inla.read.graph(filename="W3.nb_graph")
  
  # Matrix to store estimates
  bym2_df=matrix(NA, nrow=n.sim, ncol=193)
  colnames(bym2_df) <- c("sample", "para.comb",
                         
                         "True.b0.W1.Pcs", "b0.W1.Pcs", "b0.W1.Pcs.sd", "b0.W1.Pcs_0.025CI", "b0.W1.Pcs_0.975CI", 
                         "True.b1.W1.Pcs", "b1.W1.Pcs", "b1.W1.Pcs.sd", "b1.W1.Pcs_0.025CI", "b1.W1.Pcs_0.975CI", 
                         "True.b2.W1.Pcs", "b2.W1.Pcs", "b2.W1.Pcs.sd", "b2.W1.Pcs_0.025CI", "b2.W1.Pcs_0.975CI",
                         "True.tau.W1.Pcs", "tau.W1.Pcs", "tau.W1.Pcs.sd", "tau.W1.Pcs_0.025CI", "tau.W1.Pcs_0.975CI",
                         "True.phi.W1.Pcs", "phi.W1.Pcs", "phi.W1.Pcs.sd", "phi.W1.Pcs_0.025CI", "phi.W1.Pcs_0.975CI",
                         
                         "True.b0.W2.Pcs", "b0.W2.Pcs", "b0.W2.Pcs.sd", "b0.W2.Pcs_0.025CI", "b0.W2.Pcs_0.975CI", 
                         "True.b1.W2.Pcs", "b1.W2.Pcs", "b1.W2.Pcs.sd", "b1.W2.Pcs_0.025CI", "b1.W2.Pcs_0.975CI", 
                         "True.b2.W2.Pcs", "b2.W2.Pcs", "b2.W2.Pcs.sd", "b2.W2.Pcs_0.025CI", "b2.W2.Pcs_0.975CI",
                         "True.tau.W2.Pcs", "tau.W2.Pcs", "tau.W2.Pcs.sd", "tau.W2.Pcs_0.025CI", "tau.W2.Pcs_0.975CI",
                         "True.phi.W2.Pcs", "phi.W2.Pcs", "phi.W2.Pcs.sd", "phi.W2.Pcs_0.025CI", "phi.W2.Pcs_0.975CI",
                         
                         "True.b0.W3.Pcs", "b0.W3.Pcs", "b0.W3.Pcs.sd", "b0.W3.Pcs_0.025CI", "b0.W3.Pcs_0.975CI", 
                         "True.b1.W3.Pcs", "b1.W3.Pcs", "b1.W3.Pcs.sd", "b1.W3.Pcs_0.025CI", "b1.W3.Pcs_0.975CI", 
                         "True.b2.W3.Pcs", "b2.W3.Pcs", "b2.W3.Pcs.sd", "b2.W3.Pcs_0.025CI", "b2.W3.Pcs_0.975CI",
                         "True.tau.W3.Pcs", "tau.W3.Pcs", "tau.W3.Pcs.sd", "tau.W3.Pcs_0.025CI", "tau.W3.Pcs_0.975CI",
                         "True.phi.W3.Pcs", "phi.W3.Pcs", "phi.W3.Pcs.sd", "phi.W3.Pcs_0.025CI", "phi.W3.Pcs_0.975CI",
                         
                         "True.b0.iid", "b0.iid", "b0.iid.sd", "b0.iid_0.025CI", "b0.iid_0.975CI", 
                         "True.b1.iid", "b1.iid", "b1.iid.sd", "b1.iid_0.025CI", "b1.iid_0.975CI", 
                         "True.b2.iid", "b2.iid", "b2.iid.sd", "b2.iid_0.025CI", "b2.iid_0.975CI",
                         "True.tau.iid", "tau.iid", "tau.iid.sd", "tau.iid_0.025CI", "tau.iid_0.975CI",
                         "True.phi.iid", "phi.iid", "phi.iid.sd", "phi.iid_0.025CI", "phi.iid_0.975CI",
                         
                         "True.b0.glm", "b0.glm", "b0.glm.sd", "b0.glm_0.025CI", "b0.glm_0.975CI", 
                         "True.b1.glm", "b1.glm", "b1.glm.sd", "b1.glm_0.025CI", "b1.glm_0.975CI", 
                         "True.b2.glm", "b2.glm", "b2.glm.sd", "b2.glm_0.025CI", "b2.glm_0.975CI",
                         "True.tau.glm", "tau.glm", "tau.glm.sd", "tau.glm_0.025CI", "tau.glm_0.975CI",
                         "True.phi.glm", "phi.glm", "phi.glm.sd", "phi.glm_0.025CI", "phi.glm_0.975CI",
                         
                         "WAIC.W1.Pcs", "WAIC.P.eff.W1.Pcs", "DIC.W1.Pcs", "Deviance.W1.Pcs", "DIC.P.eff.W1.Pcs", "MLIK.W1.Pcs",
                         "WAIC.W2.Pcs", "WAIC.P.eff.W2.Pcs", "DIC.W2.Pcs", "Deviance.W2.Pcs", "DIC.P.eff.W2.Pcs", "MLIK.W2.Pcs",
                         "WAIC.W3.Pcs", "WAIC.P.eff.W3.Pcs", "DIC.W3.Pcs", "Deviance.W3.Pcs", "DIC.P.eff.W3.Pcs", "MLIK.W3.Pcs",
                         
                         "WAIC.iid", "WAIC.P.eff.iid", "DIC.iid", "Deviance.iid", "DIC.P.eff.iid", "MLIK.iid",
                         "WAIC.glm", "WAIC.P.eff.glm", "DIC.glm", "Deviance.glm", "DIC.P.eff.glm", "MLIK.glm",
                         
                         "CPO.W1.Pcs", "CPO.W2.Pcs", "CPO.W3.Pcs", "CPO.iid", "CPO.glm",
                         
                         "MSE.W1.Pcs", "MSE.W2.Pcs", "MSE.W3.Pcs", "MSE.iid", "MSE.glm",
                         
                         "MI.stat.W1", "MI.p-v.W1", "MI.stat.W2", "MI.p-v.W2", "MI.stat.W3", "MI.p-v.W3",
                         
                         "Pre.Time_W1.Pcs", "Run.Time_W1.Pcs", "Post.Time_W1.Pcs", "Total.Time_W1.Pcs",
                         "Pre.Time_W2.Pcs", "Run.Time_W2.Pcs", "Post.Time_W2.Pcs", "Total.Time_W2.Pcs",
                         "Pre.Time_W3.Pcs", "Run.Time_W3.Pcs", "Post.Time_W3.Pcs", "Total.Time_W3.Pcs",
                         
                         "Pre.Time_iid", "Run.Time_iid", "Post.Time_iid", "Total.Time_iid",
                         "Pre.Time_glm", "Run.Time_glm", "Post.Time_glm", "Total.Time_glm")
  
  W.listw <- spdep::nb2listw(W.nb, style = "W")
  A.mat <- spdep::nb2mat(W.nb)
  A.mat[(A.mat>0)] = 1
  D.mat <- diag(rowSums(A.mat))
  Q = D.mat - A.mat
  Q.scaled = INLA::inla.scale.model(Q, constr = list(A = matrix(1, nrow = 1, ncol = n), e = 1E-40))
  Q.scaled <- as (Q.scaled, "matrix")
  inQ.scaled <- MASS::ginv(Q.scaled)
  mu0 <- cbind(rep(0, n))
  I <- diag(1,n)
  
  set.seed(j)
  u <- MASS::mvrnorm(n = 31*n.sim, mu = mu0, Sigma = inQ.scaled)
  u <- t(u)
  v <- MASS::mvrnorm(n = 31*n.sim, mu = mu0, Sigma = I)
  v <- t(v)
  
  # Set Pc Priors  
  prior.list1 = list(
    PC.prior.cs = list(
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.9)),
      phi = list(
        prior = "pc",
        param = c(0.5, 0.9)))
  )
  
  for (l in 1:n.sim)
  {print(l)
    k = 30*l
    repeat{
      b <- (sqrt(1-phi)*v[,k] + sqrt(phi)*u[,k])/sqrt(tau)
      set.seed(j)
      Y_e.pois <- rpois(n=n, lambda=exp(X%*%beta + b))
      sim_df <- data.frame(district = 1:n, X[,-1], Y=Y_e.pois)
      
      model.test <- INLA::inla(formula = Y ~ X1 + X2, 
                               data = sim_df, 
                               family = "poisson", 
                               control.compute = list(waic=TRUE, dic = TRUE), 
                               control.predictor = list(link=1), 
                               num.threads = 3, verbose = F)
      Y.test <- model.test$summary.fitted.values$mean
      resid.test <- Y_e.pois - Y.test
      
      MI.test <- spdep::moran.mc(resid.test, listw=W.listw, nsim = 1000, alternative="two.sided")
      pv <- MI.test[["p.value"]]
      k = k + 1
      
      if((pv < 0.05) & (max(Y_e.pois) < 5000))
      {
        break
      }
    }
    
    models_w1 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W1.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })   
    bym2_df[l,1] <- l
    bym2_df[l,2] <- j
    bym2_df[l,3] <- beta[1]
    bym2_df[l,4] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,5] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,6] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,7] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,8] <- beta[2]
    bym2_df[l,9] <- round(models_w1$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,10] <- round(models_w1$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,11] <- round(models_w1$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,12] <- round(models_w1$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,13] <- beta[3]
    bym2_df[l,14] <- round(models_w1$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,15] <- round(models_w1$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,16] <- round(models_w1$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,17] <- round(models_w1$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,18] <- tau
    bym2_df[l,19] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,20] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,21] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,22] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,23] <- phi
    bym2_df[l,24] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,25] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,26] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,27] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,128] <- round(models_w1$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,129] <- round(models_w1$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,130] <- round(models_w1$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,131] <- round(models_w1$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,132] <- round(models_w1$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,133] <- round(models_w1$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W1.Pcs <- log(models_w1[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,158] <- -sum(logCPO.W1.Pcs)
    
    sim_df$Y.W1.Pcs <- models_w1$PC.prior.cs$summary.fitted.values$mean
    resid.W1.Pcs <- sim_df$Y - sim_df$Y.W1.Pcs
    bym2_df[l,163] <-  round(mean(resid.W1.Pcs^2),3)
    
    bym2_df[l,168] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,169] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,170] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,171] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    models_w2 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W2.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })
    
    bym2_df[l,28] <- beta[1]
    bym2_df[l,29] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,30] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,31] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,32] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,33] <- beta[2]
    bym2_df[l,34] <- round(models_w2$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,35] <- round(models_w2$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,36] <- round(models_w2$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,37] <- round(models_w2$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,38] <- beta[3]
    bym2_df[l,39] <- round(models_w2$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,40] <- round(models_w2$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,41] <- round(models_w2$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,42] <- round(models_w2$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,43] <- tau
    bym2_df[l,44] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,45] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,46] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,47] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,48] <- phi
    bym2_df[l,49] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,50] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,51] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,52] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,134] <- round(models_w2$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,135] <- round(models_w2$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,136] <- round(models_w2$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,137] <- round(models_w2$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,138] <- round(models_w2$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,139] <- round(models_w2$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W2.Pcs <- log(models_w2[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,159] <- -sum(logCPO.W2.Pcs)
    
    sim_df$Y.W2.Pcs <- models_w2$PC.prior.cs$summary.fitted.values$mean
    resid.W2.Pcs <- sim_df$Y - sim_df$Y.W2.Pcs
    bym2_df[l,164] <-  round(mean(resid.W2.Pcs^2),3)
    
    bym2_df[l,172] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,173] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,174] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,175] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    models_w3 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W3.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })       
    
    
    bym2_df[l,53] <- beta[1]
    bym2_df[l,54] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,55] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,56] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,57] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,58] <- beta[2]
    bym2_df[l,59] <- round(models_w3$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,60] <- round(models_w3$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,61] <- round(models_w3$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,62] <- round(models_w3$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,63] <- beta[3]
    bym2_df[l,64] <- round(models_w3$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,65] <- round(models_w3$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,66] <- round(models_w3$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,67] <- round(models_w3$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,68] <- tau
    bym2_df[l,69] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,70] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,71] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,72] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,73] <- phi
    bym2_df[l,74] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,75] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,76] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,77] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,140] <- round(models_w3$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,141] <- round(models_w3$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,142] <- round(models_w3$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,143] <- round(models_w3$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,144] <- round(models_w3$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,145] <- round(models_w3$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W3.Pcs <- log(models_w3[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,160] <- -sum(logCPO.W3.Pcs)
    
    sim_df$Y.W3.Pcs <- models_w3$PC.prior.cs$summary.fitted.values$mean
    resid.W3.Pcs <- sim_df$Y - sim_df$Y.W3.Pcs
    bym2_df[l,165] <-  round(mean(resid.W3.Pcs^2),3)
    
    bym2_df[l,176] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,177] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,178] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,179] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    model_glm_iid <- INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "iid"), 
                                data = sim_df, 
                                family = "poisson", 
                                control.compute = list(waic=T, dic=T, cpo = TRUE, config = TRUE), 
                                control.predictor = list(link=1), 
                                num.threads = 3, verbose = F)
    
    bym2_df[l,78] <- beta[1]
    bym2_df[l,79] <- round(model_glm_iid$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,80] <- round(model_glm_iid$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,81] <- round(model_glm_iid$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,82] <- round(model_glm_iid$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,83] <- beta[2]
    bym2_df[l,84] <- round(model_glm_iid$summary.fixed['X1', 'mean'],3)
    bym2_df[l,85] <- round(model_glm_iid$summary.fixed['X1', 'sd'],3)
    bym2_df[l,86] <- round(model_glm_iid$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,87] <- round(model_glm_iid$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,88] <- beta[3]
    bym2_df[l,89] <- round(model_glm_iid$summary.fixed['X2', 'mean'],3)
    bym2_df[l,90] <- round(model_glm_iid$summary.fixed['X2', 'sd'],3)
    bym2_df[l,91] <- round(model_glm_iid$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,92] <- round(model_glm_iid$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,93] <- tau
    bym2_df[l,94] <- round(model_glm_iid$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,95] <- round(model_glm_iid$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,96] <- round(model_glm_iid$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,97] <- round(model_glm_iid$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,98] <- phi
    bym2_df[l,99] <- round(model_glm_iid$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,100] <- round(model_glm_iid$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,101] <- round(model_glm_iid$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,102] <- round(model_glm_iid$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,146] <- round(model_glm_iid[["waic"]][["waic"]],3)
    bym2_df[l,147] <- round(model_glm_iid[["waic"]][["p.eff"]],3)
    bym2_df[l,148] <- round(model_glm_iid[["dic"]][["dic"]],3)
    bym2_df[l,149] <- round(model_glm_iid[["dic"]][["p.eff"]],3)
    bym2_df[l,150] <- round(model_glm_iid[["dic"]][["mean.deviance"]],3)
    bym2_df[l,151] <- round(model_glm_iid[["mlik"]][[2,1]],3)
    
    logCPO.iid <- log(model_glm_iid[["cpo"]][["cpo"]])
    bym2_df[l,161] <- -sum(logCPO.iid)
    
    sim_df$Y.iid <- model_glm_iid$summary.fitted.values$mean
    resid.iid <- sim_df$Y - sim_df$Y.iid
    bym2_df[l,166] <-  round(mean(resid.iid^2),3)
    
    bym2_df[l,180] <- model_glm_iid[["cpu.used"]][["Pre"]]
    bym2_df[l,181] <- model_glm_iid[["cpu.used"]][["Running"]]
    bym2_df[l,182] <- model_glm_iid[["cpu.used"]][["Post"]]
    bym2_df[l,183] <- model_glm_iid[["cpu.used"]][["Total"]]
    
    model_glm <- INLA::inla(formula = Y ~ X1 + X2, 
                            data = sim_df, 
                            family = "poisson", 
                            control.compute = list(waic=TRUE, dic = TRUE, cpo = TRUE, config = TRUE), 
                            control.predictor = list(link=1), 
                            num.threads = 3, verbose = FALSE)
    
    bym2_df[l,103] <- beta[1]
    bym2_df[l,104] <- round(model_glm$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,105] <- round(model_glm$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,106] <- round(model_glm$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,107] <- round(model_glm$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,108] <- beta[2]
    bym2_df[l,109] <- round(model_glm$summary.fixed['X1', 'mean'],3)
    bym2_df[l,110] <- round(model_glm$summary.fixed['X1', 'sd'],3)
    bym2_df[l,111] <- round(model_glm$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,112] <- round(model_glm$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,113] <- beta[3]
    bym2_df[l,114] <- round(model_glm$summary.fixed['X2', 'mean'],3)
    bym2_df[l,115] <- round(model_glm$summary.fixed['X2', 'sd'],3)
    bym2_df[l,116] <- round(model_glm$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,117] <- round(model_glm$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,118] <- tau
    bym2_df[l,119] <- NA
    bym2_df[l,120] <- NA
    bym2_df[l,121] <- NA
    bym2_df[l,122] <- NA
    bym2_df[l,123] <- phi
    bym2_df[l,124] <- NA
    bym2_df[l,125] <- NA
    bym2_df[l,126] <- NA
    bym2_df[l,127] <- NA
    
    bym2_df[l,152] <- round(model_glm[["waic"]][["waic"]],3)
    bym2_df[l,153] <- round(model_glm[["waic"]][["p.eff"]],3)
    bym2_df[l,154] <- round(model_glm[["dic"]][["dic"]],3)
    bym2_df[l,155] <- round(model_glm[["dic"]][["p.eff"]],3)
    bym2_df[l,156] <- round(model_glm[["dic"]][["mean.deviance"]],3)
    bym2_df[l,157] <- round(model_glm[["mlik"]][[2,1]],3)
    
    logCPO.glm <- log(model_glm[["cpo"]][["cpo"]])
    bym2_df[l,162] <- -sum(logCPO.glm)
    
    sim_df$Y.glm <- model_glm$summary.fitted.values$mean
    resid.glm <- sim_df$Y - sim_df$Y.glm
    bym2_df[l,167] <-  round(mean(resid.glm^2),3)
    
    MI1 <- spdep::moran.mc(resid.glm, listw=W1.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,184] <- round(MI1[["statistic"]][["statistic"]], 3)
    bym2_df[l,185] <- round(MI1[["p.value"]], 3)
    
    MI2 <- spdep::moran.mc(resid.glm, listw=W2.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,186] <- round(MI2[["statistic"]][["statistic"]], 3)
    bym2_df[l,187] <- round(MI2[["p.value"]], 3)
    
    MI3 <- spdep::moran.mc(resid.glm, listw=W3.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,188] <- round(MI3[["statistic"]][["statistic"]], 3)
    bym2_df[l,189] <- round(MI3[["p.value"]], 3)
    
    bym2_df[l,190] <- model_glm[["cpu.used"]][["Pre"]]
    bym2_df[l,191] <- model_glm[["cpu.used"]][["Running"]]
    bym2_df[l,192] <- model_glm[["cpu.used"]][["Post"]]
    bym2_df[l,193] <- model_glm[["cpu.used"]][["Total"]]
    
  }
  
  
  return(bym2_df)
}    




#########################################################################################
# Here, $\phi = 0.9 > 0.5$ and $\tau_\epsilon = 4 > 1$
bym2.sim.result_4 <- function (j, n.sim) 
  
{ # - j: (integer), gives the particular combination to consider
  # - n.sim (integer>1) denotes the number of simulations 
  
  # Load file containing the list of parameters and weights
  load("/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Simulation/Param.RData")
  
  # The number of possible combinations 
  n.comb <- prod(unlist(lapply(list.sim.par , length)))
  
  # We create an array for all the different possibilities
  arr.comb <- array(1:n.comb, unlist(lapply(list.sim.par, length)))
  
  n <- nrow(X)
  ind.par <- as.numeric(which(arr.comb==j, arr.ind=T))
  print(ind.par)
  phi <- list.sim.par[[1]][ind.par[1]]
  W.nb <- list.sim.par[[2]][[ind.par[2]]]
  tau <- list.sim.par[[3]][ind.par[3]]
  
  W1.nb <- list.sim.par[["W"]][[1]]
  W1.listw <- spdep::nb2listw(W1.nb, style = "W")
  spdep::nb2INLA("W1.nb_graph",W1.nb)
  W1.nb_graph <- INLA::inla.read.graph(filename="W1.nb_graph")
  
  W2.nb <- list.sim.par[["W"]][[2]]
  W2.listw <- spdep::nb2listw(W2.nb, style = "W")
  spdep::nb2INLA("W2.nb_graph",W2.nb)
  W2.nb_graph <- INLA::inla.read.graph(filename="W2.nb_graph")
  
  W3.nb <- list.sim.par[["W"]][[3]]
  W3.listw <- spdep::nb2listw(W3.nb, style = "W")
  spdep::nb2INLA("W3.nb_graph",W3.nb)
  W3.nb_graph <- INLA::inla.read.graph(filename="W3.nb_graph")
  
  # Matrix to store estimates
  bym2_df=matrix(NA, nrow=n.sim, ncol=193)
  colnames(bym2_df) <- c("sample", "para.comb",
                         
                         "True.b0.W1.Pcs", "b0.W1.Pcs", "b0.W1.Pcs.sd", "b0.W1.Pcs_0.025CI", "b0.W1.Pcs_0.975CI", 
                         "True.b1.W1.Pcs", "b1.W1.Pcs", "b1.W1.Pcs.sd", "b1.W1.Pcs_0.025CI", "b1.W1.Pcs_0.975CI", 
                         "True.b2.W1.Pcs", "b2.W1.Pcs", "b2.W1.Pcs.sd", "b2.W1.Pcs_0.025CI", "b2.W1.Pcs_0.975CI",
                         "True.tau.W1.Pcs", "tau.W1.Pcs", "tau.W1.Pcs.sd", "tau.W1.Pcs_0.025CI", "tau.W1.Pcs_0.975CI",
                         "True.phi.W1.Pcs", "phi.W1.Pcs", "phi.W1.Pcs.sd", "phi.W1.Pcs_0.025CI", "phi.W1.Pcs_0.975CI",
                         
                         "True.b0.W2.Pcs", "b0.W2.Pcs", "b0.W2.Pcs.sd", "b0.W2.Pcs_0.025CI", "b0.W2.Pcs_0.975CI", 
                         "True.b1.W2.Pcs", "b1.W2.Pcs", "b1.W2.Pcs.sd", "b1.W2.Pcs_0.025CI", "b1.W2.Pcs_0.975CI", 
                         "True.b2.W2.Pcs", "b2.W2.Pcs", "b2.W2.Pcs.sd", "b2.W2.Pcs_0.025CI", "b2.W2.Pcs_0.975CI",
                         "True.tau.W2.Pcs", "tau.W2.Pcs", "tau.W2.Pcs.sd", "tau.W2.Pcs_0.025CI", "tau.W2.Pcs_0.975CI",
                         "True.phi.W2.Pcs", "phi.W2.Pcs", "phi.W2.Pcs.sd", "phi.W2.Pcs_0.025CI", "phi.W2.Pcs_0.975CI",
                         
                         "True.b0.W3.Pcs", "b0.W3.Pcs", "b0.W3.Pcs.sd", "b0.W3.Pcs_0.025CI", "b0.W3.Pcs_0.975CI", 
                         "True.b1.W3.Pcs", "b1.W3.Pcs", "b1.W3.Pcs.sd", "b1.W3.Pcs_0.025CI", "b1.W3.Pcs_0.975CI", 
                         "True.b2.W3.Pcs", "b2.W3.Pcs", "b2.W3.Pcs.sd", "b2.W3.Pcs_0.025CI", "b2.W3.Pcs_0.975CI",
                         "True.tau.W3.Pcs", "tau.W3.Pcs", "tau.W3.Pcs.sd", "tau.W3.Pcs_0.025CI", "tau.W3.Pcs_0.975CI",
                         "True.phi.W3.Pcs", "phi.W3.Pcs", "phi.W3.Pcs.sd", "phi.W3.Pcs_0.025CI", "phi.W3.Pcs_0.975CI",
                         
                         "True.b0.iid", "b0.iid", "b0.iid.sd", "b0.iid_0.025CI", "b0.iid_0.975CI", 
                         "True.b1.iid", "b1.iid", "b1.iid.sd", "b1.iid_0.025CI", "b1.iid_0.975CI", 
                         "True.b2.iid", "b2.iid", "b2.iid.sd", "b2.iid_0.025CI", "b2.iid_0.975CI",
                         "True.tau.iid", "tau.iid", "tau.iid.sd", "tau.iid_0.025CI", "tau.iid_0.975CI",
                         "True.phi.iid", "phi.iid", "phi.iid.sd", "phi.iid_0.025CI", "phi.iid_0.975CI",
                         
                         "True.b0.glm", "b0.glm", "b0.glm.sd", "b0.glm_0.025CI", "b0.glm_0.975CI", 
                         "True.b1.glm", "b1.glm", "b1.glm.sd", "b1.glm_0.025CI", "b1.glm_0.975CI", 
                         "True.b2.glm", "b2.glm", "b2.glm.sd", "b2.glm_0.025CI", "b2.glm_0.975CI",
                         "True.tau.glm", "tau.glm", "tau.glm.sd", "tau.glm_0.025CI", "tau.glm_0.975CI",
                         "True.phi.glm", "phi.glm", "phi.glm.sd", "phi.glm_0.025CI", "phi.glm_0.975CI",
                         
                         "WAIC.W1.Pcs", "WAIC.P.eff.W1.Pcs", "DIC.W1.Pcs", "Deviance.W1.Pcs", "DIC.P.eff.W1.Pcs", "MLIK.W1.Pcs",
                         "WAIC.W2.Pcs", "WAIC.P.eff.W2.Pcs", "DIC.W2.Pcs", "Deviance.W2.Pcs", "DIC.P.eff.W2.Pcs", "MLIK.W2.Pcs",
                         "WAIC.W3.Pcs", "WAIC.P.eff.W3.Pcs", "DIC.W3.Pcs", "Deviance.W3.Pcs", "DIC.P.eff.W3.Pcs", "MLIK.W3.Pcs",
                         
                         "WAIC.iid", "WAIC.P.eff.iid", "DIC.iid", "Deviance.iid", "DIC.P.eff.iid", "MLIK.iid",
                         "WAIC.glm", "WAIC.P.eff.glm", "DIC.glm", "Deviance.glm", "DIC.P.eff.glm", "MLIK.glm",
                         
                         "CPO.W1.Pcs", "CPO.W2.Pcs", "CPO.W3.Pcs", "CPO.iid", "CPO.glm",
                         
                         "MSE.W1.Pcs", "MSE.W2.Pcs", "MSE.W3.Pcs", "MSE.iid", "MSE.glm",
                         
                         "MI.stat.W1", "MI.p-v.W1", "MI.stat.W2", "MI.p-v.W2", "MI.stat.W3", "MI.p-v.W3",
                         
                         "Pre.Time_W1.Pcs", "Run.Time_W1.Pcs", "Post.Time_W1.Pcs", "Total.Time_W1.Pcs",
                         "Pre.Time_W2.Pcs", "Run.Time_W2.Pcs", "Post.Time_W2.Pcs", "Total.Time_W2.Pcs",
                         "Pre.Time_W3.Pcs", "Run.Time_W3.Pcs", "Post.Time_W3.Pcs", "Total.Time_W3.Pcs",
                         
                         "Pre.Time_iid", "Run.Time_iid", "Post.Time_iid", "Total.Time_iid",
                         "Pre.Time_glm", "Run.Time_glm", "Post.Time_glm", "Total.Time_glm")
  
  W.listw <- spdep::nb2listw(W.nb, style = "W")
  A.mat <- spdep::nb2mat(W.nb)
  A.mat[(A.mat>0)] = 1
  D.mat <- diag(rowSums(A.mat))
  Q = D.mat - A.mat
  Q.scaled = INLA::inla.scale.model(Q, constr = list(A = matrix(1, nrow = 1, ncol = n), e = 1E-40))
  Q.scaled <- as (Q.scaled, "matrix")
  inQ.scaled <- MASS::ginv(Q.scaled)
  mu0 <- cbind(rep(0, n))
  I <- diag(1,n)
  
  set.seed(j)
  u <- MASS::mvrnorm(n = 31*n.sim, mu = mu0, Sigma = inQ.scaled)
  u <- t(u)
  v <- MASS::mvrnorm(n = 31*n.sim, mu = mu0, Sigma = I)
  v <- t(v)
  
  # Set Pc Priors  
  prior.list1 = list(
    PC.prior.cs = list(
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.9)),
      phi = list(
        prior = "pc",
        param = c(0.5, 0.9)))
  )
  
  for (l in 1:n.sim)
  {print(l)
    k = 30*l
    repeat{
      b <- (sqrt(1-phi)*v[,k] + sqrt(phi)*u[,k])/sqrt(tau)
      set.seed(j)
      Y_e.pois <- rpois(n=n, lambda=exp(X%*%beta + b))
      sim_df <- data.frame(district = 1:n, X[,-1], Y=Y_e.pois)
      
      model.test <- INLA::inla(formula = Y ~ X1 + X2, 
                               data = sim_df, 
                               family = "poisson", 
                               control.compute = list(waic=TRUE, dic = TRUE), 
                               control.predictor = list(link=1), 
                               num.threads = 3, verbose = F)
      Y.test <- model.test$summary.fitted.values$mean
      resid.test <- Y_e.pois - Y.test
      
      MI.test <- spdep::moran.mc(resid.test, listw=W.listw, nsim = 1000, alternative="two.sided")
      pv <- MI.test[["p.value"]]
      k = k + 1
      
      if((pv < 0.05) & (max(Y_e.pois) < 5000))
      {
        break
      }
    }
    
    models_w1 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W1.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })   
    bym2_df[l,1] <- l
    bym2_df[l,2] <- j
    bym2_df[l,3] <- beta[1]
    bym2_df[l,4] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,5] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,6] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,7] <- round(models_w1$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,8] <- beta[2]
    bym2_df[l,9] <- round(models_w1$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,10] <- round(models_w1$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,11] <- round(models_w1$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,12] <- round(models_w1$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,13] <- beta[3]
    bym2_df[l,14] <- round(models_w1$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,15] <- round(models_w1$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,16] <- round(models_w1$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,17] <- round(models_w1$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,18] <- tau
    bym2_df[l,19] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,20] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,21] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,22] <- round(models_w1$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,23] <- phi
    bym2_df[l,24] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,25] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,26] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,27] <- round(models_w1$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,128] <- round(models_w1$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,129] <- round(models_w1$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,130] <- round(models_w1$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,131] <- round(models_w1$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,132] <- round(models_w1$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,133] <- round(models_w1$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W1.Pcs <- log(models_w1[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,158] <- -sum(logCPO.W1.Pcs)
    
    sim_df$Y.W1.Pcs <- models_w1$PC.prior.cs$summary.fitted.values$mean
    resid.W1.Pcs <- sim_df$Y - sim_df$Y.W1.Pcs
    bym2_df[l,163] <-  round(mean(resid.W1.Pcs^2),3)
    
    bym2_df[l,168] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,169] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,170] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,171] <- models_w1[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    models_w2 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W2.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })
    
    bym2_df[l,28] <- beta[1]
    bym2_df[l,29] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,30] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,31] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,32] <- round(models_w2$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,33] <- beta[2]
    bym2_df[l,34] <- round(models_w2$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,35] <- round(models_w2$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,36] <- round(models_w2$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,37] <- round(models_w2$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,38] <- beta[3]
    bym2_df[l,39] <- round(models_w2$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,40] <- round(models_w2$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,41] <- round(models_w2$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,42] <- round(models_w2$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,43] <- tau
    bym2_df[l,44] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,45] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,46] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,47] <- round(models_w2$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,48] <- phi
    bym2_df[l,49] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,50] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,51] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,52] <- round(models_w2$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,134] <- round(models_w2$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,135] <- round(models_w2$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,136] <- round(models_w2$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,137] <- round(models_w2$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,138] <- round(models_w2$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,139] <- round(models_w2$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W2.Pcs <- log(models_w2[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,159] <- -sum(logCPO.W2.Pcs)
    
    sim_df$Y.W2.Pcs <- models_w2$PC.prior.cs$summary.fitted.values$mean
    resid.W2.Pcs <- sim_df$Y - sim_df$Y.W2.Pcs
    bym2_df[l,164] <-  round(mean(resid.W2.Pcs^2),3)
    
    bym2_df[l,172] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,173] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,174] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,175] <- models_w2[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    models_w3 <- lapply (prior.list1, function(hyper.prior){
      INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "bym2", graph = W3.nb_graph, hyper = hyper.prior,
                                           scale.model = TRUE, constr = TRUE), 
                 data = sim_df, 
                 family = "poisson", 
                 control.compute = list(waic = T, dic = T, return.marginals.predictor = TRUE, cpo = TRUE, config = TRUE), 
                 control.predictor = list(compute = TRUE), 
                 num.threads = 3, verbose = F)
    })       
    
    
    bym2_df[l,53] <- beta[1]
    bym2_df[l,54] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,55] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,56] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,57] <- round(models_w3$PC.prior.cs$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,58] <- beta[2]
    bym2_df[l,59] <- round(models_w3$PC.prior.cs$summary.fixed['X1', 'mean'],3)
    bym2_df[l,60] <- round(models_w3$PC.prior.cs$summary.fixed['X1', 'sd'],3)
    bym2_df[l,61] <- round(models_w3$PC.prior.cs$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,62] <- round(models_w3$PC.prior.cs$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,63] <- beta[3]
    bym2_df[l,64] <- round(models_w3$PC.prior.cs$summary.fixed['X2', 'mean'],3)
    bym2_df[l,65] <- round(models_w3$PC.prior.cs$summary.fixed['X2', 'sd'],3)
    bym2_df[l,66] <- round(models_w3$PC.prior.cs$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,67] <- round(models_w3$PC.prior.cs$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,68] <- tau
    bym2_df[l,69] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,70] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,71] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,72] <- round(models_w3$PC.prior.cs$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,73] <- phi
    bym2_df[l,74] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,75] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,76] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,77] <- round(models_w3$PC.prior.cs$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,140] <- round(models_w3$PC.prior.cs[["waic"]][["waic"]],3)
    bym2_df[l,141] <- round(models_w3$PC.prior.cs[["waic"]][["p.eff"]],3)
    bym2_df[l,142] <- round(models_w3$PC.prior.cs[["dic"]][["dic"]],3)
    bym2_df[l,143] <- round(models_w3$PC.prior.cs[["dic"]][["p.eff"]],3)
    bym2_df[l,144] <- round(models_w3$PC.prior.cs[["dic"]][["mean.deviance"]],3)
    bym2_df[l,145] <- round(models_w3$PC.prior.cs[["mlik"]][[2,1]],3)
    
    logCPO.W3.Pcs <- log(models_w3[["PC.prior.cs"]][["cpo"]][["cpo"]])
    bym2_df[l,160] <- -sum(logCPO.W3.Pcs)
    
    sim_df$Y.W3.Pcs <- models_w3$PC.prior.cs$summary.fitted.values$mean
    resid.W3.Pcs <- sim_df$Y - sim_df$Y.W3.Pcs
    bym2_df[l,165] <-  round(mean(resid.W3.Pcs^2),3)
    
    bym2_df[l,176] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Pre"]]
    bym2_df[l,177] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Running"]]
    bym2_df[l,178] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Post"]]
    bym2_df[l,179] <- models_w3[["PC.prior.cs"]][["cpu.used"]][["Total"]]
    
    model_glm_iid <- INLA::inla(formula = Y ~ X1 + X2 + f(district, model = "iid"), 
                                data = sim_df, 
                                family = "poisson", 
                                control.compute = list(waic=T, dic=T, cpo = TRUE, config = TRUE), 
                                control.predictor = list(link=1), 
                                num.threads = 3, verbose = F)
    
    bym2_df[l,78] <- beta[1]
    bym2_df[l,79] <- round(model_glm_iid$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,80] <- round(model_glm_iid$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,81] <- round(model_glm_iid$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,82] <- round(model_glm_iid$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,83] <- beta[2]
    bym2_df[l,84] <- round(model_glm_iid$summary.fixed['X1', 'mean'],3)
    bym2_df[l,85] <- round(model_glm_iid$summary.fixed['X1', 'sd'],3)
    bym2_df[l,86] <- round(model_glm_iid$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,87] <- round(model_glm_iid$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,88] <- beta[3]
    bym2_df[l,89] <- round(model_glm_iid$summary.fixed['X2', 'mean'],3)
    bym2_df[l,90] <- round(model_glm_iid$summary.fixed['X2', 'sd'],3)
    bym2_df[l,91] <- round(model_glm_iid$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,92] <- round(model_glm_iid$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,93] <- tau
    bym2_df[l,94] <- round(model_glm_iid$summary.hyperpar['Precision for district', 'mean'],3)
    bym2_df[l,95] <- round(model_glm_iid$summary.hyperpar['Precision for district', 'sd'],3)
    bym2_df[l,96] <- round(model_glm_iid$summary.hyperpar['Precision for district', '0.025quant'],3)
    bym2_df[l,97] <- round(model_glm_iid$summary.hyperpar['Precision for district', '0.975quant'],3)
    bym2_df[l,98] <- phi
    bym2_df[l,99] <- round(model_glm_iid$summary.hyperpar['Phi for district', 'mean'],3)
    bym2_df[l,100] <- round(model_glm_iid$summary.hyperpar['Phi for district', 'sd'],3)
    bym2_df[l,101] <- round(model_glm_iid$summary.hyperpar['Phi for district', '0.025quant'],3)
    bym2_df[l,102] <- round(model_glm_iid$summary.hyperpar['Phi for district', '0.975quant'],3)
    
    bym2_df[l,146] <- round(model_glm_iid[["waic"]][["waic"]],3)
    bym2_df[l,147] <- round(model_glm_iid[["waic"]][["p.eff"]],3)
    bym2_df[l,148] <- round(model_glm_iid[["dic"]][["dic"]],3)
    bym2_df[l,149] <- round(model_glm_iid[["dic"]][["p.eff"]],3)
    bym2_df[l,150] <- round(model_glm_iid[["dic"]][["mean.deviance"]],3)
    bym2_df[l,151] <- round(model_glm_iid[["mlik"]][[2,1]],3)
    
    logCPO.iid <- log(model_glm_iid[["cpo"]][["cpo"]])
    bym2_df[l,161] <- -sum(logCPO.iid)
    
    sim_df$Y.iid <- model_glm_iid$summary.fitted.values$mean
    resid.iid <- sim_df$Y - sim_df$Y.iid
    bym2_df[l,166] <-  round(mean(resid.iid^2),3)
    
    bym2_df[l,180] <- model_glm_iid[["cpu.used"]][["Pre"]]
    bym2_df[l,181] <- model_glm_iid[["cpu.used"]][["Running"]]
    bym2_df[l,182] <- model_glm_iid[["cpu.used"]][["Post"]]
    bym2_df[l,183] <- model_glm_iid[["cpu.used"]][["Total"]]
    
    model_glm <- INLA::inla(formula = Y ~ X1 + X2, 
                            data = sim_df, 
                            family = "poisson", 
                            control.compute = list(waic=TRUE, dic = TRUE, cpo = TRUE, config = TRUE), 
                            control.predictor = list(link=1), 
                            num.threads = 3, verbose = FALSE)
    
    bym2_df[l,103] <- beta[1]
    bym2_df[l,104] <- round(model_glm$summary.fixed['(Intercept)', 'mean'],3)
    bym2_df[l,105] <- round(model_glm$summary.fixed['(Intercept)', 'sd'],3)
    bym2_df[l,106] <- round(model_glm$summary.fixed['(Intercept)', '0.025quant'],3)
    bym2_df[l,107] <- round(model_glm$summary.fixed['(Intercept)', '0.975quant'],3)
    bym2_df[l,108] <- beta[2]
    bym2_df[l,109] <- round(model_glm$summary.fixed['X1', 'mean'],3)
    bym2_df[l,110] <- round(model_glm$summary.fixed['X1', 'sd'],3)
    bym2_df[l,111] <- round(model_glm$summary.fixed['X1', '0.025quant'],3)
    bym2_df[l,112] <- round(model_glm$summary.fixed['X1', '0.975quant'],3)
    bym2_df[l,113] <- beta[3]
    bym2_df[l,114] <- round(model_glm$summary.fixed['X2', 'mean'],3)
    bym2_df[l,115] <- round(model_glm$summary.fixed['X2', 'sd'],3)
    bym2_df[l,116] <- round(model_glm$summary.fixed['X2', '0.025quant'],3)
    bym2_df[l,117] <- round(model_glm$summary.fixed['X2', '0.975quant'],3)
    bym2_df[l,118] <- tau
    bym2_df[l,119] <- NA
    bym2_df[l,120] <- NA
    bym2_df[l,121] <- NA
    bym2_df[l,122] <- NA
    bym2_df[l,123] <- phi
    bym2_df[l,124] <- NA
    bym2_df[l,125] <- NA
    bym2_df[l,126] <- NA
    bym2_df[l,127] <- NA
    
    bym2_df[l,152] <- round(model_glm[["waic"]][["waic"]],3)
    bym2_df[l,153] <- round(model_glm[["waic"]][["p.eff"]],3)
    bym2_df[l,154] <- round(model_glm[["dic"]][["dic"]],3)
    bym2_df[l,155] <- round(model_glm[["dic"]][["p.eff"]],3)
    bym2_df[l,156] <- round(model_glm[["dic"]][["mean.deviance"]],3)
    bym2_df[l,157] <- round(model_glm[["mlik"]][[2,1]],3)
    
    logCPO.glm <- log(model_glm[["cpo"]][["cpo"]])
    bym2_df[l,162] <- -sum(logCPO.glm)
    
    sim_df$Y.glm <- model_glm$summary.fitted.values$mean
    resid.glm <- sim_df$Y - sim_df$Y.glm
    bym2_df[l,167] <-  round(mean(resid.glm^2),3)
    
    MI1 <- spdep::moran.mc(resid.glm, listw=W1.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,184] <- round(MI1[["statistic"]][["statistic"]], 3)
    bym2_df[l,185] <- round(MI1[["p.value"]], 3)
    
    MI2 <- spdep::moran.mc(resid.glm, listw=W2.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,186] <- round(MI2[["statistic"]][["statistic"]], 3)
    bym2_df[l,187] <- round(MI2[["p.value"]], 3)
    
    MI3 <- spdep::moran.mc(resid.glm, listw=W3.listw, nsim = 1000, alternative="two.sided")
    bym2_df[l,188] <- round(MI3[["statistic"]][["statistic"]], 3)
    bym2_df[l,189] <- round(MI3[["p.value"]], 3)
    
    bym2_df[l,190] <- model_glm[["cpu.used"]][["Pre"]]
    bym2_df[l,191] <- model_glm[["cpu.used"]][["Running"]]
    bym2_df[l,192] <- model_glm[["cpu.used"]][["Post"]]
    bym2_df[l,193] <- model_glm[["cpu.used"]][["Total"]]
    
  }
  
  
  return(bym2_df)
}    




#########################################################################################
#Run for estimates

n.sim = 2
j = 1 #W1, phi = 0.3, tau = 4/9 
comb1_W1 <- data.frame(bym2.sim.result_1(j, n.sim)) 
save(comb1_W1,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb1_W1.RData")

j = 2 #W1, phi = 0.9, tau = 4/9 
comb2_W1 <- data.frame(bym2.sim.result_2(j, n.sim)) 
save(comb2_W1,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb2_W1.RData")

j = 3 #W2, phi = 0.3, tau = 4/9 
comb1_W2 <- data.frame(bym2.sim.result_1(j, n.sim))  
save(comb1_W2,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb1_W2.RData")

j = 4 #W2, phi = 0.9, tau = 4/9
comb2_W2 <- data.frame(bym2.sim.result_2(j, n.sim))
save(comb2_W2,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb2_W2.RData")

j = 5 #W3, phi = 0.3, tau = 4/9
comb1_W3 <- data.frame(bym2.sim.result_1(j, n.sim))
save(comb1_W3,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb1_W3.RData")

j = 6 #W3, phi = 0.9, tau = 4/9
comb2_W3 <- data.frame(bym2.sim.result_2(j, n.sim))
save(comb2_W3,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb2_W3.RData")

j = 7 #W1, phi = 0.3, tau = 4
comb3_W1 <- data.frame(bym2.sim.result_3(j, n.sim))
save(comb3_W1,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb3_W1.RData")

j = 8 #W1, phi = 0.9, tau = 4
comb4_W1 <- data.frame(bym2.sim.result_4(j, n.sim))
save(comb4_W1,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb4_W1.RData")

j = 9 #W2, phi = 0.3, tau = 4
comb3_W2 <- data.frame(bym2.sim.result_3(j, n.sim))
save(comb3_W2,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb3_W2.RData")

j = 10 #W2, phi = 0.9, tau = 4
comb4_W2 <- data.frame(bym2.sim.result_4(j, n.sim))
save(comb4_W2,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb4_W2.RData")

j = 11 #W3, phi = 0.3, tau = 4
comb3_W3 <- data.frame(bym2.sim.result_3(j, n.sim))
save(comb3_W3,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb3_W3.RData")

j = 12 #W3, phi = 0.9, tau = 4
comb4_W3 <- data.frame(bym2.sim.result_4(j, n.sim))
save(comb4_W3,file="/Users/soms/Documents/TEXfiles/PhDThesis/Codes/Articles/Weights Misspecification/Data/simulated_data_comb4_W3.RData")

