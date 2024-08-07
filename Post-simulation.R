# Analyzing simulated estimates
rm(list=ls(all=TRUE))

##--------- Load all packages
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(cowplot))

##--------- Load Simulated estimates
# Combination 1 represents phi1 = 0.3 tau1 = 4/9
# Combination 2 represents phi2 = 0.9 tau1 = 4/9
# Combination 3 represents phi1 = 0.3 tau2 = 4.0
# Combination 4 represents phi2 = 0.9 tau2 = 4.0

load("/Users/soms/Documents/Data/simulated_data_comb1_W1.RData") 
load("/Users/soms/Documents/Data/simulated_data_comb1_W2.RData") 
load("/Users/soms/Documents/Data/simulated_data_comb1_W3.RData") 
load("/Users/soms/Documents/Data/simulated_data_comb2_W1.RData") 
load("/Users/soms/Documents/Data/simulated_data_comb2_W2.RData") 
load("/Users/soms/Documents/Data/simulated_data_comb2_W3.RData") 
load("/Users/soms/Documents/Data/simulated_data_comb3_W1.RData") 
load("/Users/soms/Documents/Data/simulated_data_comb3_W2.RData") 
load("/Users/soms/Documents/Data/simulated_data_comb3_W3.RData") 
load("/Users/soms/Documents/Data/simulated_data_comb4_W1.RData") 
load("/Users/soms/Documents/Data/simulated_data_comb4_W2.RData") 
load("/Users/soms/Documents/Data/simulated_data_comb4_W3.RData") 


###################
# Parameter values
###################

b0 = 2; b1 = 0.25; b2 = -2; phi1 = 0.3; phi2 = 0.9; tau1 = 4/9; tau2 = 4


##########################
# Moran's I statistic plot
##########################
# Average Moran's I Statistics with samples
tab_MIstat_W <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each = 12),
                           Weight = rep(c(1:3), 1),
                           para.comb = rep(c(1, 2, 3, 4), each = 3),
                           MI_est = cbind(c(mean(comb1_W1$MI.stat.W1), mean(comb1_W1$MI.stat.W2), mean(comb1_W1$MI.stat.W3),
                                            mean(comb2_W1$MI.stat.W1), mean(comb2_W1$MI.stat.W2), mean(comb2_W1$MI.stat.W3),
                                            mean(comb3_W1$MI.stat.W1), mean(comb3_W1$MI.stat.W2), mean(comb3_W1$MI.stat.W3),
                                            mean(comb4_W1$MI.stat.W1), mean(comb4_W1$MI.stat.W2), mean(comb4_W1$MI.stat.W3),
                                            
                                            mean(comb1_W2$MI.stat.W1), mean(comb1_W2$MI.stat.W2), mean(comb1_W2$MI.stat.W3),
                                            mean(comb2_W2$MI.stat.W1), mean(comb2_W2$MI.stat.W2), mean(comb2_W2$MI.stat.W3),
                                            mean(comb3_W2$MI.stat.W1), mean(comb3_W2$MI.stat.W2), mean(comb3_W2$MI.stat.W3),
                                            mean(comb4_W2$MI.stat.W1), mean(comb4_W2$MI.stat.W2), mean(comb4_W2$MI.stat.W3),
                                            
                                            mean(comb1_W3$MI.stat.W1), mean(comb1_W3$MI.stat.W2), mean(comb1_W3$MI.stat.W3),
                                            mean(comb2_W3$MI.stat.W1), mean(comb2_W3$MI.stat.W2), mean(comb2_W3$MI.stat.W3),
                                            mean(comb3_W3$MI.stat.W1), mean(comb3_W3$MI.stat.W2), mean(comb3_W3$MI.stat.W3),
                                            mean(comb4_W3$MI.stat.W1), mean(comb4_W3$MI.stat.W2), mean(comb4_W3$MI.stat.W3))))

tab_MIstat_W$para.comb = as.factor(tab_MIstat_W$para.comb)
tab_MIstat_W$Graph = as.factor(tab_MIstat_W$Graph)
levels(tab_MIstat_W$para.comb) <- c(expression(paste(phi,"=0.3 and ", tau,"=4/9")), expression(paste(phi,"=0.9 and ", tau,"=4/9")),
                                    expression(paste(phi,"=0.3 and ", tau,"=4")), expression(paste(phi,"=0.9 and ", tau,"=4")))
levels(tab_MIstat_W$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

#Merged MI Stat over parameter combinations
ggplot(tab_MIstat_W, aes(x = Weight, y = MI_est, group=para.comb, colour = para.comb, linetype=para.comb)) +
  scale_x_continuous(breaks = c(1, 2, 3)) + 
  geom_line(linewidth = 2.5) +
  labs(x = "Model Graph",
       y = "Moran's I Statistic") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 35),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 35),
        legend.key.size = unit(2.5, "cm"),
        legend.key.width = unit(2.5,"cm"), 
        strip.text = element_text(size = 30),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  scale_color_discrete(labels = c(expression(paste("(", phi,"=0.3, ", tau,"=4/9)")), expression(paste("(", phi,"=0.9, ", tau,"=4/9)")),
                                    expression(paste("(", phi,"=0.3, ", tau,"=4)")), expression(paste("(", phi,"=0.9, ", tau,"=4)")))) + 
  scale_linetype_discrete(labels = c(expression(paste("(", phi,"=0.3, ", tau,"=4/9)")), expression(paste("(", phi,"=0.9, ", tau,"=4/9)")),
                                     expression(paste("(", phi,"=0.3, ", tau,"=4)")), expression(paste("(", phi,"=0.9, ", tau,"=4)")))) +
  facet_grid(~Graph, labeller = label_parsed)

########################
# Moran's I p-value plot
########################
# Average Moran's I p-value within samples
tab_MI.pv_W <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each = 12),
                          Weight = rep(c(1:3), 1),
                          para.comb = rep(c(1, 2, 3, 4), each = 3),
                          MI.pv_est = cbind(c(mean(comb1_W1$MI.p.v.W1), mean(comb1_W1$MI.p.v.W2), mean(comb1_W1$MI.p.v.W3),
                                              mean(comb2_W1$MI.p.v.W1), mean(comb2_W1$MI.p.v.W2), mean(comb2_W1$MI.p.v.W3),
                                              mean(comb3_W1$MI.p.v.W1), mean(comb3_W1$MI.p.v.W2), mean(comb3_W1$MI.p.v.W3),
                                              mean(comb4_W1$MI.p.v.W1), mean(comb4_W1$MI.p.v.W2), mean(comb4_W1$MI.p.v.W3),
                                              
                                              mean(comb1_W2$MI.p.v.W1), mean(comb1_W2$MI.p.v.W2), mean(comb1_W2$MI.p.v.W3),
                                              mean(comb2_W2$MI.p.v.W1), mean(comb2_W2$MI.p.v.W2), mean(comb2_W2$MI.p.v.W3),
                                              mean(comb3_W2$MI.p.v.W1), mean(comb3_W2$MI.p.v.W2), mean(comb3_W2$MI.p.v.W3),
                                              mean(comb4_W2$MI.p.v.W1), mean(comb4_W2$MI.p.v.W2), mean(comb4_W2$MI.p.v.W3),
                                              
                                              mean(comb1_W3$MI.p.v.W1), mean(comb1_W3$MI.p.v.W2), mean(comb1_W3$MI.p.v.W3),
                                              mean(comb2_W3$MI.p.v.W1), mean(comb2_W3$MI.p.v.W2), mean(comb2_W3$MI.p.v.W3),
                                              mean(comb3_W3$MI.p.v.W1), mean(comb3_W3$MI.p.v.W2), mean(comb3_W3$MI.p.v.W3),
                                              mean(comb4_W3$MI.p.v.W1), mean(comb4_W3$MI.p.v.W2), mean(comb4_W3$MI.p.v.W3))))
tab_MI.pv_W$para.comb = as.factor(tab_MI.pv_W$para.comb)
tab_MI.pv_W$Graph = as.factor(tab_MI.pv_W$Graph)
levels(tab_MI.pv_W$para.comb) <- c(expression(paste(phi[1], " and ", tau[1])), expression(paste(phi[2], " and ", tau[1])),
                                   expression(paste(phi[1], " and ", tau[2])), expression(paste(phi[2], " and ", tau[2])))
levels(tab_MI.pv_W$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

#Merged MI p-value over parameter combinations
ggplot(tab_MI.pv_W, aes(x = Weight, y = MI.pv_est, group=para.comb, colour = para.comb, linetype=para.comb)) +
  scale_x_continuous(breaks = c(1, 2, 3)) + 
  geom_line(linewidth = 2.5) +
  labs(x = "Model Graph",
       y = "Moran's p-value") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 35),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(2.5, "cm"),
        legend.key.width = unit(2.5,"cm"), 
        strip.text = element_text(size = 30),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  scale_color_discrete(labels = c(expression(paste("(", phi,"=0.3, ", tau,"=4/9)")), expression(paste("(", phi,"=0.9, ", tau,"=4/9)")),
                                  expression(paste("(", phi,"=0.3, ", tau,"=4)")), expression(paste("(", phi,"=0.9, ", tau,"=4)")))) + 
  scale_linetype_discrete(labels = c(expression(paste("(", phi,"=0.3, ", tau,"=4/9)")), expression(paste("(", phi,"=0.9, ", tau,"=4/9)")),
                                     expression(paste("(", phi,"=0.3, ", tau,"=4)")), expression(paste("(", phi,"=0.9, ", tau,"=4)")))) +
  facet_grid(~Graph, labeller = label_parsed)



###############################################################################
# What happens to the model estimates (ie. b0, b1, b2, tau1, tau2, phi1, phi2) 
# and fit statistics (ie. WAICs, DIC, Deviance, MLIK, CPO, Bias, MSE)
# if we only misspecify the spatial graphs without misspecifying the pc priors?
###############################################################################
# Boxplot of fixed estimates
############################
#Beta_0
tab_b0.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each = 12*nrow(comb1_W1)), 
                         para.comb = rep(c(1, 2, 3, 4), each = 3*nrow(comb1_W1)), 
                         Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = nrow(comb1_W1)), 
                         b0_est = cbind(c(comb1_W1$b0.W1.Pcs, comb1_W1$b0.W2.Pcs, comb1_W1$b0.W3.Pcs, 
                                          comb2_W1$b0.W1.Pcs, comb2_W1$b0.W2.Pcs, comb2_W1$b0.W3.Pcs,
                                          comb3_W1$b0.W1.Pcs, comb3_W1$b0.W2.Pcs, comb3_W1$b0.W3.Pcs,
                                          comb4_W1$b0.W1.Pcs, comb4_W1$b0.W2.Pcs, comb4_W1$b0.W3.Pcs,
                                          
                                          comb1_W2$b0.W1.Pcs, comb1_W2$b0.W2.Pcs, comb1_W2$b0.W3.Pcs, 
                                          comb2_W2$b0.W1.Pcs, comb2_W2$b0.W2.Pcs, comb2_W2$b0.W3.Pcs,
                                          comb3_W2$b0.W1.Pcs, comb3_W2$b0.W2.Pcs, comb3_W2$b0.W3.Pcs,
                                          comb4_W2$b0.W1.Pcs, comb4_W2$b0.W2.Pcs, comb4_W2$b0.W3.Pcs,
                                          
                                          comb1_W3$b0.W1.Pcs, comb1_W3$b0.W2.Pcs, comb1_W3$b0.W3.Pcs, 
                                          comb2_W3$b0.W1.Pcs, comb2_W3$b0.W2.Pcs, comb2_W3$b0.W3.Pcs,
                                          comb3_W3$b0.W1.Pcs, comb3_W3$b0.W2.Pcs, comb3_W3$b0.W3.Pcs,
                                          comb4_W3$b0.W1.Pcs, comb4_W3$b0.W2.Pcs, comb4_W3$b0.W3.Pcs)))
tab_b0.Pcs$para.comb = as.factor(tab_b0.Pcs$para.comb)
tab_b0.Pcs$Graph = as.factor(tab_b0.Pcs$Graph)
levels(tab_b0.Pcs$para.comb) <- c(expression(paste(phi,"=0.3 and ", tau,"=4/9")), expression(paste(phi,"=0.9 and ", tau,"=4/9")),
                                  expression(paste(phi,"=0.3 and ", tau,"=4")), expression(paste(phi,"=0.9 and ", tau,"=4")))
levels(tab_b0.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

ggplot(tab_b0.Pcs, aes(Model.Graph, b0_est, colour = Model.Graph)) +
  geom_boxplot(size = 1)  +
  scale_color_manual(breaks = c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"),
                     values = c("brown", "darkgoldenrod", "aquamarine4")) +
  labs(y = expression(beta[0])) +
  geom_hline(aes(yintercept = b0), linetype='dashed', col="red", linewidth = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.5, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  facet_grid(para.comb~Graph, labeller = label_parsed)

#Beta_1
tab_b1.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each = 12*nrow(comb1_W1)), 
                         para.comb = rep(c(1, 2, 3, 4), each = 3*nrow(comb1_W1)), 
                         Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = nrow(comb1_W1)), 
                         b1_est = cbind(c(comb1_W1$b1.W1.Pcs, comb1_W1$b1.W2.Pcs, comb1_W1$b1.W3.Pcs, 
                                          comb2_W1$b1.W1.Pcs, comb2_W1$b1.W2.Pcs, comb2_W1$b1.W3.Pcs,
                                          comb3_W1$b1.W1.Pcs, comb3_W1$b1.W2.Pcs, comb3_W1$b1.W3.Pcs,
                                          comb4_W1$b1.W1.Pcs, comb4_W1$b1.W2.Pcs, comb4_W1$b1.W3.Pcs,
                                          
                                          comb1_W2$b1.W1.Pcs, comb1_W2$b1.W2.Pcs, comb1_W2$b1.W3.Pcs, 
                                          comb2_W2$b1.W1.Pcs, comb2_W2$b1.W2.Pcs, comb2_W2$b1.W3.Pcs,
                                          comb3_W2$b1.W1.Pcs, comb3_W2$b1.W2.Pcs, comb3_W2$b1.W3.Pcs,
                                          comb4_W2$b1.W1.Pcs, comb4_W2$b1.W2.Pcs, comb4_W2$b1.W3.Pcs,
                                          
                                          comb1_W3$b1.W1.Pcs, comb1_W3$b1.W2.Pcs, comb1_W3$b1.W3.Pcs, 
                                          comb2_W3$b1.W1.Pcs, comb2_W3$b1.W2.Pcs, comb2_W3$b1.W3.Pcs,
                                          comb3_W3$b1.W1.Pcs, comb3_W3$b1.W2.Pcs, comb3_W3$b1.W3.Pcs,
                                          comb4_W3$b1.W1.Pcs, comb4_W3$b1.W2.Pcs, comb4_W3$b1.W3.Pcs)))
tab_b1.Pcs$para.comb = as.factor(tab_b1.Pcs$para.comb)
tab_b1.Pcs$Graph = as.factor(tab_b1.Pcs$Graph)
levels(tab_b1.Pcs$para.comb) <- c(expression(paste(phi,"=0.3 and ", tau,"=4/9")), expression(paste(phi,"=0.9 and ", tau,"=4/9")),
                                  expression(paste(phi,"=0.3 and ", tau,"=4")), expression(paste(phi,"=0.9 and ", tau,"=4")))
levels(tab_b1.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

ggplot(tab_b1.Pcs, aes(Model.Graph, b1_est, colour = Model.Graph)) +
  geom_boxplot(size = 1)  +
  scale_color_manual(breaks = c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"),
                     values = c("brown", "darkgoldenrod", "aquamarine4")) +
  labs(y = expression(beta[1])) +
  geom_hline(aes(yintercept = b1), linetype='dashed', col="red", linewidth = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.5, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  facet_grid(para.comb~Graph, labeller = label_parsed)

#Beta_2
tab_b2.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each = 12*nrow(comb1_W1)), 
                         para.comb = rep(c(1, 2, 3, 4), each = 3*nrow(comb1_W1)), 
                         Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = nrow(comb1_W1)), 
                         b2_est = cbind(c(comb1_W1$b2.W1.Pcs, comb1_W1$b2.W2.Pcs, comb1_W1$b2.W3.Pcs, 
                                          comb2_W1$b2.W1.Pcs, comb2_W1$b2.W2.Pcs, comb2_W1$b2.W3.Pcs,
                                          comb3_W1$b2.W1.Pcs, comb3_W1$b2.W2.Pcs, comb3_W1$b2.W3.Pcs,
                                          comb4_W1$b2.W1.Pcs, comb4_W1$b2.W2.Pcs, comb4_W1$b2.W3.Pcs,
                                          
                                          comb1_W2$b2.W1.Pcs, comb1_W2$b2.W2.Pcs, comb1_W2$b2.W3.Pcs, 
                                          comb2_W2$b2.W1.Pcs, comb2_W2$b2.W2.Pcs, comb2_W2$b2.W3.Pcs,
                                          comb3_W2$b2.W1.Pcs, comb3_W2$b2.W2.Pcs, comb3_W2$b2.W3.Pcs,
                                          comb4_W2$b2.W1.Pcs, comb4_W2$b2.W2.Pcs, comb4_W2$b2.W3.Pcs,
                                          
                                          comb1_W3$b2.W1.Pcs, comb1_W3$b2.W2.Pcs, comb1_W3$b2.W3.Pcs, 
                                          comb2_W3$b2.W1.Pcs, comb2_W3$b2.W2.Pcs, comb2_W3$b2.W3.Pcs,
                                          comb3_W3$b2.W1.Pcs, comb3_W3$b2.W2.Pcs, comb3_W3$b2.W3.Pcs,
                                          comb4_W3$b2.W1.Pcs, comb4_W3$b2.W2.Pcs, comb4_W3$b2.W3.Pcs)))
tab_b2.Pcs$para.comb = as.factor(tab_b2.Pcs$para.comb)
tab_b2.Pcs$Graph = as.factor(tab_b2.Pcs$Graph)
levels(tab_b2.Pcs$para.comb) <- c(expression(paste(phi,"=0.3 and ", tau,"=4/9")), expression(paste(phi,"=0.9 and ", tau,"=4/9")),
                                  expression(paste(phi,"=0.3 and ", tau,"=4")), expression(paste(phi,"=0.9 and ", tau,"=4")))
levels(tab_b2.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

ggplot(tab_b2.Pcs, aes(Model.Graph, b2_est, colour = Model.Graph)) +
  geom_boxplot(size = 1)  +
  scale_color_manual(breaks = c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"),
                     values = c("brown", "darkgoldenrod", "aquamarine4")) +
  labs(y = expression(beta[2])) +
  geom_hline(aes(yintercept = b2), linetype='dashed', col="red", linewidth = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.5, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  facet_grid(para.comb~Graph, labeller = label_parsed)

################
#Hyperparameters
################
#tau1
tab_tau1.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6*nrow(comb2_W1)), 
                           para.comb = rep(c(1, 2), each = 3*nrow(comb2_W1)), 
                           Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = nrow(comb2_W1)), 
                           tau_est = cbind(c(comb1_W1$tau.W1.Pcs, comb1_W1$tau.W2.Pcs, comb1_W1$tau.W3.Pcs,
                                             comb2_W1$tau.W1.Pcs, comb2_W1$tau.W2.Pcs, comb2_W1$tau.W3.Pcs,
                                             
                                             comb1_W2$tau.W1.Pcs, comb1_W2$tau.W2.Pcs, comb1_W2$tau.W3.Pcs,
                                             comb2_W2$tau.W1.Pcs, comb2_W2$tau.W2.Pcs, comb2_W2$tau.W3.Pcs,
                                             
                                             comb1_W3$tau.W1.Pcs, comb1_W3$tau.W2.Pcs, comb1_W3$tau.W3.Pcs, 
                                             comb2_W3$tau.W1.Pcs, comb2_W3$tau.W2.Pcs, comb2_W3$tau.W3.Pcs)))
tab_tau1.Pcs$para.comb = as.factor(tab_tau1.Pcs$para.comb)
tab_tau1.Pcs$Graph = as.factor(tab_tau1.Pcs$Graph)
levels(tab_tau1.Pcs$para.comb) <- c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))
levels(tab_tau1.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

ggplot(tab_tau1.Pcs, aes(Model.Graph, tau_est, colour = Model.Graph)) +
  geom_boxplot(size = 1)  +
  scale_color_manual(breaks = c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"),
                     values = c("brown", "darkgoldenrod", "aquamarine4")) +
  labs(y = expression(tau[1])) +
  geom_hline(aes(yintercept = tau1), linetype='dashed', col="red", linewidth = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.5, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  facet_grid(para.comb~Graph, labeller = label_parsed)

#tau2
tab_tau2.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6*nrow(comb4_W1)), 
                           para.comb = rep(c(1, 2), each = 3*nrow(comb4_W1)), 
                           Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = nrow(comb4_W1)), 
                           tau_est = cbind(c(comb3_W1$tau.W1.Pcs, comb3_W1$tau.W2.Pcs, comb3_W1$tau.W3.Pcs,
                                             comb4_W1$tau.W1.Pcs, comb4_W1$tau.W2.Pcs, comb4_W1$tau.W3.Pcs,
                                             
                                             comb3_W2$tau.W1.Pcs, comb3_W2$tau.W2.Pcs, comb3_W2$tau.W3.Pcs,
                                             comb4_W2$tau.W1.Pcs, comb4_W2$tau.W2.Pcs, comb4_W2$tau.W3.Pcs,
                                             
                                             comb3_W3$tau.W1.Pcs, comb3_W3$tau.W2.Pcs, comb3_W3$tau.W3.Pcs, 
                                             comb4_W3$tau.W1.Pcs, comb4_W3$tau.W2.Pcs, comb4_W3$tau.W3.Pcs)))
tab_tau2.Pcs$para.comb = as.factor(tab_tau2.Pcs$para.comb)
tab_tau2.Pcs$Graph = as.factor(tab_tau2.Pcs$Graph)
levels(tab_tau2.Pcs$para.comb) <- c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))
levels(tab_tau2.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

ggplot(tab_tau2.Pcs, aes(Model.Graph, tau_est, colour = Model.Graph)) +
  geom_boxplot(size = 1)  +
  scale_color_manual(breaks = c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"),
                     values = c("brown", "darkgoldenrod", "aquamarine4")) +
  labs(y = expression(tau[2])) +
  geom_hline(aes(yintercept = tau2), linetype='dashed', col="red", linewidth = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.5, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  facet_grid(para.comb~Graph, labeller = label_parsed)

#phi1
tab_phi1.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6*nrow(comb2_W1)), 
                           para.comb = rep(c(1, 2), each = 3*nrow(comb2_W1)), 
                           Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = nrow(comb2_W1)), 
                           phi_est = cbind(c(comb1_W1$phi.W1.Pcs, comb1_W1$phi.W2.Pcs, comb1_W1$phi.W3.Pcs,
                                             comb3_W1$phi.W1.Pcs, comb3_W1$phi.W2.Pcs, comb3_W1$phi.W3.Pcs,
                                             
                                             comb1_W2$phi.W1.Pcs, comb1_W2$phi.W2.Pcs, comb1_W2$phi.W3.Pcs,
                                             comb3_W2$phi.W1.Pcs, comb3_W2$phi.W2.Pcs, comb3_W2$phi.W3.Pcs,
                                             
                                             comb1_W3$phi.W1.Pcs, comb1_W3$phi.W2.Pcs, comb1_W3$phi.W3.Pcs, 
                                             comb3_W3$phi.W1.Pcs, comb3_W3$phi.W2.Pcs, comb3_W3$phi.W3.Pcs)))
tab_phi1.Pcs$para.comb = as.factor(tab_phi1.Pcs$para.comb)
tab_phi1.Pcs$Graph = as.factor(tab_phi1.Pcs$Graph)
levels(tab_phi1.Pcs$para.comb) <- c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))
levels(tab_phi1.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

ggplot(tab_phi1.Pcs, aes(Model.Graph, phi_est, colour = Model.Graph)) +
  geom_boxplot(size = 1)  +
  scale_color_manual(breaks = c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"),
                     values = c("brown", "darkgoldenrod", "aquamarine4")) +
  labs(y = expression(phi[1])) +
  geom_hline(aes(yintercept = phi1), linetype='dashed', col="red", linewidth = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.5, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  facet_grid(para.comb~Graph, labeller = label_parsed)

#phi2
tab_phi2.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6*nrow(comb2_W1)), 
                           para.comb = rep(c(1, 2), each = 3*nrow(comb2_W1)), 
                           Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = nrow(comb2_W1)), 
                           phi_est = cbind(c(comb2_W1$phi.W1.Pcs, comb2_W1$phi.W2.Pcs, comb2_W1$phi.W3.Pcs,
                                             comb4_W1$phi.W1.Pcs, comb4_W1$phi.W2.Pcs, comb4_W1$phi.W3.Pcs,
                                             
                                             comb2_W2$phi.W1.Pcs, comb2_W2$phi.W2.Pcs, comb2_W2$phi.W3.Pcs,
                                             comb4_W2$phi.W1.Pcs, comb4_W2$phi.W2.Pcs, comb4_W2$phi.W3.Pcs,
                                             
                                             comb2_W3$phi.W1.Pcs, comb2_W3$phi.W2.Pcs, comb2_W3$phi.W3.Pcs, 
                                             comb4_W3$phi.W1.Pcs, comb4_W3$phi.W2.Pcs, comb4_W3$phi.W3.Pcs)))
tab_phi2.Pcs$para.comb = as.factor(tab_phi2.Pcs$para.comb)
tab_phi2.Pcs$Graph = as.factor(tab_phi2.Pcs$Graph)
levels(tab_phi2.Pcs$para.comb) <- c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))
levels(tab_phi2.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

ggplot(tab_phi2.Pcs, aes(Model.Graph, phi_est, colour = Model.Graph)) +
  geom_boxplot(size = 1)  +
  scale_color_manual(breaks = c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"),
                     values = c("brown", "darkgoldenrod", "aquamarine4")) +
  labs(y = expression(phi[2])) +
  geom_hline(aes(yintercept = phi2), linetype='dashed', col="red", linewidth = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.5, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  facet_grid(para.comb~Graph, labeller = label_parsed)

# Fit estimates
# WAIC
tab_WAIC.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each = 12*nrow(comb2_W1)),
                           sample = rep(c(comb1_W1$sample, comb2_W1$sample, comb3_W1$sample, comb4_W1$sample, 
                                          comb1_W2$sample, comb2_W2$sample, comb3_W2$sample, comb4_W2$sample,
                                          comb1_W3$sample, comb2_W3$sample, comb3_W3$sample, comb4_W3$sample), 3),
                           para.comb = rep(c(1, 2, 3, 4), each = 3*nrow(comb2_W1)), 
                           Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = nrow(comb2_W1)),
                           WAIC_est = cbind(c(comb1_W1$WAIC.W1.Pcs, comb1_W1$WAIC.W2.Pcs, comb1_W1$WAIC.W3.Pcs,
                                              comb2_W1$WAIC.W1.Pcs, comb2_W1$WAIC.W2.Pcs, comb2_W1$WAIC.W3.Pcs,
                                              comb3_W1$WAIC.W1.Pcs, comb3_W1$WAIC.W2.Pcs, comb3_W1$WAIC.W3.Pcs,
                                              comb4_W1$WAIC.W1.Pcs, comb4_W1$WAIC.W2.Pcs, comb4_W1$WAIC.W3.Pcs,
                                              
                                              comb1_W2$WAIC.W1.Pcs, comb1_W2$WAIC.W2.Pcs, comb1_W2$WAIC.W3.Pcs,
                                              comb2_W2$WAIC.W1.Pcs, comb2_W2$WAIC.W2.Pcs, comb2_W2$WAIC.W3.Pcs,
                                              comb3_W2$WAIC.W1.Pcs, comb3_W2$WAIC.W2.Pcs, comb3_W2$WAIC.W3.Pcs,
                                              comb4_W2$WAIC.W1.Pcs, comb4_W2$WAIC.W2.Pcs, comb4_W2$WAIC.W3.Pcs,
                                              
                                              comb1_W3$WAIC.W1.Pcs, comb1_W3$WAIC.W2.Pcs, comb1_W3$WAIC.W3.Pcs,
                                              comb2_W3$WAIC.W1.Pcs, comb2_W3$WAIC.W2.Pcs, comb2_W3$WAIC.W3.Pcs,
                                              comb3_W3$WAIC.W1.Pcs, comb3_W3$WAIC.W2.Pcs, comb3_W3$WAIC.W3.Pcs,
                                              comb4_W3$WAIC.W1.Pcs, comb4_W3$WAIC.W2.Pcs, comb4_W3$WAIC.W3.Pcs)))
tab_WAIC.Pcs$para.comb = as.factor(tab_WAIC.Pcs$para.comb)
tab_WAIC.Pcs$Graph = as.factor(tab_WAIC.Pcs$Graph)
levels(tab_WAIC.Pcs$para.comb) <- c(expression(paste(phi,"=0.3 and ", tau,"=4/9")), expression(paste(phi,"=0.9 and ", tau,"=4/9")),
                                    expression(paste(phi,"=0.3 and ", tau,"=4")), expression(paste(phi,"=0.9 and ", tau,"=4")))
levels(tab_WAIC.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

ggplot(tab_WAIC.Pcs, aes(sample, WAIC_est, colour = Model.Graph)) +
  geom_line(size = 0.5) +
  scale_color_manual(breaks = c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"),
                     values = c("brown", "darkgoldenrod", "aquamarine4")) +
  labs(y = "WAIC") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.5, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  facet_grid(para.comb~Graph, labeller = label_parsed, scales = "free")

# DIC
tab_DIC.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each = 12*nrow(comb2_W1)),
                          sample = rep(c(comb1_W1$sample, comb2_W1$sample, comb3_W1$sample, comb4_W1$sample, 
                                         comb1_W2$sample, comb2_W2$sample, comb3_W2$sample, comb4_W2$sample,
                                         comb1_W3$sample, comb2_W3$sample, comb3_W3$sample, comb4_W3$sample), 3),
                          para.comb = rep(c(1, 2, 3, 4), each = 3*nrow(comb2_W1)), 
                          Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = nrow(comb2_W1)),
                          DIC_est = cbind(c(comb1_W1$DIC.W1.Pcs, comb1_W1$DIC.W2.Pcs, comb1_W1$DIC.W3.Pcs,
                                            comb2_W1$DIC.W1.Pcs, comb2_W1$DIC.W2.Pcs, comb2_W1$DIC.W3.Pcs,
                                            comb3_W1$DIC.W1.Pcs, comb3_W1$DIC.W2.Pcs, comb3_W1$DIC.W3.Pcs,
                                            comb4_W1$DIC.W1.Pcs, comb4_W1$DIC.W2.Pcs, comb4_W1$DIC.W3.Pcs,
                                            
                                            comb1_W2$DIC.W1.Pcs, comb1_W2$DIC.W2.Pcs, comb1_W2$DIC.W3.Pcs,
                                            comb2_W2$DIC.W1.Pcs, comb2_W2$DIC.W2.Pcs, comb2_W2$DIC.W3.Pcs,
                                            comb3_W2$DIC.W1.Pcs, comb3_W2$DIC.W2.Pcs, comb3_W2$DIC.W3.Pcs,
                                            comb4_W2$DIC.W1.Pcs, comb4_W2$DIC.W2.Pcs, comb4_W2$DIC.W3.Pcs,
                                            
                                            comb1_W3$DIC.W1.Pcs, comb1_W3$DIC.W2.Pcs, comb1_W3$DIC.W3.Pcs,
                                            comb2_W3$DIC.W1.Pcs, comb2_W3$DIC.W2.Pcs, comb2_W3$DIC.W3.Pcs,
                                            comb3_W3$DIC.W1.Pcs, comb3_W3$DIC.W2.Pcs, comb3_W3$DIC.W3.Pcs,
                                            comb4_W3$DIC.W1.Pcs, comb4_W3$DIC.W2.Pcs, comb4_W3$DIC.W3.Pcs)))
tab_DIC.Pcs$para.comb = as.factor(tab_DIC.Pcs$para.comb)
tab_DIC.Pcs$Graph = as.factor(tab_DIC.Pcs$Graph)
levels(tab_DIC.Pcs$para.comb) <- c(expression(paste(phi,"=0.3 and ", tau,"=4/9")), expression(paste(phi,"=0.9 and ", tau,"=4/9")),
                                   expression(paste(phi,"=0.3 and ", tau,"=4")), expression(paste(phi,"=0.9 and ", tau,"=4")))
levels(tab_DIC.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

ggplot(tab_DIC.Pcs, aes(sample, DIC_est, colour = Model.Graph)) +
  geom_line(size = 0.5) +
  scale_color_manual(breaks = c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"),
                     values = c("brown", "darkgoldenrod", "aquamarine4")) +
  labs(y = "DIC") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.5, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  facet_grid(para.comb~Graph, labeller = label_parsed, scales = "free")

# CPO
tab_CPO.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each = 12*nrow(comb2_W1)),
                          sample = rep(c(comb1_W1$sample, comb2_W1$sample, comb3_W1$sample, comb4_W1$sample, 
                                         comb1_W2$sample, comb2_W2$sample, comb3_W2$sample, comb4_W2$sample,
                                         comb1_W3$sample, comb2_W3$sample, comb3_W3$sample, comb4_W3$sample), 3),
                          para.comb = rep(c(1, 2, 3, 4), each = 3*nrow(comb2_W1)), 
                          Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = nrow(comb2_W1)),
                          CPO_est = cbind(c(comb1_W1$CPO.W1.Pcs, comb1_W1$CPO.W2.Pcs, comb1_W1$CPO.W3.Pcs,
                                            comb2_W1$CPO.W1.Pcs, comb2_W1$CPO.W2.Pcs, comb2_W1$CPO.W3.Pcs,
                                            comb3_W1$CPO.W1.Pcs, comb3_W1$CPO.W2.Pcs, comb3_W1$CPO.W3.Pcs,
                                            comb4_W1$CPO.W1.Pcs, comb4_W1$CPO.W2.Pcs, comb4_W1$CPO.W3.Pcs,
                                            
                                            comb1_W2$CPO.W1.Pcs, comb1_W2$CPO.W2.Pcs, comb1_W2$CPO.W3.Pcs,
                                            comb2_W2$CPO.W1.Pcs, comb2_W2$CPO.W2.Pcs, comb2_W2$CPO.W3.Pcs,
                                            comb3_W2$CPO.W1.Pcs, comb3_W2$CPO.W2.Pcs, comb3_W2$CPO.W3.Pcs,
                                            comb4_W2$CPO.W1.Pcs, comb4_W2$CPO.W2.Pcs, comb4_W2$CPO.W3.Pcs,
                                            
                                            comb1_W3$CPO.W1.Pcs, comb1_W3$CPO.W2.Pcs, comb1_W3$CPO.W3.Pcs,
                                            comb2_W3$CPO.W1.Pcs, comb2_W3$CPO.W2.Pcs, comb2_W3$CPO.W3.Pcs,
                                            comb3_W3$CPO.W1.Pcs, comb3_W3$CPO.W2.Pcs, comb3_W3$CPO.W3.Pcs,
                                            comb4_W3$CPO.W1.Pcs, comb4_W3$CPO.W2.Pcs, comb4_W3$CPO.W3.Pcs)))
tab_CPO.Pcs$para.comb = as.factor(tab_CPO.Pcs$para.comb)
tab_CPO.Pcs$Graph = as.factor(tab_CPO.Pcs$Graph)
levels(tab_CPO.Pcs$para.comb) <- c(expression(paste(phi,"=0.3 and ", tau,"=4/9")), expression(paste(phi,"=0.9 and ", tau,"=4/9")),
                                   expression(paste(phi,"=0.3 and ", tau,"=4")), expression(paste(phi,"=0.9 and ", tau,"=4")))
levels(tab_CPO.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

ggplot(tab_CPO.Pcs, aes(sample, CPO_est, colour = Model.Graph)) +
  geom_line(size = 0.5) +
  scale_color_manual(breaks = c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"),
                     values = c("brown", "darkgoldenrod", "aquamarine4")) +
  labs(y = "CPO") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.5, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  facet_grid(para.comb~Graph, labeller = label_parsed, scales = "free")

# MLIK
tab_MLIK.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each = 12*nrow(comb2_W1)),
                           sample = rep(c(comb1_W1$sample, comb2_W1$sample, comb3_W1$sample, comb4_W1$sample, 
                                          comb1_W2$sample, comb2_W2$sample, comb3_W2$sample, comb4_W2$sample,
                                          comb1_W3$sample, comb2_W3$sample, comb3_W3$sample, comb4_W3$sample), 3),
                           para.comb = rep(c(1, 2, 3, 4), each = 3*nrow(comb2_W1)), 
                           Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = nrow(comb2_W1)),
                           MLIK_est = cbind(c(comb1_W1$MLIK.W1.Pcs, comb1_W1$MLIK.W2.Pcs, comb1_W1$MLIK.W3.Pcs,
                                              comb2_W1$MLIK.W1.Pcs, comb2_W1$MLIK.W2.Pcs, comb2_W1$MLIK.W3.Pcs,
                                              comb3_W1$MLIK.W1.Pcs, comb3_W1$MLIK.W2.Pcs, comb3_W1$MLIK.W3.Pcs,
                                              comb4_W1$MLIK.W1.Pcs, comb4_W1$MLIK.W2.Pcs, comb4_W1$MLIK.W3.Pcs,
                                              
                                              comb1_W2$MLIK.W1.Pcs, comb1_W2$MLIK.W2.Pcs, comb1_W2$MLIK.W3.Pcs,
                                              comb2_W2$MLIK.W1.Pcs, comb2_W2$MLIK.W2.Pcs, comb2_W2$MLIK.W3.Pcs,
                                              comb3_W2$MLIK.W1.Pcs, comb3_W2$MLIK.W2.Pcs, comb3_W2$MLIK.W3.Pcs,
                                              comb4_W2$MLIK.W1.Pcs, comb4_W2$MLIK.W2.Pcs, comb4_W2$MLIK.W3.Pcs,
                                              
                                              comb1_W3$MLIK.W1.Pcs, comb1_W3$MLIK.W2.Pcs, comb1_W3$MLIK.W3.Pcs,
                                              comb2_W3$MLIK.W1.Pcs, comb2_W3$MLIK.W2.Pcs, comb2_W3$MLIK.W3.Pcs,
                                              comb3_W3$MLIK.W1.Pcs, comb3_W3$MLIK.W2.Pcs, comb3_W3$MLIK.W3.Pcs,
                                              comb4_W3$MLIK.W1.Pcs, comb4_W3$MLIK.W2.Pcs, comb4_W3$MLIK.W3.Pcs)))
tab_MLIK.Pcs$para.comb = as.factor(tab_MLIK.Pcs$para.comb)
tab_MLIK.Pcs$Graph = as.factor(tab_MLIK.Pcs$Graph)
levels(tab_MLIK.Pcs$para.comb) <- c(expression(paste(phi,"=0.3 and ", tau,"=4/9")), expression(paste(phi,"=0.9 and ", tau,"=4/9")),
                                    expression(paste(phi,"=0.3 and ", tau,"=4")), expression(paste(phi,"=0.9 and ", tau,"=4")))
levels(tab_MLIK.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

ggplot(tab_MLIK.Pcs, aes(sample, MLIK_est, colour = Model.Graph)) +
  geom_line(size = 0.5) +
  scale_color_manual(breaks = c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"),
                     values = c("brown", "darkgoldenrod", "aquamarine4")) +
  labs(y = "MLIK") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 25),
        plot.subtitle = element_text(hjust=0.5, size = 30),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        axis.ticks.length = unit(0.5, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) +
  facet_grid(para.comb~Graph, labeller = label_parsed, scales = "free")

#Bias of hyperparameters
#Bias of tau1
tab_Bias_tau1.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6),
                                Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = 1),
                                Graph.id = rep(c(1:3), 1),
                                para.comb = rep(c(1, 2), each = 3), 
                                Bias_tau1_est = cbind(abs(c(mean(comb1_W1$tau.W1.Pcs)-tau1, mean(comb1_W1$tau.W2.Pcs)-tau1, mean(comb1_W1$tau.W3.Pcs)-tau1,
                                                            mean(comb2_W1$tau.W1.Pcs)-tau1, mean(comb2_W1$tau.W2.Pcs)-tau1, mean(comb2_W1$tau.W3.Pcs)-tau1,
                                                            
                                                            mean(comb1_W2$tau.W1.Pcs)-tau1, mean(comb1_W2$tau.W2.Pcs)-tau1, mean(comb1_W2$tau.W3.Pcs)-tau1,
                                                            mean(comb2_W2$tau.W1.Pcs)-tau1, mean(comb2_W2$tau.W2.Pcs)-tau1, mean(comb2_W2$tau.W3.Pcs)-tau1,
                                                            
                                                            mean(comb1_W3$tau.W1.Pcs)-tau1, mean(comb1_W3$tau.W2.Pcs)-tau1, mean(comb1_W3$tau.W3.Pcs)-tau1,
                                                            mean(comb2_W3$tau.W1.Pcs)-tau1, mean(comb2_W3$tau.W2.Pcs)-tau1, mean(comb2_W3$tau.W3.Pcs)-tau1))))
tab_Bias_tau1.Pcs$para.comb = as.factor(tab_Bias_tau1.Pcs$para.comb)
tab_Bias_tau1.Pcs$Graph = as.factor(tab_Bias_tau1.Pcs$Graph)
levels(tab_Bias_tau1.Pcs$para.comb) <- c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))
levels(tab_Bias_tau1.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

#Merged bias of tau1 over the different hyperparameter combinations
p1 <- ggplot(tab_Bias_tau1.Pcs, aes(Graph.id, Bias_tau1_est, group=para.comb, colour = para.comb, linetype=para.comb)) +
  scale_x_continuous(labels = c("1", "2", "3"), breaks = c(1, 2, 3)) +
  geom_line(size = 1)  +
  labs(y = expression(tau["1 |bias|"]),
       x = "Model Graph") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) + 
  scale_color_discrete(labels = c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))) + 
  scale_linetype_discrete(labels = c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))) + 
  facet_grid(~Graph, labeller = label_parsed, scales = "free")

#Bias of tau2
tab_Bias_tau2.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6),
                                Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = 1),
                                Graph.id = rep(c(1:3), 1),
                                para.comb = rep(c(1, 2), each = 3), 
                                Bias_tau2_est = cbind(abs(c(mean(comb3_W1$tau.W1.Pcs)-tau2, mean(comb3_W1$tau.W2.Pcs)-tau2, mean(comb3_W1$tau.W3.Pcs)-tau2,
                                                            mean(comb4_W1$tau.W1.Pcs)-tau2, mean(comb4_W1$tau.W2.Pcs)-tau2, mean(comb4_W1$tau.W3.Pcs)-tau2,
                                                            
                                                            mean(comb3_W2$tau.W1.Pcs)-tau2, mean(comb3_W2$tau.W2.Pcs)-tau2, mean(comb3_W2$tau.W3.Pcs)-tau2,
                                                            mean(comb4_W2$tau.W1.Pcs)-tau2, mean(comb4_W2$tau.W2.Pcs)-tau2, mean(comb4_W2$tau.W3.Pcs)-tau2,
                                                            
                                                            mean(comb3_W3$tau.W1.Pcs)-tau2, mean(comb3_W3$tau.W2.Pcs)-tau2, mean(comb3_W3$tau.W3.Pcs)-tau2,
                                                            mean(comb4_W3$tau.W1.Pcs)-tau2, mean(comb4_W3$tau.W2.Pcs)-tau2, mean(comb4_W3$tau.W3.Pcs)-tau2))))
tab_Bias_tau2.Pcs$para.comb = as.factor(tab_Bias_tau2.Pcs$para.comb)
tab_Bias_tau2.Pcs$Graph = as.factor(tab_Bias_tau2.Pcs$Graph)
levels(tab_Bias_tau2.Pcs$para.comb) <- c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))
levels(tab_Bias_tau2.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

#Merged bias of tau2 over the different hyperparameter combinations
p2 <- ggplot(tab_Bias_tau2.Pcs, aes(Graph.id, Bias_tau2_est, group=para.comb, colour = para.comb, linetype=para.comb)) +
  scale_x_continuous(labels = c("1", "2", "3"), breaks = c(1, 2, 3)) +
  geom_line(size = 1)  +
  labs(y = expression(tau["2 |bias|"]),
       x = "Model Graph") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) + 
  scale_color_discrete(labels = c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))) + 
  scale_linetype_discrete(labels = c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))) + 
  facet_grid(~Graph, labeller = label_parsed, scales = "free")

#Bias of phi1
tab_Bias_phi1.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6),
                                Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = 1),
                                Graph.id = rep(c(1:3), 1),
                                para.comb = rep(c(1, 2), each = 3), 
                                Bias_phi1_est = cbind(abs(c(mean(comb1_W1$phi.W1.Pcs)-phi1, mean(comb1_W1$phi.W2.Pcs)-phi1, mean(comb1_W1$phi.W3.Pcs)-phi1,
                                                            mean(comb3_W1$phi.W1.Pcs)-phi1, mean(comb3_W1$phi.W2.Pcs)-phi1, mean(comb3_W1$phi.W3.Pcs)-phi1,
                                                            
                                                            mean(comb1_W2$phi.W1.Pcs)-phi1, mean(comb1_W2$phi.W2.Pcs)-phi1, mean(comb1_W2$phi.W3.Pcs)-phi1,
                                                            mean(comb3_W2$phi.W1.Pcs)-phi1, mean(comb3_W2$phi.W2.Pcs)-phi1, mean(comb3_W2$phi.W3.Pcs)-phi1,
                                                            
                                                            mean(comb1_W3$phi.W1.Pcs)-phi1, mean(comb1_W3$phi.W2.Pcs)-phi1, mean(comb1_W3$phi.W3.Pcs)-phi1,
                                                            mean(comb3_W3$phi.W1.Pcs)-phi1, mean(comb3_W3$phi.W2.Pcs)-phi1, mean(comb3_W3$phi.W3.Pcs)-phi1))))
tab_Bias_phi1.Pcs$para.comb = as.factor(tab_Bias_phi1.Pcs$para.comb)
tab_Bias_phi1.Pcs$Graph = as.factor(tab_Bias_phi1.Pcs$Graph)
levels(tab_Bias_phi1.Pcs$para.comb) <- c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))
levels(tab_Bias_phi1.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

#Merged bias of phi1 over the different hyperparameter combinations
p3 <- ggplot(tab_Bias_phi1.Pcs, aes(Graph.id, Bias_phi1_est, group=para.comb, colour = para.comb, linetype=para.comb)) +
  scale_x_continuous(labels = c("1", "2", "3"), breaks = c(1, 2, 3)) +
  geom_line(size = 1)  +
  labs(y = expression(phi["1 |bias|"]),
       x = "Model Graph") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) + 
  scale_color_discrete(labels = c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))) + 
  scale_linetype_discrete(labels = c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))) + 
  facet_grid(~Graph, labeller = label_parsed, scales = "free")

#Bias of phi2
tab_Bias_phi2.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6),
                                Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = 1),
                                Graph.id = rep(c(1:3), 1),
                                para.comb = rep(c(1, 2), each = 3), 
                                Bias_phi2_est = cbind(abs(c(mean(comb2_W1$phi.W1.Pcs)-phi2, mean(comb2_W1$phi.W2.Pcs)-phi2, mean(comb2_W1$phi.W3.Pcs)-phi2,
                                                            mean(comb4_W1$phi.W1.Pcs)-phi2, mean(comb4_W1$phi.W2.Pcs)-phi2, mean(comb4_W1$phi.W3.Pcs)-phi2,
                                                            
                                                            mean(comb2_W2$phi.W1.Pcs)-phi2, mean(comb2_W2$phi.W2.Pcs)-phi2, mean(comb2_W2$phi.W3.Pcs)-phi2,
                                                            mean(comb4_W2$phi.W1.Pcs)-phi2, mean(comb4_W2$phi.W2.Pcs)-phi2, mean(comb4_W2$phi.W3.Pcs)-phi2,
                                                            
                                                            mean(comb2_W3$phi.W1.Pcs)-phi2, mean(comb2_W3$phi.W2.Pcs)-phi2, mean(comb2_W3$phi.W3.Pcs)-phi2,
                                                            mean(comb4_W3$phi.W1.Pcs)-phi2, mean(comb4_W3$phi.W2.Pcs)-phi2, mean(comb4_W3$phi.W3.Pcs)-phi2))))
tab_Bias_phi2.Pcs$para.comb = as.factor(tab_Bias_phi2.Pcs$para.comb)
tab_Bias_phi2.Pcs$Graph = as.factor(tab_Bias_phi2.Pcs$Graph)
levels(tab_Bias_phi2.Pcs$para.comb) <- c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))
levels(tab_Bias_phi2.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

#Merged bias of phi2 over the different hyperparameter combinations
p4 <- ggplot(tab_Bias_phi2.Pcs, aes(Graph.id, Bias_phi2_est, group=para.comb, colour = para.comb, linetype=para.comb)) +
  scale_x_continuous(labels = c("1", "2", "3"), breaks = c(1, 2, 3)) +
  geom_line(size = 1)  +
  labs(y = expression(phi["2 |bias|"]),
       x = "Model Graph") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) + 
  scale_color_discrete(labels = c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))) + 
  scale_linetype_discrete(labels = c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))) + 
  facet_grid(~Graph, labeller = label_parsed, scales = "free")

plot_grid(p1, p2, p3, p4, nrow = 2)

#MSE of hyperparameters
#MSE of tau1
tab_MSE_tau1.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6),
                               Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = 1),
                               Graph.id = rep(c(1:3), 1),
                               para.comb = rep(c(1, 2), each = 3), 
                               MSE_tau1_est = cbind(c(mean((comb1_W1$tau.W1.Pcs-tau1)^2, na.rm=TRUE), mean((comb1_W1$tau.W2.Pcs-tau1)^2, na.rm=TRUE), mean((comb1_W1$tau.W3.Pcs-tau1)^2, na.rm=TRUE),
                                                      mean((comb2_W1$tau.W1.Pcs-tau1)^2, na.rm=TRUE), mean((comb2_W1$tau.W2.Pcs-tau1)^2, na.rm=TRUE), mean((comb2_W1$tau.W3.Pcs-tau1)^2, na.rm=TRUE),
                                                      
                                                      mean((comb1_W2$tau.W1.Pcs-tau1)^2, na.rm=TRUE), mean((comb1_W2$tau.W2.Pcs-tau1)^2, na.rm=TRUE), mean((comb1_W2$tau.W3.Pcs-tau1)^2, na.rm=TRUE),
                                                      mean((comb2_W2$tau.W1.Pcs-tau1)^2, na.rm=TRUE), mean((comb2_W2$tau.W2.Pcs-tau1)^2, na.rm=TRUE), mean((comb2_W2$tau.W3.Pcs-tau1)^2, na.rm=TRUE),
                                                      
                                                      mean((comb1_W3$tau.W1.Pcs-tau1)^2, na.rm=TRUE), mean((comb1_W3$tau.W2.Pcs-tau1)^2, na.rm=TRUE), mean((comb1_W3$tau.W3.Pcs-tau1)^2, na.rm=TRUE),
                                                      mean((comb2_W3$tau.W1.Pcs-tau1)^2, na.rm=TRUE), mean((comb2_W3$tau.W2.Pcs-tau1)^2, na.rm=TRUE), mean((comb2_W3$tau.W3.Pcs-tau1)^2, na.rm=TRUE))))
tab_MSE_tau1.Pcs$para.comb = as.factor(tab_MSE_tau1.Pcs$para.comb)
tab_MSE_tau1.Pcs$Graph = as.factor(tab_MSE_tau1.Pcs$Graph)
levels(tab_MSE_tau1.Pcs$para.comb) <- c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))
levels(tab_MSE_tau1.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

#Merged MSE of tau1 over the different hyperparameter combinations
p1 <- ggplot(tab_MSE_tau1.Pcs, aes(Graph.id, MSE_tau1_est, group=para.comb, colour = para.comb, linetype=para.comb)) +
  scale_x_continuous(labels = c("1", "2", "3"), breaks = c(1, 2, 3)) +
  geom_line(size = 1)  +
  labs(y = expression(tau["1 (MSE)"]),
       x = "Model Graph") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) + 
  scale_color_discrete(labels = c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))) + 
  scale_linetype_discrete(labels = c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))) + 
  facet_grid(~Graph, labeller = label_parsed, scales = "free")

#MSE of tau2
tab_MSE_tau2.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6),
                               Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = 1),
                               Graph.id = rep(c(1:3), 1),
                               para.comb = rep(c(1, 2), each = 3), 
                               MSE_tau2_est = cbind(c(mean((comb3_W1$tau.W1.Pcs-tau2)^2, na.rm=TRUE), mean((comb3_W1$tau.W2.Pcs-tau2)^2, na.rm=TRUE), mean((comb3_W1$tau.W3.Pcs-tau2)^2, na.rm=TRUE),
                                                      mean((comb4_W1$tau.W1.Pcs-tau2)^2, na.rm=TRUE), mean((comb4_W1$tau.W2.Pcs-tau2)^2, na.rm=TRUE), mean((comb4_W1$tau.W3.Pcs-tau2)^2, na.rm=TRUE),
                                                      
                                                      mean((comb3_W2$tau.W1.Pcs-tau2)^2, na.rm=TRUE), mean((comb3_W2$tau.W2.Pcs-tau2)^2, na.rm=TRUE), mean((comb3_W2$tau.W3.Pcs-tau2)^2, na.rm=TRUE),
                                                      mean((comb4_W2$tau.W1.Pcs-tau2)^2, na.rm=TRUE), mean((comb4_W2$tau.W2.Pcs-tau2)^2, na.rm=TRUE), mean((comb4_W2$tau.W3.Pcs-tau2)^2, na.rm=TRUE),
                                                      
                                                      mean((comb3_W3$tau.W1.Pcs-tau2)^2, na.rm=TRUE), mean((comb3_W3$tau.W2.Pcs-tau2)^2, na.rm=TRUE), mean((comb3_W3$tau.W3.Pcs-tau2)^2, na.rm=TRUE),
                                                      mean((comb4_W3$tau.W1.Pcs-tau2)^2, na.rm=TRUE), mean((comb4_W3$tau.W2.Pcs-tau2)^2, na.rm=TRUE), mean((comb4_W3$tau.W3.Pcs-tau2)^2, na.rm=TRUE))))
tab_MSE_tau2.Pcs$para.comb = as.factor(tab_MSE_tau2.Pcs$para.comb)
tab_MSE_tau2.Pcs$Graph = as.factor(tab_MSE_tau2.Pcs$Graph)
levels(tab_MSE_tau2.Pcs$para.comb) <- c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))
levels(tab_MSE_tau2.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

#Merged MSE of tau2 over the different hyperparameter combinations
p2 <- ggplot(tab_MSE_tau2.Pcs, aes(Graph.id, MSE_tau2_est, group=para.comb, colour = para.comb, linetype=para.comb)) +
  scale_x_continuous(labels = c("1", "2", "3"), breaks = c(1, 2, 3)) +
  geom_line(size = 1)  +
  labs(y = expression(tau["2 (MSE)"]),
       x = "Model Graph") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) + 
  scale_color_discrete(labels = c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))) + 
  scale_linetype_discrete(labels = c(expression(paste(phi,"=0.3")), expression(paste(phi,"=0.9")))) + 
  facet_grid(~Graph, labeller = label_parsed, scales = "free")

#MSE of phi1
tab_MSE_phi1.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6),
                               Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = 1),
                               Graph.id = rep(c(1:3), 1),
                               para.comb = rep(c(1, 2), each = 3), 
                               MSE_phi1_est = cbind(c(mean((comb1_W1$phi.W1.Pcs-phi1)^2, na.rm=TRUE), mean((comb1_W1$phi.W2.Pcs-phi1)^2, na.rm=TRUE), mean((comb1_W1$phi.W3.Pcs-phi1)^2, na.rm=TRUE),
                                                      mean((comb3_W1$phi.W1.Pcs-phi1)^2, na.rm=TRUE), mean((comb3_W1$phi.W2.Pcs-phi1)^2, na.rm=TRUE), mean((comb3_W1$phi.W3.Pcs-phi1)^2, na.rm=TRUE),
                                                      
                                                      mean((comb1_W2$phi.W1.Pcs-phi1)^2, na.rm=TRUE), mean((comb1_W2$phi.W2.Pcs-phi1)^2, na.rm=TRUE), mean((comb1_W2$phi.W3.Pcs-phi1)^2, na.rm=TRUE),
                                                      mean((comb3_W2$phi.W1.Pcs-phi1)^2, na.rm=TRUE), mean((comb3_W2$phi.W2.Pcs-phi1)^2, na.rm=TRUE), mean((comb3_W2$phi.W3.Pcs-phi1)^2, na.rm=TRUE),
                                                      
                                                      mean((comb1_W3$phi.W1.Pcs-phi1)^2, na.rm=TRUE), mean((comb1_W3$phi.W2.Pcs-phi1)^2, na.rm=TRUE), mean((comb1_W3$phi.W3.Pcs-phi1)^2, na.rm=TRUE),
                                                      mean((comb3_W3$phi.W1.Pcs-phi1)^2, na.rm=TRUE), mean((comb3_W3$phi.W2.Pcs-phi1)^2, na.rm=TRUE), mean((comb3_W3$phi.W3.Pcs-phi1)^2, na.rm=TRUE))))
tab_MSE_phi1.Pcs$para.comb = as.factor(tab_MSE_phi1.Pcs$para.comb)
tab_MSE_phi1.Pcs$Graph = as.factor(tab_MSE_phi1.Pcs$Graph)
levels(tab_MSE_phi1.Pcs$para.comb) <- c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))
levels(tab_MSE_phi1.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

#Merged MSE of phi1 over the different hyperparameter combinations
p3 <- ggplot(tab_MSE_phi1.Pcs, aes(Graph.id, MSE_phi1_est, group=para.comb, colour = para.comb, linetype=para.comb)) +
  scale_x_continuous(labels = c("1", "2", "3"), breaks = c(1, 2, 3)) +
  geom_line(size = 1)  +
  labs(y = expression(phi["1 (MSE)"]),
       x = "Model Graph") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) + 
  scale_color_discrete(labels = c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))) + 
  scale_linetype_discrete(labels = c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))) + 
  facet_grid(~Graph, labeller = label_parsed, scales = "free")

#MSE of phi2
tab_MSE_phi2.Pcs <- data.frame(Graph = rep(c("Graph-1", "Graph-2", "Graph-3"), each=6),
                               Model.Graph = rep(c("Model-Graph-1", "Model-Graph-2", "Model-Graph-3"), each = 1),
                               Graph.id = rep(c(1:3), 1),
                               para.comb = rep(c(1, 2), each = 3), 
                               MSE_phi2_est = cbind(c(mean((comb2_W1$phi.W1.Pcs-phi2)^2, na.rm=TRUE), mean((comb2_W1$phi.W2.Pcs-phi2)^2, na.rm=TRUE), mean((comb2_W1$phi.W3.Pcs-phi2)^2, na.rm=TRUE),
                                                      mean((comb4_W1$phi.W1.Pcs-phi2)^2, na.rm=TRUE), mean((comb4_W1$phi.W2.Pcs-phi2)^2, na.rm=TRUE), mean((comb4_W1$phi.W3.Pcs-phi2)^2, na.rm=TRUE),
                                                      
                                                      mean((comb2_W2$phi.W1.Pcs-phi2)^2, na.rm=TRUE), mean((comb2_W2$phi.W2.Pcs-phi2)^2, na.rm=TRUE), mean((comb2_W2$phi.W3.Pcs-phi2)^2, na.rm=TRUE),
                                                      mean((comb4_W2$phi.W1.Pcs-phi2)^2, na.rm=TRUE), mean((comb4_W2$phi.W2.Pcs-phi2)^2, na.rm=TRUE), mean((comb4_W2$phi.W3.Pcs-phi2)^2, na.rm=TRUE),
                                                      
                                                      mean((comb2_W3$phi.W1.Pcs-phi2)^2, na.rm=TRUE), mean((comb2_W3$phi.W2.Pcs-phi2)^2, na.rm=TRUE), mean((comb2_W3$phi.W3.Pcs-phi2)^2, na.rm=TRUE),
                                                      mean((comb4_W3$phi.W1.Pcs-phi2)^2, na.rm=TRUE), mean((comb4_W3$phi.W2.Pcs-phi2)^2, na.rm=TRUE), mean((comb4_W3$phi.W3.Pcs-phi2)^2, na.rm=TRUE))))
tab_MSE_phi2.Pcs$para.comb = as.factor(tab_MSE_phi2.Pcs$para.comb)
tab_MSE_phi2.Pcs$Graph = as.factor(tab_MSE_phi2.Pcs$Graph)
levels(tab_MSE_phi2.Pcs$para.comb) <- c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))
levels(tab_MSE_phi2.Pcs$Graph) <- c("Graph-1", "Graph-2", "Graph-3")

#Merged MSE of phi2 over the different hyperparameter combinations
p4<- ggplot(tab_MSE_phi2.Pcs, aes(Graph.id, MSE_phi2_est, group=para.comb, colour = para.comb, linetype=para.comb)) +
  scale_x_continuous(labels = c("1", "2", "3"), breaks = c(1, 2, 3)) +
  geom_line(size = 1)  +
  labs(y = expression(phi["2 (MSE)"]),
       x = "Model Graph") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"), 
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "grey80", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1.5),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black", size = 1)) + 
  scale_color_discrete(labels = c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))) + 
  scale_linetype_discrete(labels = c(expression(paste(tau,"=4/9")), expression(paste(tau,"=4")))) + 
  facet_grid(~Graph, labeller = label_parsed, scales = "free")

plot_grid(p1, p2, p3, p4, nrow = 2)



###################################
# Summary results in a tabular form
###################################
# Graph 1 
BYM2.W1.est <- rbind(c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau1, 3), round(phi1, 3), "", "",  "", "", ""), 
                     c(round(mean(comb1_W1$b0.W1.Pcs), 3), round(mean(comb1_W1$b1.W1.Pcs), 3), round(mean(comb1_W1$b2.W1.Pcs), 3), round(mean(comb1_W1$tau.W1.Pcs), 3), round(mean(comb1_W1$phi.W1.Pcs), 3), round(mean(comb1_W1$WAIC.W1.Pcs), 3), round(mean(comb1_W1$DIC.W1.Pcs), 3), round(mean(comb1_W1$CPO.W1.Pcs), 3), round(mean(comb1_W1$MLIK.W1.Pcs), 3), round(mean(comb1_W1$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb1_W1$b0.W2.Pcs), 3), round(mean(comb1_W1$b1.W2.Pcs), 3), round(mean(comb1_W1$b2.W2.Pcs), 3), round(mean(comb1_W1$tau.W2.Pcs), 3), round(mean(comb1_W1$phi.W2.Pcs), 3), round(mean(comb1_W1$WAIC.W2.Pcs), 3), round(mean(comb1_W1$DIC.W2.Pcs), 3), round(mean(comb1_W1$CPO.W2.Pcs), 3), round(mean(comb1_W1$MLIK.W2.Pcs), 3), round(mean(comb1_W1$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb1_W1$b0.W3.Pcs), 3), round(mean(comb1_W1$b1.W3.Pcs), 3), round(mean(comb1_W1$b2.W3.Pcs), 3), round(mean(comb1_W1$tau.W3.Pcs), 3), round(mean(comb1_W1$phi.W3.Pcs), 3), round(mean(comb1_W1$WAIC.W3.Pcs), 3), round(mean(comb1_W1$DIC.W3.Pcs), 3), round(mean(comb1_W1$CPO.W3.Pcs), 3), round(mean(comb1_W1$MLIK.W3.Pcs), 3), round(mean(comb1_W1$Run.Time_W3.Pcs), 3)),
                     
                     c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau1, 3), round(phi2, 3), "", "",  "", "", ""), 
                     c(round(mean(comb2_W1$b0.W1.Pcs), 3), round(mean(comb2_W1$b1.W1.Pcs), 3), round(mean(comb2_W1$b2.W1.Pcs), 3), round(mean(comb2_W1$tau.W1.Pcs), 3), round(mean(comb2_W1$phi.W1.Pcs), 3), round(mean(comb2_W1$WAIC.W1.Pcs), 3), round(mean(comb2_W1$DIC.W1.Pcs), 3), round(mean(comb2_W1$CPO.W1.Pcs), 3), round(mean(comb2_W1$MLIK.W1.Pcs), 3), round(mean(comb2_W1$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb2_W1$b0.W2.Pcs), 3), round(mean(comb2_W1$b1.W2.Pcs), 3), round(mean(comb2_W1$b2.W2.Pcs), 3), round(mean(comb2_W1$tau.W2.Pcs), 3), round(mean(comb2_W1$phi.W2.Pcs), 3), round(mean(comb2_W1$WAIC.W2.Pcs), 3), round(mean(comb2_W1$DIC.W2.Pcs), 3), round(mean(comb2_W1$CPO.W2.Pcs), 3), round(mean(comb2_W1$MLIK.W2.Pcs), 3), round(mean(comb2_W1$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb2_W1$b0.W3.Pcs), 3), round(mean(comb2_W1$b1.W3.Pcs), 3), round(mean(comb2_W1$b2.W3.Pcs), 3), round(mean(comb2_W1$tau.W3.Pcs), 3), round(mean(comb2_W1$phi.W3.Pcs), 3), round(mean(comb2_W1$WAIC.W3.Pcs), 3), round(mean(comb2_W1$DIC.W3.Pcs), 3), round(mean(comb2_W1$CPO.W3.Pcs), 3), round(mean(comb2_W1$MLIK.W3.Pcs), 3), round(mean(comb2_W1$Run.Time_W3.Pcs), 3)),
                     
                     c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau2, 3), round(phi1, 3), "", "",  "", "", ""), 
                     c(round(mean(comb3_W1$b0.W1.Pcs), 3), round(mean(comb3_W1$b1.W1.Pcs), 3), round(mean(comb3_W1$b2.W1.Pcs), 3), round(mean(comb3_W1$tau.W1.Pcs), 3), round(mean(comb3_W1$phi.W1.Pcs), 3), round(mean(comb3_W1$WAIC.W1.Pcs), 3), round(mean(comb3_W1$DIC.W1.Pcs), 3), round(mean(comb3_W1$CPO.W1.Pcs), 3), round(mean(comb3_W1$MLIK.W1.Pcs), 3), round(mean(comb3_W1$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb3_W1$b0.W2.Pcs), 3), round(mean(comb3_W1$b1.W2.Pcs), 3), round(mean(comb3_W1$b2.W2.Pcs), 3), round(mean(comb3_W1$tau.W2.Pcs), 3), round(mean(comb3_W1$phi.W2.Pcs), 3), round(mean(comb3_W1$WAIC.W2.Pcs), 3), round(mean(comb3_W1$DIC.W2.Pcs), 3), round(mean(comb3_W1$CPO.W2.Pcs), 3), round(mean(comb3_W1$MLIK.W2.Pcs), 3), round(mean(comb3_W1$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb3_W1$b0.W3.Pcs), 3), round(mean(comb3_W1$b1.W3.Pcs), 3), round(mean(comb3_W1$b2.W3.Pcs), 3), round(mean(comb3_W1$tau.W3.Pcs), 3), round(mean(comb3_W1$phi.W3.Pcs), 3), round(mean(comb3_W1$WAIC.W3.Pcs), 3), round(mean(comb3_W1$DIC.W3.Pcs), 3), round(mean(comb3_W1$CPO.W3.Pcs), 3), round(mean(comb3_W1$MLIK.W3.Pcs), 3), round(mean(comb3_W1$Run.Time_W3.Pcs), 3)),
                     
                     c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau2, 3), round(phi2, 3), "", "",  "", "", ""), 
                     c(round(mean(comb4_W1$b0.W1.Pcs), 3), round(mean(comb4_W1$b1.W1.Pcs), 3), round(mean(comb4_W1$b2.W1.Pcs), 3), round(mean(comb4_W1$tau.W1.Pcs), 3), round(mean(comb4_W1$phi.W1.Pcs), 3), round(mean(comb4_W1$WAIC.W1.Pcs), 3), round(mean(comb4_W1$DIC.W1.Pcs), 3), round(mean(comb4_W1$CPO.W1.Pcs), 3), round(mean(comb4_W1$MLIK.W1.Pcs), 3), round(mean(comb4_W1$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb4_W1$b0.W2.Pcs), 3), round(mean(comb4_W1$b1.W2.Pcs), 3), round(mean(comb4_W1$b2.W2.Pcs), 3), round(mean(comb4_W1$tau.W2.Pcs), 3), round(mean(comb4_W1$phi.W2.Pcs), 3), round(mean(comb4_W1$WAIC.W2.Pcs), 3), round(mean(comb4_W1$DIC.W2.Pcs), 3), round(mean(comb4_W1$CPO.W2.Pcs), 3), round(mean(comb4_W1$MLIK.W2.Pcs), 3), round(mean(comb4_W1$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb4_W1$b0.W3.Pcs), 3), round(mean(comb4_W1$b1.W3.Pcs), 3), round(mean(comb4_W1$b2.W3.Pcs), 3), round(mean(comb4_W1$tau.W3.Pcs), 3), round(mean(comb4_W1$phi.W3.Pcs), 3), round(mean(comb4_W1$WAIC.W3.Pcs), 3), round(mean(comb4_W1$DIC.W3.Pcs), 3), round(mean(comb4_W1$CPO.W3.Pcs), 3), round(mean(comb4_W1$MLIK.W3.Pcs), 3), round(mean(comb4_W1$Run.Time_W3.Pcs), 3)))
PC.prior =  cbind(rep(c("", rep(c("cs"), 3)) , 4))
Model.G =  cbind(rep(c("True value", rep(c("Graph 1", "Graph 2", "Graph 3"), each = 1)) , 4))
tab_W1 <- cbind(Model.G, PC.prior, BYM2.W1.est)
colnames(tab_W1) <- c("Model Graph", "PC prior", expression(beta[0]), expression(beta[1]), expression(beta[2]), expression(tau), expression(phi), "WAIC", "DIC", "CPO", "MLIK", "Run time")

knitr::kable(tab_W1[, ], booktabs = TRUE, row.names = FALSE,
             format = "markdown",
             escape = FALSE,
             caption = "Table: (\\#tab_W1: A summary result of the simulated BYM2 model based on 200 samples generated using Graph 1.")
cat("\nTable: (\\#tab_W1:BYM2 simulations 1) A summary result of the simulated BYM2 model based on 200 samples generated using Graph 1.\n")

# Graph 2 
BYM2.W2.est <- rbind(c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau1, 3), round(phi1, 3), "", "",  "", "", ""), 
                     c(round(mean(comb1_W2$b0.W1.Pcs), 3), round(mean(comb1_W2$b1.W1.Pcs), 3), round(mean(comb1_W2$b2.W1.Pcs), 3), round(mean(comb1_W2$tau.W1.Pcs), 3), round(mean(comb1_W2$phi.W1.Pcs), 3), round(mean(comb1_W2$WAIC.W1.Pcs), 3), round(mean(comb1_W2$DIC.W1.Pcs), 3), round(mean(comb1_W2$CPO.W1.Pcs), 3), round(mean(comb1_W2$MLIK.W1.Pcs), 3), round(mean(comb1_W2$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb1_W2$b0.W2.Pcs), 3), round(mean(comb1_W2$b1.W2.Pcs), 3), round(mean(comb1_W2$b2.W2.Pcs), 3), round(mean(comb1_W2$tau.W2.Pcs), 3), round(mean(comb1_W2$phi.W2.Pcs), 3), round(mean(comb1_W2$WAIC.W2.Pcs), 3), round(mean(comb1_W2$DIC.W2.Pcs), 3), round(mean(comb1_W2$CPO.W2.Pcs), 3), round(mean(comb1_W2$MLIK.W2.Pcs), 3), round(mean(comb1_W2$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb1_W2$b0.W3.Pcs), 3), round(mean(comb1_W2$b1.W3.Pcs), 3), round(mean(comb1_W2$b2.W3.Pcs), 3), round(mean(comb1_W2$tau.W3.Pcs), 3), round(mean(comb1_W2$phi.W3.Pcs), 3), round(mean(comb1_W2$WAIC.W3.Pcs), 3), round(mean(comb1_W2$DIC.W3.Pcs), 3), round(mean(comb1_W2$CPO.W3.Pcs), 3), round(mean(comb1_W2$MLIK.W3.Pcs), 3), round(mean(comb1_W2$Run.Time_W3.Pcs), 3)),
                     
                     c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau1, 3), round(phi2, 3), "", "",  "", "", ""), 
                     c(round(mean(comb2_W2$b0.W1.Pcs), 3), round(mean(comb2_W2$b1.W1.Pcs), 3), round(mean(comb2_W2$b2.W1.Pcs), 3), round(mean(comb2_W2$tau.W1.Pcs), 3), round(mean(comb2_W2$phi.W1.Pcs), 3), round(mean(comb2_W2$WAIC.W1.Pcs), 3), round(mean(comb2_W2$DIC.W1.Pcs), 3), round(mean(comb2_W2$CPO.W1.Pcs), 3), round(mean(comb2_W2$MLIK.W1.Pcs), 3), round(mean(comb2_W2$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb2_W2$b0.W2.Pcs), 3), round(mean(comb2_W2$b1.W2.Pcs), 3), round(mean(comb2_W2$b2.W2.Pcs), 3), round(mean(comb2_W2$tau.W2.Pcs), 3), round(mean(comb2_W2$phi.W2.Pcs), 3), round(mean(comb2_W2$WAIC.W2.Pcs), 3), round(mean(comb2_W2$DIC.W2.Pcs), 3), round(mean(comb2_W2$CPO.W2.Pcs), 3), round(mean(comb2_W2$MLIK.W2.Pcs), 3), round(mean(comb2_W2$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb2_W2$b0.W3.Pcs), 3), round(mean(comb2_W2$b1.W3.Pcs), 3), round(mean(comb2_W2$b2.W3.Pcs), 3), round(mean(comb2_W2$tau.W3.Pcs), 3), round(mean(comb2_W2$phi.W3.Pcs), 3), round(mean(comb2_W2$WAIC.W3.Pcs), 3), round(mean(comb2_W2$DIC.W3.Pcs), 3), round(mean(comb2_W2$CPO.W3.Pcs), 3), round(mean(comb2_W2$MLIK.W3.Pcs), 3), round(mean(comb2_W2$Run.Time_W3.Pcs), 3)),
                     
                     c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau2, 3), round(phi1, 3), "", "",  "", "", ""), 
                     c(round(mean(comb3_W2$b0.W1.Pcs), 3), round(mean(comb3_W2$b1.W1.Pcs), 3), round(mean(comb3_W2$b2.W1.Pcs), 3), round(mean(comb3_W2$tau.W1.Pcs), 3), round(mean(comb3_W2$phi.W1.Pcs), 3), round(mean(comb3_W2$WAIC.W1.Pcs), 3), round(mean(comb3_W2$DIC.W1.Pcs), 3), round(mean(comb3_W2$CPO.W1.Pcs), 3), round(mean(comb3_W2$MLIK.W1.Pcs), 3), round(mean(comb3_W2$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb3_W2$b0.W2.Pcs), 3), round(mean(comb3_W2$b1.W2.Pcs), 3), round(mean(comb3_W2$b2.W2.Pcs), 3), round(mean(comb3_W2$tau.W2.Pcs), 3), round(mean(comb3_W2$phi.W2.Pcs), 3), round(mean(comb3_W2$WAIC.W2.Pcs), 3), round(mean(comb3_W2$DIC.W2.Pcs), 3), round(mean(comb3_W2$CPO.W2.Pcs), 3), round(mean(comb3_W2$MLIK.W2.Pcs), 3), round(mean(comb3_W2$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb3_W2$b0.W3.Pcs), 3), round(mean(comb3_W2$b1.W3.Pcs), 3), round(mean(comb3_W2$b2.W3.Pcs), 3), round(mean(comb3_W2$tau.W3.Pcs), 3), round(mean(comb3_W2$phi.W3.Pcs), 3), round(mean(comb3_W2$WAIC.W3.Pcs), 3), round(mean(comb3_W2$DIC.W3.Pcs), 3), round(mean(comb3_W2$CPO.W3.Pcs), 3), round(mean(comb3_W2$MLIK.W3.Pcs), 3), round(mean(comb3_W2$Run.Time_W3.Pcs), 3)),
                     
                     c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau2, 3), round(phi2, 3), "", "",  "", "", ""), 
                     c(round(mean(comb4_W2$b0.W1.Pcs), 3), round(mean(comb4_W2$b1.W1.Pcs), 3), round(mean(comb4_W2$b2.W1.Pcs), 3), round(mean(comb4_W2$tau.W1.Pcs), 3), round(mean(comb4_W2$phi.W1.Pcs), 3), round(mean(comb4_W2$WAIC.W1.Pcs), 3), round(mean(comb4_W2$DIC.W1.Pcs), 3), round(mean(comb4_W2$CPO.W1.Pcs), 3), round(mean(comb4_W2$MLIK.W1.Pcs), 3), round(mean(comb4_W2$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb4_W2$b0.W2.Pcs), 3), round(mean(comb4_W2$b1.W2.Pcs), 3), round(mean(comb4_W2$b2.W2.Pcs), 3), round(mean(comb4_W2$tau.W2.Pcs), 3), round(mean(comb4_W2$phi.W2.Pcs), 3), round(mean(comb4_W2$WAIC.W2.Pcs), 3), round(mean(comb4_W2$DIC.W2.Pcs), 3), round(mean(comb4_W2$CPO.W2.Pcs), 3), round(mean(comb4_W2$MLIK.W2.Pcs), 3), round(mean(comb4_W2$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb4_W2$b0.W3.Pcs), 3), round(mean(comb4_W2$b1.W3.Pcs), 3), round(mean(comb4_W2$b2.W3.Pcs), 3), round(mean(comb4_W2$tau.W3.Pcs), 3), round(mean(comb4_W2$phi.W3.Pcs), 3), round(mean(comb4_W2$WAIC.W3.Pcs), 3), round(mean(comb4_W2$DIC.W3.Pcs), 3), round(mean(comb4_W2$CPO.W3.Pcs), 3), round(mean(comb4_W2$MLIK.W3.Pcs), 3), round(mean(comb4_W2$Run.Time_W3.Pcs), 3)))
PC.prior =  cbind(rep(c("", rep(c("cs"), 3)) , 4))
Model.G =  cbind(rep(c("True value", rep(c("Graph 1", "Graph 2", "Graph 3"), each = 1)) , 4))
tab_W2 <- cbind(Model.G, PC.prior, BYM2.W2.est)
colnames(tab_W2) <- c("Model Graph", "PC prior", expression(beta[0]), expression(beta[1]), expression(beta[2]), expression(tau), expression(phi), "WAIC", "DIC", "CPO", "MLIK", "Run time")

knitr::kable(tab_W2[, ], booktabs = TRUE, row.names = FALSE,
             format = "markdown",
             escape = FALSE,
             caption = "Table: (\\#tab_W2: A summary result of the simulated BYM2 model based on 200 samples generated using Graph 2.")
cat("\nTable: (\\#tab_W2:BYM2 simulations 2) A summary result of the simulated BYM2 model based on 200 samples generated using Graph 2.\n")

# Graph 3
BYM2.W3.est <- rbind(c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau1, 3), round(phi1, 3), "", "",  "", "", ""), 
                     c(round(mean(comb1_W3$b0.W1.Pcs), 3), round(mean(comb1_W3$b1.W1.Pcs), 3), round(mean(comb1_W3$b2.W1.Pcs), 3), round(mean(comb1_W3$tau.W1.Pcs), 3), round(mean(comb1_W3$phi.W1.Pcs), 3), round(mean(comb1_W3$WAIC.W1.Pcs), 3), round(mean(comb1_W3$DIC.W1.Pcs), 3), round(mean(comb1_W3$CPO.W1.Pcs), 3), round(mean(comb1_W3$MLIK.W1.Pcs), 3), round(mean(comb1_W3$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb1_W3$b0.W2.Pcs), 3), round(mean(comb1_W3$b1.W2.Pcs), 3), round(mean(comb1_W3$b2.W2.Pcs), 3), round(mean(comb1_W3$tau.W2.Pcs), 3), round(mean(comb1_W3$phi.W2.Pcs), 3), round(mean(comb1_W3$WAIC.W2.Pcs), 3), round(mean(comb1_W3$DIC.W2.Pcs), 3), round(mean(comb1_W3$CPO.W2.Pcs), 3), round(mean(comb1_W3$MLIK.W2.Pcs), 3), round(mean(comb1_W3$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb1_W3$b0.W3.Pcs), 3), round(mean(comb1_W3$b1.W3.Pcs), 3), round(mean(comb1_W3$b2.W3.Pcs), 3), round(mean(comb1_W3$tau.W3.Pcs), 3), round(mean(comb1_W3$phi.W3.Pcs), 3), round(mean(comb1_W3$WAIC.W3.Pcs), 3), round(mean(comb1_W3$DIC.W3.Pcs), 3), round(mean(comb1_W3$CPO.W3.Pcs), 3), round(mean(comb1_W3$MLIK.W3.Pcs), 3), round(mean(comb1_W3$Run.Time_W3.Pcs), 3)),
                     
                     c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau1, 3), round(phi2, 3), "", "",  "", "", ""), 
                     c(round(mean(comb2_W3$b0.W1.Pcs), 3), round(mean(comb2_W3$b1.W1.Pcs), 3), round(mean(comb2_W3$b2.W1.Pcs), 3), round(mean(comb2_W3$tau.W1.Pcs), 3), round(mean(comb2_W3$phi.W1.Pcs), 3), round(mean(comb2_W3$WAIC.W1.Pcs), 3), round(mean(comb2_W3$DIC.W1.Pcs), 3), round(mean(comb2_W3$CPO.W1.Pcs), 3), round(mean(comb2_W3$MLIK.W1.Pcs), 3), round(mean(comb2_W3$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb2_W3$b0.W2.Pcs), 3), round(mean(comb2_W3$b1.W2.Pcs), 3), round(mean(comb2_W3$b2.W2.Pcs), 3), round(mean(comb2_W3$tau.W2.Pcs), 3), round(mean(comb2_W3$phi.W2.Pcs), 3), round(mean(comb2_W3$WAIC.W2.Pcs), 3), round(mean(comb2_W3$DIC.W2.Pcs), 3), round(mean(comb2_W3$CPO.W2.Pcs), 3), round(mean(comb2_W3$MLIK.W2.Pcs), 3), round(mean(comb2_W3$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb2_W3$b0.W3.Pcs), 3), round(mean(comb2_W3$b1.W3.Pcs), 3), round(mean(comb2_W3$b2.W3.Pcs), 3), round(mean(comb2_W3$tau.W3.Pcs), 3), round(mean(comb2_W3$phi.W3.Pcs), 3), round(mean(comb2_W3$WAIC.W3.Pcs), 3), round(mean(comb2_W3$DIC.W3.Pcs), 3), round(mean(comb2_W3$CPO.W3.Pcs), 3), round(mean(comb2_W3$MLIK.W3.Pcs), 3), round(mean(comb2_W3$Run.Time_W3.Pcs), 3)),
                     
                     c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau2, 3), round(phi1, 3), "", "",  "", "", ""), 
                     c(round(mean(comb3_W3$b0.W1.Pcs), 3), round(mean(comb3_W3$b1.W1.Pcs), 3), round(mean(comb3_W3$b2.W1.Pcs), 3), round(mean(comb3_W3$tau.W1.Pcs), 3), round(mean(comb3_W3$phi.W1.Pcs), 3), round(mean(comb3_W3$WAIC.W1.Pcs), 3), round(mean(comb3_W3$DIC.W1.Pcs), 3), round(mean(comb3_W3$CPO.W1.Pcs), 3), round(mean(comb3_W3$MLIK.W1.Pcs), 3), round(mean(comb3_W3$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb3_W3$b0.W2.Pcs), 3), round(mean(comb3_W3$b1.W2.Pcs), 3), round(mean(comb3_W3$b2.W2.Pcs), 3), round(mean(comb3_W3$tau.W2.Pcs), 3), round(mean(comb3_W3$phi.W2.Pcs), 3), round(mean(comb3_W3$WAIC.W2.Pcs), 3), round(mean(comb3_W3$DIC.W2.Pcs), 3), round(mean(comb3_W3$CPO.W2.Pcs), 3), round(mean(comb3_W3$MLIK.W2.Pcs), 3), round(mean(comb3_W3$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb3_W3$b0.W3.Pcs), 3), round(mean(comb3_W3$b1.W3.Pcs), 3), round(mean(comb3_W3$b2.W3.Pcs), 3), round(mean(comb3_W3$tau.W3.Pcs), 3), round(mean(comb3_W3$phi.W3.Pcs), 3), round(mean(comb3_W3$WAIC.W3.Pcs), 3), round(mean(comb3_W3$DIC.W3.Pcs), 3), round(mean(comb3_W3$CPO.W3.Pcs), 3), round(mean(comb3_W3$MLIK.W3.Pcs), 3), round(mean(comb3_W3$Run.Time_W3.Pcs), 3)),
                     
                     c(round(b0, 3), round(b1, 3), round(b2, 3), round(tau2, 3), round(phi2, 3), "", "",  "", "", ""), 
                     c(round(mean(comb4_W3$b0.W1.Pcs), 3), round(mean(comb4_W3$b1.W1.Pcs), 3), round(mean(comb4_W3$b2.W1.Pcs), 3), round(mean(comb4_W3$tau.W1.Pcs), 3), round(mean(comb4_W3$phi.W1.Pcs), 3), round(mean(comb4_W3$WAIC.W1.Pcs), 3), round(mean(comb4_W3$DIC.W1.Pcs), 3), round(mean(comb4_W3$CPO.W1.Pcs), 3), round(mean(comb4_W3$MLIK.W1.Pcs), 3), round(mean(comb4_W3$Run.Time_W1.Pcs), 3)),
                     c(round(mean(comb4_W3$b0.W2.Pcs), 3), round(mean(comb4_W3$b1.W2.Pcs), 3), round(mean(comb4_W3$b2.W2.Pcs), 3), round(mean(comb4_W3$tau.W2.Pcs), 3), round(mean(comb4_W3$phi.W2.Pcs), 3), round(mean(comb4_W3$WAIC.W2.Pcs), 3), round(mean(comb4_W3$DIC.W2.Pcs), 3), round(mean(comb4_W3$CPO.W2.Pcs), 3), round(mean(comb4_W3$MLIK.W2.Pcs), 3), round(mean(comb4_W3$Run.Time_W2.Pcs), 3)),
                     c(round(mean(comb4_W3$b0.W3.Pcs), 3), round(mean(comb4_W3$b1.W3.Pcs), 3), round(mean(comb4_W3$b2.W3.Pcs), 3), round(mean(comb4_W3$tau.W3.Pcs), 3), round(mean(comb4_W3$phi.W3.Pcs), 3), round(mean(comb4_W3$WAIC.W3.Pcs), 3), round(mean(comb4_W3$DIC.W3.Pcs), 3), round(mean(comb4_W3$CPO.W3.Pcs), 3), round(mean(comb4_W3$MLIK.W3.Pcs), 3), round(mean(comb4_W3$Run.Time_W3.Pcs), 3)))
PC.prior =  cbind(rep(c("", rep(c("cs"), 3)) , 4))
Model.G =  cbind(rep(c("True value", rep(c("Graph 1", "Graph 2", "Graph 3"), each = 1)) , 4))
tab_W3 <- cbind(Model.G, PC.prior, BYM2.W3.est)
colnames(tab_W3) <- c("Model Graph", "PC prior", expression(beta[0]), expression(beta[1]), expression(beta[2]), expression(tau), expression(phi), "WAIC", "DIC", "CPO", "MLIK", "Run time")

knitr::kable(tab_W3[, ], booktabs = TRUE, row.names = FALSE,
             format = "markdown",
             escape = FALSE,
             caption = "Table: (\\#tab_W3: A summary result of the simulated BYM2 model based on 200 samples generated using Graph 3.")
cat("\nTable: (\\#tab_W3:BYM2 simulations 3) A summary result of the simulated BYM2 model based on 200 samples generated using Graph 3.\n")




