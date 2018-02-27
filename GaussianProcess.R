#######################################
### 	                              ### 
###      Gaussian Process           ###
### 	   Feb. 9th, 2018		          ###
### 	                              ### 
#######################################


library(ggplot2)
library(GPfit)

#############################################################
# Gaussian Process


plotSurface <- function(data, n=10, title){
  r_values <- seq(0,1,length.out=n)
  k_values <- seq(0,1,length.out=n)
  datagrid <- expand.grid("r"=r_values, "k"=k_values)
  GPmodel <- GP_fit(X=data[,1:2], Y=data[,3])
  GPredict <- predict(GPmodel, datagrid)
  
  reconstructed <- data.frame(GPredict$complete_data)
  names(reconstructed) <- c("r", "k", "I", "MSE")
  ggplot(reconstructed, aes(r, k, color=I))  +scale_color_distiller(palette = "Spectral")+
    geom_point(shape=15, size=14) + geom_point(data=data, aes(r,k), shape=10, alpha=0.6, color='black', size=5) +
    theme_bw() + labs(title=title)
}

normalize <- function(data){
  s = sum(data$I)
  data$I = data$I / s
  return(data)
}

kl_divergence <- function(data, model_names, col="Freq", asymmetric=F){
  priors <- 1/length(model_names)
  if(asymmetric != F){
    leading_model <- model_names[asymmetric]
    I_sum <- 0
    for(outcome in data$Var1){
      current_outcome <- data[data$Var1==outcome,]
      liks1 <- current_outcome[current_outcome$Model==leading_model,c(col)]
      liks2 <- current_outcome[current_outcome$Model!=leading_model,c(col)]
      l2 <- sum(liks2*priors)
      l1 <- liks1*(1-priors)
      I_sum <- I_sum + liks1 * log( l1/l2 )
    }
    return(I_sum)
  } else {
    I <- c()
    for(leading_model in model_names){
      I_sum <- 0
      for(outcome in data$Var1){
        current_outcome <- data[data$Var1==outcome,]
        liks1 <- current_outcome[current_outcome$Model==leading_model,c(col)]
        liks2 <- current_outcome[current_outcome$Model!=leading_model,c(col)]
        l2 <- sum(liks2*priors)
        l1 <- liks1*(1-priors)
        I_sum <- I_sum + liks1 * log( l1/l2 )
      }
      I <- c(I, priors*I_sum)
    }
  }
  return(sum(I))
}


#ggplot(reconstructed, aes(X, Y, color=MSE)) + 
 # geom_point(shape=15, size=25) + geom_point(data=data, aes(X,Y), shape=13, color='red', size=5) +
 # theme_bw()


himmelblau <- function(x) {
  (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
}
# 
# 
# x_values <- seq(-4, 4,length.out=10)
# y_values <- seq(-4, 4,length.out=10)
# datagrid <- expand.grid("X"=x_values, "Y"=y_values)
# nInit <- 20
# data <- data.frame("X"=runif(nInit), "Y"=runif(nInit), "I"=runif(nInit))
# data$I <- himmelblau(data[,1:2])
# colnames(data) <- c("X", "Y", "I")
# data
# ggplot(datagrid, aes(X, Y))+geom_point()+
#   geom_point(data=data, aes(X,Y), color='red', size=5)
# 
# GPmodel <- GP_fit(data[,1:2], data[,3])
# GPredict <- predict(GPmodel, datagrid)
# GPredict
# 
# reconstructed <- data.frame(GPredict$complete_data)
# head(reconstructed)
# names(reconstructed) <- c("X", "Y", "I", "MSE")
# head(reconstructed)
# 
# ggplot(reconstructed, aes(X, Y, color=I)) + 
#   geom_point(shape=15, size=14) + geom_point(data=data, aes(X,Y), shape=13, color='red', size=5) +
#   theme_bw()
# 
# add_ucb_point <- function(reconstructed, datagrid, data){
#   y_plus_sigma  <- reconstructed$I + 2*(reconstructed$MSE)**.5
#   UCB_point <- reconstructed[which.max(y_plus_sigma),]
#   dig_next <- as.numeric(UCB_point[,1:2])
#   new_point <- data.frame("X"=dig_next[1], "Y"=dig_next[2])
#   new_point$I <- himmelblau(dig_next)
#   data <- rbind(data, new_point)
#   return(data)
# }
# 
# data <- add_ucb_point(reconstructed, datagrid, data)
# GPmodel <- GP_fit(data[,1:2], data[,3])
# GPredict <- predict(GPmodel, datagrid)
# reconstructed <- data.frame(GPredict$complete_data)
# names(reconstructed) <- c("X", "Y", "I", "MSE")
# ggplot(reconstructed, aes(X, Y, color=I)) + 
#   geom_point(shape=15, size=14) + geom_point(data=data, aes(X,Y), shape=13, color='red', size=5) +
#   theme_bw()
# 
# #plot.GP(GPmodel, surf_check = TRUE)