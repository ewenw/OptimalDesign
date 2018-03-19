library(ggplot2)
library(GPfit)
library(plotly)
library(akima)
library(rgl)
library(sensitivity)

# initialize dataframe and pick random points

init <- function(n_init=5){
  r_values <- seq(0,1,length.out=10)
  k_values <- seq(0,1,length.out=10)
  datagrid <- expand.grid(r=r_values,k=k_values)
  history <- data.frame(r=runif(n_init), k=runif(n_init), I=-1, method="INIT", stop=0.2)
  history[,3] = data.frame(himmelblau(data.frame(x=history$r, y=history$k)))
  return(history)
}

# params: history dataframe, name of the function to retrieve information from
# determines the next search method and searches the next coordinates 
# returns new history
search_once <- function(history){
  last_method <- tail(history, 1)$method
  if(last_method == "INIT" || last_method == "PE" || last_method == "PER"){
    history<-search_UCB(history)
  }
  else if (last_method == "UCB"){
    history<-search_PE(history)
  }
  searchR <- (tail(history, 1)$r - 0.5) * 4
  searchK <- (tail(history, 1)$k - 0.5) * 4
  history$I[nrow(history)] <- himmelblau(c(searchR, searchK))
  return(history)
}

search_UCB <- function(history){
  GP_model <- GP_fit(X=history[,1:2], Y=history[,3])
  GP_predict <- predict(GP_model, datagrid)
  predictions <- data.frame(GP_predict$complete_data)
  y_plus_sigma  <- predictions$Y_hat + 2*(predictions$MSE)**.5
  UCB_point <- data.frame(predictions[which.max(y_plus_sigma),])
  new_point <- data.frame(r=UCB_point$xnew.1, k=UCB_point$xnew.2, I=-1, method="UCB", stop=0.2)
  history <- rbind(history, new_point)
  return(history)
}

search_PE <- function(history){
  GP_model <- GP_fit(X=history[,1:2], Y=history[,3])
  GP_predict <- predict(GP_model, datagrid)
  predictions <- data.frame(GP_predict$complete_data)
  max_y_minus_sigma <- max(predictions$Y_hat - 2*(predictions$MSE)**.5)
  region <- which(y_plus_sigma > max_y_minus_sigma)
  if(length(region) > 1){
    space_to_sample <- predictions[region,]
    PE_point <- space_to_sample[which.max(space_to_sample[,4]),]
    method <- "PE"
  }
  else{
    PE_point <- predictions[which.max(predictions$MSE), ]
    method <- "PER"
  }
  new_point <- data.frame(r=PE_point[1,1], k=PE_point[1,2], I=-1, method=method, stop=0.2)
  history <- rbind(history, new_point)
  return(history)
}


history = init(10)
history = search_once(history)
generate_GP_plot(history)

