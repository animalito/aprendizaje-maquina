## esta función calcula los valores de cada nodo en toda la red,
## para cada entrada
feed.fow <- function(beta, x){
  a.1 <- h(beta[1] + beta[2]*x) # calcula variable 1 de capa oculta
  a.2 <- h(beta[3] + beta[4]*x) # calcula variable 2 de capa oculta
  p <- h(beta[5]+beta[6]*a.1 + beta[7]*a.2) # calcula capa de salida
  p
}

devianza.reg <- function(x, g, lambda){
  # esta función es una fábrica de funciones
  devianza <- function(beta){
    p <- feed.fow(beta, x)
    - 2 * mean(g*log(p) + (1-g)*log(1-p)) + lambda*sum(beta^2)
  }
  devianza
}



## diabetes.red.1 <- nnet(type~., data=Pima.te, size = 8,  decay = 0.5, 
##   maxit = 500, MaxNWts=10000)
## diabetes.red.2 <- nnet(type~., data=Pima.te, size = 8,  decay = 0.5, 
##   maxit = 500, MaxNWts=1000)
##   diabetes.red.3 <- nnet(type~., data=Pima.te, size = 8,  decay = 0.5, 
##   maxit = 500, MaxNWts=1000)
## plot(diabetes.red.1)
## mod.zip <-  nnet(digit~., data=zip.d, size = 10,  decay = 5, 
##   maxit = 500, MaxNWts=10000)
## preds.dig <- predict(mod.zip, data.frame(zip.test))
## pred.1 <- apply(preds.dig, 1,which.max)
## tab.1 <- table(pred.1, zip.test[,1])
## tab.1
## sum(diag(as.matrix(tab.1)))/sum(tab.1)