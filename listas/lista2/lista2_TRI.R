
mat.par.1 <- matrix(c(1.8, .7, 1.8, 1.2, 1.2, .5, 1, 1, 1, -.5, .5, 0, 0.2, 0.2, .25, .2, 0.25, .25),nrow=6)
mat.par.2 <- matrix(c(2, .5, 1.5, 1.3, 1.1, .7, -1, 1, -1.5, .5, 1.5, 2, 0.2, 0.2, .25, .2, 0.25, .25),nrow=6)

theta <- seq(-4,4,0.01)


# Questao 1 

mat.prob <- matrix(0,nrow(mat.par.1),length(theta))


# Questao 2 

mat.prob <- mat.prob.dif <- mat.info <- matrix(0,nrow(mat.par.1),length(theta))

for (i in 1:nrow(mat.par.1)) {
  for (j in 1:length(theta)) {
    mat.prob[i,j] <- mat.par.1[i,3] + (1-mat.par.1[i,3])/(1+exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2])))
    mat.prob.dif[i,j] <- mat.par.1[i,1]*(1-mat.par.1[i,3])*exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2]))/
      ((1+exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2])))^2)
    mat.info[i,j] <- (mat.prob.dif[i,j]^2)/(mat.prob[i,j]*(1-mat.prob[i,j]))
  }
}

info.1 <- apply(mat.info,2,sum)

plot(theta,info.1,type="l")

for (i in 1:nrow(mat.par.2)) {
  for (j in 1:length(theta)) {
    mat.prob[i,j] <- mat.par.2[i,3] + (1-mat.par.2[i,3])/(1+exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2])))
    mat.prob.dif[i,j] <- mat.par.2[i,1]*(1-mat.par.2[i,3])*exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2]))/
      ((1+exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2])))^2)
    mat.info[i,j] <- (mat.prob.dif[i,j]^2)/(mat.prob[i,j]*(1-mat.prob[i,j]))
  }
}

info.2 <- apply(mat.info,2,sum)

lines(theta,info.2,lty=2)
legend(-4,1.5,c("Teste 1", "Teste 2"), lty=c(1,2))


# Questao 3 


# Questao 4 


























