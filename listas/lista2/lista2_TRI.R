pacman::p_load(tidyverse,reshape2, knitr)

mat.par.1 <- data.frame("a" = c(1.8, .7, 1.8, 1.2, 1.2, .5),
                        "b" = c(1, 1, 1, -.5, .5, 0),
                        "c" = c(0.2, 0.2, .25, .2, 0.25, .25))
mat.par.2 <- data.frame("a" = c(2, .5, 1.5, 1.3, 1.1, .7), 
                        "b" = c(-1, 1, -1.5, .5, 1.5, 2), 
                        "c" = c(0.2, 0.2, .25, .2, 0.25, .25))
theta <- seq(-4,4,0.01)

mat.prob1 <- mat.prob2 <- data.frame(theta)

# Gráfico das CCI"s para o teste 1 

for (i in 1:nrow(mat.par.1)) {
  mat.prob1[paste("i", i, sep = "")] <-  mat.par.1$c[i] + (1-mat.par.1$c[i])/
    (1+exp(-mat.par.1$a[i]*(theta-mat.par.1$b[i])))
}

# Gráfico das CCI"s para o teste 2

for (i in 1:nrow(mat.par.2)) {
  mat.prob2[paste("i", i, sep = "")] <-  mat.par.2$c[i] + (1-mat.par.2$c[i])/
    (1+exp(-mat.par.2$a[i]*(theta-mat.par.2$b[i])))
}


mat.prob1$teste <- "Teste 1"
mat.prob2$teste <- "Teste 2"

rbind(mat.prob1, mat.prob2) %>% 
  melt(id.vars = c("theta", "teste")) %>%
  ggplot(aes(theta, value, color = variable)) + geom_line() +
  facet_wrap(~teste) +
  labs(color = "Item") + theme_bw()

# d) 
vec.prob.1 <- mat.par.1[,3] + (1-mat.par.1[,3])/(1+exp(-mat.par.1[,1]*(0-mat.par.1[,2])))
vec.prob.1 
vec.prob.2 <- mat.par.2[,3] + (1-mat.par.2[,3])/(1+exp(-mat.par.2[,1]*(0-mat.par.2[,2])))
vec.prob.2 

# Questão 2

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

# Questao 4

theta.Z <- 1
theta.Y <- -1

t1 <- t2 <- data.frame("i" = 1:6)

t1$p.Z <- mat.par.1$c + (1-mat.par.1$c)/(1+exp(-mat.par.1$a*(theta.Z-mat.par.1$b)))
t1$p.Y <- mat.par.1$c + (1-mat.par.1$c)/(1+exp(-mat.par.1$a*(theta.Y-mat.par.1$b)))
t1$dif <- t1$p.Z - t1$p.Y

t2$p.Z <- mat.par.2$c + (1-mat.par.2$c)/(1+exp(-mat.par.2$a*(theta.Z-mat.par.2$b)))
t2$p.Y <- mat.par.2$c + (1-mat.par.2$c)/(1+exp(-mat.par.2$a*(theta.Y-mat.par.2$b)))
t2$dif <- t2$p.Z - t2$p.Y

kable(t1, align = "c", col.names = c("Item", "p.Z", "p.Y", "Dif"))

kable(t2, align = "c", col.names = c("Item", "p.Z", "p.Y", "Dif"))
