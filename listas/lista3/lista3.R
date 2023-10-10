# Pacotes
pacman::p_load(tidyverse,reshape2,knitr,irtoys,ltm,mirt)

altura <- read.fwf(file="listas/lista3/altura211.txt", widths=c(3,4,rep(1,14)),dec=',')

no.item <- ncol(altura[, 3:16])
theta   <- seq(-4, 4, 0.01)

# modelo ajustado
altura.tpm <- tpm(altura[,3:16],constraint=cbind(1:no.item,1,0))

### A funcao "coef" fornece as estimativas dos parametros dos itens.

par.est <- coef(altura.tpm) # cc, bb, aa

ml2 <- data.frame("i" = 1:14, par.est[, 2], par.est[, 3])
rownames(ml2) <- NULL

kable(ml2, col.names = c("Item", "Dificuldade", "Discriminação"), align = "c")

####################################
df <- data.frame(theta)

for (i in 1:no.item) {
  col <- paste("i", i, sep = "")
  df[col] <- 1/(1+exp(-par.est[i, 3]*(theta-par.est[i, 2])))
}

melt(df, id.vars = "theta") %>%
  ggplot(aes(theta, value, color = variable)) + geom_line() +
  labs(color = "Item", x = "theta") +
  theme_bw()

####################################
mat.prob <- mat.prob.dif <- mat.info <- data.frame(theta)

for (i in 1:no.item) {
  col <- paste("i", i, sep = "")
  mat.prob[col] <- par.est[i, 1] + 
    (1-par.est[i,1])/(1+exp(-par.est[i, 3]*(theta-par.est[i, 2])))
  mat.prob.dif[col] <- par.est[i, 3]*(1-par.est[i, 1])*exp(-par.est[i, 3]*(theta-par.est[i, 2]))/((1+exp(-par.est[i, 3]*(theta-par.est[i, 2])))^2)
  mat.info[col] <- (mat.prob.dif[col]^2)/(mat.prob[col]*(1-mat.prob[col]))
}

mat.info %>%
  melt(id.vars = "theta") %>%
  ggplot(aes(theta, value, color = variable)) + geom_line() +
  labs(color = "Item", y = "Informação") +
  theme_bw()

####################################
mat.info$info <- rowSums(mat.info[-1])

ggplot(mat.info, aes(theta, info)) + geom_line() +
  geom_vline(xintercept = 0, color = "red") +
  labs(y = "Informação") +
  theme_bw()


####################################

theta.est.eap <- eap(altura[3:16], 
                     cbind(par.est[, 3], par.est[, 2], par.est[, 1]),
                     qu = normal.qu())

theta.est <- mean(altura[, 2]) + sd(altura[, 2])*theta.est.eap[, 1]

ggplot(NULL, aes(altura[, 2], theta.est)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Altura real", y = "Altura estimada") +
  theme_bw()

####################################

(cc <- cor(altura[, 2],theta.est))

####################################

escore <- apply(altura[, 3:16],1,sum)
escore.padr <- (escore-mean(escore))/sd(escore)
theta.est.tct <- mean(altura[, 2]) + sd(altura[, 2])*escore.padr

ggplot(NULL, aes(theta.est.tct, theta.est)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Altura estimada via TCT", y = "Altura estimada via TRI") +
  theme_bw()

####################################

(cc <- cor(theta.est.tct,theta.est))


