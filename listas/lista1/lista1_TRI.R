pacman::p_load(tidyverse,kableExtra,irtoys,ltm,mirt)

altura <- read.fwf(file="listas/lista1/altura211.txt", widths=c(3,4,rep(1,14)),dec=',')

# insere nomes nas colunas
colnames(altura) <- c("id","altura",paste(c("i"),1:14,sep=""))
altura.itens <- altura[,3:16] # utilizando apenas as colunas de respostas
altura.desc <- descript(altura.itens)



# Questao 1

# correlação ponto-bisserial
rho.PB <- altura.desc$bisCorr
rho.PB
# correlação bisserial
rho.B.vec <- rep(0,14)

for (i in 1:14) {
  pp <- colSums(altura.itens)[i]/nrow(altura.itens)
  rho.B <- sqrt(pp*(1-pp))*altura.desc$bisCorr[i]/dnorm(qnorm(pp,0,1),0,1)
  rho.B.vec[i] <- rho.B
}
kable(data.frame(rho.PB, rho.B.vec), 
      col.names = c("Ponto-bisserial", "Bisserial"), align = "c")

# Questao 2 

plot(altura.desc$bisCorr, rho.B.vec, xlab=c("correlação ponto bisserial"),
     ylab=c("correlação bisserial"),xlim=c(0,1),ylim=c(0,1))
abline(0,1)


# Questao 3 

mu <- mean(altura$altura)
desvio <- sd(altura$altura)

t <- apply(altura.itens,1,sum)
t_media <- mean(t)
s <- sd(t)

## escore padronizado 
zj <- (t-t_media)/s

## altura estimada
hj <- mu + desvio*zj

## correlação entre alturas reais e estimadas

(cor(altura$altura, hj))

plot(altura$altura, hj, xlab=c("altura real"),
     ylab=c("estimada"))


# Questao 4

cronbach.alpha(altura.itens)


# Questao 5

(1-apply(altura.itens,2,sum)/nrow(altura.itens))

# Questao 6

escore <- apply(altura.itens,1,sum)
aux <- ceiling(0.27*nrow(altura.itens))
escore.inf <- sort(escore)[aux]
escore.sup <- sort(escore)[nrow(altura.itens)-aux]

altura.inf <- altura.itens[escore<=escore.inf,]
altura.sup <- altura.itens[escore>=escore.sup,]

apply(altura.sup,2,sum)/nrow(altura.sup)-apply(altura.inf,2,sum)/nrow(altura.inf)
