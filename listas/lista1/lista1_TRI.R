pacman::p_load(tidyverse,kable,irtoys,ltm,mirt)

altura <- read.fwf(file="listas/lista1/altura211.txt", widths=c(3,4,rep(1,14)),dec=',')

# insere nomes nas colunas
colnames(altura) <- c("id","altura",paste(c("i"),1:14,sep=""))
altura.itens <- altura[,3:16] # utilizando apenas as colunas de respostas
altura.desc <- descript(altura.itens)



# Questao 1

# correlação ponto-bisserial
rho.PB <- altura.desc$bisCorr

# correlação bisserial
rho.B.vec <- rep(0,14)

for (i in 1:14) {
  pp <- colSums(altura.itens)[i]/nrow(altura.itens)
  rho.B <- sqrt(pp*(1-pp))*altura.desc$bisCorr[i]/dnorm(qnorm(pp,0,1),0,1)
  rho.B.vec[i] <- rho.B
}
kable(data.frame(rho.PB, rho.B.vec), 
      col.names = c("Ponto-bisserial", "Bisserial"), align = "c")

