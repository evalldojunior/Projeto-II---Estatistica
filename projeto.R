# Evaldo Garcia de Souza Junior (egsj) -----------------

# inicio da questao (1) --------------------------------
# nesta questao so foi criar o data frame com a planilha
# do jeito que ela ta inicialmente

dfgot <- data.frame(PlanilhaGOT)
dfgot  # retorna o data frame


# inicio da questao (2) --------------------------------
# nesta questao usamos as funçoes ja presentes
# em R para a media e o devio padrao, mas tivemos
# que fazer uma função para a moda

media <- mean(dfgot[,"Nota"])
media   # retorna a media das notas

desviopadrao <- sd(dfgot[,"Nota"])
desviopadrao   # retorna o desvio padrao das notas

fmoda <- function(x) {
  f <- table(as.vector(x)); names(f)[f == max(f)]
}
moda <- fmoda(dfgot[,"Nota"])
moda   # retorna a moda das notas


# inicio da questao (3) --------------------------------
# nesta questao usamos as funçoes presentes em R
# para calcular a media, desvio padrao e a mediana
# da audiencia. Utilizamos o "as.numeric" pois o
# tipo da coluna da audiencia estava inicialmente 
# como character, ou seja, fizemos uma conversao
# dos dados

media2 <- mean(as.numeric(dfgot[,5]))
media2   # retorna a media da audiencia

desviopadrao2 <- sd(as.numeric(dfgot[,5]))
desviopadrao2 # retorna o desvio padrao da audiencia

mediana <- median(as.numeric(dfgot[,5]))
mediana # retorna a mediana da audiencia 


# inicio da questao (4) --------------------------------
# nesta questao fizemos uma função chamada funct1. nela
# utilizamos comandos para saber os eps com notas acima 
# de 9 e retornar o nome de cada um dele

funct1 <- function(x) {
  notas <- x$Nota >= 9
  return(x[notas, 2])
}
funct1(dfgot) # listas de eps com nota maior ou igual a 9

# subset(dfgot, Nota >= 9, select = c(2))
# dfgot[dfgot$Nota >= 9, "Episodio"]


# inicio da questao (5) --------------------------------

# falta continuar ainda
