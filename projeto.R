# Evaldo Garcia de Souza Junior (egsj) -----------------
# Matheus Luiz Borba Alves da Silva (mlbas) ------------
# Rodrigo de Lima Oliveira (rlo) -----------------------

# install.packages("readr")
library(readr) 

# inicio da questao (1) --------------------------------
# nesta questao so foi criar o data frame com a planilha
# do jeito que ela ta inicialmente

dfgot1 <- read_csv("C:/Users/Evaldo Júnior/Desktop/PlanilhaGOT.csv")
# mudar para onde tiver localizado o arquivo da planilha no seu pc
dfgot <- data.frame(dfgot1)
View(dfgot)
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


# inicio da questao (5) --------------------------------
# nesta questao separamos cada temporada por subset e 
# achamos o indice dos eps que possuem maior e menor nota.
# com isso, fizemos um data frame adicionando a linha que 
# indica o menor e maior indice de cada temprada. a linha 
# contem as informaçoes de nome, nota e temporada.

funct2 <- function(x) {
  res <- data.frame()
  for (i in 1:8) {
    l <- subset(x, x$Temporada==i, select = c(2, 3, 1))
    min <- which.min(l[, 2])
    max <- which.max(l[, 2])
    res <- rbind(res, l[min,])
    res <- rbind(res, l[max,])
  }
  return(print(res))
}
funct2(dfgot) # retorna nome, nota e temporada de eps 
# com maiores e menores notas de cada temporada


# inicio da questao (6) --------------------------------
# nesta questao fizemos um subset para cada temporada e
# calculamos o desvio padrao de cada uma delas e adicionamos
# a um data frame apenas contendo o desvio padrao e a temp.
# com isso, utilizamos a função "which.min" da coluna de 
# desvio padrao e pegamos o indice do menor dele, usando ele
# na coluna das temporadas pra achar a qual se referia

funct3 <- function(x){
    resposta <- data.frame()
    for (i in 1:8) {
      k <- subset(x, x$Temporada==i, select = c(1, 5))
      sdi <- sd(k[,2])
      resposta <- rbind(resposta, c(sdi, i))
    }
    mini <- which.min(resposta[,1])
    return(print(paste("Temporada", resposta[mini,2])))
}
funct3(dfgot) # retorna qual temp teve o menor desvio padrao


# inicio da questao (7) --------------------------------
# nesta questao separamos os personagens de cada episodio
# e colocamos em um vetor para assim usar a busca linear e
# conferir se a Brienne faz parte do ep. se sim, adicionamos
# a nota do ep a um vetor(isso pra todos os eps) e depois 
# calculamos a media dele, resultando na media das notas
# dos eps que ela faz parte

funct4 <- function(x){
  notasss <- c()
  for (i in 1:length(x$Nota)) {
    pers <- unlist(strsplit(x[i,4], ","))
    for (j in 1:length(pers)) {
      if (pers[j]=="Brienne of Tarth(Gwendoline Christie)"){
        notasss <- rbind(notasss, x[i,3])
      }
    }
  }
  resp <- mean(notasss)
  return(resp)
}
funct4(dfgot) # retorna a media dos eps que Brienne faz parte


# inicio da questao (8) --------------------------------
# O objetivo é medir na temporada 4, quais personagens só 
# aparecerem uma vez. Pra isso percorremos toda a tabela 
# procurando apenas pelas infos da temporada 4 (poderiamos 
# fazer um subset só a temporada 4 também, por questão de 
# otmização, mas preferimos ir pelo mais simples). Quando 
# restringimos apenas para temporada 4 damos um split pra 
# fazer um vetor com os nomes separado por virgula, depois
# passamos pra um vector que tem um objeto com frequencia e 
# nome, por fim selecionamos apenas os com menor frequencia
# (frequencia 1 = apareceu uma vez)

funct5 <- function(x){
  char <- c()
  for (i in 1:length(dfgot$Temporada)) {
    if(dfgot[i,1]==4){
      pers <- unlist(strsplit(as.character(dfgot[i,4]), ","))
      char <- c(char, pers)
    }
  }
  freq <- table(char)
  resposta1 <- list(names(freq)[freq == min(freq)])
  return(resposta1)
}
funct5(dfgot) # retorna uma lista de nomes que apareceram em um unico episodio na temp 4


# inicio da questao (9) --------------------------------
# Como o objetivo da questão é medir a frequencia de 
# aparições de cada personagem(input) por temporada, 
# criamos um laço de repetição que passa por todos os 
# episodios da serie checando se o personagem x apareceu 
# ou não, ao fim temos uma relação entre a quantidade de 
# aparições e a temporada que pode ser observada através
# do verctor ocorrencia que guarda essa "relação" 
# frenquencia - temporada

funct6 <- function(name){
  ocorrencia <- c()
  contadorTemp = 1
  for (i in 1:length(dfgot$Nota)) {
    pers1 <- as.character(dfgot[i,4])
    if(contadorTemp == dfgot[i,1]){
      if(grepl(name, pers1)){
        ocorrencia <- c(ocorrencia, contadorTemp)
      }
    } else {
      contadorTemp <- contadorTemp+1
    }
  }
  hist(ocorrencia,
       main = paste(name),
       xlab = "Temporada",
       ylab = "Ocorrencia", 
       breaks = c(0,1,2,3,4,5,6,7,8),
       col = "red") 
}
funct6(name = "Brienne") # adicionar nome aqui


# end --------------------------------------------------
