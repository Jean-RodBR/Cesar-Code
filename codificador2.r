#quebra código
library(magrittr)
library(openxlsx)
#codificador ------ código funciona em ordem crescente

tabela.alfabeto <- read.xlsx("utf8-tabela.xlsx",1)[34:126,2] %>% unlist() 
texto <- c("sustain spread false excess type ginger churn panda sketch ginger usual reopen brave") %>% toupper() 


j<-17 #POR ENQUANTO NÃO VOU DEIXAR ALEATÓRIO
#j <- runif(1,5,80) %>% round() #gerar j de forma aleatória
#quantos pular, rnorm gera numeros aleatorios com distruibuição normal, round sigfnifica arrendodnr e abs torna o número absoluto
#somei 2 para sempre dar um número acima de zero 

n <- nchar(texto)
save.chr <- c()
save.chr2 <- c()
pos <- c()

for(i in 1:n){
  save.chr[i] <- substr(texto, i,i)
  
  if(save.chr[i] != " "){
      pos <- which(save.chr[i] == tabela.alfabeto) #posição da letra que quero no alfabeto
      ifelse(pos+j > length(tabela.alfabeto),
           save.chr2[i] <- tabela.alfabeto[pos+j-length(tabela.alfabeto)],
           save.chr2[i] <- tabela.alfabeto[pos+j])
      
      }
 
   if(save.chr[i] == " "){
    save.chr2[i] <- " "
  }
  
}

texto2 <- save.chr2 %>% paste(., collapse=', ' ) %>% gsub(", ", "",.)
print(texto2)


#---------------------------------------------------------------------------------
#decodificador
texto3 <- texto2 

n <- nchar(texto3)
save.chr <- c()
save.chr2 <- c()
pos <- c()
for(i in 1:n){
  save.chr[i] <- substr(texto3, i,i)
  
  if(save.chr[i] != " "){
    pos <- which(save.chr[i] == tabela.alfabeto) #posição da letra que quero no alfabeto
    ifelse(pos-j < 2,
           save.chr2[i] <- tabela.alfabeto[pos-j+length(tabela.alfabeto)-1],
           save.chr2[i] <- tabela.alfabeto[pos-j])
  }
  if(save.chr[i] == " "){
    save.chr2[i] <- " "
  }
}

texto3 <- save.chr2 %>% paste(., collapse=', ' ) %>% gsub(", ", "",.)
print(texto3)
#  which(save.chr == "D") #encontrando a posição de cada letra

