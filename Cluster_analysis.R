library(GGally)
library(NbClust) 


setwd("C:/Users/Usuario/Dropbox/MIP/Processamento de papers/Em processo/Paper Daiane/Analises/Agrupamento")
entrada<-read.table("agrupamento.txt", header = TRUE)

#exploratorio
head(entrada)
str(entrada)
ggpairs(entrada, title = "Scatterplot Matrix")
dados <- entrada[c(1, 3:4)]
str(dados)


#testando os metodos de agrupamento
##metodo 1 - vizinho mais proximo
dis<-dist(dados, method = "euclidean")^2 
agr1<-hclust(dis, method = "single") 
plot(agr1,hang=-1,xlab="",ylab="DDistancia Euclidiana Quadratica",main="vizinho mais proximo") 
#agr1$height #pontos de fusao
mojena1=mean(agr1$height)+1.25*sd(agr1$height)#criterio de Mojena (determina o numero de grupos (altura do corte no agrupamento))
k1=length(agr1$height[agr1$height>mojena1])+1 
k1
rect.hclust(agr1,k=k1)#plotando
gru1<-cutree(agr1,k=k1)#extraindo
gru1

##metodo 2 - vizinho mais distante
agr2<-hclust(dis, method = "complete") 
plot(agr2,hang=-1,xlab="",ylab="Distancia Euclidiana Quadratica",main="vizinho mais distante") 
#agr2$height #pontos de fusao
mojena2=mean(agr2$height)+1.25*sd(agr2$height)#criterio de Mojena (determina o numero de grupos (altura do corte no agrupamento))
k2=length(agr2$height[agr2$height>mojena2])+1 
k2
rect.hclust(agr2,k=k2)#plotando
gru2<-cutree(agr2,k=k2)#extraindo
gru2

##metodo 3 - UPGMA
agr3<-hclust(dis, method = "average") 
plot(agr3,hang=-1,xlab="",ylab="DDistancia Euclidiana Quadratica",main="UPGA") 
#agr3$height #pontos de fusao
mojena3=mean(agr3$height)+1.25*sd(agr3$height)#criterio de Mojena (determina o numero de grupos (altura do corte no agrupamento))
k3=length(agr3$height[agr3$height>mojena3])+1 
k3
rect.hclust(agr3,k=k3)#plotando
gru3<-cutree(agr3,k=k3)#extraindo
gru3

##metodo 4a - Ward (1)
agr4a<-hclust(dis, method = "ward.D") 
plot(agr4a,hang=-1,xlab="",ylab="Distancia Euclidiana Quadratica",main="Ward (1)") 
#agr4a$height #pontos de fusao
mojena4a=mean(agr4a$height)+1.25*sd(agr4a$height)#criterio de Mojena (determina o numero de grupos (altura do corte no agrupamento))
k4a=length(agr4a$height[agr4a$height>mojena4a])+1 
k4a
rect.hclust(agr4a,k=k4a)#plotando
gru4a<-cutree(agr4a,k=k4a)#extraindo
gru4a

##metodo 4b - Ward (2)
agr4b<-hclust(dis, method = "ward.D2") 
plot(agr4b,hang=-1,xlab="",ylab="Distancia Euclidiana Quadratica",main="Ward (2)") 
#agr4b$height #pontos de fusao
mojena4b=mean(agr4b$height)+1.25*sd(agr4b$height)#criterio de Mojena (determina o numero de grupos (altura do corte no agrupamento))
k4b=length(agr4b$height[agr4b$height>mojena4b])+1 
k4b
rect.hclust(agr4b,k=k4b)#plotando
gru4b<-cutree(agr4b,k=k4b)#extraindo
gru4b

#Ng<-NbClust(dados, diss=NULL, distance ="euclidean", min.nc=2, max.nc=4, method = "ward.D2", index= "kl")
#Ng$Best.nc

#verificando o grau de ajuste dos metodos
##correlacao cofenetica
dis1<-cophenetic(agr1) 
cor1<-cor(dis,dis1)#vizinho mais proximo
dis2<-cophenetic(agr2) 
cor2<-cor(dis,dis2)#vizinho mais distante
dis3<-cophenetic(agr3) 
cor3<-cor(dis,dis3)#UPGA
dis4a<-cophenetic(agr4a) 
cor4a<-cor(dis,dis4a)#Ward (1)
dis4b<-cophenetic(agr4b) 
cor4b<-cor(dis,dis4b)#Ward (2)

#comparando os metodos
corger<-c(cor1, cor2, cor3, cor4a, cor4b)#correlacao cofenetica
corger
gruger<-c(k1, k2, k3, k4a, k4b)#numero de grupos (criterio de Mojena)
gruger

#modelo final
##metodo 5 - Ward (2) - k=5
agr5<-hclust(dis, method = "ward.D2") 
plot(agr5,hang=-1,xlab="",ylab="Distancia Euclidiana Quadratica",main="Ward (2) - k=5") 
#agr5$height #pontos de fusao
k5=5
rect.hclust(agr5,k=k5)#plotando
gru5<-cutree(agr5,k=k5)#extraindo
gru5

