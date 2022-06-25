library(worldfootballR)
library(dplyr)
library(tidyr)
library(xlsx)


brasil_league_table_2022 <- get_season_team_stats(country = "BRA", gender = "M", season_end_year = "2022", tier = "1st", stat_type = "league_table")
brasil_league_table_2022_home_away <- get_season_team_stats(country = "BRA", gender = "M", season_end_year = "2022", tier = "1st", stat_type = "league_table_home_away")

BD_table <- brasil_league_table_2022 %>% select(5,7:16)
BD_table_Home_Away <- brasil_league_table_2022_home_away %>% select(5,8:25)


#View(brasil_league_table_2022)
#View(brasil_league_table_2022_home_away)
#View(BD_table_Home_Away)



BD<- left_join(BD_table, BD_table_Home_Away, by= c('Squad'))

BD$MP_Home <- as.integer(BD$MP_Home)
BD$W_Home <- as.integer(BD$W_Home)
BD$D_Home <- as.integer(BD$D_Home)
BD$L_Home <- as.integer(BD$L_Home)
BD$GF_Home <- as.integer(BD$GF_Home)
BD$GA_Home <- as.integer(BD$GA_Home)
BD$GD_Home <- as.integer(BD$GD_Home)
BD$Pts_Home <- as.integer(BD$Pts_Home)
BD$Pts_per_MP_Home <- as.double(BD$Pts_per_MP_Home)
BD$MP_Away <- as.integer(BD$MP_Away)
BD$W_Away <- as.integer(BD$W_Away)
BD$D_Away <- as.integer(BD$D_Away)
BD$L_Away <- as.integer(BD$L_Away)
BD$GF_Away <- as.integer(BD$GF_Away)
BD$GA_Away <- as.integer(BD$GA_Away)
BD$GD_Away <- as.integer(BD$GD_Away)
BD$Pts_Away <- as.integer(BD$Pts_Away)
BD$Pts_per_MP_Away <- as.double(BD$Pts_per_MP_Away)


#glimpse(BD)
#View(BD)
BDclusters <- BD

#musica
#alo barra funda jonas petroleo


#install.packages('caret')
library(caret)

#NORMALIZANDO DADOS E ARMAZENANDO NA VARIAVEL "DADOS"
dados<- scale(BDclusters[,c(2:29)])

#REALIZANDO PREDICAO COM ALGORITMO DE CLUSTERIZACAO E UTILIZANDO METODO DE NORMALIZACAO "SCALE"    
BDclusters <- predict(preProcess(BDclusters, method ="scale") ,BDclusters)
#COMANDO PARA GARANTIR QUE O LEITOR CHEGUE AO MESMO RESULTADO
set.seed(1)

#SEPARANDO DIFERENTES TIPOS DE CLUSTERS
G2 <- kmeans(BDclusters[2:29], centers=2)
G3 <- kmeans(BDclusters[2:29], centers=3)
G4 <- kmeans(BDclusters[2:29], centers=4)
G5 <- kmeans(BDclusters[2:29], centers=5)
G6 <- kmeans(BDclusters[2:29], centers=6)
G7 <- kmeans(BDclusters[2:29], centers=7)
G8 <- kmeans(BDclusters[2:29], centers=8)
G9 <- kmeans(BDclusters[2:29], centers=9)
G10 <- kmeans(BDclusters[2:29], centers=10)

library(factoextra) #fviz_cluster function
viz_G2 <- fviz_cluster(G2, data = BDclusters[2:29],palette = c("#2E9FDF", "#00AFBB"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G3 <- fviz_cluster(G3, data = BDclusters[2:29],palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G4 <- fviz_cluster(G4, data = BDclusters[2:29],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G5 <- fviz_cluster(G5, data = BDclusters[2:29],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G6 <- fviz_cluster(G6, data = BDclusters[2:29],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G7 <- fviz_cluster(G7, data = BDclusters[2:29],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
#Paletas de cores
#viz_G10 <- fviz_cluster(G10, data = BDclusters[2:29],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493", "#FF4500", "#FFFF00", "#B0E0E6", "#7B68EE", "#00FF00", "#00FFFF", "#00BFFF"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())

#Comparando grupos
library(gridExtra)
grid.arrange(viz_G2,viz_G3, viz_G4, viz_G5, nrow=2, ncol=2)

#INSERINDO OS MODELOS NO DATASET PARA EXPLORACAO
BD$G2 <- G2$cluster
BD$G3 <- G3$cluster
BD$G4 <- G4$cluster
BD$G5 <- G5$cluster
BD$G6 <- G6$cluster
BD$G7 <- G7$cluster


library(ggplot2)
par(mfrow=c(2,3))

#comparacao dos modelos por variavel especifica - pontos
boxplot(BD$Pts ~ BD$G2, col= c("#2E9FDF", "#00AFBB"), main="Pontos")
boxplot(BD$Pts ~ BD$G3, col= c("#2E9FDF", "#00AFBB", "#E7B800"), main="Pontos")
boxplot(BD$Pts ~ BD$G4, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF"), main="Pontos")
boxplot(BD$Pts ~ BD$G5, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), main="Pontos")
boxplot(BD$Pts ~ BD$G6, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000"), main="Pontos")
boxplot(BD$Pts ~ BD$G7, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000","#C71585"), main="Pontos")

#explicando a escolha do modelo 5
#usando a variavel pontos a mais relevante, o modelo 5 conseguiu separar de melhor forma os grupos de melhor e pior perfomance quando comparado com os outros grupos
#separando entre melhores, bons, medios, ruins, piores
#Proporcao razoavel de clubes por grupos
#criar grafico visualizando a quantidade de times por grupos comparando os modelos
#explicar homogeneidade dos boxplots..... tamanho das caixas


#Avaliando o modelo escolhido
#CLUSTERS
G5$cluster
#CENTROS DOS CLUSTERS
G5$centers
#TAMANHO DOS CLUSTERS
G5$size

fviz_cluster(G5, data = BDclusters[2:29],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), main="Modelo 5", geom = "point", ellipse.type = "convex", ggtheme = theme_bw())

#Proporcao da quantidade de individuos por grupo
round(table(BD$G5)*100/nrow(BD),2)
table(BD$G5)

par(mfrow=c(2,4))
#Explorando caracteristicas dos grupos
boxplot(BD$Pts ~ BD$G5, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), main="Pontos")
boxplot(BD$W ~ BD$G5, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), main="Vitorias")
boxplot(BD$D ~ BD$G5, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), main="Empates")
boxplot(BD$L ~ BD$G5, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), main="Derrotas")
boxplot(BD$GF ~ BD$G5, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), main="Gols feitos")
boxplot(BD$GA ~ BD$G5, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), main="Gols sofridos")
boxplot(BD$GD ~ BD$G5, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), main="Saldo de gols")
boxplot(BD$Pts.MP ~ BD$G5, col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), main="Pontos por partida")
#variaveis de jogos como mandante
#Destacando visualizacao de grupo especifico
boxplot(BD$MP_Home ~ BD$G5, col= c("#808080", "#808080", "#808080", "#ffa500", "#808080"), main="Gols feitos em casa", xlab="", ylab="")
boxplot(BD$GF_Home ~ BD$G5, col= c("#808080", "#808080", "#808080", "#ffa500", "#808080"), main="Gols feitos em casa", xlab="", ylab="")
boxplot(BD$W_Home ~ BD$G5, col= c("#808080", "#808080", "#808080", "#ffa500", "#808080"), main="Vitorias em casa", xlab="", ylab="")
boxplot(BD$D_Home ~ BD$G5, col= c("#808080", "#808080", "#808080", "#ffa500", "#808080"), main="Empates em casa", xlab="", ylab="")
boxplot(BD$L_Home ~ BD$G5, col= c("#808080", "#808080", "#808080", "#ffa500", "#808080"), main="Derrotas em casa", xlab="", ylab="")
boxplot(BD$GF_Home ~ BD$G5, col= c("#808080", "#808080", "#808080", "#ffa500", "#808080"), main="Gols feitos como mandante", xlab="", ylab="")
boxplot(BD$GA_Home ~ BD$G5, col= c("#808080", "#808080", "#808080", "#ffa500", "#808080"), main="Gols sofridos como mandante", xlab="", ylab="")
boxplot(BD$GD_Home ~ BD$G5, col= c("#808080", "#808080", "#808080", "#ffa500", "#808080"), main="Saldo de gols como mandante", xlab="", ylab="")
boxplot(BD$Pts_Home ~ BD$G5, col= c("#808080", "#808080", "#808080", "#ffa500", "#808080"), main="Pontos ganhos como mandante", xlab="", ylab="")
boxplot(BD$Pts_per_MP_Home ~ BD$G5, col= c("#808080", "#808080", "#808080", "#ffa500", "#808080"), main="Pontos por jogo como mandante", xlab="", ylab="")


par(mfrow=c(2,2))
ggplot(BD, aes(x=G5, y=GF_Home)) + 
  geom_bar(stat = "identity", width=0.2) 

ggplot(BD, aes(x=G5, y=GA_Home)) + 
  geom_bar(stat = "identity", width=0.2) 



?boxplot

#ffa500
#808080
#C0C0C0
glimpse(BD)

