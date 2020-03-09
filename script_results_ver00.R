########################################################################
## Description: Padronização dos resultados do artigo do aluno Felipe
## Dalcin...
##
## Maintainer: UNIVALI / Pós-Graduação em Big Data
## Author: Rodrigo Sant'Ana
## Created: seg mar  2 15:58:51 2020 (-0300)
## Version: 0.0.1
##
## URL:
## Doc URL:
##
## Database info:
##
### Commentary:
##
### Code:
########################################################################

########################################################################
######@> Carregando os pacotes R...

######@> Lista de pacotes...
library(caret)
library(naivebayes)
library(C50)
library(tidyverse)
library(ggRandomForests)
library(randomForestExplainer)
library(viridis)
library(RColorBrewer)
library(scales)

########################################################################
######@> Configurando algumas coisas no R...

######@> Tema dos plots...
seta <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open")
my_theme <- function (base_size = 14, base_family = "Arial") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(axis.ticks = element_blank(),
              axis.line = element_line(arrow = seta),
              legend.background = element_blank(),
              legend.key = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              strip.background = element_blank(),
              plot.background = element_blank(),
              complete = TRUE)
}

########################################################################
######@> Carregando os resultados dos modelos...

######@> Lista de modelos...
mod.svm <- readRDS("models/support_vector_machine.rds")
mod.knn <- readRDS("models/k_nearest_neighbors.rds")
mod.bayes <- readRDS("models/naive_bayes.rds")
mod.tree <- readRDS("models/decision_tree.rds")
mod.forest <- readRDS("models/random_forest.rds")

########################################################################
######@> Carregando as bases de dados...

######@> Base de dados para teste...
teste <- read.table("data/pmp-necropsia-testing.csv", header = TRUE,
                    sep = ",", dec = ".")

######@> Base de dados para treino...
treino <- read.table("data/pmp-necropsia-training.csv", header = TRUE,
                     sep = ",", dec = ".")

######@> Base de dados para predição...
pred <- read.table("data/pmp-necropsia-final.csv", header = TRUE,
                   sep = ",", dec = ".")

########################################################################
######@> Consolidação dos resultados para o Artigo...

######@> Verificando o melhor modelo a partir dos ajustes - método de
######@> reamostragem...
tmp01 <- resamples(list(svm = mod.svm, knn = mod.knn, bayes = mod.bayes,
                        tree = mod.tree, forest = mod.forest))

######@> Predizendo os resultados para todos os modelos...

#####@> Teste...
teste$pred.svm <- predict(mod.svm, teste)
teste$pred.knn <- predict(mod.knn, teste)
teste$pred.bayes <- predict(mod.bayes, teste)
teste$pred.tree <- predict(mod.tree, teste)
teste$pred.forest <- predict(mod.forest, teste)

#####@> Treino...
treino$pred.svm <- predict(mod.svm, treino)
treino$pred.knn <- predict(mod.knn, treino)
treino$pred.bayes <- predict(mod.bayes, treino)
treino$pred.tree <- predict(mod.tree, treino)
treino$pred.forest <- predict(mod.forest, treino)

######@> Matrizes de confusão...
mat.svm <- confusionMatrix(treino$pred.svm, treino$interacao_tipo)
mat.knn <- confusionMatrix(treino$pred.knn, treino$interacao_tipo)
mat.bayes <- confusionMatrix(treino$pred.bayes, treino$interacao_tipo)
mat.tree <- confusionMatrix(treino$pred.tree, treino$interacao_tipo)
mat.forest <- confusionMatrix(treino$pred.forest, treino$interacao_tipo)

#####@> Tabela com a comparação da acurácia dos modelos - em termos de
#####@> sua capacidade preditiva...
tab02 <- data.frame(Modelo = c("SVM", "KNN", "NaiveBayes",
                               "Árvore de Decisão", "Random Forest"),
                    Acurácia = c(mat.svm$overall[1],
                                 mat.knn$overall[1],
                                 mat.bayes$overall[1],
                                 mat.tree$overall[1],
                                 mat.forest$overall[1]),
                    I.C.inf = c(mat.svm$overall[3],
                                mat.knn$overall[3],
                                mat.bayes$overall[3],
                                mat.tree$overall[3],
                                mat.forest$overall[3]),
                    I.C.sup = c(mat.svm$overall[4],
                                mat.knn$overall[4],
                                mat.bayes$overall[4],
                                mat.tree$overall[4],
                                mat.forest$overall[4]),
                    P.valor = c(mat.svm$overall[6],
                                mat.knn$overall[6],
                                mat.bayes$overall[6],
                                mat.tree$overall[6],
                                mat.forest$overall[6]),
                    Kappa = c(mat.svm$overall[2],
                              mat.knn$overall[2],
                              mat.bayes$overall[2],
                              mat.tree$overall[2],
                              mat.forest$overall[2]),
                    Tempo = c(3149, 68, 86, 481, 4028))
tab02 <- arrange(tab02, desc(Acurácia))

######@> Com base nos resultados alcançados, o melhor modelo foi o
######@> Random Forest - Utilizando este modelo para as análises
######@> finais...

#####@> Importancia das variáveis na explicação do modelo final...
tab03 <- varImp(mod.forest, scale = TRUE)$importance
tab03$Var <- rownames(tab03)
rownames(tab03) <- 1:nrow(tab03)
tab03 <- arrange(tab03, Overall)
tab03$Var <- factor(tab03$Var, levels = tab03$Var)

#####@> Gráfico...
p00 <- ggplot(tab03, aes(x = Var, y = Overall)) +
    geom_segment(aes(y = 0,
                     x = Var,
                     yend = Overall,
                     xend = Var),
                 color = "#636969") +
    geom_point(stat = "identity", fill = "#636969", size = 6, pch = 21)  +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 120, 20)) +
    labs(x = "Variável",
         y = "Contribuição") +
    coord_flip() +
    my_theme(base_size = 18)
p00

######@> Tabela com descritivo completo do modelo Random Forest...
tab04 <- mat.forest$byClass

######@> Comparando o predito com o observado...
tab05 <- as.data.frame(mat.forest$table)

#####@> Categorias de frequencia...
tab05$Class <- ifelse(tab05$Freq == 0, "Sem erro",
               ifelse(tab05$Freq <= 100, "Baixo",
               ifelse(tab05$Freq <= 200, "Médio", "Alto")))

#####@> Gráfico...
p01 <- ggplot(data = tab05,
              aes(x = Reference, y = Prediction,
                  fill = Class, label = Freq)) +
    geom_tile(colour = "black", alpha = 0.8) +
    geom_text(size = 8) +
    scale_fill_manual(values = rev(c("white", "#FC9272", "#FEE0D2",
                                     "#DE2D26"))) +
    labs(x = "Observado", y = "Predito") +
    my_theme() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none")
p01

######@> Predição final, para o total não observado...

#####@> Removendo os NAs...
pred2 <- pred[, -c(2)]
pred2 <- pred2[complete.cases(pred2),]

#####@> Predizando...
pred2$pred.forest <- predict(mod.forest, pred2)

#####@> Figura...
tab06 <- pred2 %>%
    group_by(pred.forest) %>%
    summarise(N = n()) %>%
    as.data.frame()
p02 <- ggplot(data = tab06, aes(x = pred.forest, y = N, label = N)) +
    geom_bar(stat = "identity", colour = "black", fill = "#636969") +
    geom_label(nudge_y = 450, size = 8) +
    scale_y_continuous(limits = c(0, 15000), expand = c(0, 0)) +
    labs(x = "Tipo de Interação", y = "Predição") +
    my_theme(base_size = 18)
p02

########################################################################
##
##                  Creative Commons License 4.0
##                       (CC BY-NC-SA 4.0)
##
##  This is a humam-readable summary of (and not a substitute for) the
##  license (https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode)
##
##  You are free to:
##
##  Share - copy and redistribute the material in any medium or format.
##
##  The licensor cannot revoke these freedoms as long as you follow the
##  license terms.
##
##  Under the following terms:
##
##  Attribution - You must give appropriate credit, provide a link to
##  license, and indicate if changes were made. You may do so in any
##  reasonable manner, but not in any way that suggests the licensor
##  endorses you or your use.
##
##  NonCommercial - You may not use the material for commercial
##  purposes.
##
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
##
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
##
########################################################################
