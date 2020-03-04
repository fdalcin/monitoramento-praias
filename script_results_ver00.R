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
library(tidyverse)

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

########################################################################
######@> Consolidação dos resultados para o Artigo...




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
