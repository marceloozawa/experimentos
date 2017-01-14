library(caret)

source("/home/marcelo/experimentos/gerarBaseStratificada.R")

# Local da pasta IndependenteAlgoritmo
raiz = "/home/marcelo/experimentos/bases/IndependenteAlgoritmo"
setwd(raiz)

bases = c("STREN_LV1", "STREN_LV2", "TFIDF_LV1", "TFIDF_LV2", "WORDS_LV1", "WORDS_LV2")
labels = c("Alegria", "Desgosto", "Medo", "Neutro", "Raiva", "Surpresa", "Tristeza")
methods = c("rf", "svmRadial", "rpart", "mlp", "knn", "nb")

colnames(resultados) = c("T?cnica", "P/C", "Classe", "Descritor", "LV", "N", "Acerto")
tec = vector()
prim = vector()
classe = vector()
descritor = vector()
lv = vector()
metodo = vector()
acerto = vector()

cont = 1


#                      --- ELIMINA??O DE EXEMPLOS ---

tecnica = "EE"
pc = "-"

path.ini = "BaseadoExemplos/EliminacaoExemplos/"
path.fim = ".csv"

for(i in 1:1) {
    for(k in 6:6) {
      path = paste(path.ini, bases[i],  "/", "BE_", tecnica, "_", bases[i], path.fim, sep = "")
      dataset = read.csv(path, header = TRUE)
      predictors = names(dataset)[names(dataset) != "class"]
      
      strat = gerarBaseStratificada(path)
      
      for(x in 1:10) {
        strat[[x]] = as.integer(strat[[x]])
      }
      
      ctrl = trainControl(method = "cv", index = strat, number = 10)
      model = caret::train(class ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
      
      aux = unlist(strsplit(bases[i], "_"))
      
      tec[cont] = tecnica
      prim[cont] = pc
      classe[cont] = "-------"
      descritor[cont] = aux[1]
      lv[cont] = aux[2]
      metodo[cont] = methods[k]
      acerto[cont] = model$results$Accuracy[as.numeric(row.names(model$bestTune[1]))]
      cont = cont + 1
    }
}


   #resultados = data.frame(tec, prim, classe, descritor, lv, metodo, acerto)
# #                      --- CRIA??O DE R?TULOS (PRIM?RIO) ---
# 
# tecnica = "CR"
# pc = "P"
# 
# path.ini = "BaseadoExemplos/CriacaoRotulos/Primario/"
# path.fim = ".csv"
# 
# for(i in 1:1) {
#   for(k in 5:5) {
#     path = paste(path.ini, bases[i],  "/", "BE_", tecnica, "_", pc, "_", bases[i], path.fim, sep = "")
#     dataset = read.csv(path, header = TRUE)
#     predictors = names(dataset)[names(dataset) != "class"]
#     
#     strat = gerarBaseStratificada(path)
#     
#     for(x in 1:10) {
#       strat[[x]] = as.integer(strat[[x]])
#     }
#     
#     ctrl = trainControl(method = "cv", index = strat, number = 10)
#     model = caret::train(class ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
#     
#     aux = unlist(strsplit(bases[i], "_"))
#     
#     tec[cont] = tecnica
#     prim[cont] = pc
#     classe[cont] = "-------"
#     descritor[cont] = aux[1]
#     lv[cont] = aux[2]
#     metodo[cont] = methods[k]
#     acerto[cont] = model$results$Accuracy[as.numeric(row.names(model$bestTune[1]))]
#     
#     cont = cont + 1
#   }
# }
# resultados = data.frame(tec, prim, classe, descritor, lv, metodo, acerto)
# #                      --- CRIA??O DE R?TULOS (COMPLETO) ---
# 
# tecnica = "CR"
# pc = "C"
# 
# path.ini = "BaseadoExemplos/CriacaoRotulos/Completo/"
# path.fim = ".csv"
# 
# for(i in 1:1) {
#   for(k in 5:5) {
#     path = paste(path.ini, bases[i],  "/", "BE_", tecnica, "_", pc, "_", bases[i], path.fim, sep = "")
#     dataset = read.csv(path, header = TRUE)
#     predictors = names(dataset)[names(dataset) != "class"]
#     
#     model = caret::train(class ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
#     
#     aux = unlist(strsplit(bases[i], "_"))
#     
#     tec[cont] = tecnica
#     prim[cont] = pc
#     classe[cont] = "-------"
#     descritor[cont] = aux[1]
#     lv[cont] = aux[2]
#     metodo[cont] = methods[k]
#     acerto[cont] = model$results$Accuracy[as.numeric(row.names(model$bestTune[1]))]
#     
#     cont = cont + 1
#   }
# }
# 
# resultados = data.frame(tec, prim, classe, descritor, lv, metodo, acerto)

#                      --- ELIMINA??O DE R?TULOS (PRIM?RIO) ---

tecnica = "ER"
pc = "P"

path.ini = "BaseadoExemplos/ConversaoExemplos/EliminacaoRotulos/Primario/"
path.fim = ".csv"

for(i in 1:6) {
  for(k in 6:6) {
    path = paste(path.ini, bases[i],  "/", "BE_CE_", tecnica, "_", pc, "_", bases[i], path.fim, sep = "")
    dataset = read.csv(path, header = TRUE)
    predictors = names(dataset)[names(dataset) != "class"]
    
    strat = gerarBaseStratificada(path)
    
    for(x in 1:10) {
      strat[[x]] = as.integer(strat[[x]])
    }
    
    ctrl = trainControl(method = "cv", index = strat, number = 10)
    model = caret::train(class ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
    
    aux = unlist(strsplit(bases[i], "_"))
    
    tec[cont] = tecnica
    prim[cont] = pc
    classe[cont] = "-------"
    descritor[cont] = aux[1]
    lv[cont] = aux[2]
    metodo[cont] = methods[k]
    acerto[cont] = model$results$Accuracy[as.numeric(row.names(model$bestTune[1]))]
    
    cont = cont + 1
  }
}
#resultados = data.frame(tec, prim, classe, descritor, lv, metodo, acerto)
#                      --- ELIMINA??O DE R?TULOS (COMPLETO) ---

tecnica = "ER"
pc = "C"

path.ini = "BaseadoExemplos/ConversaoExemplos/EliminacaoRotulos/Completo/"
path.fim = ".csv"

for(i in 1:6) {
  for(k in 6:6) {
    path = paste(path.ini, bases[i],  "/", "BE_CE_", tecnica, "_", pc, "_", bases[i], path.fim, sep = "")
    dataset = read.csv(path, header = TRUE)
    predictors = names(dataset)[names(dataset) != "class"]
    
    strat = gerarBaseStratificada(path)
    
    for(x in 1:10) {
      strat[[x]] = as.integer(strat[[x]])
    }
    
    ctrl = trainControl(method = "cv", index = strat, number = 10)
    model = caret::train(class ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
    
    aux = unlist(strsplit(bases[i], "_"))
    tec[cont] = tecnica
    prim[cont] = pc
    classe[cont] = "-------"
    descritor[cont] = aux[1]
    lv[cont] = aux[2]
    metodo[cont] = methods[k]
    acerto[cont] = model$results$Accuracy[as.numeric(row.names(model$bestTune[1]))]
    
    cont = cont + 1
  }
}

resultados = data.frame(tec, prim, classe, descritor, lv, metodo, acerto)


# #                      --- BASEADO EM RÓTULOS - COMPLETO ---
# 
# tecnica = "BRC"
# pc = "C"
# 
# path.ini = "BaseadoRotulosClasses/Completo/"
# path.fim = ".csv"
# 
# for(i in 1:1) {
#   for(j in 1:1) {
#     for(k in 1:1) {
#       path = paste(path.ini, bases[i],  "/", tecnica, "_", pc, "_", labels[j], "_", bases[i], path.fim, sep = "")
#       dataset = read.csv(path, header = TRUE)
#       if(i %% 2 == 0) {
#         rf.train = dataset[ , -17]
#         rf.label = as.factor(dataset[ , 17])
#       } else {
#         rf.train = dataset[ , -25]
#         rf.label = as.factor(dataset[ , 25])
#       }
#       rf1 = rf(rf.train, rf.label)
#       
#       aux = unlist(strsplit(bases[i], "_"))
#       
#       tec[cont] = tecnica
#       prim[cont] = pc
#       classe[cont] = labels[j]
#       descritor[cont] = aux[1]
#       lv[cont] = aux[2]
#       n[cont] = k
#       acerto[cont] = 1 - rf1$err.rate[500,1]
#       
#       cont = cont + 1
#     }
#   }
# }
# 
# 
# 
# #                      --- ELIMINAÇÃO DE EXEMPLOS ---
# 
# tecnica = "EE"
# pc = "-"
# 
# path.ini = "BaseadoExemplos/EliminacaoExemplos/"
# path.fim = ".csv"
# 
# for(i in 1:1) {
#     for(k in 1:1) {
#       path = paste(path.ini, bases[i],  "/", "BE_", tecnica, "_", bases[i], path.fim, sep = "")
#       dataset = read.csv(path, header = TRUE)
#       if(i %% 2 == 0) {
#         rf.train = dataset[ , -17]
#         rf.label = as.factor(dataset[ , 17])
#       } else {
#         rf.train = dataset[ , -25]
#         rf.label = as.factor(dataset[ , 25])
#       }
#       rf1 = rf(rf.train, rf.label)
#       
#       aux = unlist(strsplit(bases[i], "_"))
#       
#       tec[cont] = tecnica
#       prim[cont] = pc
#       classe[cont] = "-------"
#       descritor[cont] = aux[1]
#       lv[cont] = aux[2]
#       n[cont] = k
#       acerto[cont] = 1 - rf1$err.rate[500,1]
#       
#       cont = cont + 1
#     }
# }
# 
# 
# 
# #                      --- CRIAÇÃO DE RÓTULOS (Primário) ---
# 
# tecnica = "CR"
# pc = "P"
# 
# path.ini = "BaseadoExemplos/CriacaoRotulos/Primario/"
# path.fim = ".csv"
# 
# for(i in 1:1) {
#   for(k in 1:1) {
#     path = paste(path.ini, bases[i],  "/", "BE_", tecnica, "_", pc, "_", bases[i], path.fim, sep = "")
#     dataset = read.csv(path, header = TRUE)
#     if(i %% 2 == 0) {
#       rf.train = dataset[ , -17]
#       rf.label = as.factor(dataset[ , 17])
#     } else {
#       rf.train = dataset[ , -25]
#       rf.label = as.factor(dataset[ , 25])
#     }
#     rf1 = rf(rf.train, rf.label)
#     
#     aux = unlist(strsplit(bases[i], "_"))
#     
#     tec[cont] = tecnica
#     prim[cont] = pc
#     classe[cont] = "-------"
#     descritor[cont] = aux[1]
#     lv[cont] = aux[2]
#     n[cont] = k
#     acerto[cont] = 1 - rf1$err.rate[500,1]
#     
#     cont = cont + 1
#   }
# }
# 
# #                      --- CRIAÇÃO DE RÓTULOS (Completo) ---
# 
# tecnica = "CR"
# pc = "C"
# 
# path.ini = "BaseadoExemplos/CriacaoRotulos/Completo/"
# path.fim = ".csv"
# 
# for(i in 1:1) {
#   for(k in 1:1) {
#     path = paste(path.ini, bases[i],  "/", "BE_", tecnica, "_", pc, "_", bases[i], path.fim, sep = "")
#     dataset = read.csv(path, header = TRUE)
#     if(i %% 2 == 0) {
#       rf.train = dataset[ , -17]
#       rf.label = as.factor(dataset[ , 17])
#     } else {
#       rf.train = dataset[ , -25]
#       rf.label = as.factor(dataset[ , 25])
#     }
#     rf1 = rf(rf.train, rf.label)
#     
#     aux = unlist(strsplit(bases[i], "_"))
#     
#     tec[cont] = tecnica
#     prim[cont] = pc
#     classe[cont] = "-------"
#     descritor[cont] = aux[1]
#     lv[cont] = aux[2]
#     n[cont] = k
#     acerto[cont] = 1 - rf1$err.rate[500,1]
#     
#     cont = cont + 1
#   }
# }
# 
# 
# #                      --- ELIMINAÇÃO DE RÓTULOS (Primário) ---
# 
# tecnica = "ER"
# pc = "P"
# 
# path.ini = "BaseadoExemplos/ConversaoExemplos/EliminacaoRotulos/Primario/"
# path.fim = ".csv"
# 
# for(i in 1:1) {
#   for(k in 1:1) {
#     path = paste(path.ini, bases[i],  "/", "BE_CE_", tecnica, "_", pc, "_", bases[i], path.fim, sep = "")
#     dataset = read.csv(path, header = TRUE)
#     if(i %% 2 == 0) {
#       rf.train = dataset[ , -17]
#       rf.label = as.factor(dataset[ , 17])
#     } else {
#       rf.train = dataset[ , -25]
#       rf.label = as.factor(dataset[ , 25])
#     }
#     rf1 = rf(rf.train, rf.label)
#     
#     aux = unlist(strsplit(bases[i], "_"))
#     
#     tec[cont] = tecnica
#     prim[cont] = pc
#     classe[cont] = "-------"
#     descritor[cont] = aux[1]
#     lv[cont] = aux[2]
#     n[cont] = k
#     acerto[cont] = 1 - rf1$err.rate[500,1]
#     
#     cont = cont + 1
#   }
# }
# 
# #                      --- ELIMINAÇÃO DE RÓTULOS (Completo) ---
# 
# tecnica = "ER"
# pc = "C"
# 
# path.ini = "BaseadoExemplos/ConversaoExemplos/EliminacaoRotulos/Completo/"
# path.fim = ".csv"
# 
# for(i in 1:1) {
#   for(k in 1:1) {
#     path = paste(path.ini, bases[i],  "/", "BE_CE_", tecnica, "_", pc, "_", bases[i], path.fim, sep = "")
#     dataset = read.csv(path, header = TRUE)
#     if(i %% 2 == 0) {
#       rf.train = dataset[ , -17]
#       rf.label = as.factor(dataset[ , 17])
#     } else {
#       rf.train = dataset[ , -25]
#       rf.label = as.factor(dataset[ , 25])
#     }
#     rf1 = rf(rf.train, rf.label)
#     
#     aux = unlist(strsplit(bases[i], "_"))
#     
#     tec[cont] = tecnica
#     prim[cont] = pc
#     classe[cont] = "-------"
#     descritor[cont] = aux[1]
#     lv[cont] = aux[2]
#     n[cont] = k
#     acerto[cont] = 1 - rf1$err.rate[500,1]
#     
#     cont = cont + 1
#   }
# }


#resultados = data.frame(tec, prim, classe, descritor, lv, n, acerto)
#write.table(resultados, file="resultados_caret.csv", row.names = FALSE, sep = ",")

