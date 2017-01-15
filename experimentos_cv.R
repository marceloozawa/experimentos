library(caret)

# Local da pasta IndependenteAlgoritmo
raiz = "/home/marcelo/experimentos/bases/IndependenteAlgoritmo"
setwd(raiz)

bases = c("STREN_LV1", "STREN_LV2", "TFIDF_LV1", "TFIDF_LV2", "WORDS_LV1", "WORDS_LV2")
labels = c("Alegria", "Desgosto", "Medo", "Neutro", "Raiva", "Surpresa", "Tristeza")
methods = c("rf", "svmRadial", "rpart", "mlp", "knn", "nb")

# colnames(resultados) = c("Tecnica", "P/C", "Classe", "Descritor", "LV", "N", "Acerto")
tec = vector()
prim = vector()
classe = vector()
descritor = vector()
lv = vector()
metodo = vector()
acerto = vector()
resample = vector()
precision = vector()
recall = vector()
fmeasure = vector()

cont = 1

ctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3, savePredictions = "final")



#                      --- ELIMINACAO DE EXEMPLOS ---

tecnica = "EE"
pc = "-"

path.ini = "BaseadoExemplos/EliminacaoExemplos/"
path.fim = ".csv"

for(i in 1:6) {
  for(k in 1:5) {
    path = paste(path.ini, bases[i],  "/", "BE_", tecnica, "_", bases[i], path.fim, sep = "")
    dataset = read.csv(path, header = TRUE)
    predictors = names(dataset)[names(dataset) != "class"]
    
    # auxi = createFolds(dataset, k=10, list=TRUE)
    # ctrl = trainControl(method = "cv", index = auxi)
    
    model = caret::train(class ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
    pred = model$pred$pred
    ref = model$pred$obs
    cm = caret::confusionMatrix(pred, ref)
    prec = mean(cm$byClass[(7*2+1):(7*2+7)], na.rm = TRUE)
    rec = mean(cm$byClass[(7*0+1):(7*0+7)], na.rm = TRUE)
    fm = (2*prec*rec)/(prec+rec)
    
    aux = unlist(strsplit(bases[i], "_"))
    
    for(r in 1:30) {
      tec[cont] = tecnica
      prim[cont] = pc
      classe[cont] = "-------"
      descritor[cont] = aux[1]
      lv[cont] = aux[2]
      metodo[cont] = methods[k]
      resample[cont] = r
      acerto[cont] = model$resample$Accuracy[r]
      precision[cont] = prec
      recall[cont] = rec
      fmeasure[cont] = fm
      #melhor[cont] = model$results$Accuracy[as.numeric(row.names(model$bestTune[1]))]
      cont = cont + 1
    }
  }
}


#resultados = data.frame(tec, prim, classe, descritor, lv, metodo, resample, acerto, precision, recall, fmeasure)
#colnames(resultados) = c("Tecnica", "P/C", "Classe", "Descritor", "LV", "N", "Resample", "Acuracia", "Precisao", "Recall", "F-Measure")


#                      --- CRIACAO DE ROTULOS (PRIM?RIO) ---

tecnica = "CR"
pc = "P"

path.ini = "BaseadoExemplos/CriacaoRotulos/Primario/"
path.fim = ".csv"

for(i in 1:6) {
  for(k in 1:6) {
    path = paste(path.ini, bases[i],  "/", "BE_", tecnica, "_", pc, "_", bases[i], path.fim, sep = "")
    dataset = read.csv(path, header = TRUE)
    predictors = names(dataset)[names(dataset) != "class"]
    
    model = caret::train(class ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
    pred = model$pred$pred
    ref = model$pred$obs
    cm = caret::confusionMatrix(pred, ref)
    prec = mean(cm$byClass[(7*2+1):(7*2+7)], na.rm = TRUE)
    rec = mean(cm$byClass[(7*0+1):(7*0+7)], na.rm = TRUE)
    fm = (2*prec*rec)/(prec+rec)
    
    aux = unlist(strsplit(bases[i], "_"))
    
    for(r in 1:30) {
      tec[cont] = tecnica
      prim[cont] = pc
      classe[cont] = "-------"
      descritor[cont] = aux[1]
      lv[cont] = aux[2]
      metodo[cont] = methods[k]
      resample[cont] = r
      acerto[cont] = model$resample$Accuracy[r]
      precision[cont] = prec
      recall[cont] = rec
      fmeasure[cont] = fm
      #melhor[cont] = model$results$Accuracy[as.numeric(row.names(model$bestTune[1]))]
      cont = cont + 1
    }
  }
}
#resultados = data.frame(tec, prim, classe, descritor, lv, metodo, acerto)


#                      --- CRIACAO DE ROTULOS (COMPLETO) ---

# tecnica = "CR"
# pc = "C"
# 
# path.ini = "BaseadoExemplos/CriacaoRotulos/Completo/"
# path.fim = ".csv"
# 
# for(i in 1:1) {
#   for(k in 6:6) {
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
#     acerto[cont] = model$results$Accuracy[2]
#     
#     cont = cont + 1
#   }
# }

#resultados = data.frame(tec, prim, classe, descritor, lv, metodo, acerto)

#                      --- ELIMINACAO DE ROTULOS (PRIM?RIO) ---

tecnica = "ER"
pc = "P"

path.ini = "BaseadoExemplos/ConversaoExemplos/EliminacaoRotulos/Primario/"
path.fim = ".csv"

for(i in 1:6) {
  for(k in 1:6) {
    path = paste(path.ini, bases[i],  "/", "BE_CE_", tecnica, "_", pc, "_", bases[i], path.fim, sep = "")
    dataset = read.csv(path, header = TRUE)
    predictors = names(dataset)[names(dataset) != "class"]
    
    model = caret::train(class ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
    pred = model$pred$pred
    ref = model$pred$obs
    cm = caret::confusionMatrix(pred, ref)
    prec = mean(cm$byClass[(7*2+1):(7*2+7)], na.rm = TRUE)
    rec = mean(cm$byClass[(7*0+1):(7*0+7)], na.rm = TRUE)
    fm = (2*prec*rec)/(prec+rec)
    
    aux = unlist(strsplit(bases[i], "_"))
    
    for(r in 1:30) {
      tec[cont] = tecnica
      prim[cont] = pc
      classe[cont] = "-------"
      descritor[cont] = aux[1]
      lv[cont] = aux[2]
      metodo[cont] = methods[k]
      resample[cont] = r
      acerto[cont] = model$resample$Accuracy[r]
      precision[cont] = prec
      recall[cont] = rec
      fmeasure[cont] = fm
      #melhor[cont] = model$results$Accuracy[as.numeric(row.names(model$bestTune[1]))]
      cont = cont + 1
    }
  }
}
#resultados = data.frame(tec, prim, classe, descritor, lv, metodo, acerto)
#                      --- ELIMINACAO DE ROTULOS (COMPLETO) ---

tecnica = "ER"
pc = "C"

path.ini = "BaseadoExemplos/ConversaoExemplos/EliminacaoRotulos/Completo/"
path.fim = ".csv"

for(i in 1:6) {
  for(k in 1:6) {
    path = paste(path.ini, bases[i],  "/", "BE_CE_", tecnica, "_", pc, "_", bases[i], path.fim, sep = "")
    dataset = read.csv(path, header = TRUE)
    predictors = names(dataset)[names(dataset) != "class"]
    
    model = caret::train(class ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
    pred = model$pred$pred
    ref = model$pred$obs
    cm = caret::confusionMatrix(pred, ref)
    prec = mean(cm$byClass[(7*2+1):(7*2+7)], na.rm = TRUE)
    rec = mean(cm$byClass[(7*0+1):(7*0+7)], na.rm = TRUE)
    fm = (2*prec*rec)/(prec+rec)
    
    aux = unlist(strsplit(bases[i], "_"))
    
    for(r in 1:30) {
      tec[cont] = tecnica
      prim[cont] = pc
      classe[cont] = "-------"
      descritor[cont] = aux[1]
      lv[cont] = aux[2]
      metodo[cont] = methods[k]
      resample[cont] = r
      acerto[cont] = model$resample$Accuracy[r]
      precision[cont] = prec
      recall[cont] = rec
      fmeasure[cont] = fm
      #melhor[cont] = model$results$Accuracy[as.numeric(row.names(model$bestTune[1]))]
      cont = cont + 1
    }
  }
}

#resultados = data.frame(tec, prim, classe, descritor, lv, metodo, acerto)

# # # #                      --- BASEADO EM RÓTULOS - COMPLETO ---
# 
# tecnica = "BRC"
# pc = "C"
# 
# path.ini = "BaseadoRotulosClasses/Completo/"
# path.fim = ".csv"
# 
# for(i in 1:1) {
#   for(j in 1:7) {
#     for(k in 2:2) {
#       path = paste(path.ini, bases[i],  "/", tecnica, "_", pc, "_", labels[j], "_", bases[i], path.fim, sep = "")
#       dataset = read.csv(path, header = TRUE)
#       predictors = names(dataset)[names(dataset) != tolower(labels[j])]
#       
#       dataset[ ,ncol(dataset)] = as.factor(dataset[ ,ncol(dataset)])
#       #("Alegria", "Desgosto", "Medo", "Neutro", "Raiva", "Surpresa", "Tristeza")
#       if(j==1) {
#         model = caret::train(alegria ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
#       } else if(j==2) {
#         model = caret::train(desgosto ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
#       } else if(j==3) {
#         model = caret::train(medo ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
#       } else if(j==4) {
#         model = caret::train(neutro ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
#       } else if(j==5) {
#         model = caret::train(raiva ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
#       } else if(j==6) {
#         model = caret::train(surpresa ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
#       } else if(j==7) {
#         model = caret::train(tristeza ~ ., data = dataset, method = methods[k], trControl = ctrl, tuneLength = 5)
#       }
#       
# #       if(i %% 2 == 0) {
# #         rf.train = dataset[ , -17]
# #         rf.label = as.factor(dataset[ , 17])
# #       } else {
# #         rf.train = dataset[ , -25]
# #         rf.label = as.factor(dataset[ , 25])
# #       }
# #       rf1 = rf(rf.train, rf.label)
#       
#       aux = unlist(strsplit(bases[i], "_"))
#       
#       tec[cont] = tecnica
#       prim[cont] = pc
#       classe[cont] = labels[j]
#       descritor[cont] = aux[1]
#       lv[cont] = aux[2]
#       metodo[cont] = methods[k]
#       acerto[cont] = model$results$Accuracy[1]
#       
#       cont = cont + 1
#     }
#   }
# }

resultados = data.frame(tec, prim, classe, descritor, lv, metodo, resample, acerto, precision, recall, fmeasure)
colnames(resultados) = c("Tecnica", "P/C", "Classe", "Descritor", "LV", "N", "Resample", "Acuracia", "Precisao", "Recall", "F-Measure")
