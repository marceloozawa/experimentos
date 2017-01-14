#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

setwd("C:/Users/Marcelo/Desktop/multirrotulo/IndependenteAlgoritmo/BaseadoExemplos/EliminacaoExemplos/STREN_LV1")

library("mldr") 
library("mlr")
library("foreign")

source("C:/Users/Marcelo/Desktop/utilml_iterative_stratification.R")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

#testingArffFile = function() {
gerarBaseStratificada <- function (path) {  
  # leio o arff para realizar o preprocessamento
  #nome = "PUC_SENTIC_WORDS_LV1"
  #nome = "PUC_SENTIC_WORDS_LV2"
  #nome = "PUC_SENTIC_STREN_LV2"
  #nome = "PUC_SENTIC_TFIDF_LV1"
  #nome = "PUC_SENTIC_TFIDF_LV2"
  
  temp = read.csv(path)
  
  #teste = read.arff("C:/Users/Marcelo/Desktop/multirrotulo/dataset/PUC_SENTIC_STREN_LV1.arff")
  #classes = temp[,(ncol(temp)-7):(ncol(temp)-4)]
  #dataset = teste[,-c((ncol(teste)-3):(ncol(teste)))]
  dataset = temp
  #remover os NAs
  temp = mlr::impute(data = temp, classes = list(numeric = imputeMean(), factor = imputeMode()))
  temp = temp$data

  #converter os atributos categoricos em N colunas
  temp = createDummyFeatures(temp, method = "1-of-n")

  #salvo um arquivo temporario para ler pela funcao do Adriano
  foreign::write.arff(temp, file="temp.arff")
  nlev = nlevels(dataset$class) - 1
  mdata = mldr(filename = "temp", use_xml = FALSE, label_indices=c((ncol(temp)-nlev):ncol(temp)))

  k = 10
  r = rep(1/k, k)

  #Chama a estratificacao do adriano
  auxi = utiml_iterative_stratification(mdata, r)
  #print(aux)
  
  
  #write.arff(dataset[auxi[[1]],], file=paste(paste("datasetStratifiedClue/",nome, sep=""),"_S1.arff", sep=""))
  #write.arff(dataset[auxi[[2]],], file=paste(paste("datasetStratifiedClue/",nome, sep=""),"_S2.arff", sep=""))
  #write.arff(dataset[auxi[[3]],], file=paste(paste("datasetStratifiedClue/",nome, sep=""),"_S3.arff", sep=""))
  #write.arff(dataset[auxi[[4]],], file=paste(paste("datasetStratifiedClue/",nome, sep=""),"_S4.arff", sep=""))
  #write.arff(dataset[auxi[[5]],], file=paste(paste("datasetStratifiedClue/",nome, sep=""),"_S5.arff", sep=""))
  #write.arff(dataset[auxi[[6]],], file=paste(paste("datasetStratifiedClue/",nome, sep=""),"_S6.arff", sep=""))
  #write.arff(dataset[auxi[[7]],], file=paste(paste("datasetStratifiedClue/",nome, sep=""),"_S7.arff", sep=""))
  #write.arff(dataset[auxi[[8]],], file=paste(paste("datasetStratifiedClue/",nome, sep=""),"_S8.arff", sep=""))
  #write.arff(dataset[auxi[[9]],], file=paste(paste("datasetStratifiedClue/",nome, sep=""),"_S9.arff", sep=""))
  #write.arff(dataset[auxi[[10]],], file=paste(paste("datasetStratifiedClue/",nome, sep=""),"_S10.arff", sep=""))
  
  #aux = read.arff("datasetStratifiedClue/BE_EE_STREN_LV1.csv_S1.arff") 
  
  auxi
}
#}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

testingCsvFile = function() {

  temp = read.csv("data/Base_WORDS_LV2.csv")
  
  if(any(is.na(temp))){
      cat("Existem valores de NA!")
  } else {
    foreign::write.arff(temp, file="temp2.arff")
    k = 10
    r = rep(1/k, k)
    mdata = mldr(filename = "temp2", use_xml = FALSE, label_indices=c(17:23))
    aux = utiml_iterative_stratification(mdata, r)
    print(aux)

  }

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
