library(mldr)

utiml_iterative_stratification <- function (mdata, r) {
  D <- 1:mdata$measures$num.instances
  S <- lapply(1:length(r), function (i) integer())
  
  # Calculate the desired number of examples at each subset
  cj <- round(mdata$measures$num.instances * r)
  dif <- mdata$measures$num.instances - sum(cj)
  if (dif != 0)
    cj[1:abs(dif)] <- cj[1:abs(dif)] + c(1, -1)[c(dif>0, dif<0)]
  
  # Calculate the desired number of examples of each label at each subset
  cji <- trunc(sapply(mdata$labels$count, function (di) di * r))
  colnames(cji) <- rownames(mdata$labels)
  
  while (length(D) > 0) {
    # Find the label with the fewest (but at least one) remaining examples,
    Dl <- apply(mdata$dataset[D, mdata$labels$index], 2, function (col) as.numeric(names(which(col == 1))))
    Di <- unlist(lapply(Dl, length))
    l <- names(which.min(Di[Di>0]))
    
    for (ex in Dl[[l]]) {
      # Find the subset(s) with the largest number of desired examples for this
      # label, breaking ties by considering the largest number of desired examples
      m <- which(cji[which.max(cji[,l]),l] == cji[,l])
      if (length(m) > 1) {
        m <- intersect(m, which(cj[m[which.max(cj[m])]] == cj))
        if (length(m) > 1)
          m <- sample(m)[1]
      }
      
      S[[m]] <- c(S[[m]], ex)
      D <- D[D != ex]
      
      # Update desired number of examples
      i <- which(mdata$dataset[ex, mdata$labels$index] == 1)
      cji[m, i] <- cji[m, i] - 1
      cj[m] <- cj[m] - 1
    }
  }
  
  S
}

utiml_labelset_stratification <- function (mdata, r) {
  D <- sample(mdata$measures$num.instances)
  S <- lapply(1:length(r), function (i) integer())
  labelsets <- apply(mdata$dataset[,mdata$labels$index], 1, paste, collapse = "")
  
  # Calculate the desired number of examples of each labelset at each subset
  cji.aux <- sapply(mdata$labelsets, function (di) di * r)
  cji <- trunc(cji.aux)
  dif <- cji.aux - cji
  rest <- round(apply(dif, 1, sum))
  for (ls in rev(names(mdata$labelsets))) {
    s <- sum(dif[,ls])
    if (s > 0) {
      for (i in 1:s) {
        fold <- which.max(rest)
        rest[fold] <- rest[fold] - 1
        cji[fold, ls] <- cji[fold, ls] + 1
      }
    }
  }
  
  for (ex in D) {
    ls <- labelsets[ex]
    fold <- which.max(cji[,ls])
    if (cji[fold, ls] > 0) {
      S[[fold]] <- c(S[[fold]], ex)
      cji[fold, ls] <- cji[fold, ls] - 1
    }
  }
  
  S
}

mldr_subset <- function (mdata, rows, cols) {
  mldr_from_dataframe(
    cbind(mdata$dataset[rows, cols], mdata$dataset[rows, rownames(mdata$labels)]),
    labelIndices = (length(cols) + 1):(length(cols)+1):(length(cols)+mdata$measures$num.labels),
    name = mdata$name
  )
}

mldr_mergedata <- function (mdata1, mdata2) {
  allcols <- c(mdata1$attributesIndexes, mdata1$labels$index)
  dataset <- rbind(mdata1$dataset[,allcols], mdata2$dataset[,allcols])
  mldr_from_dataframe(dataset, mdata1$labels$index, mdata1$name)
}

split_dataset <- function (arfffile="caminho_do_arquivo.arff", testfile="caminho_do_arquivo_teste.arff", xmlfile="caminho_do_arquivo.xml", targetfile="newfilename", k=10, SEED=1) {
  mdata <- mldr(arfffile, auto_extension=FALSE, xml_file=xmlfile)
  if (!is.null(testfile)) {
    mdata2 <- mldr(testfile, auto_extension=FALSE, xml_file=xmlfile)
    mdata <- mldr_mergedata(mdata, mdata2)
  }
  set.seed(SEED)
  folds <- utiml_labelset_stratification(mdata, rep(1/k, k))
  
  for (i in 1:k) {
    new.mdata <- mldr_subset(mdata, folds[[i]], mdata$attributesIndexes)
    new.mdata$name <- paste(new.mdata$name, i, sep="_")
    write_arff(new.mdata, paste(targetfile, i, sep="_"), write.xml = FALSE)
  }
  
  set.seed(NULL)
}

# split_dataset(
#   "~/projects/mlmlbr/dataset/flags/flags-train.arff",
#   "~/projects/mlmlbr/dataset/flags/flags-test.arff",
#   "~/projects/mlmlbr/dataset/flags/flags.xml",
#   "~/Desktop/flags/flags-fold"
# )

#split_dataset(
#    "~/projects/mlmlbr/dataset/flags/flags-train.arff",
#    NULL,
#    "~/projects/mlmlbr/dataset/flags/flags.xml",
#    "~/Desktop/flags/flags-fold"
#)