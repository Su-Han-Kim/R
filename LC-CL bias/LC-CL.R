library(KoNLP)
library(dplyr)

# set working directory
setwd('C:\\Users\\user\\Desktop')
read.csv('KoCorpus_9_A2P03F.csv', header = F) -> corpus
corpus %>% filter(V1 == "CDS") -> corpus_cds

#make CV matrix
jaList <- c("ㄱ", "ㄲ", "ㅋ", "ㄷ", "ㄸ", "ㅌ", "ㅈ", "ㅉ", "ㅊ", "ㅁ", "ㅍ", "ㅂ", "ㅃ", "ㅎ", "ㄴ", "ㄹ", "ㅇ", "ㅅ", "ㅆ", 'ㄱ', 'ㄲ', 'ㄳ', 'ㄴ', 'ㄵ', 'ㄶ', 'ㄷ', 'ㄹ', 'ㄺ', 'ㄻ', 'ㄼ', 'ㄽ', 'ㄾ', 'ㄿ', 'ㅀ', 'ㅁ', 'ㅂ', 'ㅄ', 'ㅅ', 'ㅆ', 'ㅇ', 'ㅈ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ')
jaList <- unique(jaList) 
moList <- c("ㅏ", "ㅐ", "ㅑ", "ㅒ", "ㅓ", "ㅔ", "ㅕ", "ㅖ", "ㅗ", "ㅘ", "ㅙ", "ㅚ", "ㅛ", "ㅜ", "ㅝ", "ㅞ", "ㅟ", "ㅠ", "ㅡ", "ㅢ", "ㅣ")


totalLength <- length(jaList)

jaMatrix <- matrix(0,nrow=totalLength,ncol=totalLength); jaMatrix

jamoList <- c(jaList, moList)

colnames(jaMatrix) <- jaList; jaMatrix

rownames(jaMatrix) <- jaList; jaMatrix

# separate Consonant and Vowel
gsub('[0-9a-zA-Z]', '', corpus_cds$V3) -> corpus_cds$V3
strsplit(corpus_cds$V3, "\n") -> corpus_cds.list
lapply(corpus_cds.list, convertHangulStringToJamos) -> corpus_cds.list
lapply(corpus_cds.list, paste, collapse="") -> corpus_cds.list2
lapply(corpus_cds.list2, function(corpus_cds.list2) if(is.character(corpus_cds.list2)) gsub("[ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ]", "", corpus_cds.list2) else(corpus_cds.list2)) -> corpus_cds.list2
lapply(corpus_cds.list2, strsplit, split="") -> eachEle
lapply(eachEle, unlist) -> eachEle


# 연속되는 자모의 조합 빈도 저장하기

for (i in 1:length(eachEle)){
  if(length(eachEle[[i]]) != 1){
    for (j in 1:(length(eachEle[[i]])-1)){
      if (eachEle[[i]][j] != " " && eachEle[[i]][j+1] != " "){
        jaMatrix[eachEle[[i]][j],eachEle[[i]][j+1]] = jaMatrix[eachEle[[i]][j],eachEle[[i]][j+1]] + 1
      }
    }
  }
}

#filter [labial] [coronal] 
sum(jaMatrix)
jaMatrix[c("ㅂ", "ㅍ", "ㅃ", "ㅁ"), c("ㄷ", "ㅌ", "ㄸ", "ㄴ", "ㄹ", "ㅅ", "ㅆ", "ㅈ", "ㅊ", "ㅉ")]
sum(jaMatrix[c("ㅂ", "ㅍ", "ㅃ", "ㅁ"), c("ㄷ", "ㅌ", "ㄸ", "ㄴ", "ㄹ", "ㅅ", "ㅆ", "ㅈ", "ㅊ", "ㅉ")])/sum(jaMatrix)

#filter [coronal]  [labial] 
jaMatrix[c("ㄷ", "ㅌ", "ㄸ", "ㄴ", "ㄹ", "ㅅ", "ㅆ", "ㅈ", "ㅊ", "ㅉ"), c("ㅂ", "ㅍ", "ㅃ", "ㅁ")]
sum(jaMatrix[c("ㄷ", "ㅌ", "ㄸ", "ㄴ", "ㄹ", "ㅅ", "ㅆ", "ㅈ", "ㅊ", "ㅉ"), c("ㅂ", "ㅍ", "ㅃ", "ㅁ")])/sum(jaMatrix)
