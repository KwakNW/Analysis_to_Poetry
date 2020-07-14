setwd("D:/빅데이터/과제")
install.packages("plyr")

library(KoNLP)
library(plyr) #데이터를 쪼개고 적용하고 조합하는데 사용하는 패키지
library(ggplot2)


textMining = readLines("윤동주 시인_한줄연도순.txt") #한줄 한줄 읽고 각 줄이 벡터에서 하나의 원소가 된다.
useSejongDic() 

nouns <-sapply(textMining, extractNoun, USE.NAMES=F) #nouns :리스트
nouns <-unlist(nouns) #리스트를 벡터로 만들겠다

#연도만 출력하기 위해 
nouns <- gsub("년","",nouns)
year <- nouns[grep("19",nouns)]

wordFreq <-table(year) #빈도

barplot(wordFreq, xlab="연도", ylab="편수", main = '작성 연도', col = "navy", las=2)

#pie chart
x11()
pct <- round(wordFreq/sum(wordFreq)*100,1)
x <- unique(year) #중복제거
lab1 <- x
lab2 <- paste(lab1, "\n", pct, "%")
pie(wordFreq, radius = 1, init.angle=90, main = '작성 연도', label = lab2)





