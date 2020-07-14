setwd("D:/빅데이터/과제")
install.packages("tm")
install.packages("wordcloud")

library(tm)
library(wordcloud)
library(KoNLP)

textMining = readLines("윤동주 시인_한줄연도순.txt") #한줄 한줄 읽고 각 줄이 벡터에서 하나의 원소가 된다.
useSejongDic() 

nouns <-sapply(textMining, extractNoun, USE.NAMES=F) #nouns :리스트
nouns <-unlist(nouns) #리스트를 벡터로 만들겠다.
nouns <-gsub("?","", nouns)
nouns <-gsub("-","", nouns)
nouns <-gsub("―","", nouns)
nouns <-gsub("19.*","", nouns)
nouns <-gsub("가슴.*","가슴", nouns) 
nouns <-gsub("가을.*","가을", nouns)
nouns <-gsub("겨울.*","겨울", nouns) 
nouns <-gsub("간난.*","간난", nouns)
nouns <-gsub("괴로운","괴로움", nouns)
nouns <-gsub("괴롭","괴로움", nouns)
nouns <-gsub("누나.*","누나", nouns)
nouns <-gsub("누구.*","누구", nouns)
nouns <-gsub("동무.*","동무", nouns)
nouns <-gsub("달.*","달_", nouns)
nouns <-gsub("마음.*","마음", nouns)
nouns <-gsub("바다.*","바다", nouns)
nouns <-gsub("부끄.*","부끄러운", nouns)
nouns <-gsub("밤.*","밤_", nouns)
nouns <-gsub("빨알간","빨간", nouns)
nouns <-gsub("사람.*","사람", nouns)
nouns <-gsub("산골.*","산골", nouns)
nouns <-gsub("어머님","어머니", nouns)
nouns <-gsub("엄마","어머니", nouns)
nouns <-gsub("아롱.*","아롱", nouns)
nouns <-gsub("아이.*","아이", nouns)
nouns <-gsub("우리.*","우리", nouns)
nouns <-gsub("이름.*","이름", nouns)
nouns <-gsub("아빠","아버지", nouns)
nouns <-gsub("이웃.*","이웃", nouns)
nouns <-gsub("참회.*","참회", nouns)
nouns <-gsub("파아란","파란", nouns)
nouns <-gsub("하이얀","흰", nouns)
nouns <-gsub("하루.*","하루", nouns)
nouns <- nouns[nchar(nouns)>=2]

wordFreq <-table(nouns) #빈도
pal <- brewer.pal(7, "Set1")    
windowsFonts(malgun=windowsFont("맑은 고딕"))

set.seed(1000)
wordcloud(words=names(wordFreq), freq=wordFreq, scale=c(3, 0.5), colors=pal, min.freq=2, random.order=F, family="malgun")
									# scale 상대적인 크기 제일 큰 글자가 3 제일 작은 글자가 0.5

#단어 빈도를 나타내는 barplot 
#wordFreq <- sort(subset(wordFreq, wordFreq >= 10),decreasing = TRUE)
#barplot(wordFreq, xlab="시어", ylab="빈도", main = '단어 빈도',las=2, col="gray")

