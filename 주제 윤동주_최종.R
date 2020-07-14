#wordcloud
setwd("D:/빅데이터/과제") #작업 디렉토리 설정

#설치할 패키지
install.packages("wordcloud")
install.packages("wordcloud2")

#패키지 불러오기
library(wordcloud)
library(KoNLP)
library(wordcloud2)
useSejongDic() 

#파일 불러오기 - 시를 크롤링한 text
textMining = readLines("윤동주 시인_한줄연도순.txt") #각 줄이 벡터의 원소가 된다.

#전처리 작업
nouns <-sapply(textMining, extractNoun, USE.NAMES=F) #nouns :리스트
nouns <-unlist(nouns) # unlist : 리스트를 벡터로

#같은 의미의 단어나 불필요한 용어를 gsub를 이용해 삭제 및 교체
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
nouns <- nouns[nchar(nouns)>=2] #2자리 이상의 단어만 저장

wordFreq <-table(nouns) #빈도
pal <- brewer.pal(7, "Set1") #wordcloud 색 지정 7가지 색상 가능
windowsFonts(malgun=windowsFont("맑은 고딕")) #폰트 지정

set.seed(1000)
wordcloud(words=names(wordFreq), freq=wordFreq, scale=c(3, 0.5), colors=pal, min.freq=2, random.order=F, family="malgun")
# scale :  상대적인 크기 제일 큰 글자가 3 제일 작은 글자가 0.5
# min.freq : 나타낼 최소 빈도수

#wordcloud2

wordFreq_2 <- wordFreq[wordFreq >= 2] #빈도 2 이상 추출
wordcloud2(wordFreq, fontFamily='맑은 고딕', size = 0.5, color = "random-dark", backgroundColor = "white", shape="cardioid")

wordcloud2(wordFreq_2, minRotation = -pi/6, maxRotation = -pi/6, minSize = 10, rotateRatio = 1) #Rotation : 단어 회전

#단어 빈도를 나타내는 barplot 
x11()
wordFreq <- sort(subset(wordFreq, wordFreq >= 10),decreasing = TRUE)
barplot(wordFreq, xlab="시어", ylab="빈도", main = '단어 빈도', las=2, col="gray")
#빈도가 10개 이상인 단어들만 추출
#las = 2 => x값 90도 회전

#감성분석
install.packages("plyr")

library(plyr) #데이터를 쪼개고 적용하고 조합하는데 사용하는 패키지
library(ggplot2) 

score.sentiment = function(sentences, pos.words, neg.words) 
#score.sentiment 함수를 정의한다.
{
   scores = laply(sentences, #laply : plyr()패키지에 있으며 list나 벡터 각각의 원소에 대해 함수를 적용하고 그 결과를 배열로 조합하는 함수
   function(sentence, pos.words, neg.words) #함수 정의
   #{.........} 부분은 function함수에 적용할 인수
   {
      sentence = gsub("[[:punct:]]", "", sentence) #구두점(punctuation) 제거
      sentence = gsub("[[:cntrl:]]", "", sentence) #제어문자 제거
      sentence = gsub('은', '', sentence) 
      sentence = gsub('는', '', sentence)
      sentence = gsub('같은', '', sentence) 
      sentence = gsub('같이', '', sentence)
      sentence = gsub('를', '', sentence)
      sentence = gsub('에는', '', sentence)

      word.list = strsplit(sentence, "\\s+") 
#strsplit()함수는 문자열을 쪼개기 위한 함수, \\s+ 는 하나 이상의 공백문자 => 하나 이상의 공백문자로 쪼갬
      words = unlist(word.list) 

# unlist()함수는 리스트 형태를 벡터형태로 만들기 위한 함수
      pos.matches = !is.na(match(words, pos.words)) 

#match()함수는 매칭을 통해 매칭여부를 알기 위한 함수
      neg.matches = !is.na(match(words, neg.words))

#is.na()함수는 NA인지 아닌지 판단하는 함수 
      score = sum(pos.matches) - sum(neg.matches) #감성점수 계산
      return(score) 
    }, pos.words, neg.words)

   scores.df = data.frame(text=sentences, score=scores) 
#데이터 프레임으로 변환
   return(scores.df)
}
pos.words = scan('positive-words-kor.txt', what='character', comment.char=';') #scan() 함수는 파일을 읽기 위한 함수 
neg.words = scan('negative-words-kor.txt', what='character', comment.char=';')

textMining = readLines("윤동주 시인_한줄연도순.txt")
sample = c(textMining) #c : 벡터를 만드는 함수

result = score.sentiment(sample, pos.words, neg.words) 
#데이터 프레임 반환 / 함수를 여기서 호출

x11()
qplot(result$score) #ggplot2패키지에 있는 함수, quick plot을 그리기 위한 함수

#mean(result$score) #감성점수의 평균 

#연도별 감성분석

#library(plyr) #데이터를 쪼개고 적용하고 조합하는데 사용하는 패키지
#library(ggplot2) #패키지

#textMining = readLines("윤동주 시인_한줄연도순.txt")
sample_1932 <- textMining[grep("1932",textMining)]
sample_1934 <- textMining[grep("1934",textMining)]
sample_1935 <- textMining[grep("1935",textMining)]
sample_1936 <- textMining[grep("1936",textMining)]
sample_1937 <- textMining[grep("1937",textMining)]
sample_1938 <- textMining[grep("1938",textMining)]
sample_1939 <- textMining[grep("1939",textMining)]
sample_1940 <- textMining[grep("1940",textMining)]
sample_1941 <- textMining[grep("1941",textMining)]
sample_1942 <- textMining[grep("1942",textMining)]
sample_1948 <- textMining[grep("1948",textMining)]

#각 연도별 데이터 프레임 반환 / 함수 호출
result_1932 = score.sentiment(sample_1932, pos.words, neg.words) 
result_1932<- mean(result_1932$score)

result_1934 = score.sentiment(sample_1934, pos.words, neg.words)
result_1934<- mean(result_1934$score)

result_1935 = score.sentiment(sample_1935, pos.words, neg.words)
result_1935 <- mean(result_1935$score)

result_1936 = score.sentiment(sample_1936, pos.words, neg.words)
result_1936 <- mean(result_1936$score)

result_1937 = score.sentiment(sample_1937, pos.words, neg.words)
result_1937 <- mean(result_1937$score)

result_1938 = score.sentiment(sample_1938, pos.words, neg.words)
result_1938 <- mean(result_1938$score)

result_1939 = score.sentiment(sample_1939, pos.words, neg.words)
result_1939 <- mean(result_1939$score)

result_1940 = score.sentiment(sample_1940, pos.words, neg.words)
result_1940 <- mean(result_1940$score)

result_1941 = score.sentiment(sample_1941, pos.words, neg.words)
result_1941 <- mean(result_1941$score)

result_1942 = score.sentiment(sample_1942, pos.words, neg.words)
result_1942 <- mean(result_1942$score)

result_1948 = score.sentiment(sample_1948, pos.words, neg.words)
result_1948 <- mean(result_1948$score)

result <- c(result_1932, result_1934, result_1935, result_1936, 
result_1937, result_1938, result_1939,result_1940, result_1941,result_1942,
result_1948)

library(ggplot2) #패키지
x11()
barplot(result, xlab="연도", ylab="감성점수", main ="연도별 감성분석", col = "pink", las=2)

#install.packages("plyr")

#library(KoNLP)
#library(plyr) #데이터를 쪼개고 적용하고 조합하는데 사용하는 패키지
#library(ggplot2)

textMining = readLines("윤동주 시인_한줄연도순.txt") #각 줄이 벡터의 원소
#useSejongDic() 

nouns <-sapply(textMining, extractNoun, USE.NAMES=F) 
nouns <-unlist(nouns) #리스트를 벡터로

#연도만 출력하기 위해 
nouns <- gsub("년","",nouns)
year <- nouns[grep("19",nouns)]

wordFreq <-table(year) #빈도
x11()
barplot(wordFreq, xlab="연도", ylab="편수", main = '작성 연도', col = "navy", las=2)

#pie chart
x11()
pct <- round(wordFreq/sum(wordFreq)*100,1)
x <- unique(year) #중복제거
lab1 <- x
lab2 <- paste(lab1, "\n", pct, "%")
pie(wordFreq, radius = 1, init.angle=90, main = '작성 연도', label = lab2)

#단어 연관분석
# 연관 분석에 필요한 패키지 설치
#install.packages("KoNLP")
install.packages("arules")
install.packages("igraph")
install.packages("combinat")

library(KoNLP)
library(arules)
library(igraph)
library(combinat)
useSejongDic()

poem = readLines("윤동주 시인_한줄연도순.txt")

#단어 추출 및 트랜잭션 생성
tran <- Map(extractNoun, poem) 
# Map()함수 : 줄 단위 단어 추출 Map(f, ...)
tran <- unique(tran) # 중복제거1(전체 대상) 
tran <- sapply(tran, unique)# 중복제거2(줄 단위 대상) 
tran <- sapply(tran, function(x) {Filter(function(y)
  {nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)},x)}) 
tran

names(tran) <- paste("Tr", 1:length(tran), sep="")
#paste => Tr와 1:length를 대칭적으로 이어줌

wordtran <- as(tran,"transactions") #as 함수를 이용하여 트랜잭션 데이터로 변환
wordtab <- crossTable(wordtran) #교차분석
#crossTable 함수는 모든 단어의 정보를 행과 열에 똑같이 넣어서 테이블을 생성

# apriori 함수를 사용하여 데이터 연관분석
data <- apriori(wordtran,control = list(verbos=F), parameter = list(supp=0.025, conf=0.15))

#supp : 지지도 conf : 신뢰도

# inspect 함수를 사용하여 연관 분석 결과 확인
inspect(sort(data))

# labels 함수와 strsplit 함수를 사용하여 rules 변수의 내용을 입력
poems <- labels(data, ruleSep=" ")
poems <- sapply(poems, strsplit, " ", USE.NAMES = F)

# new 변수에 do.call 함수를 사용하여 데이터 입력
new <- do.call("rbind", poems) #matrix 형태로 재정렬
newg <- graph.edgelist(new[-c(1:6),], directed = F)
x11()
plot.igraph(newg, vertex.label=V(newg)$name, vertex.label.color='black',
            vertex.size=20, vertex.color = 'gray')

#단어 근접 중심성
closen <- closeness(newg)
x11()
plot(closen, col='red', xaxt='n', type='b', xlab='단어', ylab='closeness')
points(closen, pch=20, col='navy')
axis(1, seq(1, length(closen)), V(newg)$name, cex=5)

