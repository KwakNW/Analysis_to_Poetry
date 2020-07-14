setwd("D:/빅데이터/과제")

# 연관어 분석에 필요한 패키지 설치
install.packages("KoNLP")
install.packages("arules")
install.packages("igraph")
install.packages("combinat")

library(KoNLP)
useSejongDic()
library(arules)
library(igraph)
library(combinat)

poem = readLines("윤동주 시인_한줄연도순.txt")

#단어 추출 및 트랜잭션 생성
tran <- Map(extractNoun, poem) # Map()함수 : 줄 단위 단어 추출 Map(f, ...)
tran <- unique(tran)# 중복제거1(전체 대상) 
tran <- sapply(tran, unique)# 중복제거2(줄 단위 대상) 
tran <- sapply(tran, function(x) {Filter(function(y)
  {nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)},x)}) 
tran

#names(tran) <- paste("Tr", 1:length(tran), sep="")
#paste Tr와 1:length를 대칭적으로 이어줌

wordtran <- as(tran,"transactions") #as 함수를 이용하여 트랜잭션 데이터로 변환
wordtab <- crossTable(wordtran) #교차분석
#crossTable 함수는 모든 단어의 정보를 행과 열에 똑같이 넣어서 테이블을 생성

# apriori 함수를 사용하여 데이터 연관분석
data <- apriori(wordtran, control = list(verbos=F), parameter = list(supp=0.025, conf=0.15))
#supp : 지지도 conf : 신뢰도

# inspect 함수를 사용하여 연관 분석 결과 확인
inspect(sort(data))

# labels 함수와 strsplit 함수를 사용하여 rules 변수의 내용을 입력

poems <- labels(data, ruleSep=" ")
poems <- sapply(poems, strsplit, " ", USE.NAMES = F)

# new 변수에 do.call 함수를 사용하여 데이터 입력
new <- do.call("rbind", poems) #matrix 형태로 재정렬
newg <- graph.edgelist(new[-c(1:6),], directed = F)
set.seed(1000)
plot.igraph(newg, vertex.label=V(newg)$name, vertex.label.color='black',
            vertex.size=20, vertex.color = 'gray')

#단어 근접 중심성
closen <- closeness(newg)
plot(closen, col='red', xaxt='n', lty='solid', type='b', xlab='단어', ylab='closeness')
points(closen, pch=16, col='navy')
axis(1, seq(1, length(closen)), V(newg)$name, cex=5)