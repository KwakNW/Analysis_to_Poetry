setwd("D:/빅데이터/과제") #작업 디렉토리 지정

library(twitteR) #패키지를 불러온다.
library(plyr) #데이터를 쪼개고 적용하고 조합하는데 사용하는 패키지

score.sentiment = function(sentences, pos.words, neg.words) #score.sentiment 함수를 정의한다.
{
   scores = laply(sentences, #plyr()패키지에 있으며 list나 벡터 각각의 원소에 대해 함수를 적용하고 그 결과를 배열로 조합하는 함수
   function(sentence, pos.words, neg.words) #함수를 정의한다.
   #{.........} 부분은 function함수에 적용할 인수를 나타낸다.
   {
      sentence = gsub("[[:punct:]]", "", sentence) #구두점(punctuation) 제거
      sentence = gsub("[[:cntrl:]]", "", sentence) #제어문자 제거
      sentence = gsub('\\d+', '', sentence) #숫자 제거(+는 적어도 한 번 이상을 의미)
	sentence = gsub('은', '', sentence) 
	sentence = gsub('는', '', sentence)
	sentence = gsub('같은', '', sentence)
	sentence = gsub('같이', '', sentence)
	sentence = gsub('를', '', sentence)
	sentence = gsub('에는', '', sentence)

      word.list = strsplit(sentence, "\\s+") #strsplit()함수는 문자열을 쪼개기 위한 함수, \\s+ 는 하나 이상의 공백문자를 나타낸다. => 하나 이상의 공백문자로 쪼갬
      words = unlist(word.list) # unlist()함수는 리스트 형태를 벡터형태로 만들기 위한 함수

      pos.matches = !is.na(match(words, pos.words)) #match()함수는 매칭을 통해 매칭여부를 알기 위한 함수
      neg.matches = !is.na(match(words, neg.words))#is.na()함수는 NA인지 아닌지 판단하는 함수 => !is.na() 함수는  NA가 아니면 TRUE, NA이면 FALSE를 나타낸다.

      score = sum(pos.matches) - sum(neg.matches) #감성점수 계산
      return(score) #값을 되돌려준다.
    }, pos.words, neg.words)

   scores.df = data.frame(text=sentences, score=scores) #데이터 프레임으로 변환
   return(scores.df)
}

pos.words = scan('positive-words-kor.txt', what='character', comment.char=';') #scan() 함수는 파일을 읽기 위한 함수 
neg.words = scan('negative-words-kor.txt', what='character', comment.char=';')

#연도별 감성분석
#library(twitteR) #패키지를 불러온다.
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

result_1932 = score.sentiment(sample_1932, pos.words, neg.words) 
#데이터 프레임 반환 / 함수를 여기서 호출
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

x11()
library(ggplot2) #패키지
barplot(result, xlab="연도", ylab="감성점수", main ="연도별 감성분석", col = "pink", las=2)


