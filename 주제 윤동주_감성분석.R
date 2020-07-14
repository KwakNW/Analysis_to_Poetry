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
      neg.matches = !is.na(match(words, neg.words))

      #pos.matches = !is.na(pos.matches) #is.na()함수는 NA인지 아닌지 판단하는 함수 => !is.na() 함수는  NA가 아니면 TRUE, NA이면 FALSE를 나타낸다.
      #neg.matches = !is.na(neg.matches)

      score = sum(pos.matches) - sum(neg.matches) #감성점수 계산
      return(score) #값을 되돌려준다.
    }, pos.words, neg.words)

   scores.df = data.frame(text=sentences, score=scores) #데이터 프레임으로 변환
   return(scores.df)
}

pos.words = scan('positive-words-kor.txt', what='character', comment.char=';') #scan() 함수는 파일을 읽기 위한 함수 
neg.words = scan('negative-words-kor.txt', what='character', comment.char=';')
#pos.words = c(hu.liu.pos) #c는 벡터를 만드는 함수이다.
#neg.words = c(hu.liu.neg)

textMining = readLines("윤동주 시인_한줄연도순.txt")
sample = c(textMining) #c는 벡터를 만드는 함수이다.

result = score.sentiment(sample, pos.words, neg.words) #데이터 프레임 반환 / 함수를 여기서 호출
result

library(ggplot2) #패키지
qplot(result$score) #ggplot2패키지에 있는 함수, quick plot을 그리기 위한 함수

mean(result$score)