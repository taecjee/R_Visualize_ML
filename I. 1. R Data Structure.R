# 동작 확인 및 명령 실행
print('hello')
print(
'hello'
)

# 도움말 보기
?print
help('print')

# 패키지 설치 - 온라인
install.packages('e1071')

# 패키지 설치 - 오프라인
packageDist <- ('package\\e1071_1.7-3.zip')
install.packages(packageDist, repos=NULL)

# 변수
var1 <- 10
var1 <- 'a string'

한글 <- 1
b <- 2
a1 <- 3
.x <- 4
y_1 <- 5

2a <- 6
_a <- 9
.2a <- 10

# NA와 NULL
four <- NA
is.na(four)

one <- 100
is.null(one)
two <- as.null(one)
is.null(two)

# 문자열 (Character)
a <- 'This is a string'
print(a)

b <- "This is a string"
print(b)

# 소수 숫자 (Numeric)
a <- 4.5
b <- 3.6
c <- a + b
print(c)

class(c)
typeof(c)

# 정수 숫자 (Integer)
a <- 3
class(a)
b <- as.integer(a)
class(b)

b <- a + b
class(b)

# 진리값 (Logical)
TRUE & TRUE
TRUE & FALSE
TRUE | FALSE
FALSE | FALSE
!TRUE
!FALSE

# 요인 (factor)
animals <- factor("dog", c('cat', 'dog', 'horse'))
animals

nlevels(animals)
levels(animals)

factor(c('c', 'a', 'b'), ordered = TRUE)
ordered(c('low', 'high', 'middle', 'low', 'middle'))
ordered(c('low', 'high', 'middle', 'low', 'middle'), levels = c('low', 'middle', 'high'))

# 벡터 (Vector)
aaa <- numeric(length = 5)
aaa[1] <- 6
aaa[2] <- 2
class(aaa)
aaa[1] - aaa[2]
aaa[3] <- 'a string'
class(aaa)
aaa[1] - aaa[2]

x <- c(1, 2, 3, 4)
x
names(x) <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday')
x

# 벡터 내 데이터 접근
x[1]
x[-1]
x[c(1, 3, 4)]
x[2:4]
x['Monday']
x[c('Tuesday', 'Sunday')]
length(x)
NROW(x)
nrow(x)
y <- c()
y
x[10]

# 벡터의 특수 형태
seq(1, 10)
seq(1, 10, 2)
1:10

rep(1, 5)
rep(1:2, 5)
rep(1:2, each = 5)
rep(c(1,5), 3)
rep(c(1,5), 3, each = 4)

# 리스트 (List)
aaa <- list()
aaa[1] <- 4
aaa[2] <- 5
aaa[3] <- 'a string'
aaa

bbb <- list(name = 'dog1', height = 60)
bbb

bbb$name
bbb$height
bbb[[1]]

bbb[1]

# 행렬 (Matrix)
numeric.vector <- 1:20
numeric.vector
numeric.mat <- matrix(numeric.vector, 4, 5)
class(numeric.mat)
numeric.mat

matrix(numeric.vector, nrow = 4)
matrix(numeric.vector, ncol = 5)
matrix(numeric.vector, ncol = 5, byrow = T)

numeric.mat <- matrix(numeric.vector, ncol = 5, dimnames=list(c('a', 'b', 'c', 'd'), c('A', 'B', 'C', 'D', 'E')))
numeric.mat
colnames(numeric.mat)
rownames(numeric.mat)

# 행렬 (Matrix) 내 데이터 접근
numeric.mat[1, 1]
numeric.mat[2, 3]

numeric.mat[1:2, ]
numeric.mat[-3, ]
numeric.mat[c(1,3), c(3,1)]

numeric.mat['a', c('A', 'C')]

# 데이터 프레임 (Data Frame)
numeric.vector <- 1:5
character.vector <- letters[1:5]
class(numeric.vector)
class(character.vector)
df <- data.frame(x = numeric.vector, y = character.vector)
df
class(df)

df$v <- c('M', 'F', 'M', 'F', 'F')
df

df$x
df[1,]
df[2,3]

# 데이터 프레임 (Data Frame) 관련 함수
str(df)
head(df, 2)
tail(df, 2)

colnames(df)
colnames(df) <- c('first', 'second', 'third')
colnames(df)

rownames(df) <- letters[1:5]
df

# 데이터 유형 확인 및 변환
class(df)
class(df$first)
class(df$third)

is.numeric(df$first)
is.numeric(df$third)
is.data.frame(df)

as.factor(df$third)
as.numeric(df$third)
as.numeric(as.factor(df$third))
as.matrix(df)

# 제어문
if (TRUE) {
  print ('TRUE')
} else {
  print ('FALSE')
}

if (TRUE) {
  print ('TRUE')
}
else {
  print('FALSE')
}

# 반복문
for (i in 1:5) {
  print (i)
}

for (i in df$second) {
  print (i)
}

i <- 0
while (i < 5) {
  print (i)
  i <- i + 1
}

# 결측치의 처리
NA & T
NA + 1

sum(c(1, 2, 3, 4, NA))
sum(c(1, 2, 3, 4, NA), na.rm = T)

x <- data.frame(a = c(1, 2, 3), b = c('a', 'b', NA), c = c(4, NA, 6))
x
na.omit(x)
na.pass(x)
na.fail(x)




#######################
#### 연습 문제
#######################
# 다음과 같이 행렬과 데이터 프레임으로 구성되어 있는 리스트 lst를 생성한다.
lst <- list(
  mat = matrix(c(1.2, 2.5, 3.1, 1.5, 2.7, 3.2, 2.1, 2.1, 2.8), nrow = 3),
  df = data.frame(
    x1 = c('Park', 'Lee', 'Kim'),
    x2 = c(14, 16, 21)
  )
)
lst

# 리스트 lst의 각 요소에 다음과 같이 이름을 부여한다.
dimnames(lst$mat) <- list(
  c('Sub1', 'Sub2', 'Sub3'),
  c("Trt1", "Trt2", "Trt3")
)
colnames(lst$df) <- c("name", "sales")
lst
