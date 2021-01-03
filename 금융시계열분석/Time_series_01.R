a<- 1:10
a

b <- rnorm(10, 0, 1)
b

cc <- scan()
cc

dd1 <- matrix(cc, 2)
dd1

x <- dd1[1,]
y <- dd1[-1,]
x
y


dd1.reg <- lsfit(x, y, intercept = T)
dd1.reg

plot(x, y)
abline(lsfit(x, y), lty = 1)

matr <- matrix(1:9, nrow = 3)
matr

dimnames(matr) <- list(paste("row", c(1:3)), paste("col", c(1:3)))
matr

length(matr)
mode(matr)
dim(matr)
dimnames(matr)


matrix(1:9, nrow = 3)
matrix(c(1, 4, 7, 2, 5, 8, 3, 6, 9), byrow = T, ncol = 3)

r1 <- c(1, 4, 7)
r2 <- c(2, 5, 8)
r3 <- c(3, 6, 9)
rbind(r1, r2, r3)

c1 <- 1:3
c2 <- 4:6
c3 <- 7:9
cbind(c1, c2, c3)

m1 <- 1:9
dim(m1) <- c(3,3)
m1

mat <- matrix(c(1,2,3,4,5,6,7,8,9), ncol = 3, byrow = T) # byrow 의미 : 
mat
mat[1,]
mat[,3]
mat[mat[, 3] > 4, 1]
mat[mat[,3] > 4, 2]                           #3열에서 4보다 큰 행의 값 중 
mat[2,3]                                      #2행 3열의 값 추출
mat[2,, drop=F]                               #2행 값만을 추출
is.matrix(mat[2,,drop=F])                       #mat[2,,drop=F]가 
mat[-2,-3]                                    #2행과 3열의 값을 삭제
dim(mat)                                      #차수 표시 : 3행 3열
nrow(mat)                                     #행의 개수 표시 : 3행
ncol(mat)                                      #열의 개수 표시 : 3열
y <- diag(1:3)   #diag : 대각행렬을 생성
diag(y)                                        # diag(y) : 대각원소 표현
col(y)                                         #col(y) : 원소들의 열 번호 표현


# 예제
height <- c(140,155,142,175)                  #height 벡터 생성
size.1 <- matrix(c(130,26,110,24,118,25,112,25), ncol=2, byrow=T,
                   + dimnames=list(c("Abe", "Bob", "Carol", "Deb"), c("Weight", "Waist")))
#size.1 행렬 생성
size <- cbind(size.1, height)                   #size.1 행렬과 
# height 벡터의 열 기준 결합
colmean <- apply(size, 2, mean)              #2열의 평균값을 계산
rowmean <- apply(size, 1, mean)              #1행의 평균값을 계산
colvar <- apply(size, 2, var)                   #2열의 분산값을 계산
rowvar <- apply(size, 1, var)                   #1행의 분산값을 계산


A<-matrix(c(5,10,2,1), ncol=2)
B<-matrix(c(3,4,5,6), ncol=2)         
A+B         
A-B         
A*B         
A/B
A^B
A*1:2                                        #A의 1행×1, 2행×2


# 행렬 연산
m1 <- matrix(1:4, nrow=2)                     #1~4까지 2행 2열의 행렬 생성
m2 <- matrix(5:8, nrow=2)                     #5~8까지 2행 2열의 행렬 생성

# • 행렬의 곱 : %*%
m1%*%m2#m1과 m2 행렬의 곱셈

solve(m1)                            #m1행렬의 역 행렬 생성
t(m1)                                #m1행렬의 전치행렬 생성
eigen(m1)                             #m1행렬의 고유치와 고유벡터 
vec1 <- as.vector(m1)                  #행렬을 벡터로 변환
