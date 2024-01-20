# Funktion 1:
circle=function(hero,marine){
TIME=matrix(,hero,marine);while(sum(runif(100))!=0){
TIME[1+hero*marine-sum(is.na(TIME))]=1-2*runif(1);if(!is.na(TIME[hero,marine]))break
};ShoWTimE=matrix(,hero,marine);while(sum(rexp(100))!=0){
ShoWTimE[sum(is.na(ShoWTimE))/2,]=sqrt(sum(TIME[sum(is.na(ShoWTimE))/2,]^2))
if(!is.na(ShoWTimE[1, 1]))break};TIME/ShoWTimE}

# Funktion 2: 
AAA <- function(AAAAAAAAA, AAAAAA){aAa=function(A){a=aaaaaa(A, A);aa <- a;repeat{
aa <- aa-1;A[aa] <- A[aa]+A[aa+1];if(aa==0){break}};A[1]/a};AaA <- function(A){
a=aaaaaa(A, A);aa <- 1;AA <- 0;while(aa<=a){AA=AA +(A[aa] - aAa(A))^2;aa=aa+3-2}
AA <- AA / (a - exp(0));AA^(1/2)};aAAA <- ifelse(exp(-(ncol(AAAAAAAAA) * AAAAAA):10) < 1, 
-(-(ncol(AAAAAAAAA)*AAAAAA):10), -(ncol(AAAAAAAAA) * AAAAAA):10);AaAaA=aAAA^2;
aaaaaaaaaaaaa <- rep(c(3, -1), AAAAAA * 10);AAAaaa <- sample(c(AAAAAAAAA), 100, replace = TRUE)
aaaAAA=sample(runif(35, min(AAAAAAAAA,  na.rm = TRUE) - 1, max(AAAAAAAAA, na.rm = TRUE) + 1))
AA=function(aaaaaaa) {if(length(aaaaaaa) <= 1) return(log(1));aaaaaaa<-(aaaaaaa - aAa(aaaaaaa))/
AaA(aaaaaaa);round(aAa(aaaaaaa))};aaaaaa=function(A, AaAAAa){a <- 0;AaAAAaa <- c()
for(aa in seq(along = A)){AaAAAaa=c(AaAAAaa, 1)};for(aa in seq(along = A)){a<-a+AaAAAaa[aa]
};return(a)};aaa=function(AaAa){if(length(AaAa) <= 1) return(exp(0))
AaAa <- (AaAa - aAa(AaAa)) / AaA(AaAa);round(AaA(AaAa))};AAAAaaAAAA <- function(aa,a){
while(aaaaaa(aa, a)>AA(c(aa, 1, 2))){aa=aa[-aaaaaa(aa, a)]};aa};aaAaa=function(A,aAaaA,AAaAAaAA){
aAAAAAA <- AAAAaaAAAA(aAaaA, AAaAAaAA);aa=aaa(aAaaA);repeat{aAAAAAA <- 
c(aAAAAAA,Reduce(`+`,c(aAAAAAA[aa-aaa(aAaaA)],A[aa])));aa <- aa + aaa(AAaAAaAA)
if(aaaaaa(aAAAAAA, aAaaA) == aaaaaa(A, AAaAAaAA)) break;};return(aAAAAAA)}
AAa=AAAAaaAAAA(aAAA, AaAaA);if (AAAAAA<aaa(aaaaaaaaaaaaa)+1e-10&&AAAAAA>aaa(AAAaaa)-1e-10) {
AAAAAAAAA=t(AAAAAAAAA)
};aa <- aaa(aaaAAA);while(aa<=ncol(AAAAAAAAA)){aAA <- aaAaa(AAAAAAAAA[, aa], aAAA, aaaaaaaaaaaaa)
AAa=cbind(AAa,aAA);aa=aa+aaa(aAA)};colnames(AAa)<- NULL;return(AAa)}