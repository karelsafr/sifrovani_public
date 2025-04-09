library(Rcpp)
cppFunction('
int casovaNarocnostCPP(int input, int x) {
  int sum = 0;
  for(int i = 0; i <= input; i++){
    if(input < x){
    sum += i;
    } else {
    sum -= i;
    }
  }
  return sum;
}
')

cppFunction(
"
int casovaNarocnostCPP2(int input, int x) {
    // Vzorec pro součet čísel od 0 do n: n * (n + 1) / 2
    long long n = input;  // použijeme long long pro prevenci přetečení
    long long result = (n * (n + 1)) / 2;
    
    // Pokud input >= x, změníme znaménko výsledku
    if (input >= x) {
        result = -result;
    }
    
    return result;
}
")

casovaNarocnostR <- function(input, x) {
  sum <- 0
  for(i in 0:input) {
    if(input < x) {
      sum <- sum + i
    } else {
      sum <- sum - i
    }
  }
  return(sum)
}

library(microbenchmark)

casovaNarocnostCPP(10, 20)

a = 1000000
b = 50000
res=microbenchmark(CPP=casovaNarocnostCPP(a, b), CPP2=casovaNarocnostCPP2(a, b), R=casovaNarocnostR(a,b))
res
summary(res)$mean / min(summary(res)$mean)
