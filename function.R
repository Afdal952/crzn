t_test <- function(B, mx, data, n1) {
  yhat <- mx %*% B
  residuals <- data[, 1] - yhat
  SSE <- sum(residuals^2)
  MSE <- SSE / (nrow(data) - n1)
  var_cov_matrix <- MSE * ginv(t(mx) %*% mx)
  
  # Perform T-test
  t_statistics <- B / sqrt(diag(var_cov_matrix))
  p_values <- 2 * (1 - pt(abs(t_statistics), nrow(data) - n1))
  
  results <- data.frame(Parameters = colnames(mx), Estimate = B, 
                        Std.Error = sqrt(diag(var_cov_matrix)), 
                        t_value = t_statistics, P_Value = p_values)
  return(results)
}

# Run K-Means
trun <- function(data,a,power)
{
  data[data<a] <- a
  (data-a)^power
}

#INVERS MATRIX
MPL<-function(x,eps=1e-20)
{
  x<-as.matrix(x)
  xsvd<-svd(x)
  diago<-xsvd$d[xsvd$d>eps]
  if(length(diago)==1)
  {
    xplus<-as.matrix(xsvd$v[,1])%*%t(as.matrix(xsvd$u[,1])/diago)
  }
  else
  {
    xplus<-
      xsvd$v[,1:length(diago)]%*%diag(1/diago)%*%t(xsvd$u[,1:length(diago)])
  }
  return(xplus)
}

uji12=function(para){
  library(readxl) 
  data=Metpen1
  knot=read_excel("cluster12.xlsx")
  knot=knot[,-13:-14]
  data=as.matrix(data)  
  knot=as.matrix(knot)  
  ybar=mean(data[,1])  
  m=para+2  
  p=nrow(data)  
  q=ncol(data)  
  dataA=cbind(data[,m],data[,m],data[,m+1],data[,m+1],data[,m+2],data[,m+2],data[,m+3],data[,m+3],data[,m+4],data[,m+4],data[,m+5],data[,m+5])  
  dataA=as.matrix(dataA)  
  satu=rep(1,p)  
  n1=ncol(knot)  
  data.knot=matrix(ncol=n1,nrow=p)  
  for(i in 1:n1)  
  {  
    for(j in 1:p)  
    {  
      if (dataA[j,i]<knot[1,i])  
        data.knot[j,i]=0  
      else data.knot[j,i]=dataA[j,i]-knot[1,i]  
    }  
  }  
  mx=cbind(satu,data[,2],data.knot[,1:2],data[,3],data.knot[,3:4],data[,4],data.knot[,5:6],data[,5],data.knot[,7:8],data[,5],data.knot[,9:10],data[,6],data.knot[,11:12])  
  mx=as.matrix(mx) 
  B=(ginv(t(mx)%*%mx))%*%t(mx)%*%data[,1]  
  cat("=======================================","\n")  
  cat("Estimasi Parameter","\n")  
  cat("=======================================","\n")  
  print(B)  
  n1=nrow(B)  
  yhat=mx%*%B  
  res=data[,1]-yhat  
  SSE=sum((data[,1]-yhat)^2)  
  SSR=sum((yhat-ybar)^2)  
  SST=SSR+SSE 
  MSE=SSE/(p-n1) ;  MSR=SSR/(n1-1)  
  Rsq=(SSR/(SSR+SSE))*100 
  print(Rsq)
  
  # Perhitungan AIC
  L = -(p/2) * log(2*pi) - (p/2) * log(SSE/p) - (1/(2*MSE)) * SSE
  k = ncol(mx)  # Jumlah parameter yang diestimasi
  AIC = 2*k - 2*L
  cat("=======================================","\n")
  cat("Nilai AIC dari model","\n")
  cat("=======================================","\n")
  cat("AIC =", AIC, "\n")
  
  # Model tanpa knots (model regresi linear)
  fit_reduced <- lm(data[,1] ~ data[,2] + data[,3] + data[,4] + data[,5] + data[,6] + data[,7])
  yhat_reduced <- predict(fit_reduced)
  SSE_reduced <- sum((data[,1] - yhat_reduced)^2)
  MSE_reduced <- SSE_reduced / (p - length(coef(fit_reduced)))
  
  # Uji F
  F_statistic <- (((SSE_reduced - SSE)/6) / (SSE/(p-6)))
  p_value_f <- 1 - pf(F_statistic, 6, p - 6)
  print(paste("F-statistic:", F_statistic))
  print(paste("P-value (F):", p_value_f))
  
  # Uji t-statistik
  t_test_result <- t_test(B, mx, data, n1)
  print(t_test_result)
} 
uji23=function(para){
  data=Metpen2
  knot=read_excel("cluster23.xlsx")
  knot=knot[,-19:-20]
  data=as.matrix(data)  
  knot=as.matrix(knot)  
  ybar=mean(data[,1])  
  m=para+2  
  p=nrow(data)  
  q=ncol(data)  
  dataA=cbind(data[,m],data[,m],data[,m],data[,m+1],data[,m+1],data[,m+1],data[,m+2],data[,m+2],data[,m+2],
              data[,m+3],data[,m+3],data[,m+3],data[,m+4],data[,m+4],data[,m+4],data[,m+5],data[,m+5],data[,m+5])  
  dataA=as.matrix(dataA)  
  satu=rep(1,p)  
  n1=ncol(knot)  
  data.knot=matrix(ncol=n1,nrow=p)  
  for(i in 1:n1)  
  {  
    for(j in 1:p)  
    {  
      if (dataA[j,i]<knot[1,i])  
        data.knot[j,i]=0  
      else data.knot[j,i]=dataA[j,i]-knot[1,i]  
    }  
  }  
  mx=cbind(satu,data[,2],data.knot[,1:3],data[,3],data.knot[,4:6],data[,4],data.knot[,7:9],data[,5],data.knot[,10:12],data[,6],data.knot[,13:15],data[,7],data.knot[,16:18])  
  mx=as.matrix(mx) 
  B=(ginv(t(mx)%*%mx))%*%t(mx)%*%data[,1]  
  cat("=======================================","\n")  
  cat("Estimasi Parameter","\n")  
  cat("=======================================","\n")  
  print(B)  
  n1=nrow(B)  
  yhat=mx%*%B  
  res=data[,1]-yhat  
  SSE=sum((data[,1]-yhat)^2)  
  SSR=sum((yhat-ybar)^2)  
  SST=SSR+SSE 
  MSE=SSE/(p-n1) ;  MSR=SSR/(n1-1)  
  Rsq=(SSR/(SSR+SSE))*100 
  print(Rsq)   
  
  # Perhitungan AIC
  L = -(p/2) * log(2*pi) - (p/2) * log(SSE/p) - (1/(2*MSE)) * SSE
  k = ncol(mx)  # Jumlah parameter yang diestimasi
  AIC = 2*k - 2*L
  cat("=======================================","\n")
  cat("Nilai AIC dari model","\n")
  cat("=======================================","\n")
  cat("AIC =", AIC, "\n")
  
  # Model tanpa knots (model regresi linear)
  fit_reduced <- lm(data[,1] ~ data[,2] + data[,3] + data[,4] + data[,5] + data[,6] + data[,7])
  yhat_reduced <- predict(fit_reduced)
  SSE_reduced <- sum((data[,1] - yhat_reduced)^2)
  MSE_reduced <- SSE_reduced / (p - length(coef(fit_reduced)))
  
  # Uji F
  F_statistic <- (((SSE_reduced - SSE)/6) / (SSE/(p-6)))
  p_value_f <- 1 - pf(F_statistic, 6, p - 6)
  print(paste("F-statistic:", F_statistic))
  print(paste("P-value (F):", p_value_f))
  
  # Uji t-statistik
  t_test_result <- t_test(B, mx, data, n1)
  print(t_test_result)
}
uji33=function(para){
  library(readxl) 
  data=Metpen3
  knot=read_excel("cluster33.xlsx")
  knot=knot[,-19:-20]
  data=as.matrix(data)  
  knot=as.matrix(knot)  
  ybar=mean(data[,1])  
  m=para+2  
  p=nrow(data)  
  q=ncol(data)  
  dataA=cbind(data[,m],data[,m],data[,m],data[,m+1],data[,m+1],data[,m+1],data[,m+2],data[,m+2],data[,m+2],
              data[,m+3],data[,m+3],data[,m+3],data[,m+4],data[,m+4],data[,m+4],data[,m+5],data[,m+5],data[,m+5])  
  dataA=as.matrix(dataA)  
  satu=rep(1,p)  
  n1=ncol(knot)  
  data.knot=matrix(ncol=n1,nrow=p)  
  for(i in 1:n1)  
  {  
    for(j in 1:p)  
    {  
      if (dataA[j,i]<knot[1,i])  
        data.knot[j,i]=0  
      else data.knot[j,i]=dataA[j,i]-knot[1,i]  
    }  
  }  
  mx=cbind(satu,data[,2],data.knot[,1:3],data[,3],data.knot[,4:6],data[,4],data.knot[,7:9],data[,5],data.knot[,10:12],data[,6],data.knot[,13:15],data[,7],data.knot[,16:18])  
  mx=as.matrix(mx) 
  B=(ginv(t(mx)%*%mx))%*%t(mx)%*%data[,1]  
  cat("=======================================","\n")  
  cat("Estimasi Parameter","\n")  
  cat("=======================================","\n")  
  print(B)  
  n1=nrow(B)  
  yhat=mx%*%B  
  res=data[,1]-yhat  
  SSE=sum((data[,1]-yhat)^2)  
  SSR=sum((yhat-ybar)^2)  
  SST=SSR+SSE 
  MSE=SSE/(p-n1) ;  MSR=SSR/(n1-1)  
  Rsq=(SSR/(SSR+SSE))*100 
  print(Rsq)   
  
  # Perhitungan AIC
  L = -(p/2) * log(2*pi) - (p/2) * log(SSE/p) - (1/(2*MSE)) * SSE
  k = ncol(mx)  # Jumlah parameter yang diestimasi
  AIC = 2*k - 2*L
  cat("=======================================","\n")
  cat("Nilai AIC dari model","\n")
  cat("=======================================","\n")
  cat("AIC =", AIC, "\n")
  
  # Model tanpa knots (model regresi linear)
  fit_reduced <- lm(data[,1] ~ data[,2] + data[,3] + data[,4] + data[,5] + data[,6] + data[,7])
  yhat_reduced <- predict(fit_reduced)
  SSE_reduced <- sum((data[,1] - yhat_reduced)^2)
  MSE_reduced <- SSE_reduced / (p - length(coef(fit_reduced)))
  
  # Uji F
  F_statistic <- (((SSE_reduced - SSE)/6) / (SSE/(p-6)))
  p_value_f <- 1 - pf(F_statistic, 6, p - 6)
  print(paste("F-statistic:", F_statistic))
  print(paste("P-value (F):", p_value_f))
  
  # Uji t-statistik
  t_test_result <- t_test(B, mx, data, n1)
  print(t_test_result)
}