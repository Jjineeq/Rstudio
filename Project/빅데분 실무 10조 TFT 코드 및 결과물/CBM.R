################
##  CBM code  ## 
################

## 2가지 버전으로작성 
## 차이점은 역행렬과 일반적 영행렬 차이만 존재 나머지 코드는 동일

###############################
##  첫번째 버전 - ginv 사용  ##
###############################

cbm = function(trdat, tedat, alpha,k) {
  
  tr_k = kmeans(trdat,k)
  s1 = cbind(trdat,tr_k$cluster)
  cbm_mat = matrix(0,nrow(tedat),k)
  
  for(i in 1:k){
    s2 = subset(s1,s1$`tr_k$cluster`==i)
    
    obs = nrow(s2)
    dim = ncol(trdat)
    
    mu = colMeans(s2[,1:dim])
    
    sinv = ginv(cov(s2[,1:dim])) 
    
    mu_mat = repmat(mu, nrow(tedat),1)
    dte = tedat-mu_mat
    
    Tsq_mat = matrix(numeric(0), nrow(tedat),1)
    

    for( j in 1:nrow(tedat)) {
      Tsq_mat[j,1] = as.double(dte[j,]) %*% sinv %*% t(t(as.double(dte[j,])))
    }
    cbm_mat[,i] = Tsq_mat
  }
  cbm_res = apply(cbm_mat,1,min)  
  ret = list(cbm_res=cbm_res) 

  return (ret)                       
}

################################
##  두번째 버전 - solve 사용  ##
################################


cbm_solve = function(trdat, tedat, alpha,k) {
  
  tr_k = kmeans(trdat,k)
  s1 = cbind(trdat,tr_k$cluster)
  cbm_mat_solve = matrix(0,nrow(tedat),k)
  
  for(i in 1:k){
    s2 = subset(s1,s1$`tr_k$cluster`==i)
    
    obs = nrow(s2)
    dim = ncol(trdat)
    
    mu_solve = colMeans(s2[,1:dim])
    
    sinv = solve(cov(s2[,1:dim])) 
    
    mu_mat_solve = repmat(mu_solve, nrow(tedat),1) 
    dte_solve = tedat-mu_mat_solve 
    
    Tsq_mat_solve = matrix(numeric(0), nrow(tedat),1)  
    
    for( j in 1:nrow(tedat)) {
      Tsq_mat_solve[j,1] = as.double(dte_solve[j,]) %*% sinv %*% t(t(as.double(dte_solve[j,])))
    }
    cbm_mat_solve[,i] = Tsq_mat_solve
  }
  cbm_res_solve = apply(cbm_mat_solve,1,min)
  ret_solve = list(cbm_res_solve=cbm_res_solve) 
  
  return (ret_solve)                       
}
