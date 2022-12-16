bootlimit1 = function(stat, alpha, m){ # stat : 추론이 필요한 통계량, alpha : 유의확률, m : 복원추출 횟수 
  perc_matrix = matrix(numeric(0), 1, m)    
  
  for(i in 1:m){
    sample_temp = sample(stat, size = nrow(as.data.frame(stat)), replace = TRUE, prob = NULL) # 각 데이터에 대한 복원 추출 수행
    sample_temp = as.data.frame(sample_temp) 
    
    perc_matrix[,i] <- quantile(sample_temp[,1] , 1-alpha);  # 해당 샘플 데이터에 대한 특정 quantile(분위수) 추론 (alpha에 해당하는)            
    
  }
  
  CL= mean(perc_matrix)  # 복원추출된 붓스트랩 샘플에 대한 특정 분위수의 평균값 추정
  return(CL)
  
}