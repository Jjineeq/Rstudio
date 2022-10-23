names = matrix(c(1,4,8,5,7,1),ncol=2) #A,B,C 좌표 입력
namesx = matrix(names[,1]) # names A,B,C 의 X좌표
namesy = matrix(names[,2]) # names A,B,C 의 y좌표


distance = function(x,y,names){ # 새로운 함수 만들기
  dist_mat= matrix(0,3,2) # 출력 값 넣을 matrix
  for (i in 1:3) {
    temp = matrix(c(abs(x-namesx))+c(abs(y-namesy))) # 맨하탄거리 공식 abs = 절댓값
    dist_mat = temp
  }
  return(dist_mat) #matrix에 저장 후 다시 시작
}

distance(1,2,) # 새롭게 발생된 좌표 생성
a = c(distance(1,2,)) # a라고 새롭게 정의후 if문 

if (a[1]<=a[2] & a[1]<=a[3]){ #거리가 같다면 범죄이력이 많은 사람이 범인으로 설정
  result = "A가 범인"
}else if(a[2]<a[1] & a[2]<a[3]){
  result = "B가 범인"
}else {
  result = "C가 범인"
}

print(result)

#distance와 a 에 새로운 좌표 값 동일하게 넣고 ctrl + alt + R으로실행하면 범인이 출력됩니다