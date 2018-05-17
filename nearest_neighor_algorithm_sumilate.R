write_cycle <- function(x,y,r){
  
  theta = seq(0, 2*pi, length.out = 50) # θの定義
  
  plot(r*cos(theta) + x, r*sin(theta) + y, # 円の媒介変数表示
       xlim = c(-15,15), ylim = c(-15,15),
       type = "l", asp = 1, ann = F)
  
  par(new=T) 

}

show_distance_point_to_point <- function(a,b){
  c <- c(b-a)
  distance <- sqrt(c[1][1]^2 + c[2][1]^2) # 二点間の距離
  return(distance)
}

point_in_cycle <- function(dot_number, kind, cycle_center, r){# 円内にドットを打つ

norm_x <- rnorm(dot_number) * 3
norm_y <- rnorm(dot_number) * 3

i <- 1

total_random_point_x <- 0
total_random_point_y <- 0

total_random_point_x <- total_random_point_x[-1]
total_random_point_y <- total_random_point_y[-1]

for (i in 1:dot_number){
  
  random_point <- c(norm_x[i][1],norm_y[i][1])
  

  
  if(show_distance_point_to_point(cycle_center,random_point) < r){# 円内にあるかの判定

    total_random_point_x <- c(total_random_point_x,norm_x[i][1])
    total_random_point_y <- c(total_random_point_y,norm_y[i][1])
    
    
    plot(norm_x[i][1],norm_y[i][1],xlim = c(-15,15), ylim = c(-15,15), col = kind + 1, pch = kind - 1)
    par(new=T) 
  }
    
}

if(length(total_random_point_x) >= 2){
  
  
  return(list(total_random_point_x,total_random_point_y))
  
  
  
}

}

library(deldir)

total_random_point_x <- 0
total_random_point_y <- 0

total_random_point_x <- total_random_point_x[-1]
total_random_point_y <- total_random_point_y[-1]

KIND <- 1



for(i in 1:6){# 円を六つ描く
  # 円のx,y座標,半径r
    
  X = sample(c(-10:10),1)
  Y = sample(c(-10:10),1)
  R = sample(c(1:5),1)
  write_cycle(X,Y,R)

    
  cycle_center <- c(X,Y) #円の中心ベクトル
  total_random_point <- point_in_cycle(500,KIND,cycle_center,R)

  
  if(!(is.null(total_random_point))){
    total_random_point_x <- c(total_random_point_x,unlist(total_random_point[1]))
    total_random_point_y <- c(total_random_point_y,unlist(total_random_point[2]))
  }
  
  
  KIND <- KIND + 1
  
}
#print(total_random_point_x)
#print(total_random_point_y)
plot(deldir(total_random_point_x,total_random_point_y),xlim = c(-15,15), ylim = c(-15,15))

  

  

