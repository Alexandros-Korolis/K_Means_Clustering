# Set working directory
getwd()
setwd("C:/Users/admin/Desktop/clustering project")

# Create sample dataset 
x = c(0,0,1.5,5,5)
y = c(2,0,0,0,2)
data = data.frame(X = x, Y = y)

initial_centroids = function(K, data){
  #set.seed(15)
  centroids = data.frame()
  random_ind = sample(1:nrow(data), 2)
  for (i in 1:K) {
    centroids = rbind(centroids,c(data[random_ind[i],]))
  }
  return(centroids)
}

euclidean_dist = function(x,y){
  distance = sqrt(sum((x-y)^2))
  return(distance)
}

distance_matrix = function(centroids,data){
  K = nrow(centroids)
  dist_mat_df = data.frame()
  for (i in 1:K) {
    for(j in 1:dim(data)[1]){
      M = as.vector(unlist(centroids[i,]))
      y = as.vector(unlist(data[j,]))
      dist_mat_df[i,j] = euclidean_dist(M,y)
    }
  }
  return(dist_mat_df)
}

cluster_update = function(dist_mat,K){
  ind = c()
  for(i in 1:length(colnames(dist_mat))){
    ind = c(ind,which(dist_mat[,i] == min(dist_mat[,i])))
  }
  return(ind)
}

update_centroids = function(dist_mat,K,data){
  indexes = cluster_update(dist_mat,K)
  centroids = data.frame()
  for (i in 1:K) {
    centroids = rbind(centroids,apply(data[which(indexes == i),],2,mean))
  }
  return(centroids)
}

update_centroids(distance_matrix(initial_centroids(K=2,data = data),data = data),K = 2,data = data)

my_kmeans = function(data,n_clusters){
  K = n_clusters
  int_centroids = initial_centroids(K,data = data)
  dist_mat = distance_matrix(int_centroids,data = data)
  cl_update = cluster_update(dist_mat,K)
  up_centroids = update_centroids(dist_mat,K,data)
  
}





