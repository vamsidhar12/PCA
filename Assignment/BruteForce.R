#Reading The dataset
data_in <- read.csv(file = 'iris.csv',sep = ',', header = TRUE)

#Converting to dataframe
data_f <- data.frame(data_in)
data_f

#dimension of Dataframe
num_col <- (dim(data_f)[2])
num_row  <- (dim(data_f)[1])
#Selecting the independent features of the dataset
data_f[,1:4]

#standardizing the features
f1 = (data_f[,1]-mean(data_f[,1]))/sd(data_f[,1])
f1
f2 = (data_f[,2]-mean(data_f[,2]))/sd(data_f[,2])
f2
f3 = (data_f[,3]-mean(data_f[,3]))/sd(data_f[,3])
f3
f4 = (data_f[,4]-mean(data_f[,4]))/sd(data_f[,4])
f4
#Converting the features into matrices
data_m <- matrix(c(f1, f2, f3, f4), nrow = 147,byrow = TRUE, ncol = 4)
data_m

data_fe <- matrix(c(data_f[,1],data_f[,2],data_f[,3],data_f[,4]), nrow = 147,byrow = TRUE, ncol = 4)




#Printing the covariance matrix
print("The Covariance Matrix is:\n ")
print(t(data_m) %*% (data_m))


#Calculating the Eigen Vectors
eigen_vectors = eigen((t(data_m) %*% (data_m)))


#COmputing and printing the eigen values and vectors

print("The Eigen Values are:\n ")
print(eigen_vectors$values)
print("The Eigen Vectors are:\n ")
print(eigen_vectors$vectors)


((data_m)  %*% t(eigen_vectors$vectors))

#finding the principle components using eigen vectors


pca_values <- ((data_m)  %*% t(eigen_vectors$vectors))

pca_values
#Converting the Principle components into a dataframe
pca_df <- data.frame(pca_values)
pca_df

