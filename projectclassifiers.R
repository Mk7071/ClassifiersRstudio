#1) B_Classifier:
#class_a_file_read
dataset = read.csv("C:\\Users\\marya\\OneDrive\\Desktop\\R_Project\\G3\\dataset_b2.csv", header=FALSE)
head(dataset)

# Storing values from each plot and Calculating The Mean
plot_bo = (dataset[which(dataset$V1 == "bo"), "V2"])
plot_bn = (dataset[which(dataset$V1 == "bn"), "V2"])
#print(plot_bo)
#print(plot_bn)
summary(plot_bo)
summary(plot_bn)
plot_bo_mean = mean(plot_bo)
plot_bn_mean = mean(plot_bn)

classify_a = function(value) {
  
  #Finding the Difference between given value and mean of each plot
  diff_bo = (abs(value - plot_bo_mean))
  diff_bn = (abs(value - plot_bn_mean)) 
  #print(diff_bo)
  #print(diff_bn)
  
  
  #Calculating the Euclidean distance between each value and the mean of each plot
  plot_bo_distance = sqrt((value - plot_bo_mean)^2)
  plot_bn_distance = sqrt((value - plot_bn_mean)^2)
  #Calculating the weight of each plot
  plot_bo_weight <- 1 / plot_bo_distance
  plot_bn_weight <- 1 / plot_bn_distance
  
  #print(plot_bo_distance)
  #print(plot_bo_weight)
  #print(plot_bn_distance)
  #print(plot_bn_weight)
  
  #Main Assigning Block 
  if (value >= min(plot_bo) & value <= max(plot_bn)) { 
    #Handling the case for overlapping Values around 70% accuracy
    class_var = ifelse(plot_bo_weight < plot_bn_weight, "bn", "bo")
    overlap_test = 0
  } else if (diff_bo < diff_bn) {
    #Handling other cases
    class_var = "bo"
    overlap_test = 1
  } else {
    class_var = "bn"
    overlap_test = 1
  }
  
  #Output
  if (overlap_test == 1) {
    print(sprintf("We are sure that Value:%s belongs to Class:%s", value, class_var))
  } else {
    print(sprintf("Based on the data we cannot predict exactly but we expect Value:%s belongs to Class:%s", value, class_var))
  }
  return(class_var)
}
#Test Data
#value = 0.4985006497698723
#value = 0.4001947980476131
#value = 0.5079083
#value = 0.8533466020113307
#value = 0.318719881945302
value = 0.8533466020113307
predicted_class = classify_a(value)
print(predicted_class)


#2) C_Classifier:
#class_b_file read
dataset = read.csv("C:\\Users\\marya\\OneDrive\\Desktop\\R_Project\\G3\\2\\c0.csv", header=FALSE)
head(dataset)
print(dataset)

plot_cw = (dataset[which(dataset$V1 == "cw"), "V2"])
plot_cb = (dataset[which(dataset$V1 == "cb"), "V2"])
plot_cw_mean = mean(plot_cw)
plot_cb_mean = mean(plot_cb)

classify_b = function(my_vector) {
  class_vector = data.frame()
  for (value in my_vector) {
    #Finding the Difference between given value and mean of each plot
    diff_cw = (abs(value - plot_cw_mean))
    diff_cb = (abs(value - plot_cb_mean)) 
    #print(diff_cw)
    #print(diff_cb)
    if (diff_cw < diff_cb) {
      class_var = "cw"
    } else {
      class_var = "cb"
    }
    #print(sprintf("value:%f class:%s", value, class_var))
    var = data.frame(Value = value, Class = class_var)
    class_vector = rbind(class_vector, var)
  }
  return(class_vector)
}

#Test_Data for cw and cb
my_vector <- c(
  138.14029184676096,
  283.2459008370163,
  218.56034453714886,
  112.59294972762457,
  124.43228727081053,
  242.52112398529826,
  248.7063571683624,
  257.2984686191345,
  233.7010019330014,
  271.6120424590473,
  261.55972448550574,
  125.32886062708802,
  134.71299386786941,
  266.2403438369847,
  251.8423507788462,
  124.8664180608064,
  240.69260539990609,
  145.86063240704325
)

predicted_class_vector = classify_b(my_vector)
print(predicted_class_vector)

#3) O_classifier
Dataset_04<-read.table(file="C:/Users/marya/Downloads/dataset_o2.csv",header= TRUE,sep=",")#reading data and getting overview
str(Dataset_04)

classes<-unique(Dataset_04$o4)#determining the classes in the data
classes

colnames(Dataset_04)<-c("o4","V1","V2","V3","V4","V5","V6","V7","V8")#renaming my columns, with the 8 features
View(Dataset_04)

features_data<- Dataset_04[,-1]#defining the feature columns


kmeans_model <- kmeans(features_data, centers = length(classes))# Training the k-means model


data_with_labels <- cbind(features_data, class = Dataset_04$o4)# Create a data frame that includes both features and class labels

# For each cluster, find the most common class

cluster_labels <- sapply(1:length(classes), function(cluster) {
  cluster_instances <- data_with_labels[kmeans_model$cluster == cluster, ]
  most_common_class <- names(which.max(table(cluster_instances$class)))
  return(most_common_class)
})

classify_c <- function(values_vector, kmeans_model, cluster_labels) {
  
  # Find the index of the closest cluster center
  closest_cluster <- which.min(apply(kmeans_model$centers, 1, function(center) sum((values_vector - center)^2)))
  
  # Map the cluster to the most common class in that cluster
  class_label <- cluster_labels[closest_cluster]
  
  return(class_label)
}
#trying out random vectors to see if the model predicts the correct class
values_vector <- c(1.2046813
                   ,0.0041379240
                   ,0.0024713578
                   ,0.0066566249
                   ,0.066596229
                   ,9.263932e-01
                   ,1.139869538
                   ,4.273560e-02)
class_result <- classify_c(values_vector, kmeans_model,cluster_labels)
print(class_result)





