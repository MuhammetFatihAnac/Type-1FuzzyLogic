##T1FF Fonksiyonlari

#Kume Merkezlerini Hesaplayacak Fonksiyon (BU FONKSIYONDA round islemini yapmasan daha iyi olur)
centers<-function(data_train, centers=3, iter_max=100){
  set.seed(16)
  
  data_train<-na.omit(data_train)
  
  fcm<-fcm(data_train, centers=centers, iter.max = iter_max)
  centers_df <- as.data.frame(fcm$v)
  
}


#Kullanim: centers<-centers(train_x)


#Uyelik Derecelerini Hesaplayacak Fonksiyon
membership_train<-function(data, centers=3, iter_max=100){
  set.seed(16)
  fcm<-fcm(data, centers=centers, iter.max = iter_max)
  membership_train_df<- as.data.frame(fcm$u)
  return(membership_train_df)
}

#Kullanim: membership_degrees_train<-membership_train(train_x)


#Bir sonraki islemi yapmadan once uzakliklari su sekilde hesaplaman gerekiyor:
  "distances <- matrix(0, nrow = nrow(test_x), ncol = nrow(centers))
  for (i in 1:nrow(test_x)) {
    for (j in 1:nrow(centers)) {
      distances[i, j] <- as.numeric(dist(rbind(test_x[i, ], centers[j, ])))
    }
  }"


#Test Verisindeki Her Bir Gozlemin Her Bir Kumeye Olan Uyelik Derecelerini Hesaplayan Fonksiyonun Yazilmasi

membership <- function(test_x, centers, distances, m=2){
  membership_degrees <- matrix(0, nrow = nrow(test_x), ncol = nrow(centers))
  for (i in 1:nrow(test_x)) {
    for (k in 1:nrow(centers)) {
      sum_terms <- 0
      for (j in 1:nrow(centers)) {
        sum_terms <- sum_terms + (distances[i, k] / distances[i, j])^(2/(m-1))
      }
      membership_degrees[i, k] <- (sum_terms)^(-1)
    }
  }
  return(membership_degrees)
}

#Kullanim: membership_degrees_test<-membership(test_x, centers, distances)


#Uyelik Dereceleri Modele Degisken Olarak Eklendikten Sonra Kurulan Model Uzerinden Tahmin Ureten Fonksiyonun Yazilmasi

predictions<-function(data_train, data_test, dependent_variable, membership_degrees_train, membership_degrees_test){
  predictions_matrix<-matrix(0, nrow=nrow(data_test), ncol=ncol(membership_degrees_test))
  for(i in 1:ncol(membership_degrees_test)){
    mu<-membership_degrees_train[,i]
    mu<-as.data.frame(mu)
    data_train_membership<-cbind(mu, data_train)
    
    mu<-membership_degrees_test[,i]
    mu<-as.data.frame(mu)
    data_test_membership<-cbind(mu, data_test)
    
    formula <- as.formula(paste(dependent_variable, "~ ."))
    
    model<-lm(formula, data=data_train_membership)
    set.seed(16)
    prediction<-predict(model,data_test_membership)
    for(k in 1: length(prediction)){
      predictions_matrix[k,i]<-prediction[k]*membership_degrees_test[k,i]
    }
  }
  return(predictions_matrix)
}

#Kullanim: raw_preds<-predictions(data_train, data_test, "Average.Rating", membership_degrees_train, membership_degrees_test)
#          preds_fcm<-rowSums(raw_preds)
