library(readxl)
kalpVeriSeti <- read_excel("D:/hepsi/Desktop/ceyda/İSTATİSTİK-4/B) İST 452 ÖZEL KONU/kalpVeriSeti.xlsx")
View(kalpVeriSeti)


# Normalizasyon
#install.packages("clusterSim")
library(clusterSim)

norm_kalpVeriSeti<-kalpVeriSeti

norm_kalpVeriSeti$yas<-data.Normalization(norm_kalpVeriSeti$yas,type="n4",normalization = "column")

norm_kalpVeriSeti$dinkanbas<-data.Normalization(norm_kalpVeriSeti$dinkanbas,type="n4",normalization = "column")
                                           
norm_kalpVeriSeti$serkol<-data.Normalization(norm_kalpVeriSeti$serkol,type="n4",normalization = "column")
                                        
norm_kalpVeriSeti$kalpathiz<-data.Normalization(norm_kalpVeriSeti$kalpathiz,type="n4",normalization = "column")
                                           
norm_kalpVeriSeti$STDep<-data.Normalization(norm_kalpVeriSeti$STDep,type="n4",normalization = "column")

norm_kalpVeriSeti$anaDamSay<-data.Normalization(norm_kalpVeriSeti$anaDamSay,type="n4",normalization = "column")

                                           
c_kalpVeriSeti<-norm_kalpVeriSeti[,c("yas","dinkanbas","serkol","kalpathiz","STDep","anaDamSay","kalpHastaligi")]
kalpVeriSeti$kalpHastaligi<-
  as.factor(x=c_kalpVeriSeti$kalpHastaligi)


#faktöre ait seviyeleri yeniden yazdırmak için
library(plyr) 
revalue(c_kalpVeriSeti$kalpHastaligi,
        c("0"= "yok", "1"="var")) 

#seviyeleri görebilmek için 
table(c_kalpVeriSeti$kalpHastaligi) 
summary(c_kalpVeriSeti)


#kalpverisetini eğitim_verisi(%80) ve test_verisi(%20) olarak ikiye ayırma
library(caret)

set.seed(123) # set.seed komutu tekrar üretilebilir rastgele sayı elde etmeye imkan verir

egitim_indeks <- createDataPartition(y=c_kalpVeriSeti$kalpHastaligi, p = 0.8,list=FALSE)

head(egitim_indeks)

egitim <- c_kalpVeriSeti[egitim_indeks,]
test <- c_kalpVeriSeti[-egitim_indeks,]

testNitelikleri<-test[,-7] 
testHedefNitelik<-test[[7]]

egitimNitelikleri<-egitim[,-7] 
egitimHedefNitelik<-egitim[[7]]

#k değeri 1'den 5'e kadar deneniyor. Doğruluk değerleri hesaplanıyor.
kdegeri<-5
dogruluk<-NULL
library(class)
for (i in 1:kdegeri) { 
  set.seed(1) 
  tahminiSiniflar=
    knn(egitimNitelikleri,testNitelikleri,egitimHedefNitelik,k=i) 
  dogruluk[i]<-mean(tahminiSiniflar==testHedefNitelik) 
  dogruluk[i]<-round(dogruluk[i],2)
  
}
for (i in 1:kdegeri)
{
  print(paste0("k=",i,"için elde edilen doğruluk=",dogruluk[i]))
  
}

plot(dogruluk, type="b", xlab="K-degerleri",ylab="Dogruluk",col="blue")

#confusion matrix
tm<- table(tahminiSiniflar,testHedefNitelik,dnn=c("TahminiSınıflar","GercekSınıflar"))
tm
confusionMatrix(tm)

