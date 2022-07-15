library(readxl)
kalpVeriSeti <- read_excel(""C:/Users/ceyda/OneDrive/Masaüstü/ceyda/İSTATİSTİK-4/B) İST 452 ÖZEL KONU/kalpVeriSeti.xlsx")
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




################################################
                    OUTPUT
################################################


> library(readxl)
> kalpVeriSeti <- read_excel("C:/Users/ceyda/OneDrive/Masaüstü/ceyda/İSTATİSTİK-4/B) İST 452 ÖZEL KONU/kalpVeriSeti.xlsx")
> View(kalpVeriSeti)
> 
> 
> # Normalizasyon
> #install.packages("clusterSim")
> library(clusterSim)
> 
> norm_kalpVeriSeti<-kalpVeriSeti
> 
> norm_kalpVeriSeti$yas<-data.Normalization(norm_kalpVeriSeti$yas,type="n4",normalization = "column")
> 
> norm_kalpVeriSeti$dinkanbas<-data.Normalization(norm_kalpVeriSeti$dinkanbas,type="n4",normalization = "column")
>                                            
> norm_kalpVeriSeti$serkol<-data.Normalization(norm_kalpVeriSeti$serkol,type="n4",normalization = "column")
>                                         
> norm_kalpVeriSeti$kalpathiz<-data.Normalization(norm_kalpVeriSeti$kalpathiz,type="n4",normalization = "column")
>                                            
> norm_kalpVeriSeti$STDep<-data.Normalization(norm_kalpVeriSeti$STDep,type="n4",normalization = "column")
> 
> norm_kalpVeriSeti$anaDamSay<-data.Normalization(norm_kalpVeriSeti$anaDamSay,type="n4",normalization = "column")
> 
>                                            
> c_kalpVeriSeti<-norm_kalpVeriSeti[,c("yas","dinkanbas","serkol","kalpathiz","STDep","anaDamSay","kalpHastaligi")]
> kalpVeriSeti$kalpHastaligi<-
+   as.factor(x=c_kalpVeriSeti$kalpHastaligi)
> 
> 
> #faktöre ait seviyeleri yeniden yazdırmak için
> library(plyr) 
> revalue(c_kalpVeriSeti$kalpHastaligi,
+         c("0"= "yok", "1"="var")) 
The following `from` values were not present in `x`: 0, 1
  [1] "var" "yok" "var" "yok" "yok" "yok" "var" "var" "var" "var" "yok" "yok" "yok" "var" "yok"
 [16] "yok" "var" "var" "yok" "yok" "var" "yok" "yok" "yok" "yok" "yok" "yok" "yok" "var" "yok"
 [31] "var" "yok" "yok" "var" "var" "var" "var" "var" "yok" "yok" "var" "yok" "yok" "yok" "var"
 [46] "yok" "var" "var" "var" "var" "var" "yok" "yok" "yok" "yok" "yok" "var" "yok" "var" "var"
 [61] "yok" "var" "yok" "yok" "yok" "var" "yok" "var" "yok" "var" "var" "yok" "yok" "yok" "yok"
 [76] "var" "yok" "yok" "yok" "yok" "var" "var" "var" "yok" "yok" "yok" "yok" "yok" "yok" "var"
 [91] "yok" "var" "var" "var" "var" "var" "yok" "var" "yok" "yok" "yok" "var" "yok" "var" "var"
[106] "var" "yok" "var" "var" "yok" "var" "yok" "var" "yok" "yok" "yok" "var" "var" "yok" "var"
[121] "var" "var" "var" "yok" "yok" "yok" "var" "yok" "yok" "var" "var" "var" "yok" "var" "yok"
[136] "yok" "yok" "var" "yok" "yok" "var" "yok" "var" "yok" "var" "var" "var" "var" "var" "yok"
[151] "yok" "yok" "yok" "yok" "yok" "yok" "var" "yok" "yok" "var" "var" "var" "yok" "var" "yok"
[166] "yok" "yok" "yok" "yok" "var" "yok" "var" "var" "yok" "yok" "var" "var" "var" "var" "yok"
[181] "yok" "var" "var" "yok" "yok" "yok" "var" "yok" "yok" "var" "yok" "var" "yok" "var" "yok"
[196] "yok" "yok" "yok" "yok" "var" "yok" "var" "var" "var" "var" "yok" "yok" "yok" "var" "yok"
[211] "var" "yok" "yok" "var" "yok" "yok" "yok" "yok" "yok" "yok" "var" "var" "yok" "var" "yok"
[226] "yok" "var" "var" "yok" "yok" "var" "var" "yok" "var" "yok" "var" "yok" "var" "yok" "yok"
[241] "var" "yok" "yok" "var" "yok" "var" "var" "yok" "var" "var" "var" "yok" "var" "yok" "yok"
[256] "yok" "yok" "var" "var" "yok" "yok" "var" "var" "yok" "var" "yok" "yok" "yok" "yok" "var"
> 
> #seviyeleri görebilmek için 
> table(c_kalpVeriSeti$kalpHastaligi) 

var yok 
120 150 
> summary(c_kalpVeriSeti)
      yas           dinkanbas          serkol         kalpathiz          STDep       
 Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
 1st Qu.:0.3958   1st Qu.:0.2453   1st Qu.:0.1986   1st Qu.:0.4733   1st Qu.:0.0000  
 Median :0.5417   Median :0.3396   Median :0.2717   Median :0.6298   Median :0.1290  
 Mean   :0.5299   Mean   :0.3523   Mean   :0.2823   Mean   :0.6006   Mean   :0.1694  
 3rd Qu.:0.6667   3rd Qu.:0.4340   3rd Qu.:0.3516   3rd Qu.:0.7252   3rd Qu.:0.2581  
 Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
   anaDamSay      kalpHastaligi     
 Min.   :0.0000   Length:270        
 1st Qu.:0.0000   Class :character  
 Median :0.0000   Mode  :character  
 Mean   :0.2235                     
 3rd Qu.:0.3333                     
 Max.   :1.0000                     
> 
>  head(egitim_indeks)
     Resample1
[1,]         2
[2,]         7
[3,]         8
[4,]         9
[5,]        10
[6,]        11
> egitim <- c_kalpVeriSeti[egitim_indeks,]
> test <- c_kalpVeriSeti[-egitim_indeks,]
> 
> testNitelikleri<-test[,-7] 
> testHedefNitelik<-test[[7]]
> 
> egitimNitelikleri<-egitim[,-7] 
> egitimHedefNitelik<-egitim[[7]]
> 
> #k değeri 1'den 5'e kadar deneniyor. Doğruluk değerleri hesaplanıyor.
> kdegeri<-5
> dogruluk<-NULL
> library(class)
> for (i in 1:kdegeri) { 
+   set.seed(1) 
+   tahminiSiniflar=
+     knn(egitimNitelikleri,testNitelikleri,egitimHedefNitelik,k=i) 
+   dogruluk[i]<-mean(tahminiSiniflar==testHedefNitelik) 
+   dogruluk[i]<-round(dogruluk[i],2)
+   
+ }
> for (i in 1:kdegeri)
+ {
+   print(paste0("k=",i,"için elde edilen doğruluk=",dogruluk[i]))
+   
+ }
[1] "k=1için elde edilen doğruluk=0.57"
[1] "k=2için elde edilen doğruluk=0.63"
[1] "k=3için elde edilen doğruluk=0.61"
[1] "k=4için elde edilen doğruluk=0.63"
[1] "k=5için elde edilen doğruluk=0.65"
> 
> plot(dogruluk, type="b", xlab="K-degerleri",ylab="Dogruluk",col="blue")
> 
> #confusion matrix
> tm<- table(tahminiSiniflar,testHedefNitelik,dnn=c("TahminiSınıflar","GercekSınıflar"))
> tm
               GercekSınıflar
TahminiSınıflar var yok
            var  15  10
            yok   9  20
>
