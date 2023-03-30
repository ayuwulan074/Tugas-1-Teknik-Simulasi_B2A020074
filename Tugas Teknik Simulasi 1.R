# Tugas Teknik Simulasi
# Nama : Ayu Wulandari 
# Nim : B2A020074 (Genap)

# Deketahui nilai a=45,z0= 21139,m=417,n=150
# Menggunakan multiplicative_RNG dan Bernoulli_1

#Multiplicative
Gabungan_RNG_ber<-function(a,z0,m,n,p){
  xj<-matrix(NA,n,3)
  dj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ(i-1)+c","Xj","Uj")
  colnames(dj)<-c("Xj","0","1")
  for (i in 1:n){
    xj[i,1]<-(a*z0)
    xj[i,2]<-xj[i,1]%%m
    xj[i,3]<-xj[i,2]/m
    z0<-xj[i,2]
    dj[i,1]<-xj[i,2]
    dj[i,2:3]<-Bernoulli_1(xj[i,2],p)
  }
  #hist(xj[,3])
  View(xj)
  View(dj)
}
Gabungan_RNG_ber(45,21139,417,150)
