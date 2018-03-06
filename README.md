# Products-Price-bracket-creation
Segmenting different products into different price brackets .

Shopalyst assignemnt 2
================
Archit
30 January 2018

algorithm: step1: Give colnames, proper classes to labels,deal with null values Step 2: Calculated the price bracket for each category: a) applied k-means clustering on each level in the category based on its price b) give names to the clusters using max and min to identify the lowest and highest price category

diferrent approaches: 1)considered seggregating by taking quantiles\[0-25,25-75 and 75-100 in this case\] 2)k-means which gave a better o/p when compared to quantiles

``` r
setwd("C:/Users/Administrator/Desktop/Shopalyst")
set2 <- read.csv("price_sample.txt",header = FALSE,stringsAsFactors = F)

#View(set2)

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
str(set2)
```

    ## 'data.frame':    29826 obs. of  3 variables:
    ##  $ V1: chr  "2C39A09E0F9DB7B1" "2F26A59E4943BD1C" "1A5236C8968ABFF9" "6EFC18DB0E6244D7" ...
    ##  $ V2: chr  "bags" "dress materials" "bags" "heels" ...
    ##  $ V3: chr  " 9200.0" " 3999.0" " 1699.0" " 1999.0" ...

``` r
colnames(set2) <- c("Prod_id","Category","Price")

#3 colums with 1 factor and 1 integer
  set2$Category <-as.factor(set2$Category)
  temp          <-as.integer(set2$Price)
 
  #since none present there was problem in converting to int
  tempprodid<-set2[set2$Price==" None",]
  set2[set2$Prod_id %in% tempprodid$Prod_id, ] 
```

    ##                Prod_id Category Price
    ## 750   756CC4617636498B   sarees  None
    ## 2277   806C8B53987BF5F t-shirts  None
    ## 3997  7391C9022E0EB5B4  ethnics  None
    ## 10444 55255D1720144748  ethnics  None
    ## 17267 46EE3937DDD447E7 t-shirts  None
    ## 19125 10D4A8BFFFBEB854   sarees  None
    ## 20290 556FDF917DFA40A4  ethnics  None
    ## 20953 39CEBE34C2D94814     tops  None
    ## 21545 4E49705FFFBE4F47 t-shirts  None
    ## 21718 341132235BA04F7B   shirts  None
    ## 29662 7C038CD7D0304AAF   sarees  None

``` r
  #these 11 rows dont have a price attached to them so we shall                                                   ignore them
  
  finaldata<-set2[!set2$Prod_id %in% tempprodid$Prod_id,]
  str(finaldata)
```

    ## 'data.frame':    29815 obs. of  3 variables:
    ##  $ Prod_id : chr  "2C39A09E0F9DB7B1" "2F26A59E4943BD1C" "1A5236C8968ABFF9" "6EFC18DB0E6244D7" ...
    ##  $ Category: Factor w/ 40 levels "accessories",..: 2 8 2 15 19 19 1 22 36 27 ...
    ##  $ Price   : chr  " 9200.0" " 3999.0" " 1699.0" " 1999.0" ...

``` r
  finaldata$Price<- as.integer(finaldata$Price)
  View(finaldata)

  #no need of product id for our analysis
  finaldata<-finaldata[!finaldata$Price==0,2:3]
  
  
  colSums(is.na(set2))#no missing data
```

    ##  Prod_id Category    Price 
    ##        0        0        0

``` r
  finaldata$bracket<- c(rep('TBD'))
```

``` r
prac<-finaldata[-3]
#slip ons has only 1 product so it would be useless in our estimation
prac<-prac[!prac$Category=='slip-ons',]

#calculate the bracket using k means
for(i in unique(prac$Category)){
  
  tempk<-prac[prac$Category==i,2]
  
  a<-kmeans(tempk,3,nstart=15)
  prac[prac$Category==i,"bracket"]<-a$cluster 
   
  
}





#Get the bracket labels for the diffent cluster numbers assigned to each category  
for(i in unique(prac$Category)){
  m<-min(prac[prac$Category==i,2])
  l<-prac[prac$Price==m,3]
  
  h<-max(prac[prac$Category==i,2])
  b<-prac[prac$Price==h,3]
prac$bracket <-ifelse(prac$bracket==l,'LOW',ifelse(prac$bracket==b,'HIGH','MEDIUM'))}
#View(prac)
head(prac,10)
```

    ##           Category Price bracket
    ## 1             bags  9200  MEDIUM
    ## 2  dress materials  3999     LOW
    ## 3             bags  1699  MEDIUM
    ## 4            heels  1999     LOW
    ## 5        jewellery 13111  MEDIUM
    ## 6        jewellery  6139    HIGH
    ## 7      accessories   999  MEDIUM
    ## 8         leggings   599    HIGH
    ## 9         t-shirts  3599  MEDIUM
    ## 10          sarees   999  MEDIUM
