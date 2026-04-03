# Reading of files
#2.0
enh.df <- system.file("extdata","subset.enhancers.hg18.bed",
                      package= "compGenomRData")
enh.df<- read.table(enh.df,  header = FALSE)
head(enh.df)

cpgi.df <- system.file("extdata","subset.cpgi.hg18.bed",
                       package= "compGenomRData")
cpgi.df<- read.table(cpgi.df,  header = FALSE)
head(cpgi.df)

first_subdat <- en.df[2:6,3:7]
first_subdat[4,"X9.11"]

# Writing of files

genes <- c("PtGS2","NCED1","BRAC1")
score <- c(3.4,2.5,4.1)
genfile <- data.frame(genes,score)
write.table(genfile,"genfile.txt",row.names = FALSE,
            col.names=FALSE, quote=FALSE, sep="\t")
a <- "genfile.txt"
genfilee <- read.table(a)
genfilee
matrixx <- matrix(c(1:32),nrow=8)
matrixx

save(enh.df,cpgi.df, file ="myproject.RData")
load("myproject.RData")
getwd()
saveRDS(enh.df, file= "enhdf.rds")
x <- readRDS("enhdf.rds")
head(x,5)
rm(enh.df,cpgi.df)
a<-load("myproject.RData")
a
head(enh.df, 7)

# Reading large files
library(data.table)
library(readr)
largerfile <- fread(enh.df, header = FALSE, data.table= FALSE)
largerfile
read_table()

# Plotting in R with base graphics
#50 values from normal distribution
x<- rnorm(50)
x
hist(x)
y<- rnorm(60, mean=10,sd=15)
y
hist(y,main = "Histogram graph: Distribution of qauntitative data",
     col = "blue", xlab = "data values",ylab = "Freq of distribution")

# Scatter plot
x <- c(2,3,4,6,7,9,12,10,7,10)
y<- c(4,6,7,9,11,13,12,15,10,14)
z<-c(3,4,2,6,5,7,8,9,5,7)
plot(x,y, main = "Scatter plots for random samples",
     ylab = "y values",xlab = "x values")
boxplot(10)
boxplot(x,y,main="boxplots of random samples")
ls()

#Barplot
cisprecent<- c(50,75,45,70)
barplot(height= cisprecent, names.arg=c("Gcpi"
        ,"Exon","GPGi","Exon"),main=
          "cisregulatory element",ylab="percentage",
        col=c("yellow","yellow","green","green"))
legend("topright",legend = c("test","control"), fill =
         c("yellow","green"))

# Combine multiple plots
par(mfrow=c(1,2)) #

# make the plots
hist(x,main="Hello histogram!!!",col="red")
plot(x,y,main="scatterplot",
     ylab="y values",xlab="x values")
#saving plot
dir.create("mygraphs")
pdf("myplot.pdf", width = 5,height=5)
plot(x,y)
dev.off()
getwd()
list.files()

# Plotting in R with ggplot
# ggplot is operated upon at 3 levels or in 3 layers
# including the data,coordinate system and annotation

library("ggplot2")
#Histogram with ggplot
myData<- data.frame(x,y)
ggplot(myData,aes(x=x))+geom_histogram()+
  labs(title = "Histogram", x="variable",y="count")

# boxplot with ggplot
# for base r, it accepts separate vectors
# as in boxplot(x,y), while in ggplot, it
# uses just a single dataframe consisting
# two categories: values and groups.
# e.g
library(ggplot2)
#method 1 for simple data
values= c(3,2,4,5,6,7,4,8,9)
group= c("x","x","x","y","y","y","z","z","z")
df<- data.frame(values,group)
df
ggplot_box<-ggplot(df,aes(x=group,y=values))+
  geom_boxplot()
ggplot_box
# method 2 for large complex data

vw<- c(2,4,5,6,7,5,7,5,4,5,9)
gw<- rep("w",length(vw))
wd <-data.frame(values=vw,group=gw)
vx<- c(3,4,5,6,3,2,2,5,5,4,9)
gx<-rep("x",length(vx))
xd<- data.frame(values=vx,group=gx)
vy<- c(4,3,6,7,4,8,9,3,5,4,3)
gy<-rep("y",length(vy))
yd<-data.frame(values=vy,group=gy)
vz<- c(2,1,2,3,4,5,4,3,2,4,5)
gz<-rep("z",length(vz))
zd<-data.frame(values=vz,group=gz)
mydata<-rbind(wd,xd,yd,zd)
mydata
ggplot_box<- ggplot(mydata,aes(y=values,x=group))+geom_boxplot()+
  labs(title = "ggplot_box", x="Group",y="Values")
ggplot_box

# Combining multiple ggplot from same dataframe

# facet put separate groups within a colum
# dataframe into dif graphs

library(ggplot2)

values= c(3,2,4,5,6,7,4,8,9)
group= c("x","x","x","y","y","y","z","z","z")
df<- data.frame(values,group)
df
ggplot_box<-ggplot(df,aes(x=group,y=values))+
  geom_boxplot()
ggplot_box

# for facet
facet_ggplo<- ggplot(df,aes(x=values))+geom_histogram()+facet_grid(.~group)
facet_ggplo

# for Combining multiple different plots/datasets
# use cowplot, patchwork or gridExtra packages
# instead of par(),layout() from base R which
#wont woro cos ggplot uses diff graphics system

# example for scatter plot and histogram
library(cowplot)
#sca
sca_data<- data.frame(x=x,y=y,z=z)
sca_ggplot<- ggplot(sca_data,aes(x,z))+geom_point()
sca_data
sca_ggplot
#hist
facet_ggplo
# The the 2 plots in a grid and labelled as Scat and Hist
plot_grid(sca_ggplot,facet_ggplo, labels = c("Scat","Hist", label_size=10))

#Tidyverse and ggplot
#Tiduverse is a collection of packages designed for data science, which
# use the same philosophy, grammar and data structure.
# Core packages used include: ggplot, dplyr, tidyr, readr, and tibble

# Function in R Control/Conditional statement.

sq_sum <- function(x,y){
  result = x^2 + y^2
}

cpgi.df <- read.table(textConnection("
chr1	1000	1200
chr2	20000	20750
chr3	500	650
chr1	15000	17000
chrX	3000	3500
"), header=FALSE)
cpgi.df

cpg_size <- function(bedrow){
  cpg_len<- bedrow[3] - bedrow[2]+1
  if (cpg_len > 1400){
    cat("The cpg is lengthy")
  }
  else if(cpg_len <= 1400 & cpg_len>700){
    cat("The length is moderate")
  }
  else
    cat("cpg_len is quite low")
}
cpg_size(cpgi.df[4,])

chrom <- c("chr2","chr5","chr21","chr3","chr7")
start <- c(600,500,900,200,1500)
end <-  c(1500,1500,1600,1600,3000)
cpgi.df <- data.frame(chrom,start,end)
cpgi.df

cpgi_data <- function(bedrow){
  if(bedrow[3] - bedrow[2]+1 <= 800){
    cat("This is the lowest genomic region")
  }
  else if(bedrow[3]-bedrow[2]+1 >= 800 & bedrow[3]-bedrow[2]+1 <= 1200){
    cat("Moderate genomic region length")
  }
  else{
    "Most lenghty genomic region"
  }
}

cpgi_data(cpgi.df[2,])


# Loops

for(i in 1:10){
  cat("its loop")
  print(i)
}

# loop to calc the length of CpG islands

#cgpi_length <- function(bedrow){
  #length <- bedrow[3]-bedrow[2]
 # for()
#}

# calculates cpgi length for each row in the new column "len"

for(i in 1:nrow(cpgi.df)){
  len= cpgi.df$end[i] - cpgi.df$start[i]+1
  print(len)
}

# calculates for each row, sums up the total and find the mean

total= 0

for(i in 1:nrow(cpgi.df)){

  cpgi.df$len= cpgi.df$end[i] - cpgi.df$start[i]+1

  total<- total+cpgi.df$len[i]
  }

mea_n<- total/nrow(cpgi.df)
mea_n

#Use of Apply family functions instead of loop: Apply, Lapply,Tapply,Mapply e.t.c

# Apply - for Matrices and Data frame.

# sum every row(1) or column(2). same can be done for max,min,mean,sd etc)

mat_table <- matrix(1:24, nrow = 6)
mat_table
apply(mat_table,1,sum)

# Adding extra function
mat_table <- matrix(1:24, nrow = 6)
mat_table
apply(mat_table,1,function(x)sum(x)+x^3+(x+x))

# Lapply - for lists and vectors

#list
li_st <- list(3:6,4:8,3:6,4:6,5:10,5,7:23,4:12,3:8)
lapply(li_st,sum)

# vector
vec_tor <- c(3,6,5,4,8)
vec_tor^3
lapply(vec_tor,function(x)x^3)

# Tapply - for vectors groupd by factors
vec_tor <- c(3,4,5,7,8,9)
factor <- c("big","big","bigger","bigger","biggest","biggest")
tapply(vec_tor, factor,sum)

#Mapply - for multiple vectors
vec_tor1 <- c(3,6,5,4,8)
vec_tor2 <- c(3,6,5,4,8)
vec_tor3 <- c(3,6,5,4,8)
mapply(sum,vec_tor1,vec_tor2,vec_tor3)

# Vectorized functions in R

vec_tor1 <- c(3,6,5,4,8)
vec_tor2 <- c(3,6,5,4,8)
vec_tor3 <- c(3,6,5,4,8)

vec_tor1+vec_tor2+vec_tor3

# Vectorize for table data

mat_table1 <- matrix(1:24, nrow = 6)

colSums(mat_table1)
rowSums(mat_table1)
# 2.10 - Exercises
# sum
2+3
# square root
sqrt(36)
#Logarithm of numbers
log10(1000)
log2(234)
#Assignment to variable
x <- 2+3+4
#Absolute value of -145
abs(5-150)
# Various Maths operations
x<-sqrt(625)/5
x
y<-log10(1000)
y
x<- y*10000
x
# Data structures
#Vectors
vec <- c(1,2,3,5,10)
vec
length(vec)
vec <- c(2:15)
vec<- rep(4,10)
vec<- rep(x,tim=10)
vec
vec<- c(FALSE,TRUE,TRUE,TRUE)
length((vec))
vec<- c("PAX6","ZIC2","OCT4","SOX2")
vec
#Subset vector
vec<- c(2:6)
vec
vec[c(3,5)]
#Subset vector using logical vector
vec[c(TRUE,FALSE,TRUE,FALSE,TRUE)]
vec
vec[vec<4]
sd(vec)
#matric by row
matr<- matrix(1:15,nrow=5, byrow = TRUE)
matr
#Extract subset from matrix
#1st 3 rows and columns
matrr<-matr[1:3,1:3]
#last 2 rows
matrr[2:3,]
matt<-matrr
#1st 2 columns
mat_rix<-matt[,1:2]
class(mat_rix)
mat_rix
mat_r<-mat_rix[,1]
class(mat_r)

#Making data frames
#with 3 columns and 5 rows
col1<-c(0,1,2,3,4)
col2<-c(5:9)
col3<-c(10:14)
df<-data.frame(col1,col2,col3)
df
#Extract 1st 2 col and row
df[1:2,1:2]
#Last 2 rows
df[4:5,]
#Last 2 cols
df[,2:3]
#E second col
df["col1"]
df$col1
df[,1]
#Extract rows when all the 1st column is greater than 400
df[df[,1]>400,]
#Extract rows where the 1st column is larger than or equal to 300
df[df[,1]>=300,]
#Convert dataframe to matrix
df_mat<-as.matrix(df)
df_mat
#List containing  elements
li_st<- list(25,"young",FALSE,x="old")
li_st
#Extract or select elements from lists
li_st["x"]
li_st$x
#Create a factor object
fa <- factor(c("a","a","b","b","c"))
fa
#Convert a character vector into a factor
vec <- c("gene1","gene2","gene3","gene3","gene1")
fa<- as.factor(vec)
#Conversion back to character vector
as.character(fa)

#Reading and writing data out in
#filepath through through the pc folder - downloaded packAGE from github
cpgi_filepath<- read.table("C:\\Users\\oluwa\\Downloads\\compGenomRData-master\\compGenomRData-master\\inst\\extdata\\CpGi.table.hg18.txt", row.names=NULL)
head(cpgi_filepath,6)
#filepath through R library - Installed package in R

cpgFilePath=system.file("extdata",
                        "CpGi.table.hg18.txt",
                        package="compGenomRData")

cpgFilePath <- read.table(cpgFilePath, row.names = NULL)
X<-head(cpgFilePath,6)
#why doesnt this work ?
cpgtFilePath=system.file("extdata",
                         "CpGi.table.hg18.txt",
                         package="compGenomRData")
cpgtFilePath
cpgi.df=read.table(cpgFilePath,header=FALSE,sep="\t")
head(cpgi.df,10)
#When stringas... is set to false, what happens
cpgiHF=read.table("C:\\Users\\oluwa\\Downloads\\compGenomRData-master\\compGenomRData-master\\inst\\extdata\\CpGi.table.hg18.txt",
                  header=FALSE,sep="\t",
                  stringsAsFactors=FALSE)
head(cpgiHF,10)

sapply(cpgiHF, class)
sapply(X, class)

#Save your CpG data (dataframe) into a text file

saved_cpgi<- write.table(cpgi.df,file="my.cpgi.file.txt")
saved_cpgi
saved_cpgi2<- write.table(cpgi.df, file="my.cpgi.file2.txt",
                          quote = FALSE, sep = "\t",row.names = FALSE)
saved_cpgi2

#Read both again and compare
saved_cpgi<-read.table("my.cpgi.file.txt", header = TRUE)
head(saved_cpgi)
# and
saved_cpgi2<- read.table("my.cpgi.file2.txt", sep="\t",header = TRUE)
head(saved_cpgi2)
# Checck programmaatic identity
identical(saved_cpgi,saved_cpgi2)

#check rownames
row.names(saved_cpgi)
row.names(saved_cpgi2)
# write First 10 rows of cpgi.df
ist_tenrows <- write.table(cpgi.df[1:10],"cpgi.ftenrow.txt")
ist_tenrows
#write only on chr1 (first col)
ist_col<- write.table(cpgi.df[,1]== "chr1","cpgi.fcol.txt")
ist_col
head(read.table("gpgi.fcol.txt"))
# Filte rows where v5 is 439
rows_439 <- cpgi.df[cpgi.df$v5==439,]
rows_439
#Reading 2 files

rfb_pat1 <-system.file("extdata/rn4.refseq.bed", package="compGenomRData")
rfb_df1<-read.table(rfb_pat1, header = FALSE)

reft_pat2 <- system.file("extdata/rn4.refseq2name.txt", package = "compGenomRData")
reft_df2<- read.table(reft_pat2,header = FALSE)

#check their outputs and compare
head(rfb_df1,6)
head(reft_df2)

# merge output, using by.x and by.y to indicate where ids are on the 2 dfs
# what happens here: first and sec datasets has 12 and 2 cols respectively
# merge() unite them by removing id from both and make it one in the
# new data. the id plus 11+1 makes 13 altogether in the final data.
# r sorts the dataset based on the id, and rearranges others with respect to
#this ids. the former was sorted based on chr

new.df<-merge(rfb_df1,reft_df2, by.x="V4",by.y = "V1")

head(new.df)
tail(new.df)

#Plotting iN R
#Using set.seed() with seed 1001, to set initial values for the PRNG
#here the generator is rnorm()
set.seed(1001)

x1=1:100+rnorm(100,mean=0,sd=15)
y1=1:100
plot(x1,y1,main = "Scatter plot",xlab
     = "x-axis",ylab = "y-axis")
#Margin text
mtext(side = 2,text = "hi there")
mtext(side = 1,text = "hey there")
mtext(side = 3,text = "hello there")
mtext(side = 4,text = "whats up there")
#margin text plus paste(), inside which more than
# 1 strings can be placed, yet output as a single string
#if vector c() is used, r sees the elements inside as separate strings
# and places them separately untop one another
# also paste0() joins printed texts together

mtext(side = 3,text = paste("paste","paste2"))
mtext(side = 2,text = c("paste","paste2","paste2"))
mtext(side = 1,text = paste0("paste","paste2"))

# Calc the correlation btw 2 vectors
r<-cor(x1,y1)
#Display correlation coefficient on scatter plot
mtext(side =3,text=paste("Correlation is ",round(r,2)))
# Use col argument in plot to change colors
plot(x1,y1,col="red")
#using pch(plotting character)=19 as  an argument in plot
#used to define shape or a symbol of a point in r

plot(x1,y1,col="red",pch=12)

#Make an histogram: Graphical representation of data distribution

x1=c(2,3,4,4,4,4,rep(5,7),6,6,6,6,6,7,7,8)
hist(x1)

# use hist() arguments
hist(x1, main = "Graphical data distribution",xlab = "Data",
     col = "blue")
# Make a boxplots
boxplot(x1,y1)
# use horizontal =TRUE argument
boxplot(x1,y1,horizontal =TRUE)

#make multiple plot(Normal run before plots are made)
#2 rows 2 columns
par(mfrow=c(2,2))
#2 rows 1 column
multplot<-par(mfrow=c(2,1))
multplot
hist(x1, main = "Graphical data distribution",xlab = "Data",
     col = "blue")
boxplot(x1,y1)
getwd()
C:/Users/oluwa/Downloads/
  compGenomRData-master/compGenomRData-master/R
/Reading of files in Comgenomdatapackage.R
