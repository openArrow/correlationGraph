set.seed(12345)
ptm<-proc.time()
library(PCIT)
library(igraph)
#set row and column dimension of the matrix
row<-63
col<-63
totLen<-(row*col)
#set the directory of your file here
setwd('C:/Users/vsingh22/Downloads/Data_for_subregion/CS310_project_subregion')
files <- list.files(pattern = "*$", all.files = FALSE,
                    recursive = TRUE,
                    include.dirs = FALSE)
height<-length(files)
dataMatrix<-array(data=0,dim=c(row, col,height ))
#reading the files
for (i in c(1:height)) {
  conne <- file(files[i], "rb")
  dataMatrix[1:row,1:col,i]<- readBin(conne, numeric(), n = totLen, size = 4, endian = "little")
  dataMatrix[1:row,1:col,i]<-t(dataMatrix[1:row,1:col,i])
  close(conne)
}
dataMatrix[dataMatrix == 157]<-0


heightDiv = height/3
heightDiv<-as.integer(heightDiv)
start=0
end=heightDiv
#plotting the Graph
for(i in c(1:3))
{
  g<-graph.empty(directed = FALSE) + vertices(1:totLen)
  cl<-0
  rl<-0
  n<-0
  d<-0
  count<-0
  for(r1 in c(1:(row))) 
  {
    for(c1 in c(1:(col)))
    { 
      rl<-rl+1 
      for(r2 in c(1:row)) 
      {
        for(c2 in c(1:col))
        {
          cl<-cl+1
          if(!are.connected(g,rl,cl) && dataMatrix[r1,c1,1:1] != 168 && dataMatrix[r2,c2,1:1] != 168 )
          {
            d<-cor(dataMatrix[r1,c1,start:end],dataMatrix[r2,c2,start:end],method = "pearson")
            if(!is.na(d) && d>=0.90 && d<=0.95)
            {
              g<- add_edges(g,c(rl,cl))
              count<-count+1
            }
          }
        }
        cl<-0
      }
    }
  }  
  print(proc.time()-ptm)
  degs<-degree(g)
  avgPathLenght<-average.path.length(g)
  cc<-sum(transitivity(g, type=c("localundirected"),isolate="zero"))
  start<-start+heightDiv
  end<-end+heightDiv
  degNew<-degs[degs!=0]
  x11("Histogram")
  hist(degNew[degNew],main="Histogram for Degree Distribution", xlab="Degree",ylab = "no.Of Vertices",xlim=c(1,30),las=1,breaks=20)
  output<-matrix(unlist(degs),ncol=col,byrow = TRUE)
  avgDegree<-(sum(degs)/totLen) 
  print("Average Path Lenght of Graph is:")
  print(avgPathLenght)
  print("Correlation Coefficient of Graph is:")
  print(cc)
  print("Average Path Lengh of Random Graph is:")
  print(log(totLen)/log(avgDegree))
  print("Correlation Coefficient of Random Graph is:")
  print(avgDegree/totLen)
  x<-vector()
  y<-vector()
  for(r1 in c(1:(row))) 
  {
    for(c1 in c(1:(col)))
    { 
      if(output[r1,c1]>avgDegree+10)
      {
        x<-c(x,r1)
        y<-c(y,c1)
      }
    }
  }  
  x11("Super Nodes")
  plot(x,y,main = "SuperNodes Plot")
  rm(g)
}