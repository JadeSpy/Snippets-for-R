#goodSort
glob <<- 2
 goodSort <- function(sortable,key,reverse=FALSE){
   if(missing(key)){
     return(sort(sortable,reverse))
   }
   sortableExtract <- vector()
   for (entry in sortable){
     glob <<- key(entry)
     sortableExtract <- append(sortableExtract,key(entry))
   }
  
   newOrdering <- order(sortableExtract,decreasing=reverse)
   
   return (sortable[newOrdering])
   
   
 }
sortUsingIndex <- function(sortable,index,reverse=FALSE){
  extraction <- function (entry){
    return(entry[index])
  }
  goodSort(sortable,extraction)
}
#toSort <- list(c(1,2,1),c(3,2,1),c(-1,3,6))
#a<-goodSort(toSort,key=extraction)
#b<-sortUsingIndex(toSort,1)
