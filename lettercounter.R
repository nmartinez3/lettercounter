lettercounter<-function(word){
      full<-strsplit(word,NULL)[[1]]
      letter<-sort(unique(full))
      counts<-vector(length=length(letter))
      for(i in 1:length(letter)){
            counts[i]<-sum(full %in% letter[i])
      }
      result<-list()
      for(i in 1:length(letter)){
            result[i]<-counts[i]
      }
      names(result)<-letter
      return(result)
}