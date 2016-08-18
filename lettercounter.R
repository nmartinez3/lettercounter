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
      new<-list()
      namelist<-vector()
      for(i in seq_along(result)){
            new[[i]]<-result[[which.max(result)]]
            namelist[i]<-names(result)[which.max(result)]
            result[[which.max(result)]]<-NULL
      }
      names(new)<-namelist
      return(new)
}