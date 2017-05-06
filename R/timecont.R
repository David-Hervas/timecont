#Time-control
Start<-function() {
  start_time<<-Sys.time()
}

End<-function(group=NA, logdir=rev(strsplit(getwd(), "/")[[1]])[3], name=rev(strsplit(getwd(), "/")[[1]])[2], project=rev(strsplit(getwd(), "/")[[1]])[1], financed=FALSE, comment=""){
  if(exists("start_time")){
    save_dir<- substr(getwd(), 1, gregexpr(logdir, getwd())[[1]]+nchar(logdir))
    filename<-paste("time_log_", strsplit(as.character(Sys.time()), "-")[[1]][1], ".csv", sep="")
    if(filename %in% list.files(path=save_dir)){
      write.table(data.frame(Fecha=strsplit(as.character(Sys.time()), " ")[[1]][1], Tiempo=round(as.numeric(difftime(Sys.time(), start_time, units = "mins"))), Nombre=name, Proyecto=project, Financiado=financed, Grupo=group, Comentarios=comment), paste(save_dir, filename, sep=""), append=T, row.names=F, col.names=F, dec=",", sep=";")
    }
    else{
      write.table(data.frame(Fecha=strsplit(as.character(Sys.time()), " ")[[1]][1], Tiempo=round(as.numeric(difftime(Sys.time(), start_time, units = "mins"))), Nombre=name, Proyecto=project, Financiado=financed, Grupo=group, Comentarios=comment), paste(save_dir, filename, sep=""), row.names=F, dec=",", sep=";")
    }
    rm("start_time", envir=.GlobalEnv)
  }
}




