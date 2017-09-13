#Time-control
Start<-function() {
  start_time<<-Sys.time()
}

End<-function(group=rev(strsplit(getwd(), "/")[[1]])[3], logdir=rev(strsplit(getwd(), "/")[[1]])[4], name=rev(strsplit(getwd(), "/")[[1]])[2], project=rev(strsplit(getwd(), "/")[[1]])[1], financed=FALSE, user=Sys.getenv("USERNAME"), comment=""){
  if(exists("start_time")){
    save_dir<- substr(getwd(), 1, gregexpr(logdir, getwd())[[1]]+nchar(logdir))
    filename<-paste("time_log_", strsplit(as.character(Sys.time()), "-")[[1]][1], ".csv", sep="")
    if(filename %in% list.files(path=save_dir)){
      write.table(data.frame(Fecha=strsplit(as.character(Sys.time()), " ")[[1]][1], Tiempo=round(as.numeric(difftime(Sys.time(), start_time, units = "mins"))), Nombre=name, Proyecto=project, Financiado=financed, Grupo=group, Usuario=user, Comentarios=comment), paste(save_dir, filename, sep=""), append=T, row.names=F, col.names=F, dec=",", sep=";")
    }
    else{
      write.table(data.frame(Fecha=strsplit(as.character(Sys.time()), " ")[[1]][1], Tiempo=round(as.numeric(difftime(Sys.time(), start_time, units = "mins"))), Nombre=name, Proyecto=project, Financiado=financed, Grupo=group, Usuario=user, Comentarios=comment), paste(save_dir, filename, sep=""), row.names=F, dec=",", sep=";")
    }
    rm("start_time", envir=.GlobalEnv)
  }
}

#Check time
check_time<- function(group=NULL, user=NULL, unit="h", global=FALSE){
  units <- ifelse(unit =="h", 60, 1)
  datos <- read.csv2(paste("\\\\iislafesql16/RC_bioestadistica/Proyectos/", paste("time_log_", strsplit(as.character(Sys.time()), "-")[[1]][1], ".csv", sep=""), sep=""))
  datos$Tiempo<-round(datos$Tiempo/units,2)
  datos$Grupo<-factor(tolower(iconv(datos$Grupo, to="ASCII//TRANSLIT")))
  datos$Nombre<-factor(tolower(iconv(datos$Nombre, to="ASCII//TRANSLIT")))
  if(!is.null(group)){
    subdatos <- datos[grep(tolower(group), datos$Grupo),]
  }
  if(!is.null(user)){
    subdatos <- datos[grep(tolower(user), datos$Nombre),]
  }
  if(exists("subdatos")){
    total <- tapply(subdatos$Tiempo, factor(subdatos$Grupo), function(x) sum(x, na.rm=TRUE))
    desglose <- sapply(unique(subdatos$Grupo), function(x) tapply(subdatos$Tiempo[subdatos$Grupo %in% x], factor(subdatos$Nombre[subdatos$Grupo %in% x]), function(x) sum(x, na.rm=TRUE)))
    if (class(desglose) == "list") names(desglose)<-unique(subdatos$Grupo)
    return(list(Total=total, Desglosado=desglose))
  }
  if(global){
    sort(tapply(datos$Tiempo, datos$Grupo, function(x) sum(x, na.rm=TRUE)), decreasing=TRUE)
  } else {
    return(paste("Horas totales del año", round(sum(datos$Tiempo, na.rm=TRUE),2)))
  }
}

list_groups <- function(pattern="[a-z]"){
  datos <- read.csv2(paste("\\\\iislafesql16/RC_bioestadistica/Proyectos/", paste("time_log_", strsplit(as.character(Sys.time()), "-")[[1]][1], ".csv", sep=""), sep=""))
  as.character(unique(datos$Grupo[grep(pattern, tolower(iconv(datos$Grupo, to = "ASCII//TRANSLIT")))]))
}
