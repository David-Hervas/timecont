#Time-control
.timecontenv <- new.env(parent = baseenv())

Start <- function(){
  assign(".start_time", Sys.time(), envir = .timecontenv)
}

End <- function(type="analisis", project=rev(strsplit(getwd(), "/")[[1]])[1], logdir="H:\\Prestacion servicios\\timelogs\\", name="", financed=FALSE, user=Sys.getenv("USERNAME"), comment=""){
  if(exists(".start_time", envir=.timecontenv)){
    save_dir <- logdir
    filename<-paste("time_log_", strsplit(as.character(Sys.time()), "-")[[1]][1], ".csv", sep="")
    if(filename %in% list.files(path=save_dir)){
      write.table(data.frame(Fecha=strsplit(as.character(Sys.time()), " ")[[1]][1], Tiempo=round(as.numeric(difftime(Sys.time(), .timecontenv$.start_time, units = "mins"))), Nombre=name, Proyecto=project, Tipo=type, Financiado=financed, Usuario=user, Comentarios=comment), paste(save_dir, filename, sep=""), append=T, row.names=F, col.names=F, dec=",", sep=";")
    }
    else{
      write.table(data.frame(Fecha=strsplit(as.character(Sys.time()), " ")[[1]][1], Tiempo=round(as.numeric(difftime(Sys.time(), .timecontenv$.start_time, units = "mins"))), Nombre=name, Proyecto=project, Tipo=type, Financiado=financed, Usuario=user, Comentarios=comment), paste(save_dir, filename, sep=""), row.names=F, dec=",", sep=";")
    }
    rm(".start_time", envir=.timecontenv)
  }
}

load_timelog <- function(log_file=paste("H:/Prestacion servicios/timelogs/", paste("time_log_", strsplit(as.character(Sys.time()), "-")[[1]][1], ".csv", sep=""), sep="")){
  read.csv2(log_file)
}

setup_project <- function(){
  writeLines(c("library(timecont)",
               "Start()",
               ".Last <- function(){",
               "  End()",
               "}"), ".Rprofile")
  .Last <<- function(){
    End()
  }
  Start()
}
