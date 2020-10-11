library(rvest)
library(xml2)
library(httr)
library(plyr)
library(stringr)
library(readr)
library(reshape2)

url='https://observatoriolegislativo.ec/asamblea-nacional/asambleistas/'

pg=html_session(url)
tpg=as.character(xml2::read_html(pg))

p1=grepRaw("Asambleistas.porCurul = [",tpg,fixed = T,all = T)
p2=grepRaw("]",tpg,fixed = T,all = T)
p2=min(p2[p2>p1])
txt1=substr(tpg,p1+22,p2-2)
txt1=gsub("\\}\\]$|^\\[\\{","",txt1)
txt1=gsub("\\}\\, \\{","\n",txt1)

p1=grepRaw("Asambleistas.porCurulex = [",tpg,fixed = T,all = T)
p2=grepRaw("]",tpg,fixed = T,all = T)
p2=min(p2[p2>p1])
txt2=substr(tpg,p1+24,p2-2)
txt2=gsub("\\}\\]$|^\\[\\{","",txt2)
txt2=gsub("\\}\\, \\{","\n",txt2)

sink("a.txt")
cat(txt1)
sink()
txt1=read.delim("a.txt",header = F,sep = ",")

sink("a.txt")
cat(txt2)
sink()
txt2=read.delim("a.txt",header = F,sep = ",")


txt1=
  lapply(txt1,function(x){
    x=gsub("^ {1,}","",x)
    x=lapply(strsplit(x,"\\:"),function(x)  c(x[1],paste0(x[-1],collapse = ":")))
    x=t(simplify2array(x))
    x=apply(x,2,function(z) gsub("^ {1,}| {1,}$","",z))
    x=apply(x,2,function(z) gsub(" {1,}"," ",z))
    nm=unique(x[,1])[1]
    x=matrix(x[,2],ncol=1)
    colnames(x)=nm
    x
  }
  )
txt2=
  lapply(txt2,function(x){
    x=gsub("^ {1,}","",x)
    x=lapply(strsplit(x,"\\:"),function(x)  c(x[1],paste0(x[-1],collapse = ":")))
    x=t(simplify2array(x))
    x=apply(x,2,function(z) gsub("^ {1,}| {1,}$","",z))
    x=apply(x,2,function(z) gsub(" {1,}"," ",z))
    nm=unique(x[,1])[1]
    x=matrix(x[,2],ncol=1)
    colnames(x)=nm
    x
  }
  )


names(txt1)=NULL
names(txt2)=NULL



txt1=do.call(cbind,txt1)
txt1=as.data.frame(txt1,stringsAsFactors=F)
txt2=do.call(cbind,txt2)
txt2=as.data.frame(txt2,stringsAsFactors=F)

txt=rbind.fill(txt1,txt2)
rm(txt1,txt2)

txt$cal=NULL
txt$calificacion=NULL
txt$sendgrid_lista=NULL
txt$dump2=NULL
txt$transparente=NULL
txt$cal=NULL
txt$clase=NULL

acent=c("Á","É","Í","Ó","Ú","Ü","Ñ","¡","¿",tolower(c("Á","É","Í","Ó","Ú","Ü","Ñ")))
remp=paste0("\\u00",sapply(acent,charToRaw))
aux=function(z) gsub("<br>","\n",
                     str_replace_all(z,paste0(gsub("\\","\\\\",remp,fixed=T),collapse = "|"),
                                     function(x) {acent[match(x,remp)]}),fixed = T)


txt$comision=aux(txt$comision)
txt$partido_id=as.integer(txt$partido_id)
txt$id=as.integer(txt$id)
txt$provincia=factor(aux(txt$provincia))
txt$tipo=factor(aux(txt$tipo))
txt$partido=factor(aux(txt$partido))
txt$nombre=aux(txt$nombre)
txt$curul=as.integer(txt$curul)
txt$nombre1=aux(txt$nombre1)
txt$nombre2=aux(txt$nombre2)
txt$provincia_id=as.integer(txt$provincia_id)
txt$activo=txt$activo=="true"
txt$video_texto=aux(txt$video_texto)

txt=txt[order(txt$curul),]
txt=txt[order(-txt$activo),]
rownames(txt)=NULL
txt$csvDetalle=paste0("https://observatoriolegislativo.ec/informes/asambleistas/descarga/detalle-votos/",txt$slug,"?formato=csv")
txt$partidoP=factor(gsub("[ ]{0,}-[ ]{0,}.{1,}","",txt$partido))
txt$partidoP=factor(txt$partidoP,names(sort(table(txt$partidoP),decreasing = T)))
levels(txt$partido)=gsub(" {1,}- {1,}","-",levels(txt$partido))
txt$partido=factor(as.character(txt$partido),names(sort(table(txt$partido),decreasing = T)))

votos=
  suppressMessages({
    lapply(txt$csvDetalle,function(x) {
      try({
        vot=read_delim(x,delim = "|",col_names=F)
        vot=unique(vot)
        vot$X1[1]=paste0(vot$X1[1],",Suplente")
        vot=read.csv(text=paste0(vot$X1,collapse = "\n"))
        vot
      },silent = T)
    })
  })

err=which(sapply(votos,function(x) "try-error" %in% class(x)))

while(length(err)!=0) {
  votos[err]=
    suppressMessages({
      lapply(txt$csvDetalle[err],function(x) {
        try({
          vot <- suppressWarnings(read_csv(x))
          vot
        },silent = T)
      })
    })
  err=which(sapply(votos,function(x) "try-error" %in% class(x)))
}

votos=
  lapply(votos,function(x) {
    x=unique(x)
    x=x[!(grepl("Vota",x$Votación) | is.na(x$Votación)),]
    x=unique(x)
    if(nrow(x)==0) {
      x$id=integer()
    } else {
      x$id=1:nrow(x)
    }
    x
  })


names(votos)=txt$slug
votos=reshape2::melt(votos,id.vars=c("Sesión","Votación","Voto","Suplente","id"))
votos$fecha=as.Date(gsub("^.{1,} \\- ","",votos$Sesión))
votos=votos[order(votos$fecha,decreasing = T),]
rownames(votos)=NULL

rm(list = ls()[!(ls() %in% c("txt","votos"))])

votos$Suplente[(votos$Suplente=="") | is.na(votos$Suplente)]=NA
votos$S=0+(!is.na(votos$Suplente))
votos$vt=paste0(votos$Voto,".",votos$S)

vor=c("Sí.0"=1,
      "Sí.1"=1.5,
      "No.0"=2,
      "No.1"=2.5,
      "Blanco.0"=3,
      "Blanco.1"=4,
      "Abstención.0"=5,
      "Abstención.1"=6,
      "Ausente.0"=7,
      "Ausente.1"=8,
      "Excusa.0"=9,
      "Excusa.1"=10)
vor=vor[order(vor)]
votos$aux=vor[paste0(votos$Voto,".",votos$S)]

library(dplyr)
vt=
  votos %>%
  group_by(Sesión,Votación,L1,fecha) %>%
  summarise(
    Voto=paste0(unique(paste0(Voto,".",S)[aux==min(aux)]),collapse = ",")
  ) %>% ungroup()
votos$aux=NULL

vt$suplente=matrix(unlist(strsplit(vt$Voto,"\\.")),ncol=2,byrow = T)[,2]
vt$suplente=as.integer(vt$suplente)
vt$Voto=factor(matrix(unlist(strsplit(vt$Voto,"\\.")),ncol=2,byrow = T)[,1])
vt$Votación=as.integer(vt$Votación)
txt=txt[,c("slug","id","activo","curul","video_url","partido","partido_id","partidoP","provincia","provincia_id","tipo","nombre","nombre1","nombre2","genero","comision","twitter","video_texto","urlImagen","urlDetalle","csvDetalle")]
txt$activo=as.numeric(txt$activo)

write.csv(x = vt,"asamblea_votaciones.csv",row.names = F,fileEncoding = "UTF-8")
write.csv(x = txt,"asambleistas.csv",row.names = F,fileEncoding = "UTF-8")

