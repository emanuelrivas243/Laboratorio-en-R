#------------------------------------------------------#
#-- Universidad del Valle: Escuela de Estadística    --#
#-- Curso: Probabilidad y Estadística             --#
#-- Integrantes: Emanuel Rivas - Sebastian Quintero - John Regino --#
#------------------------------------------------------#

#------------------------------------------------------#
#   0. Configuración inicial-Librerias requeridas      #
#------------------------------------------------------#

#install.packages("easypackages")        # Libreria especial para hacer carga automática de librerias
library("easypackages")

lib_req<-c("lubridate","dplyr","visdat","missMDA","mice","DMwR2","editrules", "corrplot")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)         # Verificación, instalación y carga de librerias.


#----------------------------------------------------------------------#
#### 1. Inspección y actualización de niveles para factores     ####
#----------------------------------------------------------------------#

#Importando la base de datos en Excel

library(readxl)
paisesdatos <- read_excel("paises.xls")
View(paisesdatos)



str(paisesdatos)
summary(paisesdatos)

### Observando las etiquetas en algunas de las variables tipo Factor
table(paisesdatos$País)
table(paisesdatos$GRUPOS)

# Declaración de niveles correctos para las variables tipo Factor
level_grupospaises <- c(africa="africa", AFRICA="africa", Africa="africa", 
                        asia="asia", Asia="asia", ASIA="asia",
                        EuropaOriental="EuropaOriental", EUROPAORIENTAL="EuropaOriental",
                        iberoamerica="iberoamerica", Iberoamerica="iberoamerica", IBEROAMERICA="iberoamerica")

## Modificación del formato y transformación de variables
paisesdatos <- transform(paisesdatos,
                   GRUPOS=factor(dplyr::recode(GRUPOS, !!!level_grupospaises))
)

str(paisesdatos)
summary(paisesdatos)
str(paisesdatos$GRUPOS)

## Reglas de consistencia para la base de datos

#Importación del archivo txt con las reglas

Rules <- editrules::editfile("consistencia.txt")

windows()
plot(Rules)

# Verificación de las reglas sobre los datos
editrules::violatedEdits(Rules, paisesdatos)
Valid_Data = editrules::violatedEdits(Rules, paisesdatos)
summary(Valid_Data)

#Identificar que observaciones presentan violaciones a las reglas
#which(Valid_Data)
#matrix(data=1:55, 5, 11)

# Visualización del diagnóstico
windows()
plot(Valid_Data)


#Reporte datos faltantes en la base de datos

is.na(paisesdatos)
x11()
visdat::vis_miss(paisesdatos)

miss<-function(paisesdatos,plot=T){  
  n=nrow(paisesdatos);p=ncol(paisesdatos)
  names.obs<-rownames(paisesdatos)
  
  nobs.comp=sum(complete.cases(paisesdatos))         # Cuenta los registros completos
  Obs.comp=which(complete.cases(paisesdatos))        # Identifica los registros completos
  nobs.miss = sum(!complete.cases(paisesdatos))      # Identifica los registros con datos faltantes.
  Obs.miss=which(!complete.cases(paisesdatos))       # Identifica los registros con datos faltantes.
  
  paisesdatos.NA<-is.na(paisesdatos)
  Var_Num<- sort(colSums(paisesdatos.NA),decreasing=T)
  Var_per<-round(Var_Num/n,3)
  Obs_Num<-rowSums(paisesdatos.NA)
  names(Obs_Num)<-names.obs
  Obs_Num<-sort(Obs_Num,decreasing=T)
  Obs_per<-round(Obs_Num/p,3)
  lista<-list(n.row = n, n.col = p,n.comp = nobs.comp,Obs.comp = Obs.comp,n.miss = nobs.miss,Obs.miss = Obs.miss, Var.n = Var_Num , Var.p = Var_per, Obs.n= Obs_Num, Obs.per= Obs_per)
  
  if(plot){
    windows(height=10,width=15)
    par(mfrow=c(1,2))
    coord<-barplot(Var_per,plot=F)
    barplot(Var_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por variable")
    axis(2,at=coord,labels=names(Var_per), cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2),pos=0)
    
    coord<-barplot(Obs_per,plot=F)
    barplot(Obs_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por registro")
    axis(2,at=coord,labels=names(Obs_per),cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2))
  }
  return(invisible(lista))
}

Summary.NA = miss(paisesdatos) 

##Imputación de datos por la media en columnas esperanza.vida.mujer, pnb, poblacion

attach(paisesdatos) 

mean(Esperanza.vida.mujer,na.rm =T)
mean(PNB,na.rm = T)
mean(Población..miles.,na.rm = T)
summary(paisesdatos)


imputM = mice::mice(paisesdatos, maxit = 1, method = "mean",seed = 2018,print=F)
paisesdatos_ImputM = mice::complete(imputM)
windows(height=10,width=15); visdat::vis_miss(paisesdatos_ImputM) 


#Solución de los puntos de la actividad

#1. Como está conformada la muestra (distribución) de países según grupo.

names(paisesdatos_ImputM)

#Instalar librerias
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

#Construcción del diagrama de barras

grupospaises <- paisesdatos_ImputM %>%
  group_by(GRUPOS) %>%
  count() %>%
  ungroup() %>%
  mutate(porcentaje = `n`/sum(`n`)*100)

grupospaises

ggplot(grupospaises, aes(x=GRUPOS, y=porcentaje, fill=GRUPOS)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label=paste0(round(porcentaje,1),"%")), vjust= 1.5)+
  theme_void()



## Segundo punto ##

# Graficos con tasa de natalidad, tasa de mortalidad y tasa de mortalidad infantil

graficotasanat <- ggplot(paisesdatos_ImputM, aes(x=GRUPOS,y = Tasa.natalidad, fill=GRUPOS)) + 
  geom_boxplot() + theme_classic()


graficotasam <- ggplot(paisesdatos_ImputM, aes(x=GRUPOS,y = Tasa.mortalidad, fill=GRUPOS)) + 
  geom_boxplot() + theme_classic()


graficotasami <- ggplot(paisesdatos_ImputM, aes(x=GRUPOS,y = Mortalidad.infantil, fill=GRUPOS)) + 
  geom_boxplot() + theme_classic()

# Unión de los tres gráficos en uno solo

require(gridExtra) # Ejecutar este codigo primero
F1 <- grid.arrange(graficotasanat, graficotasam, graficotasami, nrow = 2)
F1


# Tercer Punto - Genere una nueva variable denominada PNB per cápita,
# que equivale al cociente entre PNB/# habitantes.

paisesdatos_ImputM$PNBpercapita = round((paisesdatos_ImputM$PNB/paisesdatos_ImputM$Población..miles.),4)

# Construccion del diagrama 3er punto

library(ggplot2) #ejecutar comando de librería
ggplot(paisesdatos_ImputM, aes(x=GRUPOS,y = PNBpercapita, fill=GRUPOS)) + 
  geom_bar(stat = "identity") + theme_classic()


# Cuarto Punto - Sobre la nueva variable calculada, calcule los respectivos Cuartiles, Generando 4 grupos

attach(paisesdatos_ImputM)

# Variables que contienen los cuartiles
q1 <- quantile(paisesdatos_ImputM$PNBpercapita, 0.25)
q2 <- quantile(paisesdatos_ImputM$PNBpercapita, 0.50)
q3 <- quantile(paisesdatos_ImputM$PNBpercapita, 0.75)

# Filtrando los datos con base a los cuartiles y condiciones

quantile(paisesdatos_ImputM$PNBpercapita)


paisesdatos_punto4 <- dplyr::mutate(paisesdatos_ImputM, 
                              Estrato = 
                                ifelse(PNBpercapita < q2, ifelse(PNBpercapita < q1, "Bajo", "Medio Bajo"), 
                                ifelse(PNBpercapita < q3, "Medio Alto", "Alto")
                                )
                              )


# Graficos clasificando con base al PNB per capita, a los paises en la columna Grupos
