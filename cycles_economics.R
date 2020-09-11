# APLICACIÓN DE LA MEDICIÓN DE CICLOS ECONÓMICOS
#Clean Memory
rm(list = ls())

library(mFilter)
# Hodrick y Prescott Filter

#Setting Work Directory
setwd("D:/Dropbox/-Learning/Economia-UdeA/J2020-1/MACRO I/Unid1/1Cycles/Assigment1/Economic_Cycles2")
# X <- read.txt("../data/tema7/data_COL.txt")

#Reading Data.
X<-read.table("data_Cycles.txt", header=T, dec=",")

# PVC: Prendas de vestir y calzado
# RH:	Restaurantes y hoteles
# V:	Vivienda
# PPI:	Productos de propiedad intelectual
# MC:	Minas y Canteras
# SE:	Servicios a las empresas

#le decimos al programa que cada una de las columnas de los datos sea reconocida como
#una variable diferente.
attach(X)

# Time Series Data
pib<-ts(PIB, start=c(2005,1), frequency=4)    #PIB a Precios Constantes
PVC<-ts(PVC, start=c(2005,1), frequency=4)    #PVC:Prendas de vestir y calzado
RH<-ts(RH, start=c(2005,1), frequency=4)      #RH: Restaurantes y hoteles
V<-ts(V, start=c(2005,1), frequency=4)        #V:  Vivienda
PPI<-ts(PPI, start=c(2005,1), frequency=4)    #PPI:Productos de propiedad intelectual
MC<-ts(MC, start=c(2005,1), frequency=4)      #MC: Minas y Canteras
SE<-ts(SE, start=c(2005,1), frequency=4)      #SE: Servicios a las empresas

#Applying Hodrick y Prescott Filter
# The series yt, is made up of a trend component T and a cyclical component and an error component e
# hp$cycle
# hp$trend
# lambda suggestion
# quarterly data: lambda = 1600
# monthly data: lambda = 14400
# annual data: lambda = 129600

pib.hp<-hpfilter(pib, freq=1600, type="lambda", drift=FALSE)
PVC.hp<-hpfilter(PVC, freq=1600, type="lambda", drift=FALSE)
RH.hp<-hpfilter(RH, freq=1600, type="lambda", drift=FALSE)
V.hp<-hpfilter(V, freq=1600, type="lambda", drift=FALSE)
PPI.hp<-hpfilter(PPI, freq=1600, type="lambda", drift=FALSE)
MC.hp<-hpfilter(MC, freq=1600, type="lambda", drift=FALSE)
SE.hp<-hpfilter(SE, freq=1600, type="lambda", drift=FALSE)

# Plot Hodrick y Prescott Filter
plot(pib.hp)
# plot(PVC.hp)     # PVC: Prendas de vestir y calzado
# plot(RH.hp)    # RH:  Restaurantes y hoteles
# plot(V.hp)     # V:   Vivienda
# plot(PPI.hp)   # PPI:	Productos de propiedad intelectual
# plot(MC.hp)    # MC:	Minas y Canteras
# plot(SE.hp)    # SE:	Servicios a las empresas

# percentage deviations from trend
desv.pib=(pib.hp$cycle/pib.hp$trend)
desv.PVC=(PVC.hp$cycle/PVC.hp$trend)
desv.RH=(RH.hp$cycle/RH.hp$trend)
desv.V=(V.hp$cycle/V.hp$trend)
desv.PPI=(PPI.hp$cycle/PPI.hp$trend)
desv.MC=(MC.hp$cycle/MC.hp$trend)
desv.SE=(SE.hp$cycle/SE.hp$trend)

plot(desv.pib, main="Comportamiento del componente ciclico", ylab="Desv. respecto a la tendencia", type="l",
     col="blue")
legend("top", #Ubicacion de la legenda 
       text.width = strwidth("PIB"), #Tama?o de la ventana de leyenda
       lty = 1, #Tipo de lineas usados en la leyenda
       cex=1, #Tama?o relativo de la caja de leyenda
       bty = "n", #Con esto se le eliminan los bordes a la leyenda
       c("PIB"), #Nombre de las leyendas
       col=c("blue") #Color de las lineas
)

# plot(desv.pib, main="Comportamiento del componente ciclico", ylab="Desv. respecto a la tendencia", type="l",col="blue")
# legend("topright", text.width = strwidth("PIB"), lty = 1, cex=1, bty = "n", c("PIB"), col=c("blue"))
# 
# plot(desv.PVC, main="Comportamiento del componente ciclico", ylab="Desv. respecto a la tendencia", type="l",col="blue")
# legend("topright", text.width = strwidth("PVC"), lty = 1, cex=1, bty = "n", c("PVC"), col=c("blue"))
# 
# plot(desv.RH, main="Comportamiento del componente ciclico", ylab="Desv. respecto a la tendencia", type="l",col="blue")
# legend("topright", text.width = strwidth("RH"), lty = 1, cex=1, bty = "n", c("RH"), col=c("blue"))


######################################################################
##Amplitud del ciclo economico.
#####################################################################
s.pib<-sd(desv.pib)
s.PVC<-sd(desv.PVC)
s.RH<-sd(desv.RH)
s.V<-sd(desv.V)
s.PPI<-sd(desv.PPI)
s.MC<-sd(desv.MC)
s.SE<-sd(desv.SE)

plot(desv.pib, main="Comportamiento del componente ciclico", ylab="Desv. respecto a la tendencia", type="l",
     col="blue")
lines(desv.PVC, col=1)
legend("bottomleft", #Ubicacion de la legenda 
       text.width = strwidth("Prendas de vestir y calzado"), #Tama?o de la ventana de leyenda
       lty = 1, #Tipo de lineas usados en la leyenda
       cex=0.9, #Tama?o relativo de la caja de leyenda
       bty = "n", #Con esto se le eliminan los bordes a la leyenda
       c("Prendas de vestir y calzado","PIB"), #Nombre de las leyendas
       col=c("blue", "black") #Color de las lineas
)

# PVC: Prendas de vestir y calzado
# RH:  Restaurantes y hoteles
# V:   Vivienda
# PPI: Productos de propiedad Intelectual
# MC:  Minas y Canteras
# SE:  Servicios a las empresas

plot(desv.pib, main="Comportamiento del componente ciclico", ylab="Desv. respecto a la tendencia", type="l",
     col="blue", ylim=c(-0.35,0.35),
)
lines(desv.V, col=1)
legend("bottomleft", text.width = strwidth("Vivienda"), lty = 1, cex=0.9, bty = "n",
       c("Vivienda","PIB"), col=c("blue", "black"))


#vector de amplitud relativa:
amplitude<-c(s.pib, s.PVC, s.RH, s.V, s.PPI, s.MC, s.SE) 
amplitude<-matrix(c(amplitude), nrow = 1, ncol=6, byrow = TRUE)
dimnames(amplitude)=list(c("Desv. estandar relativa"), c("PVC", "RH", "V", "PPI", "MC", "SE"))

######################################################################
##Calculo de comovimientos.
######################################################################
# variables son prociclicas, contraciclicas o aciclicas.

#Grafica de dispersion.
plot.default(desv.pib, desv.PVC, 
             main="Correlacion entre PIB y Prendas de vestir y calzado", 
             xlab = "PIB",
             ylab = "Prendas de vestir y calzado", 
             frame.plot = F, #Eliminar los bordes del grafico
             col="black", 
             pch=16) #Elegir puntos rellenos de color negro
abline(lm(desv.PVC~desv.pib), col="blue")

# PVC:  Prendas de vestir y calzado
# RH:	Restaurantes y hoteles
# V:	Vivienda
# PPI:	Productos de Propiedad Intelectual
# MC:	Minas y Canteras
# SE:	Servicios a las empresas
plot.default(desv.pib, desv.SE, main="Correlación entre PIB y Servicios a las Empresas", xlab = "PIB",
             ylab = "Servicios a las Empresas", frame.plot = F, col="black", pch=16)
abline(lm(desv.SE~desv.pib), col="blue")

# Correlaciones de las variables de interes en el periodo t.
corr_t<-c(cor(desv.pib, desv.PVC), cor(desv.pib, desv.RH), cor(desv.pib, desv.V), 
            +cor(desv.pib, desv.PPI), cor(desv.pib, desv.MC), cor(desv.pib, desv.SE))

#Poner el resultado anterior en forma vectorial
corr_t<-matrix(c(corr_t), nrow = 6, ncol=1, byrow = TRUE) #Se transforma en un vector columna de 11 entradas
dimnames(corr_t)=list(c("PVC", "RH", "V", "PPI", "MC", "SE"), c("t")) 

##########################################################################
#Liderazgo o rezago
##########################################################################

#Se especifica el numero de periodos de adelanto de la variable
nlead=1
cor_lead_1_PVC.pib<-cor(desv.pib[(1+nlead):length(desv.pib),1], 
                        +desv.PVC[1:(length(desv.PVC)-nlead),1])

#Se especifica el numero de periodos de rezago con la variable nlag
nlag=1
cor_lag_1_PVC.pib<-cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.PVC[(1+nlag):length(desv.PVC),1])


#calculo de los coeficientes de correlacion de todas las variables
#adelantadas

#adelantadas un periodos.
nlead=1
corr_lead_1<-c(cor(desv.pib[(1+nlead):length(desv.pib),1], desv.PVC[1:(length(desv.PVC)-nlead),1]),
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.RH[1:(length(desv.RH)-nlead),1]), 
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.V[1:(length(desv.V)-nlead),1]), 
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.PPI[1:(length(desv.PPI)-nlead),1])
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.MC[1:(length(desv.MC)-nlead),1])
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.SE[1:(length(desv.SE)-nlead),1]))

#Poner el resultado anterior en forma vectorial
corr_lead_1<-matrix(c(corr_lead_1), nrow = 6, ncol=1, byrow = TRUE)

dimnames(corr_lead_1)=list(c("PVC", "RH", "V", "PPI", "MC", "SE"), c("t-1"))

#Adelantadas dos periodos.
nlead=2
corr_lead_2<-c(cor(desv.pib[(1+nlead):length(desv.pib),1],desv.PVC[1:(length(desv.PVC)-nlead),1]),
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.RH[1:(length(desv.RH)-nlead),1]), 
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.V[1:(length(desv.V)-nlead),1]), 
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.PPI[1:(length(desv.PPI)-nlead),1])
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.MC[1:(length(desv.MC)-nlead),1])
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.SE[1:(length(desv.SE)-nlead),1]))

#Poner el resultado anterior en forma vectorial
corr_lead_2<-matrix(c(corr_lead_2), nrow = 6, ncol=1, byrow = TRUE)
dimnames(corr_lead_2)=list(c("PVC", "RH", "V", "PPI", "MC", "SE"), c("t-2"))

#Adelantadas tres periodos.
nlead=3
corr_lead_3<-c(cor(desv.pib[(1+nlead):length(desv.pib),1], desv.PVC[1:(length(desv.PVC)-nlead),1]),
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.RH[1:(length(desv.RH)-nlead),1]), 
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.V[1:(length(desv.V)-nlead),1]), 
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.PPI[1:(length(desv.PPI)-nlead),1])
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.MC[1:(length(desv.MC)-nlead),1])
               +cor(desv.pib[(1+nlead):length(desv.pib),1], desv.SE[1:(length(desv.SE)-nlead),1]))

#Poner el resultado anterior en forma vectorial
corr_lead_3<-matrix(c(corr_lead_3), nrow = 6, ncol=1, byrow = TRUE)
dimnames(corr_lead_3)=list(c("PVC", "RH", "V", "PPI", "MC", "SE"), c("t-3"))

#calculo de los coeficientes de correlacion de todas las variables
#rezagadas

#rezagadas un periodo.
nlag=1
corr_lag_1<-c(cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.PVC[(1+nlag):length(desv.PVC),1]),
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.RH[(1+nlag):length(desv.RH),1]),
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.V[(1+nlag):length(desv.V),1]),
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.PPI[(1+nlag):length(desv.PPI),1])
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.MC[(1+nlag):length(desv.MC),1])
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.SE[(1+nlag):length(desv.SE),1]))


#Poner el resultado anterior en forma vectorial
corr_lag_1<-matrix(c(corr_lag_1), nrow = 6, ncol=1, byrow = TRUE)
dimnames(corr_lag_1)=list(c("PVC", "RH", "V", "PPI", "MC", "SE"), c("t+1"))


#rezagadas dos periodos
nlag=2
corr_lag_2<-c(cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.PVC[(1+nlag):length(desv.PVC),1]),
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.RH[(1+nlag):length(desv.RH),1]),
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.V[(1+nlag):length(desv.V),1]),
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.PPI[(1+nlag):length(desv.PPI),1])
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.MC[(1+nlag):length(desv.MC),1])
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.SE[(1+nlag):length(desv.SE),1]))

#Poner el resultado anterior en forma vectorial
corr_lag_2<-matrix(c(corr_lag_2), nrow = 6, ncol=1, byrow = TRUE)
dimnames(corr_lag_2)=list(c("PVC", "RH", "V", "PPI", "MC", "SE"), c("t+2"))

#Rezagada tres periodos
nlag=3
corr_lag_3<-c(cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.PVC[(1+nlag):length(desv.PVC),1]),
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.RH[(1+nlag):length(desv.RH),1]),
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.V[(1+nlag):length(desv.V),1]),
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.PPI[(1+nlag):length(desv.PPI),1])
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.MC[(1+nlag):length(desv.MC),1])
              +cor(desv.pib[1:(length(desv.pib)-nlag),1],desv.SE[(1+nlag):length(desv.SE),1]))

#Poner el resultado anterior en forma vectorial
corr_lag_3<-matrix(c(corr_lag_3), nrow = 6, ncol=1, byrow = TRUE)
dimnames(corr_lag_3)=list(c("PVC", "RH", "V", "PPI", "MC", "SE"),c("t+3"))

# Tabla de Correlación
comovements<-cbind(corr_lead_3, corr_lead_2, corr_lead_1, corr_t, corr_lag_1, 
                   +corr_lag_2, corr_lag_3)
comovements

#Matriz de Correlación
library(ggplot2)
library(corrplot)
round(comovements, digits = 2)
# corrplot(comovements)
col <-colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(comovements, method = "square", tl.col = "black", tl.srt = 45, col = col(200), 
         addCoef.col = "black", addshade = "all")
