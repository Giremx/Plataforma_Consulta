# Plataforma de Consulta
## Grupo de Información en Reproducción Elegida
PENDIENTE LOGO

En Gire, estamos comprometidas para facilitar el acceso de información en materia de justicia reproductiva. Este repositorio tiene el objetivo de, primero, brindar y visualizar la información que consideramos esencial en nuestros temas y, segundo, transparentar la forma en la que procesamos estos datos.

Todas las bases de datos -utilizadas y construídas- se encuentran en nuestro repositorio. Por lo tanto, si trabajas con este código, las ligas a éstas serán suficientes: no es necesario descargarlas, a menos de que así lo desees.

Éste es un esfuerzo para brindar más información de forma más transparente.

Los temas se dividieronde la siguiente manera:
* Embarazo adolescente.
  + Porcentaje de embarazos adolescentes por entidad federativa
* Penalización del aborto
  + Clasificación del marco normativo del aborto en códigos penales
* Violencia obstétrica
  + Porcentaje de mujeres que sufrieron al menos un tipo de violencia obstétrica
* Muerte materna
  + Razón de muerte materna nacional vs. Objetivo
  + RMM por entidad federativa


### Paquetes requeridos
Recomendamos ampliamente instalar "pacman": éste te permite instalar —en caso de que no lo tengas instalado— y prender un paquete con un solo comando.
```{r}
# install.packages("pacman")
require(pacman)
p_load(tidyverse,
       foreign, readr, Hmisc,
       ggalt, ggthemes, devtools, maps, mapdata, ggmap)
```

Dos comandos bien importantes:
```{r}
# Este comando permite que R lea la "ñ" y los acentos.
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
# Este comando desabilita la notación científica.
options(scipen=999)
```

¡Empecemos nuestro análisis!

***

### Embarazo adolescente
PENDIENTE EXPLICACIÓN

Hay muchas formas de analizar esta información: por entidad de ocurrencia, por entidad de registro, por residencia habitual de la madre, etcétera. Elige bien a tu guerrero. Acá trabajaremos con la variable de residencia habitual; los datos son descargables desde los [tabulados de natalidad del INEGI](http://www.beta.inegi.org.mx/app/tabulados/pxweb/inicio.html?rxid=fdd12ae8-d551-46fd-a8b5-b5b159c1c3ea&db=Natalidad&px=Natalidad_2).
```{r}
embado <- read.csv("https://raw.githubusercontent.com/Giremx/justiciareproductiva/master/porc_embado.csv")
data <- data.frame(embado$entidad)
n <- 6
data <- do.call("rbind", replicate(n, data, simplify = FALSE))
data$anio <- rep(2012:2017, times=1, each=32)

# Y ahora asignaremos cada valor a su respectiva entidad
embado <- embado[c(-1)]
embado <-  tidyr::gather(embado)
data$embado <- embado$value
colnames(data)[1] <- "entidad"
# Colores
col1 = "#D9E1F1" 
col2 = "#325694"

# ploteamos
ggplot(data = data, 
       aes(x = anio, y = fct_rev(entidad))) + 
  geom_tile(aes(fill = embado), colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="%")) +
  labs(title = "% Embarazo Adolescente por entidad",
       x = "Año", y = "") +
  scale_x_continuous(breaks = data$anio)
```

PENDIENTE VISUALIZACIÓN

### Penalización del aborto

PENDIENTE EXPLICACIÓN
PENDIENTE CÓDIGO

### Violencia obstétrica

PENDIENTE EXPLICACIÓN
PENDIENTE CÓDIGO

### Muerte materna
PENDIENTE MEJORAR VIS CON CÓDIGO DATA
En el año 2000, uno de los Objetivos de Desarrollo del Milenio (ODM), propuestos por la Organización de Naciones Unidas y firmados por México, fue reducir la mortalidad materna en 75%; es decir, registrar alrededor de 22 muertes maternas por cada 100 mil nacidos vivos. ¿Se habrá cumplido el objetivo?

Abrimos nuestra data:
```{r}
mm <- read.csv("https://raw.githubusercontent.com/Giremx/justiciareproductiva/master/mm_tasa.csv")
```

Esta base de datos fue construida con base en dos fuentes:
* Los datos reportados entre 2002 y 2014, corresponden a los calculados por el Observatorio de Muerte Materna (OMM). El OMM utiliza datos reportados por la Secretaría de Salud.
* Los datos reportados en 2015 y 2016, fueron calculados con base en las estadísticas de natalidad y defunciones para calcular la muerte materna de INEGI, pues la información usada por el OMM (estadísticas reportadas por Secretaría de Salud) no se encontraba disponible.

Ahora, nos vamos a quedar con un objeto que sólo tenga la RMM nacional:
```{r}
mm_nac <- mm[33,]
mm_nac <- mm_nac[c(2:16)]
mm_nac <- t(mm_nac)
mm_nac <- as.data.frame(mm_nac)
colnames(mm_nac) <- "mm_nac"
mm_nac$anio <- seq(2002,2016,1)
```

El objetivo lo podemos sacar al resolver una ecuación bastante sencilla: primero, nuestras "x" son los años y nuestras "y" es la Razón de Muerte Materna; segundo, tenemos las coordenadas de dos puntos de nuestra línea objetivo:
$x~1~ = 2002; x~2~ = 2015$
$y~1~ = 54.18; y~2~ = 22$
Por lo tanto, podemos obtener la pendiente "m" si resolvemos la siguiente ecuación:
$m = (y~2~ - y~1~) / (x~2~ - x~1~)$
```{r}
x1 <- 2002
x2 <- 2015
y1 <- 54.18
y2 <- 22
m <- (y2-y1)/(x2-x1)
b <- y2-m*x2
eq = function(x){
  m*x+b
}
```

Ésta es nuestra línea objetivo
```{r}
ggplot(data.frame(x=c(0, 10)), aes(x=x)) + 
  stat_function(fun=eq, geom="line", size=1.5) +
  xlab("Año") + ylab("RMM") + 
  xlim(2002,2017)
```

Ahora ploteemos el objetivo y la RMM nacional:
```{r}
fiuff <- "Razón de muerte materna en México (2002-2016)"
fiuf <- "Uno de los objetivos del milenio firmados por México fue reducir la mortalidad materna en 75% para 2015; es decir, registrar 22 muertes maternas por cada 100 mil nacidos vivos. Este objetivo, como lo muestra la gráfica, no se cumplió."

ggplot(data=mm_nac, aes(anio)) + 
  geom_line(aes(y = mm_nac, color="Nacional"))  + 
  stat_function(fun=eq, geom="line", aes(color="Objetivo")) +
  scale_color_manual("",
                     values = c("red","blue")) +
  ggtitle(subtitle = str_wrap(fiuf, width = 100), label = fiuff) +
  xlab("Año") +
  ylab("Razón de Muerte Materna") 
```

PENDIENTE VISUALIZACIÓN

Otro factor importante que analizar es la RMM por entidad federativa. Esta información ya la tenemos en nuestro df "mm".

Una forma bonita para visualizarla es hacer un mapa de calor. Tenemos que crear un dataframe adecuado para hacerlo.
```{r}
# Dropeamos el nacional
mm <- mm[1:32,]
# Creamos un dataframe con entidades y años en columnas
data <- data.frame(mm$Entidad)
colnames(data) <- "entidad"
n <- 15
data <- do.call("rbind", replicate(n, data, simplify = FALSE))
# Agregamos los años para cada entidad
data$anio <- rep(2002:2016, times=1, each=32)

# Y ahora asignaremos cada valor a su respectiva entidad
mm <- mm[c(-1)]
mm <-  tidyr::gather(mm)
data$rmm <- mm$value

# Colores
col1 = "#D9E1F1" 
col2 = "#325694"
anio <- data$anio

# ploteamos
ggplot(data = data, 
       aes(x = anio, y = fct_rev(entidad))) + 
  geom_tile(aes(fill = rmm), colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="RMM")) +
  labs(title = "Razón de Muerte Materna, por entidad",
       x = "Año", y = "Entidad") +
  scale_x_continuous(breaks = anio)
```

PENDIENTE VISUALIZACIÓN
