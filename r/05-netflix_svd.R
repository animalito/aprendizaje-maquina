library(ggplot2)
library(dplyr)

load(file='datos/netflix_full.Rdata')

pelis.nombres <- read.csv('datos/movies_title_fix.csv', header=FALSE, stringsAsFactors=FALSE)
head(pelis.nombres)
names(pelis.nombres) <- c('peli_id','a','nombre')
head(dat.1)
nrow(dat.1)

## Calcular promedios por pelicula
prom.peli <- dat.1 %>%
    group_by(peli_id) %>%
    summarise(media.calif = mean(calif), num.calif=length(calif))
prom.peli <- prom.peli %>%
    left_join(pelis.nombres)

arrange(prom.peli, desc(media.calif)) %>%
    select( nombre,media.calif,num.calif) %>%
    head(150) %>%
    data.frame()

## Calificaciones promedio de películas:
ggplot(prom.peli, aes(x = media.calif)) + geom_histogram()
ggplot(prom.peli, aes(num.calif, y = media.calif)) + geom_point() +
    scale_x_log10()

# Ahora separamos entrenamiento y validación.
set.seed(2804)
usuarios <- unique(dat.1$usuario_id)

num.usuarios <- length(usuarios)
usuarios.val <- sample(usuarios, 100000)

peliculas <- unique(dat.1$peli_id)
num.pelis <- length(peliculas)
num.pelis
pelis.val <- sample(peliculas, 8000)

dat.e <- dat.1 %>%
    filter(  !(usuario_id %in% usuarios.val) | !(peli_id %in% pelis.val))

dat.v <- dat.1 %>%
    filter( (usuario_id %in% usuarios.val) & (peli_id %in% pelis.val))


## Si predecimos con media general
media.gral <- mean(dat.e$calif)
media.gral
sqrt(mean((dat.v$calif-mean(dat.e$calif))^2))


# Ahora usamos niveles de películas y de usuario.

usuarios.media <- dat.e %>%
    group_by(usuario_id) %>%
    summarise(media.usu = mean(calif))
pelis.media <- dat.e %>%
    group_by(peli_id) %>%
    summarise(media.peli = mean(calif))

dat.pred <- dat.v %>%
    left_join(usuarios.media) %>%
    left_join(pelis.media)

dat.pred$pred <- dat.pred$media.usu + (dat.pred$media.peli - media.gral)
sqrt(mean((dat.pred$pred - dat.pred$calif)^2, na.rm = T))


## ojo:  perdimos algunos usuarios/peliculas, en este caso deberíamos predecir de otra
## forma
#table(is.na(dat.pred$pred))

#filter(dat.pred, is.na(pred)) %>%
#    arrange(usuario_id, peli_id) %>%
#    head(30)



## Ahora intentamos una mejor manera de hacer predicciones. Comenzamos
## centrando los datos de entrenamiento:
dat.e2 <- dat.e %>% 
    group_by(usuario_id) %>%
    mutate(media.usu = mean(calif)) %>%
    group_by(peli_id) %>%
    mutate(media.peli = mean(calif) - media.gral) %>%
    mutate(calif.aj = calif - (media.usu + media.peli))
    
library(Matrix)
library(irlba)
i <- dat.e2$usuario_id
j <- dat.e2$peli_id
y <- dat.e2$calif.aj

## esta no es la mejor manera, pues pone películas no vistas como  0 (promedio),
## pero de todas formas mejora el modelo:
## requiere más de 8 Gb de memoria
X <- sparseMatrix(i, j, x = y)
X.decomp <- irlba(X, nu = 2, nv = 2)


pelis.v <- X.decomp$v
dim(pelis.v)
usuarios.u <- X.decomp$u
valor.singular <- X.decomp$d


## Podemos ver las dimensiones de las películas
pelis.v2 <- data.frame(pelis.v)
pelis.v2$peli_id <- 1:nrow(pelis.v2)
usuarios.u2 <- data.frame(usuarios.u)
usuarios.u2$usuario_id <- 1:nrow(usuarios.u2)
names(usuarios.u2) <- c('Z1','Z2','usuario_id')

pelis.v3 <- prom.peli %>%
    left_join(pelis.v2)

# eje de "seriedad":
arrange(pelis.v3 %>% left_join(pelis.nombres), X1)
arrange(pelis.v3 %>% left_join(pelis.nombres), desc(X1))

# eje de "poblacion objetivo" (hombres o mujeres):
arrange(pelis.v3 %>% left_join(pelis.nombres), X2)
arrange(pelis.v3 %>% left_join(pelis.nombres), desc(X2))

## Ahora veamos predicciones:
dat.pred.2 <- dat.pred %>%
    left_join(pelis.v3) %>%
    left_join(usuarios.u2)

dat.pred.2$nueva.pred <- dat.pred.2$pred + valor.singular[1]*dat.pred.2$X1*dat.pred.2$Z1 +
     valor.singular[2]*dat.pred.2$X2*dat.pred.2$Z2
head(dat.pred.2)
sqrt(mean((dat.pred.2$nueva.pred - dat.pred.2$calif)^2, na.rm = T))




