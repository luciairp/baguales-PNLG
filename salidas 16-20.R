library(tidyverse)
library(lubridate)
datos <- read_csv("datos_salida.csv")


datos <- datos %>% 
  mutate(fecha = make_date(y,m,d))

datos.mes <- datos %>% 
  group_by(y,m, zona) %>% 
  summarise(cantmensual = sum(cantidad),
            kmmensual = sum(km),na.rm = TRUE) %>% 
  mutate(CPUEmensual = cantmensual/kmmensual) %>%
  mutate(fecham = make_date(y,m))

ggplot(datos.mes,aes(x = fecham, y = CPUEmensual, col= zona))+
  geom_point()+
  geom_smooth(se = F)+
  theme_bw()

+
facet_wrap(. ~ zona)

datos.mes %>% 
  filter(zona == "BR") %>% 
ggplot(aes(fecham, CPUEmensual))+
  geom_point()+
  geom_smooth(method = "lm", se=T, color="red", formula = y ~ x)

datos.mes %>% 
  filter(zona == "BSO") %>% 
  ggplot(aes(fecham, CPUEmensual))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x)


