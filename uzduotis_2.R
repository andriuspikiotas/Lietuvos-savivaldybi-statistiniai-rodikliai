library(sf)
library(plotly)
library(dplyr)
library(httr)
library(jsonlite)


gimstamumas_data <- tryCatch(
  {
     gimstamumas <- httr::GET("https://get.data.gov.lt/datasets/gov/lsd/statistika/bendrasis_gimstamumo_rodiklis/S3R692M30105012/:format/json", httr::timeout(10))
     if (httr::status_code(gimstamumas)!=200){
       stop("Klaida")
     }
     httr::content(gimstamumas,as="text")
  },
  warning=function(w){
    print(w)
  },
  error=function(e){
    print(e)
  }
)

if (is.null(gimstamumas_data)) {
  message("Gimstamumo duomenu nėra")
} else {
  message("Gimstamumo duomenys gauti sekmingai")
}

gimstamumas_data_JSON <- jsonlite::fromJSON(gimstamumas_data)

df_1 <- data.frame(gimstamumas_data_JSON[1])

df_1 <- df_1[df_1$X_data.teritorija_tipas=="Savivaldybė" & df_1$X_data.laikotarpis_id==2024,c(5,12)]
df_1 <- df_1[df_1$X_data.teritorija!="Marijampolės r. sav.",]

df_1 <- df_1 %>% arrange(X_data.teritorija)
df_1 <- df_1 %>% rename(Gimstamumas=X_data.verte)
df_1 <- df_1 %>% rename(Savivaldybe=X_data.teritorija)


mirtingumas_data <- tryCatch(
  {
    mirtingumas <- httr::GET("https://get.data.gov.lt/datasets/gov/lsd/statistika/bendrasis_mirtingumas/S3R694M30106012/:format/json")
    if (httr::status_code(mirtingumas)!=200){
      stop("Klaida")
    }
    httr::content(mirtingumas,as="text")
  },
  warning=function(w){
    print(w)
  },
  error=function(e){
    print(e)
  }
)

if (is.null(mirtingumas_data)) {
  message("Mirtingumo duomenu nėra")
} else {
  message("Mirtingumo duomenys gauti sekmingai")
}

mirtingumas_data_JSON <- jsonlite::fromJSON(mirtingumas_data)

df_2 <- data.frame(mirtingumas_data_JSON[1])
df_2 <- df_2[df_2$X_data.teritorija_tipas=="Savivaldybė" & df_2$X_data.laikotarpis_id==2024,c(5,12)]
df_2 <- df_2[df_2$X_data.teritorija!="Marijampolės r. sav.",]


df_2 <- df_2 %>% arrange(X_data.teritorija)
df_2 <- df_2 %>% rename(Mirtingumas=X_data.verte)
df_2 <- df_2 %>% rename(Savivaldybe=X_data.teritorija)


gyventoju_sk_data <- tryCatch(
  {
    gyventoju_sk <- httr::GET("https://get.data.gov.lt/datasets/gov/lsd/statistika/nuolatiniai_gyventojai/S3R167M3010202")
    if (httr::status_code(gyventoju_sk)!=200){
      stop("Klaida")
    }
    httr::content(gyventoju_sk,as="text")
  },
  warning=function(w){
    print(w)
  },
  error=function(e){
    print(e)
  }
)

if (is.null(gyventoju_sk_data)) {
  message("Gyventoju skaiciaus duomenu nėra")
} else {
  message("Gyventoju skaiciaus duomenys gauti sekmingai")
}


gyventoju_sk_data_JSON <- jsonlite::fromJSON(gyventoju_sk_data)

df_3 <- data.frame(gyventoju_sk_data_JSON[1])

mask <- grepl("sav",df_3$X_data.administracine_teritorija)
df_3 <- df_3[mask,]
df_3 <- df_3[df_3$X_data.amzius_id=='g000g999' & df_3$X_data.laikotarpis_id==2024 & df_3$X_data.lytis_id==0,]

sugrupavimas <- df_3 |>
  group_by(X_data.administracine_teritorija) |>
  summarise(Gyv_sk=sum(X_data.verte,na.rm=TRUE))
sugrupavimas <- sugrupavimas[sugrupavimas$X_data.administracine_teritorija!="Marijampolės r. sav.",]

df_3 <- sugrupavimas
df_3 <- df_3 %>% arrange(X_data.administracine_teritorija)
df_3 <- df_3 %>% rename(Savivaldybe=X_data.administracine_teritorija)


svietimo_ist_data <- tryCatch(
  {
    svietimo_ist <- httr::GET("https://get.data.gov.lt/datasets/gov/lsd/svietimo_istaigos/SvietimoIstaiga")
    if (httr::status_code(svietimo_ist)!=200){
      stop("Klaida")
    }
    httr::content(svietimo_ist,as="text")
  },
  warning=function(w){
    print(w)
  },
  error=function(e){
    print(e)
  }
)

if (is.null(svietimo_ist_data)) {
  message("Svietimo istaigu duomenu nėra")
} else {
  message("Svietimo istaigu duomenys gauti sekmingai")
}

svietimo_ist_data_JSON <- jsonlite::fromJSON(svietimo_ist_data)

df_4 <- data.frame(svietimo_ist_data_JSON[1])

unique(df_4$X_data.ins_pagr_tipas)
svietimo_istaigu_tipai <- c('Mokyklos - darželiai',
                            'Pradinė mokykla',
                            'Vidurinė mokykla',
                            'Pagrindinė mokykla',
                            'Darželiai','Gimnazija',
                            'Progimnazija',
                            'Profesinės mokyklos',
                            'Universitetas',
                            'Mokytojų švietimo centras',
                            'Neformaliojo suaugusiųjų švietimo mokykla',
                            'Aukštesniosios mokyklos',
                            'Profesinio mokymo įstaiga',
                            'Lopšeliai - darželiai',
                            'Jaunimo mokyklos',
                            'Kolegija',
                            'Konservatorijos',
                            'Profesinio mokymo centrai',
                            'Lopšeliai',
                            'Neformaliojo suaugusiųjų švietimo mokyklos',
                            'Ikimokyklinio ugdymo mokykla',
                            'Neformaliojo vaikų švietimo mokykla ir formalųjį švietimą papildančio ugdymo mokykla',
                            'Konservatorijos-aukštesniosios mokyklos')


df_4 <- df_4[!is.na(df_4$X_data.ins_pagr_tipas) & df_4$X_data.ins_pagr_tipas %in% svietimo_istaigu_tipai & is.na(df_4$X_data.ins_isreg_smir_data),]

grupavimas <- df_4 |>
  group_by(X_data.sav_pav) |>
  summarise(Svietimo_ist_sk=length(X_data.ins_pavad_lt))


df_4 <- grupavimas
df_4 <- df_4 %>% arrange(df_4$X_data.sav_pav)
df_4 <- df_4 %>% rename(Savivaldybe=X_data.sav_pav)

apgyv_ist_data <- tryCatch(
  {
    apgyv_ist <- httr::GET("https://get.data.gov.lt/datasets/gov/lsd/statistika/apgyv_ist/S8R862M40903034")
    if (httr::status_code(apgyv_ist)!=200){
      stop("Klaida")
    }
    httr::content(apgyv_ist,as="text")
  },
  warning=function(w){
    print(w)
  },
  error=function(e){
    print(e)
  }
)

if (is.null(apgyv_ist_data)) {
  message("Apgyvendinimo istaigu duomenu nėra")
} else {
  message("Apgyvendinimo istaigu duomenys gauti sekmingai")
}

apgyv_ist_data_JSON <- jsonlite::fromJSON(apgyv_ist_data)

df_5 <- data.frame(apgyv_ist_data_JSON[1])
df_5 <- df_5[df_5$X_data.teritorijos_tipas=="Savivaldybė" & df_5$X_data.laikotarpis_id==2024,]

duom_grup <- df_5 |>
  group_by(X_data.teritorija) |>
  summarise(Ap_ist_sk=sum(X_data.verte,na.rm=TRUE))

df_5 <- duom_grup
df_5 <- df_5 %>% arrange(X_data.teritorija)
df_5 <- df_5 %>% rename(Savivaldybe=X_data.teritorija)

bendra_lentele <- inner_join(df_1,df_2,by="Savivaldybe")
bendra_lentele <- inner_join(bendra_lentele,df_3,by="Savivaldybe")
bendra_lentele <- inner_join(bendra_lentele,df_4,by="Savivaldybe")
bendra_lentele <- inner_join(bendra_lentele,df_5,by="Savivaldybe")

map <- read_sf('admin_ribos.shp')
map <- st_transform(map, crs = 4326)
sav_map <- map[map$ADMIN_LEVE==5,]
sav_map <- st_cast(sav_map, "POLYGON")
sav_map <- sav_map %>% arrange(NAME)


savivaldybes <- bendra_lentele
informacija <- paste0("\nGyventoju skaicius: ",savivaldybes$Gyv_sk,
                      "\nGimstamumas: ",savivaldybes$Gimstamumas,
                      "\nMirtingumas: ",savivaldybes$Mirtingumas,
                      "\nSvietimo istaigu skaicius: ",savivaldybes$Svietimo_ist_sk,
                      "\nApgyvendinimo istaigu skaicius: ",savivaldybes$Ap_ist_sk)

sav_map$informacija <- informacija
laikas <- Sys.time()
zemelapis <- plot_ly(sav_map,
        color=~NAME,
        split=~informacija,
        showlegend=FALSE,
        hoveron="fills",
        hoverinfo="text") %>%
  layout(annotations=list(text=laikas,x=1,y=0,showarrow=FALSE,xref = "paper",yref = "paper"))
  
zemelapis


