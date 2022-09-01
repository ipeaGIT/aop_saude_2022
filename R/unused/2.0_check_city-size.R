# 
#
# check population influence
#
# 
library(aopdata)
library(geobr)
library(sidrar)
library(magrittr)
library(data.table)
library(mapview)

# 1) download CENSUS--------------------

pop2010_raw <- sidrar::get_sidra(x = 202
                                 , variable = 93
                                 , period = "2010"
                                 , geo = "City"
                                 , classific = c("c1"))
pop2010 <- pop2010_raw
# fix names
data.table::setDT(pop2010)
names(pop2010) <- janitor::make_clean_names(names(pop2010))
pop2010 <- pop2010[sexo == "Total" & situacao_do_domicilio == "Urbana",]


# download metro

dt_metro_raw <- geobr::read_metro_area()
data.table::setDT(dt_metro_raw)

dt_metro <- data.table::copy(dt_metro_raw)
# download cities aop

dt_aop_raw <- aopdata::read_population(city = "all")
dt_aop <- data.table::copy(dt_aop_raw)


# 2) Merge -----

dt_aop[1]
pop2010[1]
dt_metro[dt_aop,on = "code_muni",name_capital := i.name_muni]
metro_include <- unique(dt_metro[!is.na(name_capital),]$name_metro)

dt_metro <- dt_metro[name_metro %in% metro_include,]
dt_metro[!is.na(name_capital),name_capital := "capital"]
dt_metro[is.na(name_capital),name_capital := "others"]
dt_metro[,code_muni := as.character(code_muni)]

dt_metro
pop2010[1]
dt_metro[pop2010,on = c("code_muni" = "municipio_codigo"),pop := valor]

# 3) Sum
dt_sum <- dt_metro[,sum(pop),by = .(name_metro,name_capital)]
dt_sum[,prop := round(100*V1/sum(V1),2),by = name_metro]
dt_sum
