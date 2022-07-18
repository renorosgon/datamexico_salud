*Creación de Mapa 1. Participaciones y fallecimientos por COVID19 en México
use "final_abm.dta",clear
merge m:m cve_ent  using "pob_salud.dta"
rename cve_ent id 
preserve 
collapse (sum)covid fallec part pob ent,by(id a) 
egen partt=total(part)
replace part=part*100000/pob
egen fallect=total(fallec)
replace fallec=fallec*100000/pob
drop partt fallect
save "var_map.dta",replace
merge 1:1 id using "estados.dta"
save "point.dta",replace
use "var_map.dta",clear
spmap part using "estadoscoord.dta",id(id) fcolor(Blues) subtitle("A. Participaciones")
graph save  m1.gph,replace
spmap  fallec using "estadoscoord.dta",id(id) fcolor(Blues) subtitle("B. Fallecimientos") 
graph save  m2.gph,replace
graph combine m1.gph m2.gph
restore

*Gráfica 2. Comportamiento de la mezcla de petróleo maya de 1974 a 2021.
use "spot.dta",clear
gen m=substr(fecha,4,2)
gen a=y
destring a m,force replace
gen fecha_m=ym(a,m)
format fecha_m %tm
keep spot fecha_m
egen spot_m=mean(spot)
save "mex_spot.dta",replace
tsset fecha_m
tsline spot spot_m

*Gráfica 3. Evolución de los ingresos tributarios y petroleros en México (1990-2022)
use "ingresos.dta",clear
tsset a
gen petro_per=petro/ing
gen trib_per=trib/ing
gen rel=petro/trib
tsline  trib_per petro_per

*Gráfica 4. Relación entre ingresos tributarios y petroleros en México (1990-2022)
tsline rel

*Desarrollo del modelo de regresión MCO y de Datos Panel.
*Unión de bases 
clear
use "Participaciones.dta"
gen m=substr(fecha,1,2)
gen a=substr(fecha,4,4)
destring a m,force replace
gen fecha_m=ym(a,m)
recode m (1/3=1) (2/6=2) (7/9=3) (10/12=4),generate(m1)
format fecha_m %tm
gen fecha_q=yq(a,m1)
format fecha_q %tq
reshape long part, i(fecha_m) j(ent)
rename ent id
merge m:m id using "estados.dta"
tabstat part ,statistics(sum) by(NOM_ENT)
tabstat part if a==2018 & m<=8,statistics(sum) by(NOM_ENT)
tabstat part if a==2019 & m<=8,statistics(sum) by(NOM_ENT)
tabstat part if a==2020 & m<=8,statistics(sum) by(NOM_ENT)
tabstat part if a==2021 & m<=8,statistics(sum) by(NOM_ENT)
keep fecha_m id part a m
rename id cve_ent
merge m:m fecha_m cve_ent using "Covid.dta"
drop _merge
merge m:m fecha_m  using "mex_spot.dta"
keep if _merge==3 
drop _merge 
merge m:m fecha_m cve_ent using "imss1.dta"

*Definición del período de análisis
keep if a>=2020
collapse (mean) trab_imss part spot (sum) covid fallec letali,by(fecha_m cve_ent)
save "final_abm.dta",replace
merge m:m cve_ent  using "pob_salud.dta"

*Creación de variables 
foreach c in part trab_imss covid fallec letali {
gen `c'_pob=`c'*100/pob	
gen l`c'=ln(`c')
gen l`c'_pob=ln(`c'_pob)
}
gen lspot=ln(spot)
xtset cve_ent fecha

foreach c in lletali_pob lcovid_pob lpart_pob lspot letali_pob covid_pob part_pob spot{
	gen D`c'=D.`c'
	gen L`c'=L.`c'
	
}

*Creación de la variable t (tiempo)
gen t=.
by cve_ent: replace t = cond(L.t == ., 1, L.t + 1)

*Regresión de la tabla 2.Estimación del impacto de las participaciones en fallecimientos
reg Dlletali_pob Dlcovid_pob Dlpart_pob Dlspot t 
estimates store M
xtset cve_ent fecha
xtreg Dlletali_pob Dlcovid_pob Dlpart_pob Dlspot t,fe
estimates store Mfe
xtreg Dlletali_pob Dlcovid_pob Dlpart_pob Dlspot t,re
estimates store Mre
hausman Mfe
estout M Mfe Mre, cells(b(star fmt(3)) t(par fmt(2)))  stats(r2 bic N) starlevels(* 0.1 ** 0.05 *** 0.01)

