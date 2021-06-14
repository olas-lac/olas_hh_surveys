# olas_public

Olas_public contains the code to create the OLAS Houshold Survey Dataset (create_final_dataset.R) from the master dataset (jefe231020.Rda) which contains survey answers from 18 countries in LAC. 

## Files 
## Code
**create_final_dataset.R** - code that creates datasets

## Inputs
**jefe231020.Rda** - dataset containing standardized data from household surveys from 18 countries in LAC. 

**poblacion2.csv** - population data from the 2019 World Bank Development Indicators dataset (total, rural, and urban populations).

### Household Survey Sources: 

Argentina
2018
Encuesta Permanente de Hogares
Instituto Nacional de Estadística y Censos (INDEC)

Bolivia
2018
Encuesta de Hogares
Instituto Nacional de Estadística (INE)

Brazil
2019
Pesquisa Nacional por Amostra de Domicílios Contínua
Instituto Brasileiro de Geografia e Estatística (IBGE)

Chile
2017
Encuesta de Caracterización Socioeconómica Nacional
Ministerio de Desarrollo Social y Familia

Colombia
2018
Gran Encuesta Integrada de Hogares
Departamento Administrativo Nacional de Estadística (DANE)

Costa Rica
2018**
Encuesta Nacional de Hogares
Instituto Nacional de Estadística y Censos (INEC)

Dominican Republic
2018
Encuesta Nacional Continua de Fuerza de Trabajo
Banco Central

Ecuador
2017*
Encuesta Nacional de Empleo, Desempleo y Subempleo
Instituto Nacional de Estadística y Censos (INEC)

El Salvador
2018
Encuesta de Hogares de Propósitos Múltiples
Dirección General de Estadística y Censos (DIGESTYC)

Guatemala
2018
Encuesta Nacional de Empleo e Ingresos
Instituto Nacional de Estadística (INE)

Honduras
2018
Encuesta Permanente de Hogares de Propósitos Múltiples
Instituto Nacional de Estadística (INE)

Jamaica
2015
Survey of living Conditions
Planning Institute of Jamaica (PIOJ)
Statistical Institute of Jamaica (STATIN)

Mexico
2018
Encuesta Nacional de Ingreso y Gasto de los Hogares
Instituto Nacional de Estadística y Geografía (INEGI)

Nicaragua
2014
Encuesta Nacional de Hogares sobre Medición de Nivel de Vidas
Instituto Nacional de Información de Desarrollo (INIDE)

Panama
2018
Encuesta de Hogares de Propósitos Múltiples
Instituto Nacional de Estadística y Censos (INEC)

Paraguay
2017
Encuesta Permanente de Hogares
Instituto Nacional de Estadística (INE)

Peru
2018
Encuesta Nacional de Hogares sobre Condiciones de Vida y Pobreza
Instituto Nacional de Estadística e Informática (INEI)

Uruguay
2018
Encuesta Continua de Hogares
Instituto Nacional de Estadística (INE)


### Output folders
**countrydata** - folder contains outputs from code related to country-level datasets named by ISO code and overall dataset - OLAS Country Surveys

**validation** - files for data validation, checking missing values, and country-level metrics. 
