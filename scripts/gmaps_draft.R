p_load("gmapsdistance")

centros_bogota <- c("Centro Internacional de Bogota", "Plaza de Mercado Corabastos, Bogota", "Torre Davivienda Centro Financiero, Bogota",
  "Business Park Teleport Mizpah Group, Bogota", "Aeropuerto Internacional El Dorado, Bogota", "El Gran San, Bogota", 
  "Plaza de Mercado 7 de agosto fruver, Bogota", "Ciudad Empresarial Sarmiento Angulo, Bogota")

coordenadas_propiedades <- paste0(data$lat, ",", data$lon)

#Comenzamos únicamente con la Torre Davivienda (Para probar)
times_driving_cfinanciero <- c()
iteration <-1
for (p in coordenadas_propiedades[5018:length(coordenadas_propiedades)]){
  iteration <- iteration+1
  
  if (iteration%%100==0){
    print(paste0("Iteration ", iteration))
  }
  
  
  results <- gmapsdistance(origin = p, 
                           destination = c("Torre Davivienda Centro Financiero, Bogota"), 
                           mode = "driving",
                           combinations = "all",
                           traffic_model = "pessimistic",
                           dep_date = "2023-10-25", # provided as string in ISO 8601 format
                           dep_time = "12:00:00", # provided as string in HH:MM:SS format
                           key = Sys.getenv("GOOGLEGIDSTANCESKEY"))
  
  times_driving_cfinanciero <- c(times_driving_cfinanciero, results$Time)
}

t <- data.frame(cbind(data$property_id[5018:9002], times_driving_cfinanciero))
colnames(t) <- c("property_id", "time_cfinanciero")
write_csv(t, "t2_til9002.csv")




results <- gmapsdistance(origin = "4.70384+-74.067853", 
                         destination = c("Torre Davivienda Centro Financiero, Bogota"), 
                         mode = "driving",
                         combinations = "all",
                         traffic_model = "pessimistic",
                         dep_date = "2023-10-25", # provided as string in ISO 8601 format
                         dep_time = "12:00:00", # provided as string in HH:MM:SS format
                         key = Sys.getenv("GOOGLEGIDSTANCESKEY"))

results1 <- gmapsdistance(origin = "4.641624+-74.154555", 
                         destination = c("Torre Davivienda Centro Financiero, Bogota"), 
                         mode = "driving",
                         combinations = "all",
                         traffic_model = "pessimistic",
                         dep_date = "2023-10-25", # provided as string in ISO 8601 format
                         dep_time = "12:00:00", # provided as string in HH:MM:SS format
                         key = Sys.getenv("GOOGLEGIDSTANCESKEY"))
