# Definition of pressure types to each pressure in the Baltic Sea
# example pressure dataset

press_type_ex <- data.frame(
	press = names(press_ex)[-1],
	press_type = c(rep("Climate", 2), rep("Eutrophication", 2), rep("Fishing", 3))
	)

save(press_type_ex, file = "data/press_type_ex.rda")
