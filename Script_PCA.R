# Principal Component Analysis (PCA). libraries: 'factoextra', 'FactoMiner' ‘zip’, ‘openxlsx’, ‘carData’, ‘pbkrtest’, ‘rio’, ‘car’, ‘flashClust’, ‘leaps’, ‘scatterplot3d’, ‘FactoMineR’, ‘ca’, ‘igraph’
# ЧАСТЬ 1 создаем датафрейм
	# шаг-1. вчитываем таблицу. делаем из нее датафрейм.
MDepths <- read.csv("Depths.csv", header=TRUE, sep = ",")
df<- read.csv("Depths.csv", header=TRUE, sep = ",")
	# шаг-2. чистим датафрейм от NA значений
MDF<- na.omit(MDepths) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) # проверяем, удалил ли все NA
sum(row.has.na) # суммируем все NA, должно получиться: [1] 0
head(MDF) # смотрим очищенный датафрейм.

# ЧАСТЬ 2. Создаем график анализа главных компонент 
	# шаг-3. делаем анализ главных компонент и рисуем его график
PCA_Mariana <- autoplot(prcomp(MDF), loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3) +
	geom_point(color = "blue") +
	scale_color_brewer(palette="Dark2") +
	labs(title="马里亚纳海沟。剖面1-25。Mariana Trench, Profiles Nr.1-25.", 
	subtitle = "统计图表。主成分分析。PCA (Principal Component Analysis)",
	caption = "Statistics Processing and Graphs: \nR Programming. Data Source: QGIS") +  # и добавляем тему
	theme(
		plot.margin = margin(5, 10, 20, 5),
		plot.title = element_text(family = "Kai", face = "bold", size = 12), # китайский шрифт "Кай"
		plot.subtitle = element_text(family = "Hei", face = "bold", size = 10), # китайский шрифт "Хэй"
		plot.caption = element_text(face = 2, size = 6),
		panel.background=ggplot2::element_rect(fill = "white"),
		legend.justification = "bottom", 
		legend.position = "bottom",
		legend.box.just = "right",
		legend.direction = "horizontal",
		legend.box = "horizontal",
		legend.box.background = element_rect(colour = "honeydew4",size=0.2),
		legend.background = element_rect(fill = "white"),
		legend.key.width = unit(1,"cm"),
		legend.key.height = unit(.5,"cm"),
		legend.spacing.x = unit(.2,"cm"),
		legend.spacing.y = unit(.1,"cm"),
		legend.text = element_text(colour="black", size=6, face=1),
		legend.title = element_text(colour="black", size=6, face=1),
		strip.text.x = element_text(colour = "white", size=6, face=1),
		panel.grid.major = element_line("gray24", size = 0.1, linetype = "solid"),
		panel.grid.minor = element_line("gray24", size = 0.1, linetype = "dotted"),
		axis.text.x = element_text(face = 3, color = "gray24", size = 6, angle = 45),
		axis.text.y = element_text(face = 3, color = "gray24", size = 6, angle = 15),
		axis.ticks.length=unit(.1,"cm"),
		axis.line = element_line(size = .3, colour = "grey80"),
		axis.title.y = element_text(margin = margin(t = 20, r = .3), face = 2, size = 8),
		axis.title.x = element_text(face = 2, size = 8, margin = margin(t = .2))) +
		guides(col = guide_legend(nrow = 1, ncol = 6, byrow = TRUE)) # подправляем дизайн легенды.
PCA_Mariana