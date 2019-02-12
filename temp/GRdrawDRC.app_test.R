### GR draw DRC test

caseA = read.csv("/Users/nicholasclark/Git/grcalculator/resources/caseA_example.csv")
fitData = GRfit(caseA, groupingVariables = c("cell_line", "treatment"), case = "A")
# metric = c("GR", "rel_cell")
# experiments = list()
# min = "auto"
# max = "auto"
# color = "experiment"
# points = c("average", "all", "none")
# curves = c("sigmoid", "line", "biphasic", "sigmoid_high", "sigmoid_low", "none")
# bars = c("none", "sd", "se")
# xrug = c("none", "GR50", "GEC50", "IC50", "EC50")
# yrug = c("none", "GRinf", "GRmax", "Einf", "Emax")
# theme = c("classic", "minimal", "bw")
# palette = c("default","npg", "aaas")
# facet = "none"
# plot_type = c("static", "interactive")
# output_type = c("together", "separate")
metric = "GR"
experiments = list()
min = "auto"
max = "auto"
color = "experiment"
points = "average"
curves = "line"
bars = "none"
xrug = "none"
yrug = "none"
theme = "classic"
palette = "default"
facet = "cell_line"
plot_type = "static"
output_type = "separate"
test = GRdrawDRC.app(fitData, output_type = "separate", facet = "cell_line", curves = "biphasic")
test$plot[[1]]
