require(XML)

### util function trims preceeding/trailing white space and removes newlines
whiteTrim = function(str) gsub(" *$","", gsub("^ *","", gsub("\\n", "", str)))

### util to Uppercase on each word (sep=" ")
toupperWord = function(sentence) Reduce(paste, unlist(lapply(strsplit(sentence, " "), function(word) paste0(toupper(substring(word, 1, 1)), tolower(substring(word, 2))))))

### States names (minus alaska)
statesNames = gsub("\"", "", scan("http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/stateNames.txt", what="", sep="\n"))[-1][-2]

### Voter data (Source 1)
voterData = data.frame()
for(stateName in statesNames){
  counties = xpathApply(xmlParse(paste0("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2012/", stateName, ".xml")), "//tbody", xmlValue)
  countiesAsMatrix = lapply(counties, function(county) t(matrix(strsplit(whiteTrim(gsub("\t", "", gsub("\r", ";", county))), ";")[[1]][-1][-1], ncol=4, byrow=T))[-4,][-4,])
  state = data.frame(t(sapply(countiesAsMatrix, function(county){ l = county[3,]; names(l) = county[1,]; return(l)})))
  state$County = gsub(" [0-9].*", "", whiteTrim(counties))
  state$State = rep(toupperWord(gsub("-", " ", stateName)), length(counties))
  if(stateName != 'alabama')
    voterData = merge(voterData, state, all=T)
  else
    voterData = state
}
voterData$County = paste0(voterData$County, " County, ", voterData$State)
voterData = voterData[1:4]

### Demographic Data (Source 2)
B01 = read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/B01003.csv")
DP02 = read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/DP02.csv")
DP03 = read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/DP03.csv")

allIDs = unique(c(unique(B01$GEO.id2),unique(DP02$GEO.id2),unique(DP03$GEO.id2)))
B01Data = data.frame(ID=allIDs)
county = unique(c(as.vector(unique(B01$GEO.display.label)),as.vector(unique(DP02$GEO.display.label)),as.vector(unique(DP03$GEO.display.label))))
B01Data$County = county
B01Data$State = gsub("^.*, ","", county)

B01Data$"Total Population Estimate" = sapply(B01Data$ID,function(x) B01$HD01_VD01[which(B01$GEO.id2==x)][which(B01$POPGROUP.id[which(B01$GEO.id2==x)]==1)])
B01Data$"White alone Estimate" = sapply(B01Data$ID,function(x) B01$HD01_VD01[which(B01$GEO.id2==x)][which(B01$POPGROUP.id[which(B01$GEO.id2==x)]==2)])
B01Data$"Black or African American alone Estimate" = sapply(B01Data$ID,function(x) B01$HD01_VD01[which(B01$GEO.id2==x)][which(B01$POPGROUP.id[which(B01$GEO.id2==x)]==4)])
B01Data$"Total Population Margin of Error" = sapply(B01Data$ID,function(x) B01$HD02_VD01[which(B01$GEO.id2==x)][which(B01$POPGROUP.id[which(B01$GEO.id2==x)]==1)])
B01Data$"White alone Margin of Error" = sapply(B01Data$ID,function(x) as.character(B01$HD02_VD01[which(B01$GEO.id2==x)][which(B01$POPGROUP.id[which(B01$GEO.id2==x)]==2)]))
B01Data$"Black or African American alone Margin of Error" = sapply(B01Data$ID,function(x) as.character(B01$HD02_VD01[which(B01$GEO.id2==x)][which(B01$POPGROUP.id[which(B01$GEO.id2==x)]==4)]))

colnames(DP02)=paste("DP02",colnames(DP02))
colnames(DP03)=paste("DP03",colnames(DP03))
DP02$County = DP02[["DP02 GEO.display.label"]]
DP03$County = DP03[["DP03 GEO.display.label"]]

demographicData = merge(B01Data, DP02, by="County")
demographicData = merge(demographicData, DP03, by="County")

### Location Data (Source 3)
locationByState = xpathApply(xmlParse("http://www.stat.berkeley.edu/users/nolan/data/Project2012/counties.gml"), "//state", xmlToList)
countyLocs = data.frame(matrix(unlist(lapply(locationByState, function(state) lapply(state[-1], function(entry)  c(paste0(whiteTrim(entry$name), ", ",toupperWord(whiteTrim(state[[1]]$text))), whiteTrim(entry$location$coord$X), whiteTrim(entry$location$coord$Y), toupperWord(whiteTrim(state[[1]]$text)))))),   ncol=4, byrow=T))
names(countyLocs) = c("County", "X", "Y", "State")

### Minor Cooersions to County's (for better merging)
voterData$County = gsub("Saint", "St.", gsub("District of Columbia County", "District of Columbia", gsub("County, Louisiana", "Parish, Louisiana", gsub("city", "County", voterData$County))))
countyLocs$County = gsub("Saint", "St.", gsub("city", "County", countyLocs$County, ignore.case=T))
demographicData$County = gsub("city", "County", demographicData$County, ignore.case=T)

### Merge Data Frames
locsPlusVotes = merge(countyLocs, voterData, by = "County")
theBeast = merge(locsPlusVotes, demographicData, by = "County")

### Eliminating duplicate state columns
theBeast = theBeast[,!(names(theBeast) %in% c("State.x","State.y"))]

################################### STEP 3 ######################################

require(rpart)
require(class)
require(RColorBrewer)

#Reading in 2004 Results
results2004 = read.table("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2004.txt", head=T)
results2004$County = whiteTrim(gsub("St ", "St. ", gsub("Saint", "St.", gsub("District of Columbia County", "District of Columbia", gsub("County, Louisiana", "Parish, Louisiana", gsub("city", "County", sapply(strsplit(as.character(results2004$countyName), ","), function(pair) paste0(toupperWord(pair[2]), " County, ", toupperWord(pair[1]))), ignore.case=T))))))
results2004$winner2004 = rep("Dem", nrow(results2004))
results2004$winner2004[results2004$bushVote > results2004$kerryVote] ="Rep"
theBeast = merge(theBeast, results2004[4:5], by="County")

#Finding 2012 results
makeComparable = function(value) as.numeric(gsub("%", "", as.character(value)))
theBeast$winner = rep("Dem", nrow(theBeast))
theBeast$winner[makeComparable(theBeast[['M..Romney']]) > makeComparable(theBeast[['B..Obama..i.']])] = "Rep"

# Part A
colnames(theBeast) = gsub(" ", ".", colnames(theBeast))
theBeast$bpercent = (as.numeric(theBeast$Black.or.African.American.alone.Estimate) / theBeast$Total.Population.Estimate)
theBeast$bpercent[is.na(theBeast$bpercent)] = 0
oldcols = c('winner', 'winner2004', 'bpercent', 'Total.Population.Estimate', 'DP03.HC03_VC156', 'DP03.HC01_VC74', 'DP02.HC03_VC94', 'DP02.HC03_VC134', 'DP02.HC01_VC20', 'DP02.HC03_VC170', 'DP03.HC03_VC13')
readableCols = c("winner","winner2004","PercentageBlack","TotalPopulation","PercentageBelowPovertyLine","EstimatedIncome","PercentageBachDegrees","PercentageForeignBirth","EstimatedAvgHouseholdSize","PercentageNonEnglishSpeakers", "PercentageUnemployed")
for (i in 1:length(oldcols)) {
  colnames(theBeast) = gsub(oldcols[i], readableCols[i], colnames(theBeast))
}
partData = theBeast[readableCols]

#winner in 2004, percent african american, total population, percent familes below poverty line in last 12 months, estimated income, bacholors or higher education, household size
form = "winner ~ "
for(name in readableCols[-1])
  form = paste(form, name, sep="+")
form = as.formula(gsub("~ +", "~ ", form))
partitions = rpart(form, data = partData)

# Plot

png(file="rpartplot.png",width=1000,height=700)
par(xpd=T, cex.lab=0.8, cex.main=0.9, cex.axis=0.8)
plot(partitions, margin=0.01)
text(partitions, splits=T, cex=0.75)
title("RPart Plot")
dev.off()

# Part B - percent of blacks, percent below poverty line
plotMiscalc = function(pair, color){
  testSet = theBeast[c('X', 'Y', pair[1], pair[2])]
  testSet$X = as.numeric(as.character(testSet$X))/1000000
  testSet$Y = as.numeric(as.character(testSet$Y))/1000000
  cl = factor(theBeast$winner2004)
  miscalcs = sapply(1:50, function(kval){ knnprob = knn.cv(testSet, cl, k=kval, prob=T, use.all=F); c(kval, length(which(cl != knnprob)) / length(knnprob))})
  plot(miscalcs[1,], miscalcs[2,], col=color, xlim=c(1, 50), ylim=c(0.1, .25), pch=15, xlab="", ylab="")
}
featurePairs = list(c("PercentageBlack","PercentageBelowPovertyLine"),c("PercentageBelowPovertyLine","EstimatedIncome"),c("PercentageBlack","PercentageBachDegrees"),c("PercentageBachDegrees","PercentageNonEnglishSpeakers"),c("PercentageNonEnglishSpeakers","PercentageForeignBirth"),c("PercentageBachDegrees","PercentageForeignBirth"),c("EstimatedIncome","PercentageForeignBirth"))
colors = brewer.pal(9,"Set1")

png(file="miscalcs.png",width=1000,height=700)
for(i in 1:length(featurePairs)){
  plotMiscalc(featurePairs[[i]], colors[i])
  par(new=TRUE, cex.lab=0.8, cex.main=0.9, cex.axis=0.8)
}
title(main="Proportion of Misclassified Counties", xlab="Values of k", ylab="Misclassification Rate")
legend(x=30, y=.25, fill=colors, cex=1, legend=sapply(featurePairs, function(pair) paste(pair[1], pair[2])), bg=rgb(.5,.5,.5,.1))
dev.off()

################################### STEP 4 ######################################

#Fancy Plot
require(maps)
fplot = merge(theBeast[c('X', 'Y', 'M..Romney', 'B..Obama..i.', 'County')], results2004[c('bushVote', 'kerryVote', 'County')])
fplot$X = as.numeric(as.character(fplot$X))/1000000
fplot$Y = as.numeric(as.character(fplot$Y))/1000000

voterShift = 2 * ((makeComparable(fplot[,4])/100) - (fplot[,6] / (fplot[,6] + fplot[,7])))
repub = which(voterShift > 0)
democ = which(voterShift < 0)

png(file="fancyplot.png",width=1000,height=700)

map("state", fill=T, col=rgb(0.3,0.5,0.1,0.2))

# 0.5 constant is to make all visible
title("Shift in Election Results from 2004 to 2010")
arrows(fplot$X[repub], fplot$Y[repub], fplot$X[repub] + voterShift[repub] + 0.3, fplot$Y[repub] + voterShift[repub] + 0.3, length = .05, col = "red")
arrows(fplot$X[democ], fplot$Y[democ], fplot$X[democ] + voterShift[democ] - 0.3, fplot$Y[democ] + voterShift[democ] - 0.3, length = .05, col = "blue")
dev.off()


### Plot 2

par(cex.lab=0.8, cex.main=0.9, cex.axis=0.8, family="Courier New")
normalize = function(x, lst) (x-min(lst))/((max(lst)-min(lst)))
normed = as.data.frame(sapply(theBeast[readableCols[3:11]], function(feature) sapply(feature, function(x, lst) normalize(x, lst=feature))))
relationship = function(features) {
  plot(lowess(features[1][features[1]>0], makeComparable(theBeast$B..Obama..i.)[features[1]>0], f=.001, iter=3, delta=0.01), pch=20, cex=0.8, col=colors[1],main="County Feature and Percentage Voting Democrat ", xlab="Feature (Normalized to [0,1])", ylab="Percent Democrat", xlim=c(0,1.2))
  for (i in 2:length(features))
    points(lowess(features[i][features[i]>0], makeComparable(theBeast$B..Obama..i.)[features[i]>0], f=.001, iter=3, delta=0.01), pch=20,cex=0.8, col=colors[i])
  legend(.95, 80, legend=readableCols[3:11], title="Feature of Counties", bg=rgb(.5,.5,.5,.1), col=colors, pch=20)
}
png(file="partyvsfeat.png", width=1000,height=700)
relationship(normed)
dev.off()


### Plot 3

plot3Data = merge(theBeast[c('X', 'Y', 'M..Romney', 'B..Obama..i.', 'County', readableCols)], results2004[c('bushVote', 'kerryVote', 'County')])
voterShift = abs((makeComparable(plot3Data$M..Romney)/100) - (plot3Data$bushVote / (plot3Data$bushVote + plot3Data$kerryVote)))
magLarge = which(voterShift > .10)
magMed = intersect(which(voterShift <= .10), which(voterShift >= .05))
magSmall = which(voterShift < .05)

# split into small, med, large changes
largeData = plot3Data[magLarge,]
largeBars = sapply(readableCols[-(1:2)], function(colname)var(largeData[[colname]]))
medData = plot3Data[magMed,]
medBars = sapply(readableCols[-(1:2)], function(colname)var(medData[[colname]]))
smallData = plot3Data[magSmall,]
smallBars = sapply(readableCols[-(1:2)], function(colname)var(smallData[[colname]]))
featlist = c("PercentageBlack", "TotalPopulation", "PercentageBelowPovertyLine", "EstimatedIncome",
             "PercentageBachDegrees", "PercentageForeignBirth", "EstimatedAvgHouseholdSize",
             "PercentageNonEnglishSpeakers", "PercentageUnemployed")

counts = matrix(c(smallBars, medBars, largeBars), ncol=3, byrow=T, dimnames=list(featlist, c("small", "med", "large")))
# plotted on log scale
png(file="varvsmag.png",width=1000,height=700)
par(xpd=T)
barplot(counts, log="y", main="Variability of Features vs Magnitude of Vote Change from 2004 to 2012", xlab="Magnitude of change", col=colors[1:9], beside=TRUE)
legend(x=21, y=1e11, fill=colors[1:9], bg=rgb(.5,.5,.5,.1), cex=1.2, legend=featlist)
dev.off()
