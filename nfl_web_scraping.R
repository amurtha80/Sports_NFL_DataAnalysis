#Set variables for the target webpages to extract the following data;
#2014 NFL Running Back Statistics

  ##Setting up the variables for the static portion of the 3 url's
u0 <- 'http://www.nfl.com/stats/categorystats?tabSeq=1&'
u1 <- 'season=2014&seasonType=REG&experience=&Submit=Go&archive=true&conference=null&'
u2 <- 'statisticPositionCategory=RUNNING_BACK&qualified=true'

  ##Setting up the variables for the actual full url's for each
  ##of the three web pages
url1 <- paste(u0,u1,"d-447263-p=1&",u2, sep="")
url2 <- paste(u0,u1,"d-447263-p=2&",u2, sep="")
url3 <- paste(u0,u1,"d-447263-p=3&",u2 ,sep="")

#Scan each table from each target web page

  ##Import the target table into a data frame
table1 <- readHTMLTable(url1, header=FALSE)
table2 <- readHTMLTable(url2, header=FALSE)
table3 <- readHTMLTable(url3, header=FALSE)

  ##Extract the "result" information form the HTML Code
body1 <- table1$result
body2 <- table2$result
body3 <- table3$result

  ##Append the data from the previously created variables into one 
  ##data frame
nflData <- rbind(body1, body2, body3)

#Clean and shape data for display

nflData <- transform(nflData, V1 = as.numeric(V1))
nflData <- transform(nflData, V7 = as.numeric(gsub(",","",V7, fixed=TRUE)))

  ##Extract header names from data frame
names(nflData)

  ##Set variable names for data frame header
header <- c('Rk','Player','Team','Pos','Att','Att_G','Yds','Avg','Yds_G','TD','Lng','1st','1st%','20+','40+','FUM')

  ##Replace header names
names(nflData) <- header
is.factor(nflData$Rk)

  ##Order the data in ascending order by Yards and only reutrn the first
  ##25 records
datPlot <- nflData[order(nflData$Yds,decreasing=TRUE)[1:25],]

#Plot Results

  ##Plot the results from the data frame datPlot
ggplot(datPlot, aes(x= reorder(Player,Yds), y = Yds)) + 
  geom_point(stat = "identity") + theme_bw() + coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Yards") + xlab("Player")

#Summarize yards by team

  ##Roll up total yards by team
teamYds <- aggregate(nflData$Yds, by=list(Team=nflData$Team), FUN=sum)

  ##Sort the data in descending order by Yards
teamyds_Sort <- teamYds[order(teamYds$x, decreasing=TRUE)[1:32],]
