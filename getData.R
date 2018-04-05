# Get the data.
myurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSV4O64FOQthFtMQGxa9Z6bAjQ-gGG-a26PmiSVVrAKiL5O0aY-z37rwwMejueycckn8U3x9DQfG-If/pub?gid=915553877&single=true&output=csv"
x <- read.csv(url(myurl), header=TRUE)

# Set up plotting parameters.
thismin <- 1
thismax <- 5
thisstep <- 0.1
dotcex <- 1.5

# Labels.
titles <- c("Research & Preparation", "Technical Literacy",
            "Analytical Skills", "Professional Presence",
            "Interpersonal Skills", "Communication Skills",
            "Leadership Potential", "Cultural Fit")
vars <- names(x)[-which(names(x) %in% c("Timestamp","IntName", "IntComp","industry","name","Invite2nd","comments","password"))]
industries <- sort(unique(x$industry))

# Compute score averages by industry.
indMeans <- aggregate(x[,vars], list(x$industry), mean, na.rm=T)
indMeans[,-1] <- round(indMeans[,-1],digits=1)