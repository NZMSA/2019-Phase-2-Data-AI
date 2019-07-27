#############################################################################################
############################### Read me #####################################################
#############################################################################################
##In this analysis, we are going to look at how ranking of the items related to other variables.
##We assume that product name/number of reviews/ price(current)/ Discount?/ number of starts/ 
##shipping options and brand name are factors related to the item ranking.
#############################################################################################
##Any questions regarding to the following code please email:
##xwan504@aucklanduni.ac.nz
##Vicky Wang
#############################################################################################
##Load the output data from github page
library(readr)
fileName <- "webOutput.csv"
output <- read_csv(fileName)
############################## Rank By Brand ################################################
##RankByBrand is the variable we are aiming to predict
rankByBrand = ave(output$brand_name==output$brand_name, output$brand_name, FUN=cumsum)
##############################################################################################

############################## Page By Brand #################################################
##Page number is the another variable we could predict
pageNum = output$page
##############################################################################################

##Some of the vairables still needs cleaning
##Let's start with ProductName which is the most important variable which contains additional
##Info that we want extract from.
############################## Product Name ##################################################
##Product Name has a lot of information related to the item
productName = as.vector(unlist(output[,2]))
##From this variable we are aiming to find 
##wordCount, ##material ##color ##phoneType ##year

######################### Word Frequence from productName ####################################
##check overall word frequency in product names
##This word count will also be useful for future visualization
productName2 = gsub("[[:punct:]]", " ", productName)

##Get the word frequency for the product name and sort the table by frequency
wordFreq = as.data.frame(sort(table(unlist(strsplit(productName2, " "))),decreasing = T))
##Delete the first row since is count for space
wordFreq = wordFreq[-1, ]

write.csv(wordFreq, file = "wordFreq.csv")
############################## Word Count By productName ######################################
##Count how many words in the productName
wordCount = NA
wordList = strsplit(productName2, " ")
for (i in 1:length(wordList)) {
  wordCount[i] = length(which(wordList[[i]] != ""))
}
############################## Material Name ##################################################
##Material options from the search bar
materialOpt = c("Silicone","TPU","Rubber","Polycarbonate","Plastic","Faux Leather","Leather")

##Create an empty vecotr to store material type has the same length as the productName
material = NA
material[1:length(productName)] = "NULL"

##Try to match the material options with product name
strings = productName
for(i in 1:length(materialOpt)){
  pattern = materialOpt[i]
  material[which(grepl(toupper(pattern), toupper(strings)) == TRUE)] = materialOpt[i]
}

############################# Color Name #######################################################
##Color options from the search bar
colorlOpt = c("Black", "Purple", "Gold", "Pink", 
              "Red", "Gunmetal", "Crystal Clear", "Matte Black", "Rose Gold", "Flower")

##Create an empty vecotr to store color
color = NA
color[1:length(productName)] = "Null"

##Try to match the material options with product name
strings = productName
for(i in 1:length(colorlOpt)){
  pattern = colorlOpt[i]
  color[which(grepl(toupper(pattern), toupper(strings)) == TRUE)] = colorlOpt[i]
}

############################# Phone Model Type #######################################################
##Extract Phone Type options
modelOpt = c("iPhone 5", "iPhone 5s", "iPhone 6", "iPhone 6s", "iPhone 7",
              "iPhone 8", "iPhone Xs", "iPhone XR", "iPhone SE")

##Create an empty vecotr to store color
model = NA
model[1:length(productName)] = "Null"

##Try to match the material options with product name
strings = productName
for(i in 1:length(modelOpt)){
  pattern = modelOpt[i]
  model[which(grepl(toupper(pattern), toupper(strings)) == TRUE)] = modelOpt[i]
}

#################################### Year #############################################################
##The latest year value
yearOpt = c("2013","2014","2015","2016","2017","2018","2019")

##Create an empty vecotr to store color
year = NA
year[1:length(productName)] = NA

##Try to match the material options with product name
strings = productName
for(i in 1:length(yearOpt)){
  pattern = yearOpt[i]
  year[which(grepl(pattern, strings) == TRUE)] = as.numeric(yearOpt[i])
}
########################## End of regExp on productName ###############################################

#################################### current Price ####################################################
##Get the current price and convert it to numeric values
##delete $ and then convert to numeric
currentPrice = gsub("[[:punct:]]", "", output$current_price)
currentPrice = gsub("NA", NA, currentPrice)
currentPrice = as.numeric(currentPrice) * 0.01

##Missing rate for currentPrice
length(which(is.na(currentPrice)==TRUE))/length(output$X1) * 100
################################## End of Current Price ###############################################

#################################### Number of Reviews ####################################################
##Get Number of Reviews and convert it to numeric values
##delete $ and then convert to numeric
NumOfReview = gsub("[[:punct:]]", "", output$number_of_reviews)
NumOfReview = gsub("NA", NA, NumOfReview)
NumOfReview = as.numeric(NumOfReview)

##NA rate for NumOfReview
length(which(is.na(NumOfReview)==TRUE))/length(output$X1) * 100
################################## End of Number of Reviews ###############################################

#################################### Number of Stars ####################################################
##Get Number of Reviews and convert it to numeric values
##delete $ and then convert to numeric
NumOfStars = sub(" .*", "", output$number_of_stars)
NumOfStars = gsub("NA", NA, NumOfStars)
NumOfStars = as.numeric(NumOfStars)

##NA rate for NumOfStars
length(which(is.na(NumOfStars)==TRUE))/length(output$X1) * 100

##Compare NA for NumOfStars and NumOfReview They are actually from same obs.
################################## End of Number of Stars ######################################################

#################################### End of Product Shipping ###################################################
##We are going to create a separate table to store shipOpt and shipCode so we can refer the table in PowerBI
shipOpt = unique(output$product_shipping)
shipCode = c("Type A", "Type B","Type C","Type D","Type E","Type F","Type G")
shipping = data.frame("shipOpt" = shipOpt,"shipCode" = shipCode)

##Lets convert original variable to shipCode
productShip = unlist(output$product_shipping)
productShipCode = NA
for(i in 1:length(shipOpt))  
{productShipCode[which(productShip == shipOpt[i])] = shipCode[i]}

write.csv(shipping, file = "shippment.csv")
################################## End of Product Shipping  ######################################################

################################## Final Dataset #################################################################
iphoneCase = data.frame("brandName" = output$brand_name ,"currentPrice" = currentPrice,"discount" = output$discounted,
                       "wordCount" = wordCount, "material" = material, "color" = color, "model" = model, "year" = year,
                       "numReview" = NumOfReview, "numStars" = NumOfStars,"productShipCode" = productShipCode,
                       "pageRank" = rankByBrand, "pageNum"= pageNum)

iphoneCase = data.frame("brandName" = output$brand_name ,"currentPrice" = currentPrice,"discount" = output$discounted,
                        "wordCount" = wordCount, "material" = material, "color" = color, "model" = model,
                        "numReview" = NumOfReview, "numStars" = NumOfStars,"productShipCode" = productShipCode,
                        "pageRank" = rankByBrand, "pageNum"= pageNum)


iphoneCaseBI = data.frame("productName" = output$product_name, "brandName" = output$brand_name ,"currentPrice" = currentPrice,"discount" = output$discounted,
                        "wordCount" = wordCount, "material" = material, "color" = color, "model" = model,
                        "numReview" = NumOfReview, "numStars" = NumOfStars,"productShipCode" = productShipCode,
                        "pageRank" = rankByBrand, "pageNum"= pageNum)

##Output the final dataset
write.csv(iphoneCase, file = "iphoneCase.csv")

write.csv(iphoneCaseBI, file = "iphoneCaseBI.csv")
###################################################################################################################

############################### Don't forget to check your data ############################### 
############################### Pairplots ############################### 
############################### Correlation Plots ###############################
numIphone <- iphoneCase[,c(2,4,8,9,11)]
numIphone2 <-na.omit(numIphone)

res <- cor(numIphone2)

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

############################### Model Selection ############################### 
library(leaps)
subset <- regsubsets(pageRank ~. , data = numIphone2, method = "forward")
summary(subset)







