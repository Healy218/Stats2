# This will serve as both your homework this week as well as a lab. 
################################################################################
# 1)Fisher's 1936 iris dataset is very famous. It is used a lot in Machine Learning
# It even has a wiki page take a look at it. This data set is also built into R.
# This dataset consists of 3 types of iris species: "setsosa", "versicolor", and
# "virginica".  The species type will be our factor, so there are 3 levels. 

# Load the dataset
data(iris)

#a) In the console below look at the dataset by typing: head(iris). This is helpful for 
# understanding what the data looks like.
head(iris)

#b)We will first be considering the sepal length of each species. 
# Generate 3 arrays of sepal length observations (one for each species).
# Does this data look normally distributed within each factor level? 
# They seem roughly normal in the historgrams. 
# Include any plots you make.
set <- split(iris$Sepal.Length, iris$Species)  #this splits the Sepal.Length variable into 3 arrays based on the levels of Species.
# Now we can look at the distribution of each array. We can do this with histograms
{
par(mfrow=c(1,3))  #this sets up the plotting area to have 1 row and 3 columns, so we can see all 3 histograms at once
for(i in 1:3){
  hist(set[[i]], main = paste("Histogram of Sepal Length for", names(set)[i]), 
  xlab = "Sepal Length", col = "lightblue")
}
 par(mfrow=c(1,1))  #reset the plotting area to default
}
#### ======== Make sure to have full screen for all the historgrams to show up! ======== ####
#c) Compute each levels mean sepal length.  I did this below for you.
group_means <- tapply(iris$Sepal.Length, iris$Species, mean)  #computes the mean of Sepal.Length for each species.

#d) Compute the standard deviation of each levels sepal length.  Is it reasonable to assume these are the same?
sd_values <- tapply(iris$Sepal.Length, iris$Species, sd)  #computes the standard deviation of Sepal.Length for each species.
print(sd_values)
#  setosa versicolor  virginica 
# 0.3524897  0.5161711  0.6358796 
# Note:You will find that the sd's vary a bit! So maybe there is reason to assume that ANOVA should not be conducted on 
# this data set.  We will bear this in mind and press on! 
 
#e) This part and part f) is how the book suggests testing normality of the data. This will be helpful for later problems
 #Compute residuals: observation minus its group mean
 residuals <- iris$Sepal.Length - group_means[iris$Species]  #the group_means[iris$Species] ensures the correct group mean with each observation.
#f) Create a normal probability plot (Q-Q plot) of the residuals
 qqnorm(residuals, main = "Q-Q Plot of Residuals")
 qqline(residuals, col = "red")
 #Comment on whether or not a normality assumption is reasonable from the plot.
 # The points in the Q-Q plot appear to follow a straight line, which suggests that the residuals are approximately normally distributed. 
 # It's fine to assume normality for the ANOVA test, but ANOVA doesn't care too much when the sample sizes are this large. 

 #The ANOVA Test is done with the following code:
 #result<- aov(response ~ factor, data = mydata)
 #response = numeric observation name. ex) Sepal.Length, Petal.Width, etc.
 #This is what you’re testing to see if it differs across levels.
 #factor – This is your categorical factor name (a factor in R).
 #data – The data frame that contains your variables.
 
 #g) Conduct a One-way ANOVA to determine if Sepal.Length differs by Species.
  anova_birds <- aov(Sepal.Length ~ Species, data = iris)
  print(summary(anova_birds))
 
 #h) What does the data suggest that at an alpha=0.05 significance?
 # The ANOVA test results show that the p-value is <2e-16 < 0.05 so we would reject the null hypothesis. 
 # This means that at least one of the species has a different mean sepal length compared to the others. 
 
 #2) Now investigate the Plant Growth dataset (also in R). These are the results from an experiment
 #to compare yields (as measured by dried weight of plants) obtained under a control and two different treatment conditions.
 #The levels of group (factor name) are ‘ctrl’, ‘trt1’, and ‘trt2’.
  
 # Load the dataset
 data("PlantGrowth")
 
 # Repeat steps a)-h) with this new data set comparing whether the 'weight' is effected by the different treatments.
 head(PlantGrowth)
 plantTreats <- split(PlantGrowth$weight, PlantGrowth$group)
{
 par(mfrow=c(1,3))
 for(i in 1:3){
   hist(plantTreats[[i]], main = paste("Histogram of Weight for", names(plantTreats)[i]), 
   xlab = "Weight", col = "lightgreen")
 }
  par(mfrow=c(1,1))
}
 group_means_plant <- tapply(PlantGrowth$weight, PlantGrowth$group, mean)
 sd_values_plant <- tapply(PlantGrowth$weight, PlantGrowth$group, sd)
 print(sd_values_plant)
 residuals_plant <- PlantGrowth$weight - group_means_plant[PlantGrowth$group]
 qqnorm(residuals_plant, main = "Q-Q Plot of Residuals for Plant Growth")
 qqline(residuals_plant, col = "red")
 anova_result_plant <- aov(weight ~ group, data = PlantGrowth)
 print(summary(anova_result_plant))
 # The ANOVA test results show that the p-value is 0.0159 < o.05 so we would reject the null hypothesis. 
 # There is a significant difference in plant weight among the different treatment groups. 
 #3) Another famous dataset is Student's Sleep data.  Essentially they drugged 10 people with 
 # two different drugs and compared the hours of sleep to their standard amount.
 
 #a) If we do One-Factor ANOVA on this, we will violate an assumption that we need.  What is it?
 # Since all 10 people were given both drugs, the observations are not independent. Which we need for ANOVA. 
 #b) Ignore that assumption and run the ANOVA test. Done below: 
 anova_sleep <- aov(extra ~ group, data = sleep)
 print(summary(anova_sleep))
 
 #c) If this was a valid ANOVA test, what does the data suggest that at an alpha=0.05 significance?
 # a P of 0.0792 > 0.05, so we would fail to reject the null hypothesis. 

 #4) EXTRA CREDIT:  Find an interesting dataset with different factors and conduct your own ANOVA Test!
# THere is another data set in R called chickwts. This dataset contains the weights of chicks fed with different types of feed. 
# The factor is the type of feed, and the response variable is the weight of the chicks. 
# Let's conduct an ANOVA test to see if there are significant differences in chick weights based on the type of feed.
chickwts <- datasets::chickwts
head(chickwts)
chickTreats <- split(chickwts$weight, chickwts$feed)
{
par(mfrow=c(2,3))
for(i in 1:6){
  hist(chickTreats[[i]], main = paste("Histogram of Weight for", names(chickTreats)[i]), 
  xlab = "Weight", col = "lightcoral")
}
  par(mfrow=c(1,1))
}
group_means_chick <- tapply(chickwts$weight, chickwts$feed, mean)
sd_values_chick <- tapply(chickwts$weight, chickwts$feed, sd)
print(sd_values_chick)
residuals_chick <- chickwts$weight - group_means_chick[chickwts$feed]
qqnorm(residuals_chick, main = "Q-Q Plot of Residuals for Chick Weights")
qqline(residuals_chick, col = "red")
anova_result_chick <- aov(weight ~ feed, data = chickwts)
print(summary(anova_result_chick))
# Our H0 is that there is no difference in chick weights among the different types of feed.
# The ANOVA test results show that the p-value is 5.94e-10 < 0.05 so we would reject the null hypothesis. 
# There is a significant difference in chick weights among the different types of feed.