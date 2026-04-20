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
# Does this data look normally distributed within each factor level? Include any plots you make.
set <- split(iris$Sepal.Length, iris$Species)  #this splits the Sepal.Length variable into 3 arrays based on the levels of Species.
# Now we can look at the distribution of each array. We can do this with histograms
par(mfrow=c(1,3))  #this sets up the plotting area to have 1 row and 3 columns, so we can see all 3 histograms at once
for(i in 1:3){
  hist(set[[i]], main = paste("Histogram of Sepal Length for", names(set)[i]), xlab = "Sepal Length", col = "lightblue")
}
#c) Compute each levels mean sepal length.  I did this below for you.
group_means <- tapply(iris$Sepal.Length, iris$Species, mean)  #computes the mean of Sepal.Length for each species.

#d) Compute the standard deviation of each levels sepal length.  Is it reasonable to assume these are the same?
sd_values <- tapply(iris$Sepal.Length, iris$Species, sd)  #computes the standard deviation of Sepal.Length for each species.
print(sd_values)
# Note:You will find that the sd's vary a bit! So maybe there is reason to assume that ANOVA should not be conducted on 
# this data set.  We will bear this in mind and press on! 
 
#e) This part and part f) is how the book suggests testing normality of the data. This will be helpful for later problems
 #Compute residuals: observation minus its group mean
 residuals <- iris$Sepal.Length - group_means[iris$Species]  #the group_means[iris$Species] ensures the correct group mean with each observation.
#f) Create a normal probability plot (Q-Q plot) of the residuals
 qqnorm(residuals)
 qqline(residuals, col = "red")
 #Comment on whether or not a normality assumption is reasonable from the plot.
# The points in the Q-Q plot appear to follow a straight line, which suggests that the residuals are approximately normally distributed. However, there may be some slight deviations from normality, especially in the tails of the distribution. Overall, it seems reasonable to assume normality for this dataset, but it is always good to keep in mind that ANOVA is somewhat robust to violations of normality, especially with larger sample sizes. 


 #The ANOVA Test is done with the following code:
 # result<- aov(response ~ factor, data = mydata)
 # response= numeric observation name. ex) Sepal.Length, Petal.Width, etc.
 #This is what you’re testing to see if it differs across levels.
 #factor – This is your categorical factor name (a factor in R).
 #data – The data frame that contains your variables.
 
 #g) Conduct a One-way ANOVA to determine if Sepal.Length differs by Species.
  anova_result <- aov(Sepal.Length ~ Species, data = iris)
  print(summary(anova_result))
 
 #h) What does the data suggest that at an alpha=0.05 significance?
 # The ANOVA test results show that the p-value is less than 0.05, which indicates that there is a statistically significant difference in sepal length among the three species of iris. This means that at least one of the species has a different mean sepal length compared to the others. To determine which specific groups differ from each other, we would need to conduct a post-hoc test, such as Tukey's HSD test.
 
 #2) Now investigate the Plant Growth dataset (also in R). These are the results from an experiment
 #to compare yields (as measured by dried weight of plants) obtained under a control and two different treatment conditions.
 #The levels of group (factor name) are ‘ctrl’, ‘trt1’, and ‘trt2’.
 
 # Load the dataset
 data("PlantGrowth")
 
 # Repeat steps a)-h) with this new data set comparing whether the 'weight' is effected by the different treatments.
 
 #3) Another famous dataset is Student's Sleep data.  Essentially they drugged 10 people with 
 # two different drugs and compared the hours of sleep to their standard amount.
 
 #a) If we do One-Factor ANOVA on this, we will violate an assumption that we need.  What is it?
 
 #b) Ignore that assumption and run the ANOVA test. Done below: 
 #anova_sleep <- aov(extra ~ group, data = sleep)
 #print(summary(anova_sleep))
 
 #c) If this was a valid ANOVA test, what does the data suggest that at an alpha=0.05 significance?
 
 
 #4) EXTRA CREDIT:  Find an interesting dataset with different factors and conduct your own ANOVA Test!