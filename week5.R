# week 5 lab 

# import dataset
library("readxl")
df = read_excel("week5.xlsx")

head(df)

# fixing missing values using is.na
df$Height_inches[which(is.na(df$Height_inches))] <-mean(df$Height_inches,na.rm = TRUE)
mean(df$Height_inches)


df$Age[which(is.na(df$Age))] <-mean(df$Age,na.rm = TRUE)
mean(df$Age)


sum(df$Height_inches)

head(df)

# fixing missing values
df$gender[which(is.na(df$gender))] <- 'female'
df$`Hair_color`[which(is.na(df$`Hair_color`))] <- 'brown'
df$`Eye_Color`[which(is.na(df$`Eye_Color`))] <- 'brown'


#   all of the pie charts


# pie chart gender
gender_lbls <- c("male","female")
slices <- c(13,17)
pie(slices,labels = lbls, col = rainbow(length(gender_lbls)),main="Pie Chart of Gender")

# pie chart hair
hair_lbls <- c("brown","blond","red")
slices <- c(15,12,3)
pie(slices, labels = lbls,col = rainbow(length(hair_lbls)),main = "Pie Chart of Hair Color")

# pie chart eye
eye_lbls <- c ("brown","green","blue")
slices <- c(13,9,8)
pie(slices, labels = lbls,col = rainbow(length(eye_lbls)),main = "Pie Chart of Eye Color")


# line graoh 
ggplot(df, aes(x = df$Height_inches, y = df$Age, type = "o")) +
  geom_line()+
  geom_point()



# regressor 
library(caTools)
set.seed(123)
split = sample.split(df$Height_inches, SplitRatio = 2/3)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Height_inches ~ Age, 
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Visualizing the Training set results
library(ggplot2)

ggplot(test_set, aes(x = test_set$Height_inches, y =  predict(regressor, newdata = test_set))) +
  geom_point() +
  stat_smooth()

print(y_pred)
print(test_set)    

#     Part 2 
# this is very unclear, I don't understand what you are asking
df2$Style[df2$Style == "bsbby"] <- "baggy"

df2 = read_excel("week5part2.xlsx")
head(df2)
ggplot(df2, aes(x = df2$Jean_Length, y = df2$Customer_Height)) +
  geom_line()+
  geom_point()
