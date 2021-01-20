# import data 
data = read.csv('Salary_Data.csv')

# Splitting dataset into trains and tests

set.seed(123)
split = sample.split(data$Salary, SplitRatio = 2/3) #split(y, splitratio)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

# training (fit) data 
# lm is fit in R
# Salary ~ YearsExperience is salary is proportional (corresponding) of experience 
regressor = lm(formula = Salary ~ YearsExperience, data = train) 
summary(regressor)

# predicting independent values (y):salary 
# from test values of dependent values (x):years
# 
y_pred = predict(regressor, newdata = test)

# plotting the train data
ggplot() +
  geom_point(aes(x = train$YearsExperience, y = train$Salary), 
             color = 'red') +
  geom_line(aes(x = train$YearsExperience, y = predict(regressor, newdata = train)), 
            color = 'blue') +
  ggtitle('Years vs Experience (Training)') + 
  xlab('Expereince') + 
  ylab('Salary')

# plotting the test data
ggplot() +
  geom_point(aes(x = test$YearsExperience, y = test$Salary), 
             color = 'red') +
  geom_line(aes(x = train$YearsExperience, y = predict(regressor, newdata = train)), 
            color = 'blue') +
  ggtitle('Years vs Experience (Test)') + 
  xlab('Expereince') + 
  ylab('Salary')

# geom_point is scatter 
# aes is Aesthetic mappings describe how variables 
# in the data are mapped to visual properties (aesthetics) of geoms

# why don't use y_pred becasue y_pred is used for test predictions 
# and the regression line is created by training data 

