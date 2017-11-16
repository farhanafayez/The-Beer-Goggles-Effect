DF <- read.table('BeerGoggles.txt', header = TRUE)

# Null hypothesis: Gender and alcohol does not affect the case of attractiveness
# Gender:  H0: α1 = α2
# Alcohol: H0:β1= β3 = β3 whiich corelates to placebo, 2 pints and 4 pints
# There is no interaction between the two factors aka alcohol and gender
Attr.aov <- aov(Attractiveness~Alcohol*Gender,data=DF)
print(summary(Attr.aov))

# Extracting just the p-value from aov
summary(Attr.aov)[[1]][["Pr(>F)"]]

# From the small p-value
# (typically ≤ 0.05) indicates strong evidence 
# against the null hypothesis
# As we can say in the case of Alcohol, so we can say alcohol
# does contribute to the ability to find someone attractive

# Creating a box plot Gender and Attractiveness
with(DF,plot(Gender,Attractiveness, col=c('hotpink','blue')))
# We can see there is a high variance among the Male 

#Creating a box plot corerelating Alcohol and Attractiveness
with(DF,plot(Alcohol,Attractiveness,
             col=c('red1','red2','red3')))
# We can see that after 4 pints all the individuals aka both genders
# have higher possibility of getting a less attractive mate

par(mfrow=c(1,2))
with(subset(DF,Gender='Female'),
     plot(Alcohol,Attractiveness,
          col=c('hotpink1','hotpink2','hotpink3')))

with(subset(DF,Gender='Male'),
     plot(Alcohol,Attractiveness,
          col=c('blue1','blue2','blue3')))

par(mfrow=c(1,1))
with(DF,plot(Alcohol:Gender,Attractiveness,
             col=c('red1','red2','red3')))
# Showing interations with colon
# Statiscally, when males drink too much, 
# in this case 4 pints, they mate with unattractive mates
# Would be cool if we could change the order of the plot
# subset?