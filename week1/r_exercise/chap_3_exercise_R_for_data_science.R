library(tidyverse)


## Section 3.3.1, exercises 1, 2, and 3


# 1. What’s gone wrong with this code? Why are the points not blue?

#Because color was inside aes(), move color = "blue" outside of aes() would fix it.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")


# 2. Which variables in mpg are categorical? Which variables are continuous? 
# (Hint: type ?mpg to read the documentation for the dataset). How can you 
# see this information when you run mpg?

str(mpg)
summary(mpg) # when statistical values like min, max are printed, that means 
# the variable are continuous.

# 3. Map a continuous variable to color, size, and shape. 
# How do these aesthetics behave differently for categorical vs. continuous variables

# when color is applied to a continuous variable, the points are color by the values of the variable. 
# in this case, smaller cty value get darker color, vice versa.  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty))


# when color is applied to a categorical variable, the points are color by the category. 
# points that have the same category has the same color

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = model))


# Section 3.5.1, exercises 1 and 4

# 1. What happens if you facet on a continuous variable?

# it would create a plot for every distinct value the variable has.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))+
  facet_wrap(~cty)


# 4. What are the advantages to using faceting instead of the 
# colour aesthetic? What are the disadvantages? How might the 
# balance change if you had a larger dataset?

# The advantage is that faceting allows us to focus on specific category we may be interested in.
# The disadvantage is that we cannot see the overall distribution of that category in the dataset. 
# The balance is do both if needed. 

ggplot(data = mpg) + 
geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)


# Section 3.6.1, exercises 5 and 6

# Will these two graphs look different? Why/why not?

# Yes, because it is based on the same inputs. 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()


ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))



# 6. Recreate the R code necessary to generate the following graphs.

#graph 1

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(se = F)

# graph 2

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(aes(group = drv), se = F)

# graph 3

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(aes(group = drv, linetype = drv), se = F, linetype = 1)


# graph 4 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color = drv)) + 
  geom_smooth(se = F)

# grpah 5


ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(aes(group = drv, linetype = drv), se = F, color = "blue")

# graph 6

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() 
