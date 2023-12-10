library(ggplot2)
install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)

library(reshape2)

college_data = read.csv("/Users/dan/Desktop/Stats202a/final_project/college_data.csv")
match_data = read.csv("/Users/dan/Desktop/Stats202a/final_project/college_match_data.csv")
college_data =  na.omit(college_data)

gender = college_data$gender
age = college_data$age
field_cd = college_data$field_cd
race = unclass(college_data$race)
loneliness = college_data$date


hist(loneliness,probability = TRUE,breaks = 6)
lonely_density = density(loneliness,bw = "nrd0")
lines(lonely_density,col = "green")


system("R CMD SHLIB kde.c")
dyn.load("kde.so")
n = as.integer(length(loneliness))
m = as.integer(1000)
g = as.double(c(seq(min(loneliness), max(loneliness), length = 1000)))
x = as.double(loneliness)
y = as.double(c(numeric(length=1000)))
re = .C("kde",n,m,g,x,y)
lines(re[[3]],re[[5]],col = "blue")
legend("topright", legend = c("C-function KDE", "Density function"), col = c("blue", "green"), lty = c(1, 1), lwd = 2)

lonely_age_lm = lm(loneliness ~ age)
plot(age,loneliness,pch = 16, cex = 1.3, col = "blue", main = "loneliness plotted against age", xlab = "age in years", ylab = "loneliness")
abline(lm(loneliness ~ age),col = "red")
legend("topright",legend = c("regression line"), col = "red", lty = c(1, 1), lwd = 2)
legend("bottomleft", legend = c("intercept: 10.1970", "dependent var: -0.2818" ))

numeric_ethnicity_dict <- as.list(c(
  "African",
  "European",
  "Hispanic",
  "Asian",
  "Indigenous",
  "Other"
))

race = factor(unlist(numeric_ethnicity_dict[race]))
loneliness_race = lm(loneliness ~ race)
bp <- boxplot(loneliness ~ race, col = "lightblue", main = "Boxplot off loneliness by ethnicity", xlab = "ethnicity", ylab = "Loneliness")


keys <- as.character(1:18)  # Convert integers 1 to 18 to strings
values <- c("Law", "Math", "Psychologist", 
            "Bio Tech", 
            "Engineering", "English", 
            "History", "Finance", 
            "Academia", "Phys Science",  
            "Political Science", "Film",
            "Social Work", "Unemployed",
            "Fine Arts", "Languages", 
            "Architecture", "Other")

occupation <- setNames(values, keys)

field_cd = as.character(field_cd)
table_occ = table(unname(occupation[field_cd]))



barplot(table_occ,col = "lightgreen",main = "Barplot of occupations",ylab = "Count")
field_cd = factor(unlist(occupation[field_cd]))
bp <- boxplot(loneliness ~ field_cd, col = "lightgreen", main = "Boxplot off loneliness by occupation", xlab = "occupation", ylab = "Loneliness")


keys = as.character(0:1)
values = c("Female", "Male")
genders = setNames(values,keys)

gender = as.character(gender)
barplot(table(unname(genders[gender])),col = "pink",main = "Barplot of Gender",ylab = "Count")
gender = factor(unlist(genders[gender]))
bp <- boxplot(loneliness ~ gender, col = "pink", main = "Boxplot off loneliness by gender", xlab = "gender", ylab = "Loneliness")

model_data <- data.frame(
  scale(age),
  model.matrix(~ gender+field_cd +race -1, contrasts.arg=list(gender=diag(nlevels(gender)), 
                                                             field_cd=diag(nlevels(field_cd)),
                                                             race = diag(nlevels(race)))))
model_data_fe <- data.frame(
  scale(age),
  model.matrix(~ gender+field_cd +race -1))

features = c("scale.age.","genderFemale","genderMale","field_cdBio.Tech","field_cdEngineering","field_cdEnglish","field_cdFinance",
             "field_cdHistory","field_cdLanguages","field_cdLaw","field_cdPsychologist","field_cdSocial.Work",
             "raceAfrican","raceAsian","raceEuropean","raceHispanic","raceOther")

names(model_data) <- features



cor_matrix <- round(cor(model_data),2)

# Find highly correlated pairs (e.g., with correlation coefficient greater than 0.7)
highly_correlated_pairs <- which(cor_matrix > 0.7 & cor_matrix < 1, arr.ind = TRUE)

# Display the pairs
print(highly_correlated_pairs)


Alec_Chan_age = 22 # 22 years old

Alec_Chan_data = data.frame(
  age = scale(Alec_Chan_age, center = 20.61111, scale = 1.115602),
  genderMale = 1, #is a male
  raceAsian = 1, #is Asian
  field_cdEngineering = 1 #is an engineering major
)

features <- setdiff(names(model_data), names(Alec_Chan_data))
Alec_Chan_data[, features] <- 0

Alec_Chan_data = Alec_Chan_data[names(model_data)]

model = lm(loneliness ~ . , data = model_data)


loneliness_pred = predict(model, Alec_Chan_data)

threshold <- 0.5  # Adjust this threshold as needed

# Find the positions (rows and columns) where the correlation exceeds the threshold
high_cor_indices <- which(cor_matrix > threshold & cor_matrix < 1, arr.ind = TRUE)

# Get variable names
variable_names <- colnames(cor_matrix)

# Extract variable pairs with high correlation
high_cor_variable_pairs <- data.frame(
  Variable1 = variable_names[high_cor_indices[, 1]],
  Variable2 = variable_names[high_cor_indices[, 2]],
  Correlation = cor_matrix[high_cor_indices]
)

# Print the variable pairs with high correlation
print(high_cor_variable_pairs)
# Fill predicted values using regression model


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cor_matrix)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
geom_tile(color = "white")+scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                                midpoint = 0, limit = c(-1,1), space = "Lab", 
                                                name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,size = 12, hjust = 1))+
  coord_fixed()

cormat = cor_matrix

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))





