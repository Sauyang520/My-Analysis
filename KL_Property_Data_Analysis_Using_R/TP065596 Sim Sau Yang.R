#################### GROUP COMPONENT ####################
# Ooi Chong Ming, TP072667
# Yeoh Zi Qing Bryan, TP072717
# Sim Sau Yang, TP065596
# Lim Wen Hann, TP065443

# Assigning Library in use
library(tidyr)
library(stringr)
library(DescTools)
library(ggplot2)

#--------------------------------------Data Import--------------------------------------
filepath = "C:\\Users\\Lenovo\\OneDrive - Asia Pacific University\\Semester 1\\Programming for Data Analytics\\PFDA Assignment\\5. kl_property_data.csv"
kl_property_data = read.csv(filepath, header=TRUE)

#---------------------------Data Cleaning / Pre-Processing------------------------------
# Forming a subset with only variables we need
Filtered_dataset <- subset(kl_property_data,select=c(Location,Price,Rooms,Size,Furnishing))

# Check number NA and "" in each Column
colSums(is.na(Filtered_dataset)) 
colSums(Filtered_dataset== "",na.rm=TRUE) 

#Replace the empty values to NA for columns: "Price", "Rooms", "Size", "Furnishing"
Filtered_dataset$Price[Filtered_dataset$Price ==""] = NA
Filtered_dataset$Rooms[Filtered_dataset$Rooms ==""] = NA
Filtered_dataset$Size[Filtered_dataset$Size ==""] = NA
Filtered_dataset$Furnishing[Filtered_dataset$Furnishing ==""] = NA

# Delete the record when 2 or more NA or 2 or ore empty values in the columns: "Price", "Rooms","Size", "Furnishing"
# >= 2 NA value in a record is considered useless
Filtered_dataset <- Filtered_dataset[rowSums(is.na(Filtered_dataset[, c("Price", "Rooms", "Size", "Furnishing")])
                                             | Filtered_dataset[, c("Price", "Rooms", "Size", "Furnishing")] == "") <= 1, ]

# Remove the location that only contains "Kuala Lumpur"
Filtered_dataset = Filtered_dataset[Filtered_dataset$Location != "Kuala Lumpur",]

# Separating the "Location" column into "Location" and "State"
Filtered_dataset = separate(Filtered_dataset, col=Location, into=c("Location", "State"),sep=", ", extra = "merge")

# Handling the record where the State contains "UOG, Kuala Lumpur" by moving "UOG" to Location Column
# Concatenate "UOG" into the Location column of the targeted row
Filtered_dataset$Location[grepl("UOG", Filtered_dataset$State)] <- 
  paste(Filtered_dataset$Location[grepl("UOG", Filtered_dataset$State)], "UOG", sep = ", ")

# Removing the "UOG" and "," in the State column of the targeted row 
Filtered_dataset$State[grepl("UOG", Filtered_dataset$State)] <- 
  gsub("UOG, ", "", Filtered_dataset$State[grepl("UOG", Filtered_dataset$State)])

# Making all the values in the "Location" and "State" column into Capitalized Form(The first letter is capital and the rest is not)
Filtered_dataset$Location = str_to_title(Filtered_dataset$Location)
Filtered_dataset$State = str_to_title(Filtered_dataset$State)

# Check whether there are places not in Kuala Lumpur
Filtered_dataset[!grepl("Kuala Lumpur", Filtered_dataset$State), ]
# It is known that all locations are in Kuala Lumpur, hence the state column is useless
# Remove the State Column
Filtered_dataset <- subset(Filtered_dataset, select = -State)

# Removing the non-numeric characters in the "Price" column: "RM", " ", ","
Filtered_dataset$Price <- gsub("[^0-9]", "", Filtered_dataset$Price)

# Convert the price column class from character to numeric
class(Filtered_dataset$Price)
Filtered_dataset$Price = as.numeric(Filtered_dataset$Price)
class(Filtered_dataset$Price)

# Add a new column "IsStudio" to define the property is studio
Filtered_dataset$IsStudio <- ifelse(!is.na(Filtered_dataset$Rooms) & Filtered_dataset$Rooms == "Studio", TRUE, FALSE)
# Moving "IsStudio" Column to 4th column
Filtered_dataset <- Filtered_dataset[, c(1:3, ncol(Filtered_dataset), 4:(ncol(Filtered_dataset)-1))]

# Replace the "Studio" in "Rooms" column with 1
Filtered_dataset$Rooms[Filtered_dataset$Rooms == "Studio"] = 1

# Calculate the sum of rooms for entries with the symbol "+" and convert to integer
Filtered_dataset$Rooms <- sapply(strsplit(Filtered_dataset$Rooms, "\\+"), function(x) sum(as.integer(x)))

#Separating the "Size" column into "Type" and "Size"
Filtered_dataset=separate(Filtered_dataset, col=Size, into=c("Type", "Size"),sep=" : ")

# Check Type Column
nlevels(factor(Filtered_dataset$Type))

abnormal_characters <- c(" ", "CornerUnit", "corner", "sq\\.ft.", "sqft", "sq\\.ft","sf", "SF", "ft", "\\(.*"
                         , "approx", ",", "`", "'", "@", "or.*", "/.*", "&#215;", "&#8217;", "&#8221;", "X", "x", "arce")
replacements <- c("", "", "", "", "", "", "", "", "", "", "", "","", "", "", "", "", "", "", "", "\\*", "\\*", "acre")

# Apply replacements to the 'Size' column
Filtered_dataset$Size <- str_replace_all(Filtered_dataset$Size, setNames(replacements, abnormal_characters))

# Replace abnormal values to NA
abnormal_values <- c("", "WP", "WilayahPersekutuan", "unknown", "nil", "NA", "N/A", "Malaysia", "KualaLumpur", "-", 
                     "0", "N", "27**", "2000+")
Filtered_dataset$Size[Filtered_dataset$Size %in% abnormal_values] = NA

# Convert acre, meter, and hectare to sq ft
acres = gsub("acre.*","",Filtered_dataset$Size[grepl("acre", Filtered_dataset$Size)])
meter = gsub("sq\\.m.","", Filtered_dataset$Size[grepl("m", Filtered_dataset$Size)])
hectare = gsub("hectare","", Filtered_dataset$Size[grepl("hectare", Filtered_dataset$Size)])
Filtered_dataset$Size[grepl("acre", Filtered_dataset$Size)] = sapply(acres, function(expr) eval(parse(text = expr)))*43560
Filtered_dataset$Size[grepl("m", Filtered_dataset$Size)] = sapply(meter, function(expr) eval(parse(text = expr)))*10.7639104
Filtered_dataset$Size[grepl("hectare", Filtered_dataset$Size)] = sapply(hectare, function(expr) eval(parse(text = expr)))*107639.104

# Clean unique data
Filtered_dataset$Size[grepl("wt", Filtered_dataset$Size)] = NA
Filtered_dataset$Size = gsub("t", "", Filtered_dataset$Size)
Filtered_dataset$Size = gsub("q\\.\\.", "", Filtered_dataset$Size)

calculate_average <- function(expr, char) {
  values <- as.numeric(strsplit(expr, "-")[[1]])
  mean(values)
}

calculate_average2 <- function(expr, char) {
  values <- as.numeric(strsplit(expr, "~")[[1]])
  mean(values)
}

# Get the average of the value 
Filtered_dataset$Size[grepl("-", Filtered_dataset$Size)] = sapply(Filtered_dataset$Size[grepl("-", Filtered_dataset$Size)], calculate_average)
Filtered_dataset$Size[grepl("~", Filtered_dataset$Size)] = sapply(Filtered_dataset$Size[grepl("~", Filtered_dataset$Size)], calculate_average2)

# Calculate the size by add and multiply with symbol + & *
Filtered_dataset$Size = sapply(Filtered_dataset$Size, function(expr) eval(parse(text = expr)))

# Check Furnishing Column
factor(Filtered_dataset$Furnishing)
nlevels(factor(Filtered_dataset$Furnishing))

#---------------------------------Data Validation---------------------------------------
# Check NA values in every column of the dataset
colSums(is.na(Filtered_dataset))

# Check the details of each column
summary(Filtered_dataset)

# Checking the outliers in "Price","Size" and "Rooms" column
boxplot(Filtered_dataset$Price,ylab = "Price",main = "Price")
boxplot(Filtered_dataset$Size,ylab = "Size",main = "Size")
boxplot(Filtered_dataset$Rooms,ylab = "Rooms",main = "Rooms")

## "Price" column ##
# Calculating the maximum and minimum outliers of the "Price" column
IQR_Price <- 1920000-580000
MaxOut_Price <- 1920000+(1.5*IQR_Price)
MinOut_Price <- 580000-(1.5*IQR_Price) #Since that the MinOut_Price is negative, means that there are no low outliers

# Calculate the mean of the "Price" column
mean_price <- mean(Filtered_dataset$Price,na.rm=TRUE)

# Replace NA values in "Price" column  with the mean
Filtered_dataset$Price[is.na(Filtered_dataset$Price)] <- mean_price

# Check the new 3rd Quartile value
summary(Filtered_dataset$Price)

# Replace upper outliers in the "Price" column with the 3rd quartile
Filtered_dataset$Price[Filtered_dataset$Price >= MaxOut_Price] <- 1900000

# Checking the outliers of the "Price" column again
boxplot(Filtered_dataset$Price,ylab = "Price",main = "Price")

## "Rooms" column ##
# Calculating the maximum and minimum outliers of the "Rooms" column
IQR_Rooms <- 4-3
MaxOut_Rooms <- 4+(1.5*IQR_Rooms)
MinOut_Rooms <- 3-(1.5*IQR_Rooms) 

# Calculate the mean of the "Rooms" column
mean_rooms <- round(mean(Filtered_dataset$Rooms,na.rm = TRUE))

# Replace NA values in "Rooms" column with the mean
Filtered_dataset$Rooms[is.na(Filtered_dataset$Rooms)] <- mean_rooms

#Check the new 1st and 3rd Quartile value
summary(Filtered_dataset$Rooms)

# Replace the upper outliers in the Rooms column with the 3rd quartile of the "Rooms" column
Filtered_dataset$Rooms[Filtered_dataset$Rooms >= MaxOut_Rooms] <- 4

# Replace lower outliers where is not a Studio house with the first quartile
Filtered_dataset$Rooms[Filtered_dataset$Rooms <=MinOut_Rooms & Filtered_dataset$IsStudio == FALSE] <- 3

# Checking the outliers of the "Rooms" column again
boxplot(Filtered_dataset$Rooms,ylab = "Rooms",main = "Rooms")

## "Size" column ##
# Calculating the maximum and minimum outliers of the "Size" column
IQR_Size <- 2378-1018
MaxOut_Size <- 2378+(1.5*IQR_Size)
MinOut_Size <- 1018-(1.5*IQR_Size) #Since that the MinOut_Size is negative, means that there are no low outliers

# Replace values greater than or equal to the maximum outliers with the 3rd quartile
Filtered_dataset$Size[ Filtered_dataset$Size >= MaxOut_Size] <- 2378

# Calculate the mean of the Size column
mean_size <- mean(Filtered_dataset$Size,na.rm=TRUE)

# Replace NA values in "Size" column with the mean
Filtered_dataset$Size[is.na(Filtered_dataset$Size)] <- mean_size

#Checking the outliers of the "Size" column again
boxplot(Filtered_dataset$Size,ylab = "Size",main = "Size")

## Type column ##
# Find the most frequently appearing character value in the Type column
most_frequent_type <- names(which.max(table(Filtered_dataset$Type)))

# Replacing the NA values in the Type column into the most_frequent_type
Filtered_dataset$Type[is.na(Filtered_dataset$Type)] <- most_frequent_type

## Furnishing column ##
# Find the most frequently appearing character value in the Furnishing column
most_frequent_furnish <- names(which.max(table(Filtered_dataset$Furnishing)))

# Replacing the NA values and Unknown value in the Furnishing column into the most_frequent_furnish
Filtered_dataset$Furnishing[is.na(Filtered_dataset$Furnishing) | Filtered_dataset$Furnishing == "Unknown"] <- most_frequent_furnish

#Remove duplicated data
Filtered_dataset <- unique(Filtered_dataset)

# Define new column names
new_column_names <- c("Property_Location", "Property_Price", "Property_Rooms", "Built_Area","Property_Size", "Property_Furnishing")
# Change column names in the dataset
names(Filtered_dataset)[names(Filtered_dataset) %in% c("Location", "Price", "Rooms","Type", "Size", "Furnishing")] <- new_column_names



#################### INDIVIDUAL COMPONENT ####################
# Sim Sau Yang, TP065596
# Hypothesis: 70% of properties in KLCC with price >1M are 
# >3 rooms, 1000-1800 sq.ft., Fully Furnished, Built-Up

# import library
library(dplyr)
library(ggplot2)
library(moments)
library(corrplot)
library(treemapify)
library(plotrix)
library(ggpubr)
library(vcd)
library(VennDiagram)


# Get the data set
df <- Filtered_dataset


# Add columns to categorize the numeric value based on hypothesis Criteria
df <- df %>% 
  mutate(IsKLCC = 
           ifelse(Property_Location == "Klcc", "KLCC", "Non-KLCC")) %>%
  mutate(Price_Category = 
           ifelse(Property_Price > 1000000, ">1M", "<=1M")) %>%
  mutate(Room_Category = 
           ifelse(IsStudio == TRUE, "Studio", ifelse(Property_Rooms <= 3, "<=3", ">3"))) %>%
  mutate(Size_Category = 
           case_when(Property_Size < 1000 ~ "<1000", Property_Size >= 1000 & 
                       Property_Size <= 1800 ~ "1000-1800", Property_Size > 1800 ~ ">1800"))


##### Analysis 3.3.1.1: What is the difference between number of rooms with KL and KLCC? #####

# Perform Wilcoxon-Mann-Whitney test
# Hypothesis 0: No difference in rooms between KL & KLCC
# Hypothesis 1: Difference in rooms between KL & KLCC
wilcox.test(Property_Rooms ~ IsKLCC, data = df)


# Create array for percentage of KLCC & Rooms Category
value = c(
  round(nrow(df[df$IsKLCC == "KLCC" & df$Room_Category == "<=3", ])
        /nrow(df[df$IsKLCC == "KLCC", ])*100,2),
  round(nrow(df[df$IsKLCC == "KLCC" & df$Room_Category == ">3", ])
        /nrow(df[df$IsKLCC == "KLCC", ])*100,2),
  round(nrow(df[df$IsKLCC == "KLCC" & df$Room_Category == "Studio", ])
        /nrow(df[df$IsKLCC == "KLCC", ])*100,2),
  round(nrow(df[df$Room_Category == "<=3", ])/nrow(df)*100,2),
  round(nrow(df[df$Room_Category == ">3", ])/nrow(df)*100,2),
  round(nrow(df[df$Room_Category == "Studio", ])/nrow(df)*100,2))

multi_pie <- data.frame(
  Location = rep(levels(factor(df$IsKLCC)), each = 3),
  Rooms = rep(levels(factor(df$Room_Category)), time = 2),
  Value = value
)

# Reorder the levels of IsKLCC
multi_pie$Location[multi_pie$Location == "Non-KLCC"] <- "All Location"
multi_pie$Location <- factor(multi_pie$Location, 
                             levels = c("All Location", "KLCC"))

# Multilevel Pie Chart: Proportion of Room Category by Location
ggplot(multi_pie, aes(x = Location, y = Value, fill = Rooms)) + 
  geom_col() +scale_x_discrete(limits = c("", "All Location","KLCC")) +
  coord_polar("y")+ geom_text(aes(label = paste0(Value, "%")),
                              position = position_stack(vjust = 0.4), 
                              size = 5) +
  labs(title = "Proportion of Room Category by Location",
       subtitle = "Level 1: All Location | Level 2: KLCC Only",
       fill = "Room Category") + theme_bw()


##### Analysis 3.3.1.2: What is relationship between number of rooms and price? #####
# Exclude Studio
a1.df = df %>% filter(IsStudio == FALSE)

# Spearman's rank correlation coefficient
# Hypothesis 0: No relationship between room & Price
# Hypothesis 1: Relationship between room & Price
cor.test(a1.df$Property_Rooms, 
         a1.df$Property_Price, 
         method = "spearman", 
         exact = FALSE)


# Box Plot: Price for number of rooms room
ggplot(a1.df, 
       aes(y = Property_Price, 
           x = Property_Rooms, 
           group = Property_Rooms)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(title = "Property Price by Number of Rooms",
       x = "Number of Rooms", 
       y = "Property Price", 
       fill = "Number of Rooms") +
  theme_minimal()


# Only KLCC
a1.df.klcc = a1.df[a1.df$IsKLCC == "KLCC", ]

# calculate mean & standard error
mean_prices <- tapply(a1.df.klcc$Property_Price, a1.df.klcc$Property_Rooms, mean)

sds <- tapply(a1.df.klcc$Property_Price, a1.df.klcc$Property_Rooms, sd)
counts <- table(a1.df.klcc$Property_Rooms)
standard_errors <- as.vector(sds / sqrt(counts))

room_prices_df <- data.frame(Rooms = names(mean_prices),
  Mean_Price = as.numeric(mean_prices), Standard_Error = standard_errors)

# Bar Chart: Mean price for number of room in KLCC
ggplot(room_prices_df, aes(x = factor(Rooms), y = Mean_Price, fill = factor(Rooms))) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = Mean_Price - Standard_Error, ymax = Mean_Price + Standard_Error), 
                position = position_dodge(width = 0.8), width = 0.5, color = "red") + 
  labs(title = "Mean Price for Number of Room in KLCC", x = "Rooms", y = "Mean Price") +
  scale_fill_manual(values = c("blue","skyblue", "lightblue", "cyan")) +
  geom_text(aes(label = sprintf("%.2f", Mean_Price)), 
            position = position_dodge(width = 0.8), vjust = -0.7)


summary_table <- a1.df.klcc %>%
  group_by(Room_Category, Price_Category) %>%
  summarise(Counts = n()) %>%
  group_by(Room_Category) %>%
  mutate(Percentage = Counts / sum(Counts) * 100)

# Percentage Stacked Bar Chart: Percentage for Price Category by Room Category
ggplot(summary_table, 
       aes(x = Room_Category, 
           y = Percentage, 
           fill = Price_Category, 
           label = sprintf("%.2f%%", Percentage))) +
  geom_bar(stat = "identity", 
           position = "fill") +
  geom_text(position = position_fill(vjust = 0.5), 
            size = 7) +
  labs(title = "Percentage for Price Category by Room Category in KLCC",
       x = "Price Category", 
       y = "Percentage",
       fill = "Room Category") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1)) +
  theme_minimal()


##### Analysis 3.3.1.3: Which Room Category presents the higher proportion intersection with price and KLCC? #####
# Calculate counts for room category & 
summary_table <- a1.df.klcc %>%
  group_by(Room_Category, Price_Category) %>%
  summarise(Counts = n()) %>%
  mutate(Price_Category = 
           paste0(Price_Category, "\n", Counts, "\n",
                  sprintf("%.2f%%", round(Counts/nrow(a1.df.klcc)*100,2))))

# Treemap: The proportion for Price Category by Room Category
ggplot(summary_table, aes(area = Counts, 
                          fill = Room_Category,
                          label = Price_Category, 
                          subgroup = Room_Category)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(place = "topleft", 
                             colour = "green", size = 25) +
  geom_treemap_text(colour = "white", place = "centre",
                    size = 25, fontface = "bold") +
  scale_fill_manual(values = 
                      c("<=3" = "brown", 
                        ">3" = "purple")) +
  labs(title = "Proportion for Price Category by Room Category in KLCC")


##### Analysis 3.3.2.1: What is the relationship between Size with Price and Location (KLCC/Non-KLCC)? #####
# Select the Size, Price, Rooms Column
size_price_rooms <- df %>% 
  select(Property_Rooms, 
         Property_Price, 
         Property_Size)

correlation_matrix <- 
  cor(size_price_rooms, 
      method="pearson")

# Correlation Plot: Relationship of Rooms, Price, Size
corrplot(correlation_matrix, 
         method = "pie", 
         type = "lower", 
         order = "hclust", 
         tl.cex = 0.7)


# Conduct t.test for Size & KLCC/Non-KLCC
# Hypothesis 0: No difference in Size between KL & KLCC
# Hypothesis 1: Difference in Size between KL & KLCC
t.test(Property_Size ~ IsKLCC, data = a1.df)


# Scatter Plot with Marginal Density Plots: Price for Size by Location
ggscatterhist(
  a1.df, x = "Property_Size", y = "Property_Price",
  color = "IsKLCC", size = 3, alpha = 0.5,
  palette = c("#00AFBB", "#FC4E07"),
  margin.params = list(fill = "IsKLCC", color = "black", size = 0.2)
)

# Grouped Scatter plot: Price by Size by Room Category by Location
ggplot(a1.df, aes(x = Property_Size, y = Property_Price), 
       group_by(Property_Rooms)) +
  geom_point(aes(color=Property_Rooms)) +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(title = "Property Size vs Price by Rooms in KLCC",
       x = "Property Size", y = "Property Price") +
  facet_wrap(~IsKLCC)


##### Analysis 3.3.2.2: What is the distribution of Size? Which size category (<1000, 1000-1800, >1800) has larger proportion? #####
# Price more than 1M
a2.df <- a1.df.klcc %>% filter(Price_Category == ">1M")

# Histogram of Size in KLCC & Price >1M
ggplot(a2.df, aes(x = Property_Size)) +
  geom_histogram(binwidth = 250, 
                 fill = "lightblue", 
                 color = "black", 
                 alpha = 0.6) +
  geom_density(aes(y=250*..count..), colour="red", adjust=3) +
  geom_vline(aes(xintercept = mean(Property_Size)), 
             linetype = "dashed", size = 0.6) +
  labs(title = "Histogram of Size in KLCC & Price > 1M",
       x = "Property Size", 
       y = "Density") +
  theme_minimal()

skewness(a2.df$Property_Size)
kurtosis(a2.df$Property_Size)


# Group the Size Category by Room Category
summary_table <- a2.df %>%
  group_by(Room_Category, Size_Category) %>%
  summarise(Counts = n()) %>%
  mutate(Percentage = round(Counts/nrow(a2.df)*100,2)) %>%
  mutate(Label = paste0(Counts, "\n",sprintf("%.2f%%", Percentage)))
summary_table$Size_Category = 
  factor(summary_table$Size_Category, levels = c(">1800", "1000-1800", "<1000"))

# Calculate row totals percentage
row_totals <- summary_table %>%
  group_by(Size_Category) %>%
  summarise(Counts = sum(Counts), Percentage = sum(Percentage))
# Calculate column totals percentage
col_totals <- summary_table %>%
  group_by(Room_Category) %>%
  summarise(Counts = sum(Counts), Percentage = sum(Percentage))

# Matrix Bubble Chart: Proportion of Size and Room Category in KLCC with Price > 1M
ggplot(summary_table,
       aes(x = Room_Category, y = Size_Category,
           colour = Room_Category, size = Counts)) +
  geom_point() +
  geom_text(aes(label = Label), colour = "black", size = 5) +
  geom_text(data = col_totals, aes(x = Room_Category, y = "Total", 
                                   label = sprintf("%.2f%%", Percentage)),
            colour = "black", size = 4, vjust = -1) +
  geom_text(data = row_totals, aes(x = "Total", y = Size_Category, 
                                   label = sprintf("%.2f%%", Percentage)),
            colour = "black", size = 4, hjust = 1) +
  scale_x_discrete(position = "top") +
  scale_size_continuous(range = c(18, 42)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Proportion of Size & Rooms in KLCC & Price > 1M",
       x = "Room Category", y = "Size Category") +
  theme(legend.position = "none", panel.background = element_blank(),
        panel.grid = element_blank(), axis.ticks = element_blank())


# Calculate percentage in room category <=3
size_count <- table(a2.df$Size_Category[a2.df$Room_Category == "<=3"])
percentage_counts <- 
  round((size_count / sum(size_count)) * 100, 2)
percentage_counts_labels <- 
  paste0(percentage_counts, "%", "\n", paste0(size_count))

# 3D Pie Chart: Percentage for Size Category in Room <=3, Price >1M, in KLCC
pie3D(size_count, labels = percentage_counts_labels, 
      main = "Percentage for Size Category in 
      Room <=3, Price >1M, in KLCC", col = factor(size_count), 
      height = 0.15, radius = 1, theta = 0.7)
legend("right", legend = names(size_count), 
       fill = factor(size_count), 
       title = "Size Category", cex = 0.8)


##### Analysis 3.3.3.1: Is there difference between Furnishing status with price and location? #####
# ANOVA testing: Price, Furnishing, Location
# Hypothesis a0: No difference in price between Property_Furnishing
# Hypothesis a1: Difference in price between Property_Furnishing
# Hypothesis b0: No difference in price between IsKLCC
# Hypothesis b1: Difference in price between IsKLCC
# Hypothesis c0: No difference in price between interaction of Property_Furnishing & IsKLCC
# Hypothesis c1: Difference in price between  interaction of Property_Furnishing & IsKLCC
anova_result <- aov(Property_Price ~ Property_Furnishing * IsKLCC, data = df)
summary(anova_result)
plot(anova_result)

# Tukey HSD test on Furnishing Status 
anova_result <- aov(Property_Price ~ Property_Furnishing, data = df)
TukeyHSD(anova_result)
plot(TukeyHSD(anova_result, conf.level=.95))


# Violin Plot with Box Plot: Price by Furnishing Status
ggplot(a1.df, aes(x = Property_Furnishing, 
                  y = Property_Price)) + 
  geom_violin(aes(fill = Property_Furnishing), 
              trim = FALSE) + 
  geom_boxplot(aes(), 
               width = 0.15, 
               position = position_dodge(0.9)) +
  labs(title = "Price by Furnishing Status", 
       x = "Furnishing Status", 
       y = "Price") +
  theme_minimal()

ggplot(a1.df.klcc, aes(x = Property_Furnishing, 
                  y = Property_Price)) + 
  geom_violin(aes(fill = Property_Furnishing), 
              trim = FALSE) + 
  geom_boxplot(aes(), 
               width = 0.15, 
               position = position_dodge(0.9)) +
  labs(title = "Price by Furnishing Status in KLCC", 
       x = "Furnishing Status", 
       y = "Price") +
  theme_minimal()


# Clustered Bar Chart: Counts for Furnishing Status by Price Category
ggplot(a1.df.klcc, aes(x = Price_Category, fill = Property_Furnishing)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  labs(title = "Count of Furnishing Status by Price Category in KLCC",
       x = "Location",
       y = "Counts") +
  scale_fill_manual(
    values = c("Fully Furnished" = "skyblue",
               "Partly Furnished" = "lightgreen",
               "Unfurnished" = "lightcoral")) +
  theme_minimal()


##### Analysis 3.3.3.2: How does the distribution of Furnishing status in KLCC and Price > 1M? #####
# 3 Dimension Table: Size, Furnishing, Room
table(a2.df$Size_Category, a2.df$Property_Furnishing, 
      a2.df$Room_Category)

# Insert 3D table data into array
three_way_array <- 
  array(c(362, 52, 621, 239, 72, 408, 6, 10, 34, 1, 
          342, 119, 3, 541, 88, 0, 37, 14), 
        dim = c(3, 3, 2),
        dimnames = list(Size_Category = 
                          c("<1000", ">1800", "1000-1800"),
                        Property_Furnishing = 
                          c("Fully Furnished", 
                            "Partly Furnished", "Unfurnished"),
                        Room_Category = c("<=3", ">3")))

# Convert to a three-way contingency table
three_way_table <- as.table(three_way_array)

# Mosaic Plot: Size by Furnishing by Room
mosaic(three_way_table, shade = TRUE, legend = TRUE)


# Get Room <=3
a3.df <- a2.df %>% filter(Room_Category == "<=3")

# Get table data for Size Category and Furnishing
summary_data <- as.data.frame(
  table(a3.df$Property_Furnishing, a3.df$Size_Category))
colnames(summary_data) <- 
  c("Property_Furnishing", "Size_Category", "Counts")
summary_data$Size_Category <- 
  factor(summary_data$Size_Category, 
         levels = c("<1000", "1000-1800", ">1800"))
summary_data$Furnishing_Size <- 
  paste(summary_data$Property_Furnishing, 
        summary_data$Size_Category, sep = " : ")


# Lollipop Chart: Counts for Furnishing Status by
#                 Size Category with Rooms <=3, Price >1M in KLCC
ggdotchart(summary_data, x = "Furnishing_Size", y = "Counts",
           title = "Counts for Furnishing Status by 
              Size Category with Rooms <=3, Price >1M in KLCC",
           color = "Size_Category",                                
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
           sorting = "asc", sort.by.groups = TRUE,
           group = "Size_Category",
           add = "segments",                            
           add.params = list(color = "lightgray", size = 3), 
           dot.size = 5,                                 
           ggtheme = theme_pubclean()) +
  font("x.text", size = 8, vjust = 0.5) +
  labs(x="Furnishing Status") +
  geom_text(aes(label = Counts), 
            position = position_dodge(width = 0.8), 
            vjust = -0.7)


##### Analysis 3.3.4.1: Is there difference between Built_Area with Location and Price? #####
contingency_table <- table(df$Built_Area, df$IsKLCC)

# Perform Chi-square test
# Hypothesis 0: No difference in Built_Area and IsKLCC
# Hypothesis 1: Difference in Built_Area and IsKLCC
chi_square_result <- chisq.test(contingency_table)


# Stacked Bar Chart of Count for Built Area by IsKLCC
ggplot(df, aes(x = Built_Area, 
               fill = IsKLCC)) +
  geom_bar(position = "stack") +
  geom_text(stat = "count", 
            aes(label = stat(count)), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Count for Built Area by IsKLCC",
       x = "Built Area",
       y = "Count") +
  theme_minimal()


# Conduct t.test for Built Area & Price
# Hypothesis 0: No difference in Price between Built-Up & Land
# Hypothesis 1: Difference in Price between Built-Up & Land
t.test(Property_Price ~ Built_Area, data = df)


mean_values <- a1.df.klcc %>%
  group_by(Built_Area) %>%
  summarize(mean_price =
              mean(Property_Price, na.rm = TRUE))

# Box Plot with Mean for price of Built Area in KLCC
ggplot(a1.df.klcc, aes(x = Built_Area, 
               y = Property_Price)) +
  geom_boxplot(notch = TRUE, 
               fill = "yellow") +
  geom_point(data = mean_values, 
             aes(x = Built_Area, 
                 y = mean_price),
             shape = 18, 
             size = 6, 
             color = "#FC4E07") +
  labs(title = "Property Prices by Built Area in KLCC",
       x = "Built Area",
       y = "Property Price") +
  theme_minimal()


##### Analysis 3.3.4.2: What is the distribution of built area? #####
# Jitter Plot: Distribution of Built Area and Size
a3.df$Size_Category <-
  factor(a3.df$Size_Category, levels = 
           c("<1000", "1000-1800", ">1800"))

ggplot(a3.df, aes(Built_Area, Size_Category)) + 
  geom_jitter(aes(color = Property_Furnishing )) + 
  ggpubr::color_palette("jco")+
  ggpubr::theme_pubclean() +
  labs(title = "Distribution of Built Area and 
       Size with Rooms <=3, in KLCC, Price >1M")


type_count <- table(a2.df$Built_Area)
percentage_counts <- round((type_count / sum(type_count)) * 100, 2)
percentage_counts_labels <- paste0(percentage_counts, "%", "\n", paste0(type_count))

# Pie Chart: Built Area in KLCC and Price >1M
pie(type_count, labels = percentage_counts_labels, 
    main = "Built Area in KLCC and Price >1M", radius = 0.9,
    col = c("skyblue","lightcoral"), clockwise = TRUE)


a4.df = a3.df %>% 
  filter(Size_Category == "1000-1800" & Property_Furnishing == "Fully Furnished")

type_count <- table(a4.df$Built_Area)
percentage_counts <- round((type_count / sum(type_count)) * 100, 2)
percentage_counts_labels <- paste0(percentage_counts, "%", "\n", paste0(type_count))

# Pie Chart: Built Area with Room <=3, 1000-1800 sqft,
#            Fully Furnished, Price>1M in KLCC
pie(type_count, labels = percentage_counts_labels, 
    main = "Built Area with Room <=3, 1000-1800sqft, 
    Fully FUrnished, Price>1M in KLCC", radius = 0.9,
    col = c("skyblue","lightcoral"), clockwise = TRUE)
legend("topright", legend = names(type_count), fill = c("skyblue","lightcoral"), 
       title = "Built Area", cex = 1)


##### Conclusion: Validation of Hypothesis #####
# Determine the area, intersection
venn <- a2.df %>% 
  select(Room_Category, Size_Category, Property_Furnishing, Built_Area)
area1 = sum(venn$Room_Category=="<=3")
area2 = sum(venn$Size_Category=="1000-1800")
area3 = sum(venn$Property_Furnishing=="Fully Furnished")
area4 = sum(venn$Built_Area=="Built-up")
n12 = sum(venn$Room_Category=="<=3"&venn$Size_Category=="1000-1800")
n13 = sum(venn$Room_Category=="<=3"&venn$Property_Furnishing=="Fully Furnished")
n14 = sum(venn$Room_Category=="<=3"&venn$Built_Area=="Built-up")
n23 = sum(venn$Size_Category=="1000-1800"&venn$Property_Furnishing=="Fully Furnished")
n24 = sum(venn$Size_Category=="1000-1800"&venn$Built_Area=="Built-up")
n34 = sum(venn$Property_Furnishing=="Fully Furnished"&venn$Built_Area=="Built-up")
n123 = sum(venn$Room_Category=="<=3"&venn$Size_Category=="1000-1800"
           &venn$Property_Furnishing=="Fully Furnished")
n124 = sum(venn$Room_Category=="<=3"&venn$Size_Category=="1000-1800"
           &venn$Built_Area=="Built-up")
n134 = sum(venn$Room_Category=="<=3"&venn$Property_Furnishing=="Fully Furnished"
           &venn$Built_Area=="Built-up")
n234 = sum(venn$Size_Category=="1000-1800"&venn$Property_Furnishing=="Fully Furnished"
           &venn$Built_Area=="Built-up")
n1234 = sum(venn$Room_Category=="<=3"&venn$Size_Category=="1000-1800"
            &venn$Property_Furnishing=="Fully Furnished"&venn$Built_Area=="Built-up")
left = sum(venn$Room_Category!="<=3"&venn$Size_Category!="1000-1800"
           &venn$Property_Furnishing!="Fully Furnished"&venn$Built_Area!="Built-up")

# 4 Set Venn Diagram
grid.newpage()
draw.quad.venn(area1 = area1, area2 = area2, area3 = area3, area4 = area4, n12 = n12,
               n13 = n13, n14 = n14, n23 = n23, n24 = n24, n34 = n34,
               n123 = n123, n124 = n124, n134 = n134, n234 = n234, n1234 = n1234, 
               fill = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"), 
               lty = "blank",
               category = 
                 c("Rooms <=3", "Size 1000-1800", "Fully Furnished", "Built-up"))
grid.text(label = "Number of Properties in KLCC with Price >1M", x = 0.5, y = 0.98,
          just = "top", gp = gpar(fontsize = 16, fontface = "bold"))
grid.text(label = left, x = 0.17, y = 0.22, just = "top")
grid.text(label = paste0("Total: ", nrow(venn)), x = 0.5, y = 0.06, just = "bottom")

round(593/2949*100, 2)