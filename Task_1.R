#---- install packages ("data.table", "ggplot2", "readr")
#install.packages("ggplot2")
#install.packages("ggmosaic")
#install.packages("readr")
#install.packages("readxl")

#---- Load required libraries

library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(readxl)

#---- Load the data

filePath <- "D:\\Projects\\Proj#3_Quantium_Data_Analysis_Retail_ChipsCategory\\"
transactionData <- read_excel(paste0(filePath, "QVI_transaction_data.xlsx"))
customerData <- fread(paste0(filePath, "QVI_purchase_behaviour.csv"))

#---- Know the transaction data

str(transactionData)
head(transactionData)
summary(transactionData)

#---- Convert the numeric date format to actual date format

transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

#---- View all product names and remove unwanted items, eg. Salsa

  # View product names
unique(transactionData$PROD_NAME)
setDT(transactionData)
  # Remove salsa
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE][, SALSA := NULL]

#---- Identify and remove outliers (such as, many quantity purchases 

transactionData[PROD_QTY > 100]
  # Remove customer who bought 200 units
transactionData <- transactionData[LYLTY_CARD_NBR != 226000]

#---- Plot transactions over time (01 July, 2018 to 01 June, 2019)

transactions_by_day <- transactionData[, .N, by = DATE]

  # To create the plot graph
ggplot(transactions_by_day, aes(x = DATE, y = N)) +
geom_line() +
labs(title = "Transactions Over Time") +
scale_x_date(breaks = "1 month") +
theme(axis.text.x = element_text(angle = 90))

#---- Converts PROD_NAME into structured variables to help with grouping and segmentation
  # Extract pack size
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

  # Extract brand name
transactionData[, BRAND := tstrsplit(PROD_NAME, ' ')[[1]]]

  # Normalize brand names
transactionData[BRAND == "RED", BRAND := "RRD"]

#---- Analyze Customer Dataset (purchase behaviour.csv)
summary(customerData)
  # Merge with transactions
data <- merge(transactionData, customerData, all.x = TRUE)

#---- Analysze Segments
  #Sales by segment
sales_summary <-data[, .(TOTAL_SALES =sum(TOT_SALES)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]
  # Units per customer
units_summary <- data[, .(UNITS = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]
  # Price per unit
price_summary <- data[, .(AVG_PRICE = mean(TOT_SALES/PROD_QTY)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

#----- Example: compare price per unit between segments
premium_group <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Premium"]
mainstream_group <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"]

t.test(premium_group$TOT_SALES / premium_group$PROD_QTY,
       mainstream_group$TOT_SALES / mainstream_group$PROD_QTY)

#---- Find favorite brands for Mainstream Young Singles
segment_data <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"]
segment_data[, .N, by = BRAND][order(-N)]

#---- Export Cleaned Dataset

fwrite(data, paste0(filePath, "Task_1_QVI_data.csv"))


###########Data Exploration - MJ
#Calculate total sales by life stage and sort in descending order
sales_by_lifestage <- data[, .(TotalSales = sum(TOT_SALES)), by = LIFESTAGE][order(-TotalSales)]

# Print the summary table
print(sales_by_lifestage)

# Plot the sales by life stage
library(ggplot2)
library(scales)

ggplot(sales_by_lifestage, aes(x = reorder(LIFESTAGE, -TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Chip Sales by Life Stage", x = "Life Stage", y = "Total Sales") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate total sales by life stage and affluence and sort in descending order
sales_by_customer_category <- data[, .(TotalSales = sum(TOT_SALES)), by = .(PREMIUM_CUSTOMER)][order(-TotalSales)]

# Plot the sales by customer category as a pie chart
library(ggplot2)
library(scales)

sales_by_customer_category[, Category := paste(PREMIUM_CUSTOMER, sep = " - ")]

# Compute percentage for labels
sales_by_customer_category[, pct := TotalSales / sum(TotalSales)]
sales_by_customer_category[, label := paste0(Category, "\n", round(pct * 100), "%")]

# Define colors: top = green, second = light green, last = yellow-red
ranked_colors <- c( "#90EE90", "#2E8B57", "#FFD700")  # green, light green, gold

# Assign fill colors based on rank
sales_by_customer_category <- sales_by_customer_category[order(-TotalSales)]
sales_by_customer_category[, fill_color := ranked_colors[1:.N]]

# Plot
ggplot(sales_by_customer_category, aes(x = "", y = TotalSales, fill = Category)) +
  geom_bar(stat = "identity", width = 1, show.legend = TRUE) +
  coord_polar("y") +
  labs(title = "Chip Sales Share by Customer Category", x = NULL, y = NULL) +
  theme_void() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = sales_by_customer_category$fill_color, guide = guide_legend(title = "Customer Category"))



# --- Additional analysis: Top and Bottom Performing Stores ---
# Calculate total sales by store
store_sales <- data[, .(TotalSales = sum(TOT_SALES)), by = STORE_NBR][order(-TotalSales)]

# Print top 5 and bottom 5 stores
cat("Top 5 performing stores:\n")
print(head(store_sales, 5))

cat("\nBottom 5 performing stores:\n")
print(tail(store_sales, 5))

# Cluster-style scatter plot with random z-scores
set.seed(42)
store_sales[, `:=`(ActualZ = scale(TotalSales),
                   PredictedZ = rnorm(.N, mean = 0, sd = 1))]

# Jitter the positions to create spacing between points
store_sales[, `:=`(x0 = as.numeric(ActualZ) + rnorm(.N, 0, 0.05),
                   y0 = as.numeric(PredictedZ) + rnorm(.N, 0, 0.05))]

store_sales[, Label := as.character(STORE_NBR)]
store_sales[, Performance := ifelse(PredictedZ < ActualZ, "Outperformed", "Underperformed")]
store_sales <- store_sales[order(Performance)]

# Plot circles with spacing
library(ggforce)

ggplot(store_sales) +
  geom_circle(aes(x0 = x0, y0 = y0, r = 0.05, fill = Performance), color = "black", alpha = 0.8) +
  geom_text(aes(x = x0, y = y0, label = Label), size = 3, fontface = "bold", color = "white") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Outperformed" = "forestgreen", "Underperformed" = "firebrick")) +
  labs(title = "Cluster Plot: Store Performance", x = "Actual Sales (Z-score)", y = "Expected Sales (Random Z-score)") +
  theme_minimal() +
  theme(legend.position = "bottom")

