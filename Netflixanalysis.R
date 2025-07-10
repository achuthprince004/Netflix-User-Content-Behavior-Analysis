library(dplyr)
library(ggplot2)


# 📊 1. Age Distribution of Netflix Users
# -------------------------------
ggplot(data = netflix_users, aes(x = Age)) +
  geom_histogram(fill = "skyblue", bins = 30, color = "black") +
  ggtitle("Age Distribution of Netflix Users") +
  xlab("Age") +
  ylab("Count")


# 📊 2. Subscription Type Distribution
# -------------------------------
ggplot(data = netflix_users, aes(x = Subscription_Type, fill = Subscription_Type)) +
  geom_bar() +
  ggtitle("Subscription Type Distribution") +
  xlab("Subscription Type") +
  ylab("Number of Users") +
  scale_fill_brewer(palette = "Set2")


# 📊 3. Favorite Genre of Netflix Users
# -------------------------------
netflix_users %>%
  count(Favorite_Genre) %>%
  ggplot(aes(x = reorder(Favorite_Genre, -n), y = n, fill = Favorite_Genre)) +
  geom_col() +
  ggtitle("Favorite Genre of Netflix Users") +
  xlab("Genre") +
  ylab("Number of Users") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 📊 4. Average Watch Time by Subscription Type
# -------------------------------
netflix_users %>%
  group_by(Subscription_Type) %>%
  summarise(Average_Watch_Time = mean(Watch_Time_Hours)) %>%
  ggplot(aes(x = Subscription_Type, y = Average_Watch_Time, fill = Subscription_Type)) +
  geom_col() +
  ggtitle("Average Watch Time by Subscription Type") +
  xlab("Subscription Type") +
  ylab("Avg. Watch Time (Hours)") +
  theme_minimal()


# 📊 5. Top 10 Countries by Netflix User Count
# -------------------------------
netflix_users %>%
  count(Country) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(Country, -n), y = n, fill = Country)) +
  geom_col() +
  ggtitle("Top 10 Countries by Netflix User Count") +
  xlab("Country") +
  ylab("User Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 📊 6. Favorite Genres by Age Group
# -------------------------------
netflix_users %>%
  mutate(Age_Group = cut(Age, breaks = c(0, 18, 30, 45, 60, 100),
                         labels = c("Teen", "Young Adult", "Adult", "Middle Age", "Senior"))) %>%
  count(Age_Group, Favorite_Genre) %>%
  ggplot(aes(x = Age_Group, y = n, fill = Favorite_Genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Favorite Genres by Age Group") +
  xlab("Age Group") +
  ylab("Count") +
  theme_minimal()


# 📊 7. Monthly Login Trend
# -------------------------------
# Ensure Last_Login column is in Date format
netflix_users$Last_Login <- as.Date(netflix_users$Last_Login)

netflix_users %>%
  mutate(Month_Year = format(Last_Login, "%Y-%m")) %>%
  count(Month_Year) %>%
  ggplot(aes(x = Month_Year, y = n)) +
  geom_line(group = 1, color = "steelblue") +
  ggtitle("Monthly Login Trend") +
  xlab("Month-Year") +
  ylab("Login Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
