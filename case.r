# 1. Read CSV file to RStudio
participant <- read.csv("Participant.csv", fileEncoding = "UTF-8-BOM")
question <- read.csv("Question.csv", fileEncoding = "UTF-8-BOM")
result <- read.csv("Result.csv", fileEncoding = "UTF-8-BOM")

is.na(participant)
is.na(question)
is.na(result)

participant <- na.omit(participant)
question <- na.omit(question)
result <- na.omit(result)

# Question 1
result_percent <- table(result$Question.2, useNA = "no")

result_percent

pie_percent <- round((result_percent/sum(result_percent) * 100), 1)

pie(result_percent, main = "Result Percentage of Question 2:
    The lecturers of Bluejack University are masters
    of the corresponding courses taught to me.",
    labels = pie_percent,
    col = rainbow(length(result_percent)))

legend("topright", c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"), 
       fill = rainbow(length(result_percent)), cex = 0.9)


# Question 2
result2_table <- table(result$Question.6)


pie2_percent <- round(result2_table/sum(result2_table) * 100, 1)

pie(result2_table, main = "Result Percentage of Question 6 for Female Participants:
    The sanitation facility of Bluejack University is
    well maintained and hygienic.",
    labels = pie2_percent,
    col = rainbow(length(result2_table)))

legend("topright", c("FALSE", "TRUE"), fill = rainbow(length(result2_table)))

# Question 3
joined_table3 <- merge(result, participant, by = "Participant.Number")
result3_table <- table(joined_table3$Gender, joined_table3$Question.1)

barplot(result3_table, 
          beside = FALSE, 
          col = rainbow(length(result3_table)),  # Color for genders
          xlab = "Responses", 
          ylab = "Count", 
          names.arg = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
          main = "Result Percentage of Question 1: The education materials provided by Bluejack
          University is up to date and promises a great
          potential for my future.", 
          legend = rownames(result3_table))

# Question 4
joined_table4 <- merge(result, participant, by = "Participant.Number")
result4_table <-    table(joined_table4$Question.1, joined_table4$Gender)

boxplot(joined_table4$Question.1 ~ joined_table4$Gender, 
        col = rainbow(length(joined_table4)), 
        xlab = "Gender", 
        ylab = "Question 1 Answer", 
        main = "Relationship of Bla bla bla")

# Question 5
result5_table <- table(result$Question.4)

hist(result$Question.4, 
     main = "Result Percentage of Question 4: 
     The staffs of Bluejack University are polite,
     respectful, and welcoming.",
     breaks = 5,
     xlab = "Question 4 Answers",
     ylim = c(0, 50),
     col = rainbow(12))

# Question 6
q.1.to.5.legend = c("Question.1", "Question.2", "Question.3", "Question.4", "Question.5")
q.1.to.5 <- result[q.1.to.5.legend]
freq.1.to.5 <- data.frame(
    Question.1 = as.vector(table(q.1.to.5[1])),
    Question.2 = as.vector(table(q.1.to.5[2])),
    Question.3 = as.vector(table(q.1.to.5[3])),
    Question.4 = as.vector(table(q.1.to.5[4])),
    Question.5 = as.vector(table(q.1.to.5[5]))
)
barplot(
    as.matrix(freq.1.to.5),
    ylim=c(0,100),
    xlim=c(0, ncol(as.matrix(freq.1.to.5)) + 2.5),
    col=rainbow(length(freq.1.to.5))
)
legend("topright", c("Answered: 1", "Answered: 2", "Answered: 3", "Answered: 4", "Answered: 5"), fill=rainbow(5))

# Question 7
dates <- as.Date(result$Date, format = "%m/%d/%Y")

result7_table <- table(dates)
max_date <- as.Date(max(dates))
min_date <- as.Date(min(dates))
plot(result7_table, 
     main = paste("Number of Participants from
     ", min_date, "to", max_date),
     type = "p",
     ylab = "Number of Participants",
     xlab = "Survey Date")

plot(result7_table, 
     main = paste("Number of Participants from
     ", min_date, "to", max_date),
     type = "l",
     ylab = "Number of Participants",
     xlab = "Survey Date")

# Question 8
plot(result7_table, 
     main = paste("Number of Participants from
     ", min_date, "to", max_date),
     type = "b",
     ylab = "Number of Participants",
     xlab = "Survey Date")

