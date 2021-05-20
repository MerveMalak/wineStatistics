# CENG212-CONCEPTS OF PROGRAMMING LANGUAGES
# wineStatistics

In this assignment, you're expected to

• give summary statistics (mean, median, and variance) for each attribute in the wine dataset,

• split the dataset into training and test set partitions with ratios 80% and 20% respectively. Before splitting, it's a good practice to apply shuffling (among instances) to avoid order effects.

• randomly classify the data instances in the test set, and report the total accuracy by comparing the results with the ground-truth labels.

For example, the following data instance belongs to the class 1 (the first attribute). In other words, the ground-truth label is 1. If you classify it as 2, it is inaccurate.

1,14.19,1.59,2.48,16.5,108,3.3,3.93,.32,1.86,8.7,1.23,2.82,1680

Please consider every row (data instance) as a Scheme list.

Write the Scheme procedures as higher order as possible.
