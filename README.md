# Soccer Passing Analysis with MongoDB & R

## Overview
This project analyzes soccer passing data using Wyscout event data stored in MongoDB. The analysis includes passing patterns, success rates, pass lengths, directional tendencies, and clustering of passing styles.

> **Info:**  
> Main files are `soccer_stats.R` and `wyscout_Kmeans_Clustering.R`


## Data Source
The data is extracted from MongoDB, where match events are stored in JSON format. Events are structured hierarchically, with each match containing multiple event entries. Only pass events are considered for analysis.

## Setup
This project requires R and the following packages:
- `mongolite` for connecting to MongoDB
- `jsonlite` for handling JSON data
- `dplyr` for data manipulation
- `ggplot2` and `plotly` for visualization

## Data Extraction
Match and event data are retrieved from MongoDB. Only Ajax matches are selected for deeper analysis. The event data is then flattened into a structured format, keeping only passing-related events.

## Key Analyses and Results

### Pass Height Distribution
Passes are categorized based on height, and the percentage distribution is calculated:
- **High passes:** **13.01%**
- **Other passes (low/medium):** **86.99%**

### Pass Success Rate
Pass accuracy is analyzed both for all teams and specifically for Ajax:
- **Overall pass success rate:** **83.86%**
- **Ajax pass success rate:** **87.04%**

### Pass Length Distribution
A histogram of pass lengths is generated, and key statistics are:
- **Mean pass length:** **18.59 meters**
- **Median pass length:** **16.00 meters**
- **Standard deviation:** **11.6 meters**
- **Maximum pass length:** **118 meters**

### Pass Direction (Forward vs. Backward)
Passes are categorized based on direction:
- **Forward passes:** **32%**
- **Backward passes:** **15%**
- **Other (sideways, unknown):** **53%**



## K-Means CLustering algorithm

### K-Means Clustering
Passes are clustered based on:
1. Pass length
2. Pass angle
3. Match time (minute)

The Algoritm takes 70 iterations to finish with the set.seed(123)
![Pass Clustering Results](Pass_KMeans_cluster.png.png)

