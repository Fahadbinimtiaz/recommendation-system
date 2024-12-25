# Movie Recommendation System Capstone Project

## Introduction
This project is a part of Harvardx Data Science Capstone Project. This project aims to develop a recommendation system using the MovieLens 10M dataset. The goal is to predict user ratings for movies they haven't seen, minimizing prediction error with Root Mean Squared Error (RMSE) as the evaluation metric.

## Project Highlights
- Dataset: Over 10 million ratings from 69,878 users on 10,677 movies.
- Exploratory Data Analysis:
  - User behavior, rating distribution, and genre trends.
  - Visualizations to derive meaningful insights.
- Model Development:
  - Incremental approach: baseline, movie effects, user effects, and genre effects.
  - Regularized model (λ=0.4) achieving an RMSE of 0.8563548.
- Final Model Evaluation:
  - Achieved RMSE: 0.8648472 on holdout test set.

## Repository Structure
- R script: R Script for data processing, data modeling and outcome.
- Data: ml-10M100K contains ratings and movies in DAT format.
- Report: Final project report in RMarkdown and PDF formats.

## Key Metrics
| Model                            | RMSE       |
|----------------------------------|------------|
| Baseline (global average)        | 1.060331   |
| Movie + User Effects             | 0.8567039  |
| Movie + User + Genre Effects     | 0.8563595  |
| Regularized Model (λ=0.4)        | 0.8563548  |

## Future Work
- Implement matrix factorization techniques.
- Explore additional variables such as user demographics.
- Apply advanced sampling methods for large-scale modeling.

## Tools and Libraries
- **Languages**: R
- **Packages**: dplyr, ggplot2, scales

## Conclusion
This capstone project showcases a systematic approach to building a movie recommendation system, from data preprocessing to model evaluation. The final regularized model successfully met performance benchmarks.

---