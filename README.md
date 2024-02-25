# Outside_Work

**How to get set up: **
1) Activate VSCode with Conda Environment (https://python.plainenglish.io/installing-vs-code-with-conda-environment-a-step-by-step-guide-with-code-example-7eb00ab89e17)

2) Install required packages
numpy==1.21.2
pandas==1.3.3

3) Install Jupyter Lab extension (optional). I find this an easy way to test code in blocks for data discovery and initial build. It can then be converted to a .py script at the end of the process for automation

**Running the model **
1) Locate the input tables (Alec shared via OneDrive) and move these into your git repo. It should look like this below, however you wont have the Output files until after you've run though the entire code
   ![image](https://github.com/malikpri90/Outside_Work/assets/129008324/b0c872e0-7740-44a0-b7aa-23166d9ada57)

2) Run the factors code from top to bottom. This will load the tables, build the factors and output the "factors.csv" file
3) Run the Model Build GLM code from top to bottom to test that it works
4) Start tweaking the Model Build GLM code to see if you can improve the results. 

Key identifiers of accuracy:
   - Test data error and Train Data Error - typically these values are between 0.6 and 0.7. Higher numbers are best. If train accuracy > test accuracy it suggests overfitting
   - acc_chart plot showing actual vs predicted score for the test group
   - Scroll down and view the actual predictions by player to see if the model is directionally correct for the players

Potential ways to tweak the model
   - Edit the factors and factors23 dataframes. Currently I have set it up so you can comment out factors we don't want to include, so you can test which scenarios produce the best outputs
   - Change test / train split
   - Standardise scores between 0 & 1
   - Factor reduction to remove highly correlated or low importance factors
   - etc
