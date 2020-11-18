<center> <h3>How we learn to value the road not taken: The role of deliberation and memory in the valuation of unchosen options</h3> </center>
<center> <h4>Natalie Biderman and Daphna Shohamy, November 2020</center> </h4>
\

<span style="font-size:1.2em;">The repository includes codes to analyze data from five Mechanical Turk experiments assessing the effects of choice outcomes on the inferred value of unchosen options (see figure below).
This README file will describe how one can use this repository to observe and analyse our data.
All codes were written by Natalie Biderman (*natalie.biderman@columbia.edu*).</span>

![Figure1. Experimental design](results/Plots/Figure1.png)  
\
<span style="font-size:1.2em;">The repository includes three foldes: (1) code, (2) data, (3) results.</span>
\

#### code folder
<span style="font-size:1.2em;">
All the results and figures reported in our manuscript are described in [Analysis_code.html](./code/Analysis_code.html). To estimate our behavioral findings we use Bayesian regression models that may take a few hours to run. We therefore saved the models we ran in the **data/Models** folder, and loaded them in our analysis code. If you wish to run these models again, you may change the parameter *run_models* in our analysis code to be equal to 1. 
The folder also includes *Preprocessing_data.Rmd* script where we preprocess the data and exclude outlier participants prior to data analysis.
</span> 

#### data folder
<span style="font-size:1.2em;">Data collected from our five MTurk experiments appear in **csv_files** subfolder. The folder includes experimental data as well as a record of participants' interactions with their computer which we use to measure their attention throughout the task.  
The raw csv files include data from all phases of the experiments. To ease interpretability of the data we rearrange the csv columns and generate data structures for each phase separately. These data lists are situated in the **Full_data_lists** subfolder. Prior to data analysis, we assess the behavior of participants to find those who did not pass our attention checks and browsed different websites during the task. **Clean_data_lists** includes data only from participants who met our predefined exclusion criteria. 
Lastly, the **Models** subfolder includes all the Bayesian models we used in our data analysis.</span>

#### results folder 
<span style="font-size:1.2em;">This folder includes the plots we generate in our analysis code.</span>
# Counterfactual_value_learning
