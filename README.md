<center> <h3>How we learn to value the road not taken: The role of deliberation and memory in the valuation of unchosen options</h3> </center>
<center> <h4>Natalie Biderman and Daphna Shohamy, November 2020</center> </h4>
\

<span style="font-size:1.2em;">
The repository accompanies our manuscript and includes codes to analyze data from five Mechanical Turk experiments assessing the effects of choice outcomes on the inferred value of unchosen options (see figure below).  
\
All codes were written by Natalie Biderman (*natalie.biderman@columbia.edu*).  
\
</span>

![Figure1. Experimental design](results/Plots/Figure1.png)  
\
<span style="font-size:1.2em;">
The repository includes three folders: (1) code, (2) data, (3) results.  
\
</span>

#### code folder
<span style="font-size:1.2em;">
All the results and figures reported in our manuscript are described in [Analysis_code.html](./code/Analysis_code.html).  
We fit hierarchical Bayesian regression models via the RStanArm package, which under the hood runs multiple independent chains of Hamiltonian Monte Carlo, and logs samples from the posterior distribution over the model parameters. These models may take a few hours to run and their size is considerabely large (>2gb). We therefore saved our fitted models in the form of RStanArm objects in this link. To reproduce our Bayesian analyses, you may (1) download the folder, unzip it, and insert the Models directory in data folder (i.e., data/Models), (2) run the models yourself by setting the parameter *run_models=TRUE* in our analysis code. 
The folder also includes *Preprocessing_data.Rmd* script where we preprocess the data and exclude outlier participants prior to data analysis.  
\
</span> 


#### data folder
<span style="font-size:1.2em;">
Data collected from our five MTurk experiments appear in **csv_files** subfolder. The folder includes experimental data as well as a record of participants' interactions with their computer which we use to measure their attention throughout the task.  
The raw csv files include data from all phases of the experiments. To ease interpretability of the data we rearrange the csv columns and generate data structures for each phase separately. These data lists are situated in the **Full_data_lists** subfolder. Prior to data analysis, we assess the behavior of participants to find those who did not pass our attention checks and browsed different websites during the task. **Clean_data_lists** includes data only from participants who met our predefined exclusion criteria. 
Lastly, the **Models** subfolder includes all the pre-trained Bayesian regression models, in the form of RStanArm objects which include samples from the posterior over model parameters.  
\
</span>


#### results folder 
<span style="font-size:1.2em;">
This folder includes the plots we generate in our analysis code.  
</span>
\
