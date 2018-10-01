# weapon-SD
Codes and datasets of the paper entitled "Fighting with style - patterns of sexual dimorphism differ between freshwater decapods (Aeglidae) with different fighting styles"

-----------------------
File summary:</br>

<b>data.adjusment.r</b>: <u>R code</u>. Code to organize the data coming from different packages into one data frame</br>
<b>model.stan</b>: <u>STAN language</u>. The custom-built ANCOVA model in which the variance parameter is also estimated</br>
<b>plot.r</b>: <u>R code</u>. Code used to plot the igures presented in the paper</br>
<b>run.analysis</b>: <u>R code</u>. Code that takes the model in stan language and runs it in R</br>
<b>simpler.model.stan</b>:<u>STAN language</u>. Similar the to the custom-built ANCOVA, but with no continuous co-variable (which would make it a custom-built ANOVA).
