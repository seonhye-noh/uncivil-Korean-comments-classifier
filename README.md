## Classifier replication

This repository contains replication code for the Cleanbot 1 and Cleanbot 2 classifiers used in Kernell and Noh, *“The AI Referee: How Online Interventions Shape Incivility and User Engagement in News Discussions”*.

The code reproduces the preprocessing, model specification, training procedure, and evaluation metrics reported in the paper.

### Model
- Transformer: KcELECTRA-base-v2022  
- Binary classification (flagged vs. not flagged)

### How to run
To train and evaluate Cleanbot 1:
```python train_cleanbot1.py```

To train and evaluate Cleanbot 2:
```python train_cleanbot2.py```

### Outputs
Each script saves:
- results_cleanbot1/metrics.csv  
- results_cleanbot1/confusion_matrix.png  
- results_cleanbot2/metrics.csv  
- results_cleanbot2/confusion_matrix.png

### Figures replication
The repository also includes the data files and R script used to reproduce figures in the manuscript.

Files are located in the 'replication_figures/' folder.

To run the R replication script:
```Rscript replication_figures/replicate_figures.R```


