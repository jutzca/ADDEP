<<<<<<< HEAD
# ADDEP dataset
This is an analysis on ADDEP dataset.
=======
# Serum albumin and neurological recovery after spinal cord injury

**Aim**: To investigate if albumin is significantly associated with outcomes after acute spinal cord injury. 

**Data Source**: ADDEP Dataset
>>>>>>> cd9781ad1d1b889f3aa9e31c22f4f4a6386881ee

**Our goal is to investigate if albumin is significantly associated with outcomes after acute spinal cord injury**. Our specific aim was to replicate from the previous study by [Bobo, Tong](https://journals.sagepub.com/doi/pdf/10.1177/1545968317746781). However, this analysis was done on the [Sygen clinical trial](https://journals.lww.com/spinejournal/Fulltext/2001/12151/The_Sygen__Multicenter_Acute_Spinal_Cord_Injury.15.aspx), which ran from 1992 to 1997, and as a result, was subject to outdated acute management practices (i.e., administration of methylprednisolone). Thus, we aim to replicate previous finding on a more recent dataset, [ADDEP](https://www.icpsr.umich.edu/icpsrweb/ADDEP/studies/36724). 

Our data cleaning + wrangling process can be found [here](https://github.com/AnhKhoaVo/ADDEP/blob/master/ADDEP_Clean_Analysis.R). In this section, we have derived new outcomes for 1-year analysis, namely Marked Recovery at 1 year and Change of LEMS scores between baseline and 1 year. 

Our analysis is published in [here](https://rpubs.com/AnhKhoaVo/586028). Briefly, we found that albumin was a crude prognostic biomarker. Serum albumin concentration could be useful in cases where injury severity cannot be accurately assessed.

It is important to note that we are building a **descriptive** model instead of a **predictive** model. Thus, we are more interested in confirming if albumin is a significant prognostic biomarker in this dataset rather than developing a model to predict for future unseen data. 
