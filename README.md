# Email Alert
MoveApps

Github repository: *github.com/movestore/Email-Alert*

## Description
Writes a text file containing a custom alert, based on a user-defined threshold value of a variable in the dataset. When the set condition is met a email will be sent out independently of the email of the scheduled automatic run of the Workflow.

## Documentation
This App checks whether and how often a user-defined relation is fulfilled in the input dataset, e.g., locations with a ground.speed above 20 m/s, or records that have been assigned to a behavioral grouping, such as a predation cluster, in a previous App in the Workflow. When the condition is met, an email will be sent out which will include a summary of the results. Note that entries with NA (not available, emtpy cells) in the relation's attribute will be ignored.

If the defined condition is fulfilled at least by one event record, then the user-provided email text, followed by all unique rows of data meeting the threshold, based on a select set of up to five event and/or track attributes, will be written into the file `email_alert_text.txt`. For each row, the selected attributes are returned, along with the first and last timestamp and the coordinates of the most central location (minimum distance to all other locations in the group) calculated for each grouping. The resulting table will be attached to the email and made available as a artefact in the App. 

Examples:
* To return individual records from the original dataset, list 'event.id' or another attribute unique to each event. In this case, the timestamps and central location coordinates will be the same as those of each individual event. 
* To return records grouped by some characteristic, list only attributes that define that grouping, for example, 'clus.ID' from the [Predation Cluster Detection App](https://www.moveapps.org/apps/browser/6ffc4b69-eebe-47dc-bb10-04ea0abaaf2b). In this case, the timestamps and central location coordinates will summarize the duration and site for each group of records.

Notes:
* This text will be included in notification e-mails if the MoveApps workflow is [scheduled for automatic runs](https://docs.moveapps.org/#/scheduled_runs). 
* The App can be used multiple times in a workflow to assess different threshold conditions. Emails will be sent out separately for each condition met. 
* To restrict the analysis and alerts to recent incoming data from [automated feeds in Movebank](https://www.movebank.org/cms/movebank-content/live-data-feeds), we recommend using the option 'Most recent data' in the 'Movebank Location' App.  
* See the public "Location Cluster Detection" workflow for an example of how this App can be used.

### Application scope
#### Generality of App usability
This App was developed for any taxonomic group. 

### Input data
`move2::move2_loc`

### Output data
`move2::move2_loc`

### Artefacts
`email_alert_text.txt`: file with alert text that will be included in the Email message of schedules Workflow Instance runs.

`central_points.csv`: csv file containing the table of the central locations and the named attributes

`Interactive_plot.html`: interactive plot of the central locations. By clicking on each dot, a pop-up appears with all the attributes and their values.

### Settings
**Location alert property (`variab`):** This is an (event or track) attribute of the input dataset according to which the data will be filtered. Spelling and case must match exactly.

**Property relation (`rel`):** This parameter defines the relation used to evaluate threshold values.      
      - *contains at least one of the following values (%in%)*: can be used for categorical, integer and numeric variables     
      - *equals (==)*:  can only be used for numeric or timestamps variables     
      - *is greater than (>)* : can only be used for numeric or timestamps variables     
      - *is smaller than (<)*:  can only be used for numeric or timestamps variables     
      - *is between two values (range)*: can only be used for numeric or timestamps variables     

**Property threshold value (`valu`):** The threshold value of the relation for assessing the selected attribute (`variab`). If the *Property relation (`rel`)* is `contains at least one of the following values` or `is between two values` than one to multiple or two value entries (respectively) must be commas-separated. If the selected attribute is a timestamp, please provide values in UTC in the format ‘YYYY-mm-dd HH:MM:SS’, for example, '2021-06-23 09:34:00'

**Time variable? (`time`):** Please tick this parameter if your selected variable is a timestamp type, so that the App can properly work with it. Default is 'false'.

**Custom e-mail text (`emailtext`):** Text that will appear at the head of the notification e-mail that will be sent out if the condition is met.

**groupbyTrk:** T/F, if F that first attrib stated below will be used

**Attributes of input data to be added to e-mail text (`attr`):** Up to five data attributes from the input dataset that you want to have printed in the notification e-mail text. All unique data rows of the listed attributes fulfilling the threshold condition will be printed. It is not possible to include timestamp variable here.

**Attribute sorting order in e-mail text (`odir`):** Define whether to order the unique data rows in increasing or decreasing order, based on the first attribute listed in `attr`.

**Create csv with central locations (`csvout`):** Create table with the central locations and the named attributes. If checked, the csv will also be attached to the email sent out. Default is 'true'.

**Create interactive plot as html file with the central locations (`plot`):** Create an interactive plot of the central locations and the selected attributes. By clicking each point, a pop-up with all attributes will be shown. If checked, the html file will also be attached to the email sent out. Default is 'true'.

### Changes in output data
The input data remains unchanged.

### Most common errors
Please report repeated errors as an issue here.

### Null or error handling:
**Setting `variab`:** If there is no individual variable with the name given here, an error will be returned. If the spelling does not match exactly, and error will be returned. Review available variables in the output Overview of the preceding App in the workflow

**Setting `rel`:** If none of the relation options are selected, an error will be returned. It has to be carefully considered that the selected relation fits with the data type of the selected variable. Only numeric and timestamps variables can relate by '==', '>', '<' or >&<.

**Setting `valu`:** If there is no value entered, an error will be returned. The data type of the entered value has to fit with the selected variable.

**Setting `time`:** If the selected variable is a timestamp and it was not indicated here, the variable will be treated as a string of text and possibly not handled correctly, leading to errors. Similarly if your variable is not a timestamp and it is indicated here. Default is 'false'.

**Setting `emailtext`:** This text is written into the Notification E-mail. It should be kept short.

**Setting `attr`:** If there is no individual variable with any name given here, an error will be returned. The inclusion of timestamp variables will lead to errors here. If you select more than 5 attributes, the latter ones are cut off and a warning is given.


