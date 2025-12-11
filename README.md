# Email Alert
MoveApps

Github repository: *github.com/movestore/Email-Alert*

## Description
Sends out an email when condition for alert is met, based on a user-defined threshold value of a variable in the dataset. The email is only sent out if the App is run in a scheduled Workflow. 

## Documentation
This App checks whether a user-defined relation is fulfilled in the input dataset, e.g., locations with a ground.speed above 20 m/s, or records that have been assigned to a behavioral grouping, such as a predation cluster, in a previous App in the Workflow. When the condition is met, an email will be sent out which will include a summary of the results. Two csv (table of centroids & all locations that comply with the condition) and html (interactive map with the locations) files will be attached to the email. The email is only sent out if the App is run in a scheduled Workflow. If any of the files (csv/html) are larger than 15MB, they can not be attached to the email. In the email a link is provided to directly access the Workflow instance.

If the defined condition is fulfilled at least by one event record, then the user-provided email text, followed by the information of the threshold settings and the 10 first rows of the table with the centroid locations for each group (default is track id), first and last timestamp, and all the attributes selected by the user. The text contained in the email, the table and interactive plot will also be available as artefacts in the App.

Examples:
* To return individual records from the original dataset, list 'event.id' or another attribute unique to each event. In this case, the timestamps and central location coordinates will be the same as those of each individual event. 
* To return records grouped by some characteristic, list only attributes that define that grouping, for example, 'clus.ID' from the [Predation Cluster Detection App](https://www.moveapps.org/apps/browser/6ffc4b69-eebe-47dc-bb10-04ea0abaaf2b). In this case, the timestamps and central location coordinates will summarize the duration and site for each group of records.

Notes:
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

`centroid_locations.csv`: csv file containing the centroid locations per group. It has fixed columns ("first_timestamp", "last_timestamp","lon_centroid","lat_centroid", "first_lon", "first_lat", "last_lon", "last_lat") and variable columns. The first column will always correspond to the attribute used for grouping the locations. The other attributes named by the user will also be included with the prefix "median_" or "mode_". 

`all_locations.csv`: csv file containing all locations that comply with the condition. It has fixed columns ("timestamp", "location_long","location_lat") and variable columns. The first column will always correspond to the attribute used for grouping the locations. It also includes all the variables included in the setting 'Attributes of input data to be added'. 

`interactive_map.html`: interactive plot containing all locations that comply with the condition, the centroid, first and last location per group. By clicking on each dot, a pop-up appears with all the attributes and their values.


### Settings
**Location alert property (`variab`):** This is an (event or track) attribute of the input dataset according to which the data will be filtered. Spelling and case must match exactly.

**Property relation (`rel`):** This parameter defines the relation used to evaluate threshold values.      
      - *contains at least one of the following values (%in%)*: can be used for categorical, integer and numeric variables     
      - *equals (==)*:  can be used for numeric or timestamps variables     
      - *is greater than (>)* : can be used for numeric or timestamps variables     
      - *is smaller than (<)*:  can be used for numeric or timestamps variables     
      - *is between two values (range)*: can be used for numeric or timestamps variables     

**Property threshold value (`valu`):** The threshold value of the relation for assessing the selected attribute (`variab`). If the *Property relation (`rel`)* is `contains at least one of the following values` or `is between two values` than one to multiple or two value entries (respectively) must be commas-separated. If the selected attribute is a timestamp, please provide values in UTC in the format ‘YYYY-mm-dd HH:MM:SS’, for example, '2021-06-23 09:34:00'

**Time variable? (`time`):** Please tick this parameter if your selected variable is a timestamp type, so that the App can properly work with it. Default is 'false'.

**Custom e-mail text (`emailtext`):** Text that will appear at the head of the notification e-mail that will be sent out if the condition is met.

**Use track id to group results (`groupbyTrk`):** If true, for each track id a centroid location will be calculated. If data should be grouped by another variable specify below, and unselect this option. Default 'true'

**Attributes of input data to be added (`attr`):** Enter a comma-separated list animal or track attributes from the input dataset to include in the table.  If the option `Use track id to group results` is unselected the first variable listed here will be used to group the results. See available variable names under Animal Attributes and Track Attributes in the output Overview of the preceding app in the workflow. It is not possible to add timestamp variables. It is not possible to include timestamp variable here.

**Create csv with centroid locations (`csvcentroids`):** Create table with the centroid locations and the named attributes. The first column will always correspond to the attribute used for grouping the locations. The other attributes named by the user in `Attributes of input data to be added` will also be included with the prefix "median_" or "mode_". Per group, the median will be calculated for numeric attributes and the mode for categorical attributes. If checked, the csv will also be attached to the email sent out. Default is 'true'.

**Create csv with all locations that comply the condition (`csvall`):** Create table with all locations that comply the set condition and the named attributes. The first column will always correspond to the attribute used for grouping the locations. The other attributes named by the user in `Attributes of input data to be added` will also be included. If checked, the csv will also be attached to the email sent out. Default is 'true'.

**Create interactive plot as html file with the central locations (`plotl`):** Create an interactive plot containing all locations complying the condition set above, the centroid locations and the first and last location are also added. By clicking each point, a pop-up with all attributes for that point will be shown. If checked, the html file will also be attached to the email sent out. Default is 'true'.

### Changes in output data
The input data remains unchanged.

### Most common errors
**No email received**: Cause 1: the condition has not been met. Cause 2: the Workflow is not scheduled. Schedule the Workflow instance to receive an email when the condition is met.

**No attachments or missing attachments in the email**: If the csv or html files are larger then 15MB they cannot be attached. Find them in the App or outputs of the Workflow.

### Null or error handling:
**Setting `variab`:** If there is no individual variable with the name given here, an error will be returned. If the spelling does not match exactly, and error will be returned. Review available variables in the output Overview of the preceding App in the workflow

**Setting `rel`:** If none of the relation options are selected, an error will be returned. It has to be carefully considered that the selected relation fits with the data type of the selected variable. Only numeric and timestamps variables can relate by '==', '>', '<' or 'range'.

**Setting `valu`:** If there is no value entered, an error will be returned. The data type of the entered value has to fit with the selected variable.

**Setting `time`:** If the selected variable is a timestamp and it was not indicated here, the variable will be treated as a string of text and possibly not handled correctly, leading to errors. Similarly if your variable is not a timestamp and it is indicated here. Default is 'false'.

**Setting `emailtext`:** This text is written into the Notification E-mail. It should be kept short.

**Setting `attr`:** If there is no individual variable with any name given here, an error will be returned. The inclusion of timestamp variables will lead to errors here. If you select more than 5 attributes, the latter ones are cut off and a warning is given.


