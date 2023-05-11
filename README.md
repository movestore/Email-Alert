# Email Alert
MoveApps

Github repository: *github.com/movestore/Email-Alert*

## Description
Writes a text file containing a custom alert, based on a user-defined threshold value of a variable in the dataset, that will be included in notification e-mails if the MoveApps workflow is scheduled for automatic runs.

## Documentation
This App checks whether and how often a user-defined relation is fulfilled in the input dataset, e.g., locations with a ground.speed above 20 m/s, or records that have been assigned to a behavioral grouping, such as a predation cluster, in a previous App in the workflow. Results are summarized in notification e-mails if the workflow is scheduled for automatic runs. Note that entries with NA (not available, emtpy cells) in the relation's attribute will be ignored.

If the defined relation is fullfilled at least by one event record, then the user-provided email text, followed by all unique rows of data meeting the threshold, based on a select set of up to five animal and/or track attributes, will be written into the file `email_alert_text.txt`. For each row, the selected attributes are returned, along with the first and last timestamp and the coordinates of the most central location (minimum distance to all other locations in the group) calculated for each grouping. 

Examples:
* To return individual records from the original dataset, list 'event.id' or another attribute unique to each event. In this case, the timestamps and central location coordinates will be the same as those of each individual event. 
* To return records grouped by some characteristic, list only attributes that define that grouping, for example, 'clus.ID' from the [Predation Cluster Detection App](https://www.moveapps.org/apps/browser/6ffc4b69-eebe-47dc-bb10-04ea0abaaf2b). In this case, the timestamps and central location coordinates will summarize the duration and site for each group of records.

Notes:
* This text will be included in notification e-mails if the MoveApps workflow is [scheduled for automatic runs](https://docs.moveapps.org/#/scheduled_runs). 
* The App can be used multiple times in a workflow to assess different threhold conditions. In this case, the alert texts are included successively in the e-mail notification. 
* To restrict the analysis and alerts to recent incoming data from [automated feeds in Movebank](https://www.movebank.org/cms/movebank-content/live-data-feeds), we currently recommend using the [Filter by Last X Days App](https://www.moveapps.org/apps/browser/861808be-fb15-4e03-af3d-533642ec797e) as a previous step in the workflow.  
* See the public "Location Cluster Detection" workflow for an example of how this App can be used.

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format

### Artefacts
`email_alert_text.txt` 

### Settings
**Location alert property (`variab`):** This is an attribute of the input dataset according to which the data will be filtered.

**Property relation (`rel`):** This parameter defines the relation used to evaluate threshold values. The possible values differ by the attribute data type: '==', '>' or '<' can only be used for numeric and timestamps variables.

**Property threshold value (`valu`):** The threshold value of the relation for assessing `variab`. If `rel` = 'is one of the following', multiple value entries must be commas-separated. If `variab` is a timestamp attribute, please provide values in UTC in the format ‘YYYY-mm-dd HH:MM:SS’, for example, '2021-06-23 09:34:00"

**Time variable? (`time`):** Please tick this parameter if your selected variable is a timestamp type, so that the App can properly work with it.

**Custom e-mail text (`emailtext`):** Text that will appear at the head of the notification e-mail provided with schedule workflow runs if the threshold conditions are met.

**Attributes of input data to be added to e-mail text (`attr`):** Up to five data attributes from the input dataset that you want to have printed in the notification e-mail text. All unique data rows of the listed attributes fulfilling the threshold condition will be printed. It is not possible to include timestamp variable here.

**Attribute sorting order in e-mail text (`odir`):** Define whether to order the unique data rows in increasing or decreasing order, based on the first attribute listed in `attr`.

### Null or error handling:
**Setting `variab`:** If there is no individual variable with the name given here, an error will be returned.

**Setting `rel`:** If none of the relation options are selected, an error will be returned. It has to be carefully considered that the selected relation fits with the data type of the selected variable. Only numeric and timestamps variables can relate by '==', '>' or '<'.

**Setting `valu`:** If there is no value entered, an error will be returned. The data type of the entered value has to fit with the selected variable.

**Setting `time`:** If the selected variable is a timestamp and it was not indicated here, the variable will be treated as a string of text and possibly not handled correctly, leading to errors. Similarly if your variable is not a timestamp and it is indicated here. Default is 'false'.

**Setting `emailtext`:** This text is written into the Notification E-mail. It should be kept short.

**Setting `attr`:** If there is no individual variable with any name given here, an error will be returned. The inclusion of timestamp variables will lead to errors here. If you select more than 5 attributes, the latter ones are cut off and a warning is given.

**Data:** The full input data set is returned for further use in a next App and cannot be empty.
