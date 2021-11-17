# Email Alert
MoveApps

Github repository: *github.com/movestore/Email-Alert*

## Description
Depending on a user defined property in your data set an alert E-mail text is written into a file. This text will be included in Notification E-mails if the MoveApps workflow is scheduled for automatic runs.

## Documentation
This App checks if and how often a user defined relation is fulfilled in the input data set (e.g. how many locations have a ground.speed above 20m/s).

If the defined relation is fullfilled at least by one location then the provided emailtext followed by up to 10 rows of data (with selected column attributes) will be written into the file `email_alert_text.txt`. This text will be included in Notification E-mails if the MoveApps workflow is scheduled for automatic runs. At the use of more than one such Apps in the workflow, the txt Texts are included successively.

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format

### Artefacts
`email_alert_text.txt` 

### Parameters 
`variab`: This is a selected individual parameter according to which the data shall be filtered. If the required parameter is not in this list, 'other' can be chosen en and its name entered below.

`rel`: By this parameter the relation in the required filter has to be selected. The possible values differ by parameter data type, only numeric and timestamps variables can relate by '==', '>' or '<'.

`valu`: Value of the relation that the filtered part of the data set has to fullfill. In case of `rel` = 'is one of the following' commas have to be used to separate the possible values. In case of a timestamp parameter please use the timestamp format with year, month, day, hour, minute and second as in the example: '2021-06-23 09:34:00"

`time`: Please tick this parameter if your selected variable is a timestamp type, so that the App can properly work with it.

`emailtext`: Text that will appear at the head of your Notification E-mail in case this workflow runs in a scheduled set.

`attr`: Data attributes that you want to have printed in the Notification E-mail text of the up to 10 (maximum) data rows/locations that fulfill the required property.

### Null or error handling:
**Parameter `variab`:** If there is no individual variable with the name given here, an error will be returned.

**Parameter `rel`:** If none of the relation options are selected, an error will be returned. It has to be carefully considered that the selected relation fits with the data type of the selected variable. Only numeric and timestamps variables can relate by '==', '>' or '<'.

**Parameter `valu`:** If there is no value entered, an error will be returned. The data type of the entered value has to fit with the selected variable.

**Parameter `time`:** If the selected variable is a timestamp and it was not indicated here, the variable will be treated as a string of text and possibly not handled correctly, leading to errors. Similarly if your variable is not a timestamp and it is indicated here. Default is 'false'.

**Parameter `emailtext`:** This text is written into the Notification E-mail. It should be kept short.

**Parameter `attr`:** If there is no individual variable with any name given here, an error will be returned.

**Data:** The full input data set is returned for further use in a next App and cannot be empty.
