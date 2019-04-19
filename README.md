
## RedcapData : A data management utility package to interface with REDCap data

----

# Preface

> [Research Electronic Data Capture](http://project-redcap.org), a web-based open source application, is used in capturing data from a variety of sources. It is actively developed and comprises of a number of institutional partners who actively use and contribute to its development.

> REDCap exposes metadata on its projects through a standardized .csv file called the data dictionary. This document defines the underlying data schematics of the repository from the data capture workflow to the variable names and their respective data types. In addition to this, REDCap provides an API for interacting with the repository external to the application hence it is easy to pull and push data and files in and out of the repository. This makes data exchange and interaction with other software applications easier.

> This is an R package developed to abstract some of the data management utility tasks when using a REDCap data repository. Some of these tasks include data input, data formatting, data cleaning and error report generation.

---

# Details

> The package is based around a set of reference classes that abstract much of the functionality. Supporting these reference classes is a set of publicly accessible functions that can be used independently and which are called by the classes' methods. In addition to these are private functions that are used internally within the package.

> Most of the functionality is based around the API which is used to pull records and metadata from the repository. Consequently code generation is used to map some of the logic and metadata abstracted in the data dictionary to R code that is then parsed to expression trees. These are then evaluated in the current R environment and workspace to achieve some desired effect or functionality.

> Much of the time, individual projects undergo structural mutations during their lifetimes causing a disconnect between the analysis code and the underlying data schema. Code generation from metadata is used as a technique in an attempt to cushion the analyst/researcher from  distorted analyses resulting from stale code. This also leads to a consistent format that can be used across different projects enabling code reuse. In cases of special or peculiar conditions, then the package also provides options to plugin customizations whenever necessary.

> The main object is the ```Redcap``` class which comprises of a cache (an R environment object), a log (character string) and a ```RedcapConfig``` object that holds the configurations. This prevents copy by value (environments are copied by reference) hence prevents blotting of memory. The object also stores data in its cache preventing time consuming processes when data is in memory.

---

# Usage

> Tasks to review:

* Installation
* Configuration
* Data input
* Data formatting
* Data cleaning
* Error report

## Installation

> The project is hosted on github and therefore requires ```devtools``` package for installation

### Code

```r
if (!is.element("devtools", .packages(all.available = T))) install.packages("devtools")
library(devtools)
devtools::install_github("smockin/RedcapData", ref = "v1.1.1")
```

## Configuration

> Instantiate a ```Redcap``` class using the ```redcap_project``` function. The configurations can be set individually using kwargs (keyword arguments) or specified in a structured .csv file (see details). Custom code for error reporting and/or update informations (changes to metadata over time - specified in a structured .csv file) can be included to customize error reporting. When using .csv files, specify the absolute paths to the csv files.

### Code

```r
library(RedcapData)

test <- redcap_project(api_url = "<url-to-host-server>/redcap/api/",
                      token = "<secret-token>",
                      chunked = T,
                      chunksize = 500,
                      local = FALSE
)
test
```

### Output

```

Instance:
A remote redcap instance running at ‘<url-to-host-server>/redcap’

Memory status:
Cache is empty

Events:
No events yet. (hint) use `obj`$load_data() to load data into memory.


```

> For details of how to configure the object, type ```help(redcap_project)```. On how to use the class instance created, type ```help(redcap_class)```.

> To view whether the project has updates, show the member ```RedcapConfig``` object.

### Code

```r
test$opts
```

### Output

```
Redcap configurations with no update info(s)
```


> To view the set of configurations specified, use the member ```RedcapConfig``` object's method list_configs.

### Code

```r
test$opts$configs$list_configs()
```

### Output

```
REDCap Configurations:
-----------------------
>> exclusion_pattern : No exclusion pattern specified
>> custom_code : No custom error reporting code specified
>> hosp_to_validate : NA
>> report_location : C:\Users\<user-name>\AppData\Local\Temp\RtmpYlxVgw\Error_Report17e028254ef5.csv
>> date_var : date_today
>> hosp_var : hosp_id
>> chunksize : 500
>> chunked : TRUE
>> local : FALSE
>> token : <secret-token>
>> api_url : <url-to-host-server>/redcap/api/
```

---

## Data Input

> Before using the object, you need to load data from the REDCap instance. To do this, use the object's ```load_data``` member function. 

### Code

```r
test$load_data()
```

> If during configuration, ```chunked``` is set to ```TRUE``` and a valid ```chunksize``` is specified, then data is downloaded in chunks.


### Output

```
loading chunked data...
downloading data from redcap... (6692 rows!)
downloaded 7.47%
downloaded 14.94%
downloaded 22.41%
downloaded 29.89%
downloaded 37.36%
downloaded 44.83%
downloaded 52.3%
downloaded 59.77%
downloaded 67.24%
downloaded 74.72%
downloaded 82.19%
downloaded 89.66%
downloaded 97.13%
downloaded 100%
records and metadata loaded
```

> Otherwise, data is downloaded in bulk.

### Output

```
loading data in bulk...
records and metadata loaded
```

> If the repository is large, it is preferable to use the chunked download to provide progress updates and to try to prevent timeouts. Otherwise use the bulk download.

> Showing the object shows that the cache has been populated due to some internal events in the object.

### Code

```r
test
```

### Output

```

Instance:
A remote redcap instance running at ‘<url-to-host-server>/redcap’

Memory status:
Cache contains 2 items

Events:
>> records loaded. (hint) use `obj`$get_raw_data() to get raw data.
>> metadata loaded. (hint) use `obj`$get_metadata() to get metadata.

Log:
Timestamp    													Level										Message
2015-01-26 [03:09PM]		***info***		(FUN: load_data) records and metadata loaded


```

---

## Data Formatting

> Most often data is captured as coded values in checkboxes, dropdowns and yesno widgets in REDCap. The data is usually saved as coded values and it is therefore necessary to format the data into factors (data labels) for a more interactive analysis session. To do this we use the object's ```get_formatted_data``` member function to get a copy of the data with data labels plugged in.

### Code

```r
fmt_data <- test$get_formatted_data()
```

### Output

```
formatting data...
formatting done
```

> The object is consequently updated. Show the object to view this.

### Code

```r
test
```

### Output

```

Instance:
A remote redcap instance running at ‘<url-to-host-server>/redcap’

Memory status:
Cache contains 4 items

Events:
>> records loaded. (hint) use `obj`$get_raw_data() to get raw data.
>> metadata loaded. (hint) use `obj`$get_metadata() to get metadata.
>> records formatted. (hint) use `obj`$get_formatted_data() to get data with data labels plugged in (factors).

Log:
Timestamp  														Level										Message
2015-01-26 [03:09PM]		***info***		(FUN: load_data) records and metadata loaded
2015-01-26 [03:25PM]		***info***		(FUN: get_raw_data) raw data accessed
2015-01-26 [03:25PM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-26 [03:25PM]		***info***		(FUN: format_records) data formatted
2015-01-26 [03:25PM]		***info***		(FUN: get_formatted_data) formatted data accessed


```

----

## Data Cleaning

> In order to prevent the use of coded-missing values in analysis, it is necessary to set these to the system's (R) missingness representation (NA) to prevent distorted analysis. To do this use the objects ```get_partially_cleaned_data``` to get a copy of data with the code missing values (for dates, first day of the first month of the current year backdated 100 years, otherwise; -1, Empty, empty, "").

### Code

```r
test$get_partially_cleaned_data()
```

### Output

```
partially cleaning data...
data partially cleaned
```

> The object is consequently updated. Show the object to view this.

### Code

```r
test
```

### Output

```

Instance:
A remote redcap instance running at ‘<url-to-host-server>/redcap’

Memory status:
Cache contains 6 items

Events:
>> records loaded. (hint) use `obj`$get_raw_data() to get raw data.
>> metadata loaded. (hint) use `obj`$get_metadata() to get metadata.
>> records formatted. (hint) use `obj`$get_formatted_data() to get data with data labels plugged in (factors).

Log:
Timestamp  														Level										Message
2015-01-26 [03:09PM]		***info***		(FUN: load_data) records and metadata loaded
2015-01-26 [03:25PM]		***info***		(FUN: get_raw_data) raw data accessed
2015-01-26 [03:25PM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-26 [03:25PM]		***info***		(FUN: format_records) data formatted
2015-01-26 [03:25PM]		***info***		(FUN: get_formatted_data) formatted data accessed
2015-01-26 [03:32PM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-26 [03:32PM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-26 [03:32PM]		***info***		(FUN: partially_clean_records) data partially cleaned
2015-01-26 [03:32PM]		***info***		(FUN: get_partially_cleaned_data) partially cleaned data accessed


```

> Often in data management it is necessary to map the data analysis to the data capture to prevent distortion. To futher remove the ouliers using the validation rules set up during tool design, we use the object's ```get_fully_cleaned_data``` member function.

### Code

```r
cln_data <- test$get_fully_cleaned_data()
```

### Output

```
fully cleaning data...
data fully cleaned
```

> The object is consequently updated. Show the object to view this.

### Code

```r
test
```

### Output

```

Instance:
A remote redcap instance running at ‘<url-to-host-server>/redcap’

Memory status:
Cache contains 8 items

Events:
>> records loaded. (hint) use `obj`$get_raw_data() to get raw data.
>> metadata loaded. (hint) use `obj`$get_metadata() to get metadata.
>> records formatted. (hint) use `obj`$get_formatted_data() to get data with data labels plugged in (factors).

Log:
Timestamp  														Level										Message
2015-01-26 [04:19PM]		***info***		(FUN: load_data) records and metadata loaded
2015-01-26 [04:19PM]		***info***		(FUN: get_raw_data) raw data accessed
2015-01-26 [04:19PM]		***info***		(FUN: get_raw_data) raw data accessed
2015-01-26 [04:19PM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-26 [04:19PM]		***info***		(FUN: format_records) data formatted
2015-01-26 [04:19PM]		***info***		(FUN: get_formatted_data) formatted data accessed
2015-01-26 [04:19PM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-26 [04:19PM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-26 [04:19PM]		***info***		(FUN: partially_clean_records) data partially cleaned
2015-01-26 [04:19PM]		***info***		(FUN: get_partially_cleaned_data) partially cleaned data accessed
2015-01-26 [04:19PM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-26 [04:19PM]		***info***		(FUN: fully_clean_records) data fully cleaned
2015-01-26 [04:19PM]		***info***		(FUN: get_fully_cleaned_data) fully cleaned data accessed


```

---

## Error reporting

> To validate the data entered during capture, it is necessary to generate an error report since REDCap allows for soft validations. This can be used after data entry (eg daily) to verify whether data captured repected the validation rules set at tool design. To create this report use the objects ```get_error_report``` member function. It provides an option whether to pop the file open using the default application set by the OS. This is only applicable in windowed environments.

### Code

```r
test$get_error_report(pop = TRUE)
```

> If, during configuration, ```chunked``` is set to ```TRUE``` and a valid chunksize is specified, then the repository is validated in chunks just as in data input.

### Output

```
generating error report code...
cleaning metadata...
metadata cleaned
error report code generated
generating report. This might take a while...
validated 7.43%...
validated 14.85%...
validated 22.28%...
validated 29.71%...
validated 37.14%...
validated 44.56%...
validated 51.99%...
validated 59.42%...
validated 66.84%...
validated 74.27%...
validated 81.7%...
validated 89.13%...
validated 96.55%...
validated 100%
report generated
Error report saved to ‘C:\Users\<user-name>\AppData\Local\Temp\RtmpeENmPq\Error_Reportf544a5f709e.csv’
```

> Else validation is done in bulk

### Output

```

generating error report code...
cleaning metadata...
metadata cleaned
error report code generated
generating report. This might take a while...
report generated
Error report saved to ‘C:\Users\<user-name>\AppData\Local\Temp\RtmpmmiNDF\Error_Report128c7fcd5a04.csv

```

> The object is consequently updated. Show the object to view this.

### Code

```r
test
```

### Output

```


Instance:
A remote redcap instance running at ‘http://10.0.10.188/redcap’

Memory status:
Cache contains 11 items

Events:
>> records loaded. (hint) use `obj`$get_raw_data() to get raw data.
>> metadata loaded. (hint) use `obj`$get_metadata() to get metadata.
>> records formatted. (hint) use `obj`$get_formatted_data() to get data with data labels plugged in (factors).
>> metadata munged. (for internal use - code generation)
>> error report code in memory.  (hint) use `obj`$get_error_report() to get error report.
>> error report created. (hint) use `obj`$get_error_report() to get error report.

Log:
Timestamp  														Level										Message
2015-01-27 [10:57AM]		***info***		(FUN: load_data) records and metadata loaded
2015-01-27 [10:57AM]		***info***		(FUN: get_raw_data) raw data accessed
2015-01-27 [10:57AM]		***info***		(FUN: get_raw_data) raw data accessed
2015-01-27 [10:57AM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-27 [10:57AM]		***info***		(FUN: format_records) data formatted
2015-01-27 [10:57AM]		***info***		(FUN: get_formatted_data) formatted data accessed
2015-01-27 [10:57AM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-27 [10:57AM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-27 [10:57AM]		***info***		(FUN: partially_clean_records) data partially cleaned
2015-01-27 [10:57AM]		***info***		(FUN: get_partially_cleaned_data) partially cleaned data accessed
2015-01-27 [10:57AM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-27 [10:57AM]		***info***		(FUN: fully_clean_records) data fully cleaned
2015-01-27 [10:57AM]		***info***		(FUN: get_fully_cleaned_data) fully cleaned data accessed
2015-01-27 [10:57AM]		***info***		(FUN: get_raw_data) raw data accessed
2015-01-27 [10:57AM]		***info***		(FUN: get_metadata) metadata accessed
2015-01-27 [10:57AM]		***info***		(FUN: get_clean_metadata) metadata cleaned
2015-01-27 [10:57AM]		***info***		(FUN: get_clean_metadata) clean metadata accessed
2015-01-27 [10:57AM]		***info***		(FUN: report_errors) error report code generated
2015-01-27 [10:57AM]		***info***		(FUN: report_errors) error report function in memory
2015-01-27 [11:06AM]		***info***		(FUN: report_errors) error report created
2015-01-27 [11:06AM]		***info***		(FUN: get_error_report) error report accessed


```
