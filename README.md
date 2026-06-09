# envloggers

## 2026-06-09

Finished processing all envloggers by location, see folder '1_process_env_location'

Intertidal
  - Scripts for intertidal sensors include hewatt or agassiz in the file name
  - Things to note when updating files
  	- add csv files into appropriate env_logger folder (by serial #)
  	- my_location
  	- check paths for csv logger files
  	- new logger files (after 2025) need to skip the first 21 lines (not 20) to get to the appropriate time and temperature column headings
  	- quality control: check to make sure there aren't duplicates files (file_i)
  - Because these loggers are downloaded in place, and then restarted in place, each individual file doesn't have extraneous data and all if it is useable

Subtidal
  - Scripts for intertidal sensors include cable or surface in the file name
  - Things to note when updating files (in addition to things noted above for the intertidal)
    - Because loggers must be removed from their subtidal location to be downloaded, there will be extreme temperatures related to exposure in air - these must be carefully inspected and filtered!
    - General approach to filtering these irrelevant data
      - remove data points before 12pm on the first day of recording (use head(d) to get first day)
      - remove data points after 9am on the last day of recording (use tail(d)), but can be sooner if the logger was allowed to record for more than a few hours after the collection dive
    - Each deployment must be hand-checked, and then combined into the final cleaned file for saving

Steps for updating:
  - Add the logger csv file to the appropriate folder
  - Update the appropriate process_env_xxx script in process_env_location folder
  - Save the compiled file into the data_output folder, and save a copy into gDrive > currentResearch > hms_sensors_compiled

For downstream analysis:
  - Sometimes, different env_loggers were used in the same location. Some comparison of different loggers should be completed to ensure that different loggers have similar accuracy and precision. Use overlapping deployments in the field, or occasional calibration attempts in the lab. 
  - Some subtidal locations are very close to each other, and it may be useful to combine them. For example, cable_090 and cable_100; cable_140 and cable_170. Before combining, some investigation is warranted. 
