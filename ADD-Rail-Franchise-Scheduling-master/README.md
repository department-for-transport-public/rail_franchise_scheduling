# Automated Rail Franchise Scheduling Service
---

## About
This product reads in an incomplete rail schedule [datasheet](#input_datasheet) and key parameters. It returns:

* Interactive rail franchise table with completed milestone date.
* Key milestone chart of the rail franchise schedule (RFS).
* Monthly and annual profile for expenses and staff resources.
* Summary facts of the baseline schedule e.g. average phase period, annual ITT, RfP, EoI, and PIN profiles.

It is also able to read in user amendments to the schedule and produce a modified milestone chart and also includes comparative statistics on the alternative schedule.

## Project Structure
Folders 📁 and files 📄.

``` 
📁 ADD-rail-franchise-service/
> 📄 rail_franchise.Rproj
> 📄 packrat.lock
> 📄 00_data_processing.R
> 📄 01_changes_functions.R
> 📄 02_changes_run.R
> 📄 03_export_visio_table.R
> 📄 10_build_datatable.R
> 📄 11_build_facts.R
> 📄 12_build_milestone.R
> 📄 13a_build_resource_and_finance_table.R
> 📄 13b_build_resource_and_finance_plot.R
> 📄 20_prep_markdown.R
> 📄 30_report_markdown.R
📁 tests/testthat/
📁 packrat
📁 data
>📄 log.txt
> 📁 inputs
 > 📄 schedule_input.xlsx
> 📁 intermediates
 >📄 schedule_baseline.rds
 >📄 schedule_baseline_changed.rds
> 📁 outputs
 >📄 schedule_baseline.csv
 >📄 schedule_baseline_change.csv
 >📄 schedule_visio.csv
 >...Annual and monthly data tables for adviser spend/resources/milestone dates 
> ...
> 📄 README.md
```
## Getting started
### Set up user guide (for the first time)
1. Open up your input `schedule_input.xlsx` and ensure that:
   * each franchise has a start date in `Input sheet` tab with the format `dd/mm/yyyy`for at least all green boxes.
   * all values in the `key parameter` tab are completed.
2. Click on the `Clone or Download` button in the Github [repository](https://github.com/departmentfortransport/ADD-Rail-Franchise-Scheduling) and download the project as a `.zip` file.

#### Cloud R
3. Unzip and move the folder into your personal area in Cloud R. 
4. In the new folder open the folder `data/inputs` and upload the input data sheet `schedule_input.xlsx`
5. Renv: in the Cloud R console make sure you have renv installed `install.packages('renv')`
6. Link the code and data you pulled over from GitHub into a R project in Cloud R by selecting `File`- `New Project` - `Existing Directory` - search your files to select the folder you set up in step 3. 
7. Renv: Dowload everything the code needs to run by pasting  and running `renv::restore()` in the command line. If you have issues downloading packages contact Sally Walsh who can point you to some code to fix package installs from the C&C teams wiki.
8. Open and run the `📄 30_report_markdown.Rmd` file by clicking the blue knitting ball icon to using the shortcut `Ctrl+Shift+K` to build the dashboard.
9. A new window should pop up with a preview. To view it properly click the `Open in browser` icon at the top, or open the `📄 30_report_markdown.html` file in the `📁 ADD-rail-franchise-service/` directory to view the dashboard using Chrome.

### Set up user guide (for updates)
1. Edit the data in the `schedule_input.xlsx` to match your new requirements. Changes need to be made directly to the version of the input data that you set up in step 1 above.
2. In Windows, open the folder where all of the code is stored and click to open `rail_franchising.rproj`
3. Once you are in, open, select and open the file `30_report_markdown.R` from the file pane on the bottom right.
4. Follow instructions 7/8 from above to run the new dashboard.

### How to use the schedule_input sheet
This is where you put in all the past data, future data and parameters you want the dashboard to consider. Once you have updated this sheet in sharepoint please contact the analytics unit to rerun the dashboard and send you the new output.

* **Input sheet**
This sheet contains all historical and known future milestones. The dashboard will fill in the blanks going forwards so the only information you have to input here is:  
Historical data: Fill in all orange and green columns


### <a id="user_changes"></a>How to build an 'alternative baseline' table
There are several types of changes you can 'try on' to build an alternative schedule to compare against the baseline. The baseline is protected and will not be changed by these user changes.

#### Changes to all future events in the schedule
Change to the whole future schedule set, can be altered by changing the default values in `schedule_input.xlsx` `Assumptions - Alt` tab. Changing the values here rebuilds the alternative schedule with different:
* Phase length gaps between milestones
* Default contract lengths
* Default available rail period extentions for a typical contract
* Change the assumptions that drive the 'ideal contract length' calculator

#### Changes to individual contract details
To make changes at an individual contract level, users can add individual changes they would like to make on the `schedule_input.xlsx` `User changes` tab.
> Changes are applied sequentially - keep this is mind if you are trying to edit multiple iterations for one franchise.
The types of changes that can be made are:
* Changing the date of a start date milestone in the future
* Adding in DA
* Using addtional rail periods to extend particular contracts
* Change baseline contract length for particular contracts

### Outputs
After running the code these file can be found in the `/data` folder:
* `test_output_data.csv` / `.rds`  - This is the fully completed baseline schedule based on the input table provided. The .csv version is for superusers to take a copy of. The .rds one is for the code to process later.
* `test_output_data_changed.csv` / `.rds`  - This is the fully completed baseline schedule based using the alternate assumptions sheet and the input table provided. The .csv version is for superusers to take a copy of. The .rds one is for the code to process later.
* `sched_for_visio.csv` - This is a version of the fully completed baseline rearranged to feed directly into MSVisio, including pixel lengths. Note that it does not include any direct award contracts and these must be added by hand.

After running the code the dashboard html can be found in the main folder:
* `30_report_markdown.html` - This is the dashboard file. You can take a copy of it to share with others, who can view it through Chrome.


## Structure

### <a id="input_datasheet"></a>User input datasheet
This excel [template](https://departmentfortransportuk.sharepoint.com/:x:/s/ASD/EcPpuygNt55HlilUmP8bG0wBjhx6b-Q7HOTBmHjRwQhlxQ?e=NdRwgH) consists of 4 tabs:

1. **Input sheet** - incomplete RFS table that **must** include the first start date for each rail franchise.
2. **Changes** - In this version, you can test an alternate schedule by editing a version of the baseline schedule in a seperate .csv sheet. This will be updated to a simpler to use feature in the changes tab in this document in a future version.
3. **Lookup** - convert dates to a rail period number.
4. **Assumptions** - parameters that define RFS regulations, DA rules, and auto-fills missing milestones.
5. **Resource Assumptions** - parameters that define the costs/resources for each project phase.
6. **Assumptions - Alt** - Alternative parameters to define preferred contract length and phase lengths. This will be compared to the baseline in the resources and finance tab of the dashboard and as a alterative milestone chart.

> Warning: if the input datasheet does not provide an initial `Franchise start date` for each rail franchise, the program will not run.
![code diagram](https://github.com/departmentfortransport/ADD-Rail-Franchise-Scheduling/blob/dev/data/inputs/code_flow.png)

### Pre-process Report
All the scripts that build key features, such as, [milestone chart](#build_milestone), [pre-processing the RFS table](#preprocess_rfs_table),  and the [interactive RFS table](#int_rfs_table) are executed in the [`20_prep_markdown.R`](1_2_prep_markdown.R) script. 

Global variables like file paths to the input datasheet are also defined here and can be used throughout the project.

### <a id="preprocess_rfs_table"></a>Pre-process RFS table
The input data is pre-process in [`00_data_processing.R`](00_data_processing.R). This will impute any missing values, reshape the input data into a suitable baseline RFS table and store it as a `.rds` file.


### <a id="preprocess_rfs_table"></a> Changes functions / run
The changes_function file contains all the coded functions that are needed to make the alternative schedule based on user changes.  
In the changes_run file these changes are actually applied to the data and the new schedule is built. The new schedule can be viewed as a csv in the `data/outputs` file, can be viewed as a GANTT milestone plot in the dashboard tab `User changed: Milestones` and also its comparison statisitics appear in the `Finance and resourcing` tab on the dashboard.


### <a id="build_milestone"></a>Build milestone chart
The [`13_build_milestone.R`](13_build_milestone.R) script reads in RFS table and returns:

1. A milestone table - each franchise-set will have a milestone event e.g. PIN, Award, and a corresponding start date for that event.
2. Interactive milestone figure - Franchise and set displayed on the y-axis and time on the x-axis. Feature include:
   * 4 months, 1yr, 10 yr, all timescale buttons.
   * Timeline scroll.
   * Milestone filters (inc. emergency and planned DAs).
   * ROI focus.
   * Export figure as `.png`.
   
 There is another chart `User changed: Milestones` to show the altered schedule that has been proposed.

### <a id="int_rfs_table"></a>Build RFS table
Once the RFS table has been completed the [`10_build_datatable.R`](10_build_datatable.R) script outputs an interactive table for the report. 

Features include:

* Bold text for fixed dates.
* Blue text for modified dates.
* Table-wide search bar.
* Column-wide filters.
* Export/copy table.
* DA filters.

### <a id="factbank"></a>Project Phases
Summary statistics and charts from the RFS table will be displayed in the facts bank tab. The milestone profile charts are built in the [`13_build_finance_and_resource.R`](13_build_finance_and_resource.R) script. The statistics table is built in the [`11_build_facts.R`](11_build_facts.R) script.

Features include:

* Quick calculator on ideal contract length given assumptions on number of franchises and ideal spacing.
* Average phase length table which is separated by direct awards and franchise competition.
* Annual milestone profiles e.g. ITT, EoI, RfP, PIN profiles.

### <a id="res_fin_table"></a>Build resource and finance table
Annual and monthly forecasts can be monitored providing the user has completed the resource assumption tab in the [input datasheet](#input_datasheet). This is built in the [`13_build_finance_and_resource.R`](13_build_finance_and_resource.R) script. If the user has made [changes](#user_changes) to the baseline RFS table, the charts will show a comparison of the two schedules.

Features include:

* Monthly and annual profile of the number of work staff required.
* Monthly and (financial) annual profile of spending required.

### Output report
All previous work is pulled together in a dashboard written in RMarkdown. When running this file [`20_report_markdown.Rmd`](20_report_markdown.R) generates an interactive dashboard in HTML that can be shared with others.  
This output should not be shared with those who do not have explicit permission to access this data.

Features include:

* Intro and details tab.
* Interactive data table as above.
* Interactive milestone plot as above.

### Tests
Are available in test/testthat/ folder. There is one test file for each core R script, but they are not currently set up to run automatically. They can be run manually using using `testthat::test_dir()`.


## License
Proprietary until agreement from the above at DfT.
