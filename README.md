
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Sociodemographic characteristics of missing data in digital phenotyping

<p align="center">
<img src="./output/fig2_dist_daily_non_collection.jpg" width="700px" style="display: block; margin: auto;" />
</p>

## Introduction

Analytic code, model results, and documentation for our paper,
[*Sociodemographic Characteristics of Missing Data in Digital
Phenotyping*](TODO). The full citation is:

> Kiang MV, Chen JT, Krieger N, Buckee CO, Alexander MJ, Baker JT,
> Buckner RL, Coombs III G, Rich-Edwards JW, Carlson KW, and Onnela JP.
> Sociodemographic characteristics of missing data in digital
> phenotyping. Forthcoming.

### A note about reproducibility

As we note in our Supplementary Information, the data used in these
analyses were *meta*data and contained only timestamps (e.g., the date
and time of a GPS ping but not the coordinates); however, the timestamps
of participants can be considered personally identifiable information.
Therefore, to minimize the potential for participant harm and
re-identification, the data are not shared publicly. Data available upon
request, contingent upon appropriate IRB approvals or exemptions from
participating institutions. While not the raw data, these data will
provide sufficient information to reproduce our results (e.g., using
shifted and/or adding noise to timestamps, re-randomized user
identifiers).

In addition, we provide example replication code, along with
documentation, in this online repository. The code and documentation are
near exact copies of the code used in this project with only minor
differences. Specifically, for this paper, we use internal study project
names which may include a year and/or month. Out of an abundance of
caution, we remove any references to these study names. However, the
code is otherwise the same. See the documentation for more information.

## Structure

-   `code`: Contains code files to be run in sequential order. See
    documentation for details.
-   `data_raw` (not on Github): Contain raw data collected using the
    Beiwe Research Platform.
-   `data_stripped` (not on Github): Contain summarized data collected
    using the Beiwe Research Platform.
-   `data_working` (not on Github): Contain working data used for plots
    and analysis.
-   `model_objects` (not on Github): Contain the RStan/brms model
    objects after fitting.
-   `output`: Contains all plots, tables, and relevant supplementary
    information.
-   `rmds`: Contains the source (i.e., rmarkdown) files for
    supplementary information.

The `config.yml` file allows you to change the path of the data files
above as well as specify the number of cores to use in your computing
environment.

## Supplementary information

-   [Documentation for the
    code](https://github.com/mkiang/beiwe_missing_data/blob/master/rmd/supp_documentation_code_walkthrough.html)
    including expected data input and output
    ([source](https://github.com/mkiang/beiwe_missing_data/blob/master/rmd/supp_documentation_code_walkthrough.Rmd)).
-   [Results from both the primary model and sensitivity
    analyses](https://github.com/mkiang/beiwe_missing_data/blob/master/rmd/supp_model_output_cleaned.html)
    ([source](https://github.com/mkiang/beiwe_missing_data/blob/master/rmd/supp_model_output_cleaned.Rmd)).
-   A fork of this repository can be found on the [onnela-lab
    account](https://github.com/onnela-lab/beiwe_missing_data).
-   The Beiwe Research Platform is open source. These are the links for
    the [backend](https://github.com/onnela-lab/beiwe-backend),
    [iOS](https://github.com/onnela-lab/beiwe-ios), and
    [Android](https://github.com/onnela-lab/beiwe-android) repositories.
    To learn more about Beiwe, see [beiwe.org](https://www.beiwe.org).
