
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Sociodemographic characteristics of missing data in digital phenotyping

<p align="center">
<img src="./output/fig2_dist_daily_non_collection_prop.jpg" width="700px" style="display: block; margin: auto;" />
</p>

## Introduction

Analytic
[code](https://github.com/mkiang/beiwe_missing_data/tree/master/code),
[model
results](http://htmlpreview.github.io/?https://github.com/mkiang/beiwe_missing_data/blob/master/rmd/supp_model_output_cleaned.html),
and
[documentation](http://htmlpreview.github.io/?https://github.com/mkiang/beiwe_missing_data/blob/master/rmd/supp_documentation_code_walkthrough.html)
for our *Scientific Reports* paper, [*Sociodemographic Characteristics
of Missing Data in Digital
Phenotyping*](https://doi.org/10.1038/s41598-021-94516-7). The full
citation is:

> Kiang MV, Chen JT, Krieger N, Buckee CO, Alexander MJ, Baker JT,
> Buckner RL, Coombs III G, Rich-Edwards JW, Carlson KW, and Onnela JP.
> Sociodemographic characteristics of missing data in digital
> phenotyping. *Scientific Reports* (July 2021). doi:
> [10.1038/s41598-021-93687-7](https://doi.org/10.1038/s41598-021-94516-7)

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

### Preprint

This paper originally appeared as a [preprint on
*medRxiv*](https://www.medrxiv.org/content/10.1101/2020.12.29.20249002v1)
(doi:
[10.1101/2020.12.29.20249002v1](https://www.medrxiv.org/content/10.1101/2020.12.29.20249002v1)).
The code affiliated with this preprint can be found [at this
commit](https://github.com/mkiang/beiwe_missing_data/tree/faba924f8ee75abd895dcc69c0f5c5c62b4eda12).

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

-   [Documentation](http://htmlpreview.github.io/?https://github.com/mkiang/beiwe_missing_data/blob/master/rmd/supp_documentation_code_walkthrough.html)
    for the code, expected data input and output, and [supplementary
    results](http://htmlpreview.github.io/?https://github.com/mkiang/beiwe_missing_data/blob/master/rmd/supp_model_output_cleaned.html)
    can be found here.
    ([Source](https://github.com/mkiang/beiwe_missing_data/tree/master/rmd).)
-   A fork of this repository can be found on the [onnela-lab
    account](https://github.com/onnela-lab/beiwe_missing_data).
-   The Beiwe Research Platform is open source. These are the links for
    the [backend](https://github.com/onnela-lab/beiwe-backend),
    [iOS](https://github.com/onnela-lab/beiwe-ios), and
    [Android](https://github.com/onnela-lab/beiwe-android)
    respositories. To learn more about Beiwe, see
    [beiwe.org](https://www.beiwe.org).
