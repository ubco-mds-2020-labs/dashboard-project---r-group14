# Milestone 2 Reflection

## General

The Board Game Analysis Dashboard is currently at a stage where it includes the functionality discussed in the proposal. However, it still requires layout and aesthetic optimization, additional in-app user documentation, and code clean-up/optimization to provide a polished product. This is similar to the state of the dashboard from Milestone 2 as this milestone involved translating the Python dashboard to R.

The features implemented, additional work required, and proposed enhancements are effectively the same as the descriptions in the `reflection-milestone2.md` document which is found [here](https://github.com/ubco-mds-2020-labs/dashboard-project-group14/blob/main/docs/reflection-milestone2.md). Updates to be made to the Python dashboard are described [here](https://github.com/ubco-mds-2020-labs/dashboard-project-group14/issues).

A general discussion on translation of the Python dashboard to R is provided below.

## Discussion of Translation to R

The translation of the Python dashboard to R ended up being a fair amount of work for our group. The Board Game Analysis Dashboard has a large amount of on-the-fly data wrangling to provide the ability to explore the dataset using various filters and summarizations. The `app_wrangling.py` file provides all of the wrangling functions required by the dashboard and had to be translated to `app_wrangling.R`. On the positive side, this provided the opportunity to clean-up some of the wrangling logic which our group did not have time to do in Milestone 2. The logic updates were also implemented in the `app_wrangling.py` file and a [PR](https://github.com/ubco-mds-2020-labs/dashboard-project-group14/pull/56) was completed for the Python dashboard as well.

We ran into a few challenges with trying to get Dash-R to plot our graphs correctly which resulted in longer than expected troubleshooting but ultimately were able to get the Milestone 3 R dashboard at the same state as the Milestone 2 Python dashboard. We did not have time to work on the enhancements discussed in the previous `reflection-milestone2.md` document.

## Moving Forward

Our Team has decided to move forward with the Python Dashboard in Milestone 4. We are making this decision for a couple of reasons:

- We feel that Dash in Python is a bit more polished than Dash-R based on the issues we ran into trying to get the R dashboard to work.
- The enhancements we started on in Milestone 2 are all written in Python and it would take additional effort to translate the enhancements to R.

The one benefit of our dashboard in R is that it is currently a little more responsive than the dashboard in Python. This is likely due to the difference in the ggplot and Altair packages; however, we have solutions such as reducing the data sent to the Altair plots and minimizing any calculations completed by Altair that we expect will increase the performance of the Python Dashboard.
