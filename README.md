# R-ACS-Maps
Here you will find examples of using R to create county-by-county maps with data from the American Community Survey (ACS), a survey product of the US Census Bureau. The ACS provides researchers, businesses, and policy makers a rich source of economic, social, and demographic information about the US population. These files are intended to show interested users how they can access ACS data using the Census API and map variables over US geographies, with modest transformations of the source data. The files are generally self contained in that they grab all of the informaiton they need on the fly from the Census, except for the shapefiles that are permanently stored after retrieval.

The ACS collects information from around 3 million households annually, about 1% of the population, on a rolling month-by-month basis. Every year a 1-year and 5-year file are released covering either the previous calendar year or previous five calendar years, respectively. The 5-year data sample enough households across the US that the Census is able to release estimates at the Census Block level, the smallest geographic area measured. This makes the ACS 5-year data the most current and comprehensive data source available about American households, which allows users to assess changes in the US population between Decennial Census years.

Overtime I intend to add sample code that will look at the state and sub-state level and cover a wider variety of topics from the ACS. In the meantime, you can read more information about the ACS here: https://www.census.gov/programs-surveys/acs/guidance.html.
