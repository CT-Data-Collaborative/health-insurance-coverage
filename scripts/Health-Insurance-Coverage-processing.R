library(dplyr)
library(devtools)
load_all('../datapkg')
library(datapkg)
library(acs)
library(stringr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Health Insurance Coverage
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

# ACS B27001, B27002, B27003
options(scipen=999)
dataset <- data.table()

# Iterate through years
for (year in c(2012:2018)) {
    year.stub = substr(as.character(year), 3, 4)
    # No Health insurance
    file <- paste("ACS_", year.stub, "_5YR_B27001_with_ann.csv", sep = "")
    without <- read.acs(
        file.path(getwd(), "raw", file),
        endyear = year,
        span = 5,
        geo = 1:3
    )
    # private
    file <- paste("ACS_", year.stub, "_5YR_B27002_with_ann.csv", sep = "")
    private <- read.acs(
        file.path(getwd(), "raw", file),
        endyear = year,
        span = 5,
        geo = 1:3
    )
    # public
    file <- paste("ACS_", year.stub, "_5YR_B27003_with_ann.csv", sep = "")
    public <- read.acs(
        file.path(getwd(), "raw", file),
        endyear = year,
        span = 5,
        geo = 1:3
    )

    # Start with totals. used for denominators, not reported
    total = without[, 1]
    total.under18 = acsSum(without, c(3, 6, 31, 34), "total.under6")
    total.1864 = acsSum(without, c(seq(9, 21, 3), seq(37, 49, 3)), "total.1864")
    total.65over = acsSum(without, c(24, 27, 52, 55), "total.65over")

    # no health insurance
    # Numbers
    without.total = acsSum(without, c(seq(5, 29, 3), seq(33, 57, 3)), "All:without health insurance")
    without.under18 = acsSum(without, c(5, 8, 33, 36), "under 18:without health insurance")
    without.1864 = acsSum(without, c(seq(11, 23, 3), seq(39, 51, 3)), "18-64:without health insurance")
    without.65over = acsSum(without, c(26, 29, 54, 57), "65+:without health insurance")
    # Percents
    percent.without.total = divide.acs(without.total, total, method = "proportion", verbose = T)
    percent.without.under18 = divide.acs(without.under18, total.under18, method = "proportion", verbose = T)
    percent.without.1864 = divide.acs(without.1864, total.1864, method = "proportion", verbose = T)
    percent.without.65over = divide.acs(without.65over, total.65over, method = "proportion", verbose = T)


    # public health insurance
    # Numbers
    public.total = acsSum(public, c(seq(4, 28, 3), seq(32, 56, 3)), "All:with public health insurance")
    public.under18 = acsSum(public, c(4, 7, 32, 35), "under 18:with public health insurance")
    public.1864 = acsSum(public, c(seq(10, 22, 3), seq(38, 50, 3)), "18-64:with public health insurance")
    public.65over = acsSum(public, c(25, 28, 53, 56), "65+:with public health insurance")
    # Percents
    percent.public.total = divide.acs(public.total, total, method = "proportion", verbose = T)
    percent.public.under18 = divide.acs(public.under18, total.under18, method = "proportion", verbose = T)
    percent.public.1864 = divide.acs(public.1864, total.1864, method = "proportion", verbose = T)
    percent.public.65over = divide.acs(public.65over, total.65over, method = "proportion", verbose = T)

    # private health insurance
    # Numbers
    private.total = acsSum(private, c(seq(4, 28, 3), seq(32, 56, 3)), "All:with private health insurance")
    private.under18 = acsSum(private, c(4, 7, 32, 35), "under 18:with private health insurance")
    private.1864 = acsSum(private, c(seq(10, 22, 3), seq(38, 50, 3)), "18-64:with private health insurance")
    private.65over = acsSum(private, c(25, 28, 53, 56), "65+:with private health insurance")
    # Percents
    percent.private.total = divide.acs(private.total, total, method = "proportion", verbose = T)
    percent.private.under18 = divide.acs(private.under18, total.under18, method = "proportion", verbose = T)
    percent.private.1864 = divide.acs(private.1864, total.1864, method = "proportion", verbose = T)
    percent.private.65over = divide.acs(private.65over, total.65over, method = "proportion", verbose = T)

    # merge in fips
    datafips <- data.table(fips = getACSFips(without))

    # Cast to separate data frames
    numberEstimates <- data.table(
        datafips$fips,
        estimate(without.total),
        estimate(without.under18),
        estimate(without.1864),
        estimate(without.65over),
        estimate(public.total),
        estimate(public.under18),
        estimate(public.1864),
        estimate(public.65over),
        estimate(private.total),
        estimate(private.under18),
        estimate(private.1864),
        estimate(private.65over),
        paste(year-4, year, sep = "-"),
        "Number",
        "Health Insurance"
    )
    numberMOES <- data.table(
        datafips$fips,
        standard.error(without.total) * 1.645,
        standard.error(without.under18) * 1.645,
        standard.error(without.1864) * 1.645,
        standard.error(without.65over) * 1.645,
        standard.error(public.total) * 1.645,
        standard.error(public.under18) * 1.645,
        standard.error(public.1864) * 1.645,
        standard.error(public.65over) * 1.645,
        standard.error(private.total) * 1.645,
        standard.error(private.under18) * 1.645,
        standard.error(private.1864) * 1.645,
        standard.error(private.65over) * 1.645,
        paste(year-4, year, sep = "-"),
        "Number",
        "Margins of Error"
    )
    numberNames <- c(
        "FIPS",
        "All:without health insurance",
        "under 18:without health insurance",
        "18-64:without health insurance",
        "65+:without health insurance",
        "All:with public health insurance",
        "under 18:with public health insurance",
        "18-64:with public health insurance",
        "65+:with public health insurance",
        "All:with private health insurance",
        "under 18:with private health insurance",
        "18-64:with private health insurance",
        "65+:with private health insurance",
        "Year",
        "Measure Type",
        "Variable"
     )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
        rbind(numberEstimates, numberMOES),
        id.vars = c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name = "Insurance Status",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
     )

    percentEstimates <- data.table(
        datafips$fips,
        estimate(percent.without.total),
        estimate(percent.without.under18),
        estimate(percent.without.1864),
        estimate(percent.without.65over),
        estimate(percent.public.total),
        estimate(percent.public.under18),
        estimate(percent.public.1864),
        estimate(percent.public.65over),
        estimate(percent.private.total),
        estimate(percent.private.under18),
        estimate(percent.private.1864),
        estimate(percent.private.65over),
        paste(year-4, year, sep = "-"),
        "Percent",
        "Health Insurance"
    )
    percentMOES <- data.table(
        datafips$fips,
        standard.error(percent.without.total) * 1.645,
        standard.error(percent.without.under18) * 1.645,
        standard.error(percent.without.1864) * 1.645,
        standard.error(percent.without.65over) * 1.645,
        standard.error(percent.public.total) * 1.645,
        standard.error(percent.public.under18) * 1.645,
        standard.error(percent.public.1864) * 1.645,
        standard.error(percent.public.65over) * 1.645,
        standard.error(percent.private.total) * 1.645,
        standard.error(percent.private.under18) * 1.645,
        standard.error(percent.private.1864) * 1.645,
        standard.error(percent.private.65over) * 1.645,
        paste(year-4, year, sep = "-"),
        "Percent",
        "Margins of Error"
    )
    percentNames <- c(
        "FIPS",
        "All:without health insurance",
        "under 18:without health insurance",
        "18-64:without health insurance",
        "65+:without health insurance",
        "All:with public health insurance",
        "under 18:with public health insurance",
        "18-64:with public health insurance",
        "65+:with public health insurance",
        "All:with private health insurance",
        "under 18:with private health insurance",
        "18-64:with private health insurance",
        "65+:with private health insurance",
        "Year",
        "Measure Type",
        "Variable"
     )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)

    percentsData.melt <- melt(
        rbind(percentEstimates, percentMOES),
        id.vars = c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name = "Insurance Status",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
     )

    dataset <- rbind(dataset, numbersData.melt, percentsData.melt)
}

#Final Additions, processing
# Split Age Range and Insurance Status out of "Insurance Status" column
dataset[,c("Age Range", "Insurance Status"):=do.call(Map, c(f = c, strsplit(`Insurance Status`, ":", fixed = T)))]

# Round Values according to type/variable
dataset[`Measure Type` == "Number", Value := round(Value, 0)]
dataset[`Measure Type` != "Number", Value := round(Value*100, 1)]

# Join town names by FIPS code
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

dataset <- merge(towns, dataset, by = "FIPS", all.x=T)

dataset$`Insurance Status` <- str_to_title(dataset$`Insurance Status`)
dataset$`Age Range` <- str_to_title(dataset$`Age Range`)

dataset$`Age Range` <- factor(dataset$`Age Range`, levels = c("All", "Under 18", "18-64", "65+"))

#set final column order
dataset <- dataset %>% 
  select(Town, FIPS, Year, `Age Range`, `Insurance Status`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Age Range`, `Insurance Status`, `Measure Type`, Variable) 

# Write to File
write.table(
    dataset,
    file.path(getwd(), "data", "health-insurance-coverage-2018.csv"),
    sep = ",",
    row.names = F,
    na = "-9999"
)
