# WA-FEWP
This repository contains data and scripts used to track the people killed during fatal encounters with police in Washington State since 2015.
These are used to produce an [online report](https://rpubs.com/moxbox/wa_since2015) that is updated weekly.

The dataset draws from two actively updating sources: 

* [Mapping Police Violence](https://mappingpoliceviolence.us/) -- 2013+ 

* [Incarcernation](https://incarcernation.com/) -- 2021+

It incorporates data from two legacy projects:

* the [Fatal Encounters Project](https://fatalencounters.org/) -- 2000-2021

* the [Washington Post data on people killed by police](https://www.washingtonpost.com/graphics/investigations/police-shootings-database/) -- 2015-2025

And it relies on a local dataset maintained by the repository owner for WA fatalities not included elsewhere,
and for WA specific information on independent investigations and medical examiner reports.  [This local dataset](https://github.com/nextstepswa/WA-FEWP/tree/main/Data/Raw) is included in the repository.

To replicate the online report:

* execute the [`MakeData.R`](https://github.com/nextstepswa/WA-FEWP/blob/main/Construction/MakeData.R) script in the `Construction` folder.

* knit the [`WAsince2015.Rmd`](https://github.com/nextstepswa/WA-FEWP/blob/main/Analyses/WAsince2015.Rmd) script in the `Analyses` folder.

If you have questions or find a bug in the code, please file an issue in this repository, thanks!