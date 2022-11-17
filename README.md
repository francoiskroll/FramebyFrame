# FramebyFrame R package

Francois Kroll, 2022 @Rihel lab, UCL.

[![alt text][1.2]][1] [@francois_kroll](https://twitter.com/francois_kroll)

:email: francois@kroll.be

<!-- icons with padding -->
[1.1]: http://i.imgur.com/tXSoThF.png (twitter icon with padding)

<!-- icons without padding -->
[1.2]: http://i.imgur.com/wWzX9uB.png (twitter icon without padding)

<!-- links to your social media accounts -->
[1]: https://twitter.com/francois_kroll


> —"_I do not know anything about R so this package is not for me._".  
I wrote everything with that in mind, I promise. The amount of R (or any coding language) you need to know in order to run a complete analysis is minuscule, and I wrote it all in section **R basics** below.

___

## Installation

Installation requires the package `devtools`. If you do not have it already, first run in R:

    install.packages("devtools")

Then:

    # load the package devtools
    library(devtools)

    # install the package FramebyFrame from the present repo
    install_github("francoiskroll/FramebyFrame")

    # load FramebyFrame
    library(FramebyFrame)


And you should be good to go!

To be clear, you do not need to re-install the package every time you open R. Next time, just load it with:

    library(FramebyFrame)

But I recommend that you run the installation again once in a while so I can update things.

___

## Minimal tutorial

Ready? Here is the quickest possible tutorial of a FramebyFrame analysis. The package can do more than what is presented here, so make sure to check the full documentation once you got the gist of it.

I will assume you have R and RStudio installed. If not, have a look at section **R basics**.

### 1– Get your frame-by-frame data from Zebralab

> "_What the hell is Zebralab?_" – if you are thinking this, please read FAQ below, you may still be able to use the FramebyFrame package.  

During your experiment, Zebralab stored all the frame-by-frame data in a .raw file (a Viewpoint-proprietary format). For a typical ~ 65 hours experiment at 25 frames-per-second it is 25–30 Gb.  

###### RAW export
Make sure you have ~ 50 Gb free on the drive where the .raw file is. If you do not have enough, make some space or move the .raw file to another drive (local or external).

On the computer connected to the Zebrabox, open Zebralab as you would normally and open the protocol you used. I think using a different protocol makes no difference, but best to use the same one to be safe.

If you are using an older version of Zebralab, you may have to:  
Go to Options > Customize > Raw Data Export Setup. Check that:
* `Split ASCII file` is ticked ON.
* `Maximum ligne number` (excuse my French) is set to 1000000 (i.e. 1 million).

Go to Raw Data > Export…  
Pick the .raw file of your experiment.  
Zebralab will now write a bunch of .xls files in the folder where the .raw file is.  
Zebralab may act like it froze but you can check it is writing files in the folder.  
For a typical ~ 65 hours experiment, you expect ~ 1000–1500 .xls files in the folder.  
This takes a few hours and you will not be able to run Zebralab on this computer on the meanwhile.  
Each file will contain 1 million rows of frame-by-frame data.

###### Transfer the .xls files
Once the raw export is done, get all the .xls files in one folder called _YYMMDD_BX1_BX2_…_rawoutput_, e.g. _220531_14_15_myexp_rawoutput_. If you have a single box connected to the computer: _YYMMDD_BX1_..._rawoutput_.  

Check how many .xls raw files you have by selecting all the files and looking at the bottom of the window. Check that th  total number of files matches the number in the name of the last file. Example: if I have 1201 files, last .xls file should be called _YYMMDD_BX1_BX2_…_rawoutput_1201.xls_.

Zip this folder by right click > Send to > Compressed (zipped) folder. You should get _YYMMDD_BX1_BX2_…_rawoutput.zip_. The zip is now ~ 2–3 Gb which makes it easier to transfer or archive.  

Delete the original rawoutput folder **now** (the unzipped version). Do not let it clog the drive. Even if you have an issue later, you can just do the raw export again from the .raw.

Transfer the zip archive to the computer when you will do the analysis. You will also need the Zebralab .xls results file, so transfer it now too. It should be called _YYMMDD_BX1_BX2_….xls_ or _YYMMDD_BX1_….xls_ for a single Zebrabox.   


### 2– Sort the frame-by-frame data

The general structure of each raw .xls file in rawoutput is: first frame, data for every well; second frame, data for every well, etc. All one below the other in a tall format. Something like:

> frame1 – well 1 – 0 px  
frame1 – well 2 – 12 px  
frame1 – well 3 – 8 px  
...  
frame1 – well 96 – 0 px  
frame2 – well 1 – 1 px  
...  


This is in theory. In practice, Zebralab makes various ordering errors, for example shuffling the order of the wells or not giving frames in the chronological order.

We will now fix this and get our frame-by-frame data in a simple format.  

Create a new folder for your experiment, e.g. _YYMMDD_myexp_. Move your _..._rawoutput.zip_ in there.  

Unzip _..._rawoutput.zip_.  
> on Windows: click once > Extract (at the top) > Extract all > Extract  
> on Mac: double-click

Fire up RStudio. See also **R tips** below for a nicer experience.  

Create a new R script (File > New File > RScript) or Cmd/Ctrl + ↑ + N.  

Install and load the FramebyFrame package (see **Installation** above).  

We then run the `vpSorter` command to sort our raw .xls files.  

Our script so far:  

    ## repeating Installation instructions...
    library(devtools)
    install_github("francoiskroll/FramebyFrame")
    library(FramebyFrame)

    ## vpSorter
    vpSorter(ffDir="~/.../220531_14_15_rawoutput/",
             zebpath="~/.../220531_14_15_myexp.xls",
             boxGen=2,
             twoBoxMode=TRUE,
             boxnum=1,
             zt0="09:00:00",
             dayduration=14)

* `ffDir`: path to the folder with your raw .xls files.  
* `zebpath`: path to the Zebralab results file (.xls).  
* `boxGen`: 2 if you are using the newer version of Zebralab (post circa 2020), 1 for previous versions.  
* `zt0`: Zeitgeber 0 (ZT0). What is the time of your sunrise?  
* `dayduration`: how long does the day last in your experiment? By day, we mean lights ON.  

These are only brief notes on the settings, please refer to the documentation for the full explanations, especially if you get stuck.  

`vpSorter` should write _220531_14_RAWs.csv_ in your experiment folder. The format is fairly simple:  

| fullts              | zhrs        | exsecs     | f1     | f2     | ...    |
|:--------------------|:------------|:-----------|:-------|:-------|:-------|
| 2022-05-31 14:08:12 | 5.136678    | 0.040393   | 0      | 29     | ...    |
| 2022-05-31 14:08:12 | 5.136689    | 0.079975   | 3      | 19     | ...    |

Each row is one frame. Each column is one well (f for fish), plus a few timestamp columns at the start.

* `fullts`: full clock date/time  
* `zhrs`: number of hours since ZT0 on the day you started the experiment (day0). ZT0 always occured _before_ you started the experiment.  
* `exsecs`: number of seconds since experiment started. If you recorded at 25 frames-per-second, you expect roughly: 0.04, 0.08, etc.  
* `f1`: data for well #1.  
* `f2`: data for well #2.  
etc.  


### 2– Prepare your genotype map
This will assign each well to a group. We say 'genotype' as our experiments often look at larvae of different genotypes (e.g. wild-type vs knockout), but your groups can be anything, e.g. different concentrations of a drug.  

Grab the template which matches your plate format in _genotypeMap_templates/_ (find `Download` button).  
To edit the genotypeMap, follow the instructions written in the file.  
Let's say you called it _220531_14_genotypeMap.xlsx_.

### 3– Generate the genotype file
We need a simple .txt file listing the wells belonging to each group. You can write this file manually (and skip making the genotypeMap above) but it is not a very rewarding task and is prone to errors. Instead, we can use the genotypeGenerator on our genotypeMap:  

    ## genotypeGenerator
    genotypeGenerator(plateMap="~/.../220531_14_genotypeMap.xlsx")

`genotypeGenerator` should write _220531_14genotype.txt_ in your experiment folder, which lists the number of the wells belonging to each group.  

Tip: it also generates _220531_14_README.txt_ which I find especially useful to quickly get the sample sizes (`N = `) when preparing figures.  

### 4– Quality check: frame rate
It is a good idea to check that the frame rate was fairly stable throughout our experiment.  

To plot instantaneous frame rates during our experiment:  

    ggFramerate(ffpath="~/.../220531_14_RAWs.csv",
                subsample=TRUE,
                subsample_by=1000,
                xstart=0,
                xstop=0,
                ymin=0,
                ymax=50,
                sunlines=TRUE,
                dayduration=14,
                xname='hours since first 9 AM',
                yname='frames-per-second',
                exportOrNo=TRUE,
                width=75,
                height=55,
                exportPath="~/.../framerate.pdf")

Every command that generates a plot starts with `gg` (it stands for "grammar of graphics", if you were curious).  

Minimal explanations of the important settings:  

* `ffpath`: full path to the RAWs.csv file.  

Plotting every instantaneous frame rates would take a while and is completely unnecessary, so instead we only plot a subset of them:  
* `subsample`: whether to plot only a subset of the instantaneous frame rates (`TRUE` strongly recommended).
* `subsample_by`: e.g. 1000 means we only plot every 1000th instantaneous frame rate.  


* `exportOrNo`: whether to export the plot to a .pdf file.  
* `width`: width of the plot pdf in mm.  
* `height`: height of the plot pdf in mm.  
* `exportPath`: where/under which name do you want to save the plot? Note, it has to finish by .pdf.  

### 5– Any aberrant larva we should be excluding?
We can plot the activity trace of every well with  

    ## activity grid
    ggActivityTraceGrid(ffpath="~/.../220531_14_RAWs.csv",
                        genopath="~/.../220531_14genotype.txt",
                        smoothOrNo=TRUE,
                        smooth_nsecs=30*60,
                        binOrNo=TRUE,
                        bin_nsecs=10*60,
                        tracecols=NA,
                        linethick=0.4,
                        ymin=0,
                        ymax=60000,
                        xstart=0,
                        xstop=0,
                        trimstart=0,
                        trimstop=0,
                        nightBgOrNo=TRUE,
                        ncol=12,
                        nrow=8,
                        exportOrNo=TRUE,
                        exportPath="~/.../activitygrid.pdf",
                        width=255,
                        height=171)

Minimal explanations of the important settings:  

* `genopath`: full path to the genotype.txt file.  
* `smoothOrNo`: whether to smooth the trace.  
* `smooth_nsecs`: a bigger number smoothes the trace more. Precisely: the size of the rolling average used for smoothing in seconds, e.g. `30*60` means 1800 sec or 30 min.  
* `binOrNo`: whether to reduce the number of datapoints plotted by binning (`TRUE` strongly recommended).  
* `bin_nsecs`: a larger number reduces the number of datapoints plotted, which speed things up. Precisely: the size of the bin in seconds. For example `10*60` means 600 seconds (10 minutes), which sums the data in 10-minute bins.   
* `tracecols`: one colour for each group in genotype file, as words (e.g. `tracecols=c("red", "blue")`) or HEX values (e.g. `tracecols=c("#fcb505", "#78ac63")`). `NA` uses automatic R colours. Wells omitted from the genotype file (excluded or empty) are always drawn in grey.  
* `nightBgOrNo`: whether to add grey backgrounds to represent the nights.  
* `ncol`: number of columns in the grid.  
* `nrow`: number of rows in the grid.  

Tip: if you match `ncol` and `nrow` to the actual plate format you used (for a 96-well plate: `ncol=12` and `nrow=8`) the grid will match your plate.  

You can check the activity trace of each larva and exclude any that look particularly aberrant, e.g. flat activity or completely missed a light transition.  

If you want to exclude a larva, edit the genotypeMap and re-run the `genotypeGenerator` command to update the genotype file.  

### 7– Activity trace by group
We are ready to draw our first nice plot! Let's start with activity:

    ## activity trace by group
    ggActivityTraceByGroup(ffpath="~/Dropbox/ZFAD/220531_SORL1/220531_14_RAWsv2.csv",
                           genopath="~/Dropbox/ZFAD/tmp/220531_14genotype.txt",
                           smoothOrNo=TRUE,
                           smooth_nsecs=30*60,
                           binOrNo=TRUE,
                           bin_nsecs=10*60,
                           grporder=c('sorl1', 'scr'),
                           tracecols=c('#417dcd', '#697a87'),
                           ribboncols=c('#a3bbdb', '#b3bcc3'),
                           linethick=0.4,
                           xname='hours since ZT0',
                           yname='activity (sum of Δ px/10 min)',
                           xtextOrNo=FALSE,
                           ytextOrNo=TRUE,
                           ymin=0,
                           ymax=50000,
                           xstart=24,
                           xstop=72,
                           trimstart=24,
                           trimstop=72,
                           nightBgOrNo=TRUE,
                           legendOrNo=FALSE,
                           exportOrNo=TRUE,
                           exportPath="~/Dropbox/ZFAD/tmp/activitybygroup.pdf",
                           width=75,
                           height=55)

`ggActivityTraceByGroup()` plots one activity trace by group as mean (main trace) ± SEM (ribbon around the trace).  

Many of the settings are the same as before. Some minimal explanations of the ones we have not encountered (or talked about) yet:
* `grporder`: preferred order of the groups. The order should match the colours you give in `tracecols` and `ribboncols`. You can also exclude some groups here: any group you do not mention will not be drawn. `NA` will put groups in alphabetical order.  
* `tracecols`: colours of the main trace. Give one colour per group or none (`tracecols=NA`) for automatic.  
* `ribboncols`: colours of the ribbon for each trace. Give one colour per group or none (`tracecols=NA`) for automatic. You probably the same colour as each trace but lighter.  
* `xname`: name of the X axis. Give `xname=''` if you want empty.  
* `yname`: name of the Y axis. Give `yname=''` if you want empty.  
* `ymin`: where the Y axis should start (`ymin=0` recommended).
* `ymax`: where the Y axis should stop. You can find a good value by trial and error.
* `xstart`: where the X axis should start, in number of hours since first ZT0.  
* `xstop`: where the X axis should start, in number of hours since first ZT0. `xstop=0` means all of the experiment.  

As you may notice, `xstart` and `xstop` are not strictly respected, it leaves a bit of a gap for aesthetic reasons. For example, `xstart=24` will start the plot around 18 hours. You can adapt this value by trial and error but you will likely end up with the trace being against the Y axis which does not look amazing. Instead, a solution is to leave this gap but trim the trace before `xstart` and after `xstop`:
* `trimstart`: trim the trace before that value.
* `trimstop`: trim the trace after that value. `xstop=0` means all of the experiment, i.e. no trimming.  


* `legendOrNo`: whether to write the legend. I personally find it a waste of time to get the legends exactly as I want them in R, so what I recommend is to draw the plot once with `legendOrNo=TRUE` to check that you are matching each group with its colour correctly then export the plot without the legend `legendOrNo=FALSE` and add it yourself in Illustrator or InkScape or whatever you use.  

Important: the `bin_nsecs` setting determines the unit of the Y axis. In example above: `bin_nsecs=1*60` means that each datapoint is the sum of Δ px in each 10-min bin, therefore the unit on the Y axis is sum of Δ px/10 min.  


### 8– Sleep trace by group
Now let's plot the sleep traces.  

    ## sleep trace by group
    ggSleepTraceByGroup(ffpath="~/.../220531_14_RAWs.csv",
                        genopath="~/.../220531_14genotype.txt",
                        epo_min=10,
                        smoothOrNo=TRUE,
                        smooth_npoints=5,
                        grporder=c('sorl1', 'scr'),
                        tracecols=c('#417dcd', '#697a87'),
                        ribboncols=c('#a3bbdb', '#b3bcc3'),
                        linethick=0.4,
                        xname='',
                        yname='',
                        xtextOrNo=FALSE,
                        ytextOrNo=TRUE,
                        ymin=0,
                        ymax=10,
                        xstart=24,
                        xstop=72,
                        trimstart=24,
                        trimstop=72,
                        nightBgOrNo=TRUE,
                        legendOrNo=FALSE,
                        exportOrNo=TRUE,
                        exportPath="~/.../sleepbygroup.pdf",
                        width=75,
                        height=55)

Some notes about the settings we have not encountered yet:
* `epo_min`: size of the epoch in minutes, e.g. `epo_min=10` means that each datapoint represent the total sleep (in minutes) in each 10-minute epoch.  
* `smoothOrNo`: whether to smooth the trace.  
* `smooth_npoints`: a larger number smoothes the trace more. Precisely: the size of the rolling average in number of epochs.  

Important: the `epo_min` setting determines the unit of the Y axis. In example above: `epo_min=10` means that each datapoint is the total time spent asleep (in minutes) in each 10-minute epoch, therefore the unit on the Y axis is min/10 min.  

As before, not all possible settings are mentioned here. Read the full documentation to learn about those.  

### 9– Calculate behaviour parameters
FramebyFrame can currently calculate 17 parameters on both day and night, for a total of 32 unique parameters (two parameters are not defined for the day).

A few examples:  
* _activityPercentageTimeActive_: the percentage of time spent active per larva per time window. For example, larva #5 spent 10% during day2.  
* _activeboutNum_: the total number of active bouts one larva performed during one time window. For example, larva #22 performed 34,736 swimming bouts (active bouts) during day1.
* _sleepHours_: the total number of hours one larva slept during one time window. It is 0 hours if the larva had no sleep bout. For example, larva #2 slept a total of 3.54 hours during night2.

> Any idea for a new behaviour parameter? I want to hear it! Raise an issue on Github (tab `Issues`) or get in touch (francois@kroll.be). The main criterion of a good parameter is: it should be calculable on the Δ px timecourse of one larva for a complete day or night and return a single value.  

In Console, you can run `allparameters` to see the list of every available parameter:  

    > allparameters
    [1] "activityPercentageTimeActive" "activityTotalPx"
    ...

or `alluparams` to see the list of every unique parameter:

    > alluparams
    [1] "day_activityPercentageTimeActive" "day_activityTotalPx"
    ...

We calculate every parameter with:

    ## calculate behaviour parameters
    multiBehaviourParameter(parameters="all",
                            ffpath="~/.../20531_14_RAWs.csv",
                            genopath="~/.../220531_14genotype.txt",
                            dayduration=14)

Did you run two Zebraboxes in parallel? I would recommend calculating the behaviour parameters for both Zebraboxes in one command, simply give the multiple `ffpath` and `genopath`. For example:

    ## for two Zebraboxes at once
    multiBehaviourParameter(parameters='all',
                            ffpath=c("~/.../220531_14_RAWs.csv",
                                     "~/.../220531_15_RAWs.csv"),
                            genopath=c("~/.../220531_14genotype.txt",
                                       "~/.../220531_15genotype.txt"),
                            dayduration=14)

Make sure that the `ffpath` and the `genopath` are given in the same order!  

Calculating every parameter for two Zebraboxes at once takes about 50 min on my MacBook 2017.  

`multiBehaviourParameter()` will write "behaviour tables" in a folder called _bhvparams/_, within your experiment folder. It writes one behaviour table per Zebrabox per parameter, so you expect 17 files for each Zebrabox.  

The name of each behaviour table is _parameter_YYMMDD_BX.csv_, e.g. _sleepHours_220531_14.csv_. The format is fairly intuitive, for example:  

| parameter           | date        | box     | fish     | grp     | night0    | day1    |  ...    |
|:--------------------|:------------|:-----------|:-------|:-------|:-------| :-------|  :-------|
| sleepHours | 220531    | 14   | f1      | wt     | 8.01    |  0.55    |   ...    |
| sleepHours | 220531    | 14   | f2      | ko     | 9.11    |  1.47    |   ...    |

The values depend on the parameter. In the example above, the parameter is sleepHours, so the values (8.01, 0.55, ...) represent total number of hours spent asleep during night0, day1, etc.  

### 10– Plot every behaviour parameter

We can plot a grid of parameter scatterplots using:  

    ## plot grid of parameters
    ggParameterGrid(paDir="~/.../bhvparams/",
                    grporder=c('sorl1', 'scr'),
                    skipNight0=TRUE,
                    colours=c('#417dcd', '#697a87'),
                    legendOrNo=FALSE,
                    ynameOrNo=FALSE,
                    yunitOrNo=TRUE,
                    xtextOrNo=FALSE,
                    titleOrNo=TRUE,
                    nightBgOrNo=TRUE,
                    statsOrNo=TRUE,
                    ncol=5,
                    nrow=4,
                    width=500,
                    height=230,
                    exportPath="~/.../paramgrid.pdf")

You will see a bunch of statistics passing by in the Console, we will get to them in a minute.  

Some of the settings we have not encountered yet:
* `paDir`: path to the _bhvparams_ directory (folder). `ggParameterGrid()` will simply plot every behaviour table it finds in that folder.  
* `skipNight0`: whether to skip the first night. In the Rihel lab, we typically collect data for three nights and two days but skip the first night as habituation period.  
* `ynameOrNo`: whether to have Y axes as small sentence, e.g. "total sleep (hr)".
* `yunitOrNo`: instead, you may prefer having Y axes as just the unit, e.g. "hr".
* `xtextOrNo`: whether to write labels on the X axis of each plot. Note; this does not work properly for the moment. Only use `xtextOrNo=TRUE` to understand the order of the groups on the X axis.  
* `titleOrNo`: whether to have the parameter name as title of each plot.  
* `statsOrNo`: whether to calculate the statistics and add the p-value asterisks on top of each plot. Read below about how these p-values are calculated.  

The parameter grid will not display in RStudio, do not be alarmed. It is relatively big so it slows things down. Instead, look directly at the pdf it generates.  

### 11– Statistics on behaviour parameter
> Any feedback on this section is welcome, especially if you think I got something wrong!

As promised, here are the statistics. It uses linear mixed effects (LME) modelling. Here is a very brief summary of the approach. For more details, please refer to the full documentation.

Remember here that "unique behaviour parameter" means one value per larva per time window, e.g. number of sleep bouts larva #12 had during night2. For each unique behaviour parameter, the question is whether the group has an effect on the values. Or in other words, the null hypothesis is that the group has no effect on the given behaviour parameter, e.g. which genotype you are (wild-type or heterozygous or homozygous) does not change your number of sleep bouts at night. In LME jargon, the group is the **fixed effect**, i.e. we want to measure how the group affects the parameter. However, we need to take into account a bunch of variables that probably affect our data. For example, it could be that larvae were always less sleepy during the second night of tracking. We do not really care about quantifying how these variables affect our data, we just want to control for them. These are the **random effects**.

The random effects in our LME model are:
* **the experiment**; _if_ you are analysing multiple replicate experiments at once (which is recommended, this is a big advantage of using LME here). Ideally, each Zebrabox tracked a single clutch of larvae (same parents & same mating), so replicate experiments control for both technical (e.g. light levels not being exactly the same between the Zebraboxes) and biological (different clutches vary a lot in most behavioural parameters) variability. Other experimental designs may work too, so please read the full documentation to adapt the settings.
* **development**, i.e. the larva’s age. If you recorded multiple days/nights (recommended), the larvae were also growing during the experiment, which likely had an effect on their behaviour. For example, we often notice that sleep is slightly lower during night2 (7 dpf for us) vs night1 (6 dpf) so we want to control for this and not simply pool the datapoints from multiple days or nights (this also risks "pseudo-replication", which occurs if, for example, you were analysing your data as if you had _n_=50 larvae when really you had _n_=25 larvae measured on two different days).
* **the larva's ID** (e.g. larva #12). This is to account for non-independence in the data. Indeed, each larva is typically sampled multiple times during the experiment. For example, larva #12 is sampled once on day1 and once on day2. Or in other words, day1 and day2 are not independent measurements as they include the same animals. This also avoids "pseudo-replication" (see above).

The formula to create the LME model looks like:

    lmer(parameter ~ group + (1|experiment/larva ID) + (1|larva age))

The model provides the slope, i.e. magnitude of the effect (e.g. in average, KO larvae slept 30 min less than WT) and the standard error. If you have more than two groups, there is a slope and error for each comparison to the reference group (e.g. heterozygous vs wild-type; homozygous vs wild-type).

We then compare this model with a model that omits the group assignment to obtain a p-value. You will obtain a _single_ p-value for each unique parameter here, regardless of the number of groups. This is because the null hypothesis is "group assignment has no effect on the behaviour parameter", so we do not worry (yet) about which group(s) exactly. The interpretation is like an ANOVA, essentially.

We then run "post-hoc" tests, comparing each group to the reference group, returning one p-value for each comparison (e.g. one p-value for heterozygous vs wild-type and one p-value for homozygous vs wild-type). Please refer to the documentation for more details.  

Do not worry if it is a bit confusing. It should make more sense once you see the output.  

So far, our examples of commands only had two groups. Let's analyse an experiment with three groups so we can better explain the output.  

To calculate the LME statistics, we run:

    ## LME stats
    LMEreport(paDir="~/Dropbox/ZFAD/220316_sorl1Stable/bhvparams/",
              grporder=c('wt', 'het', 'hom'),
              skipNight0=TRUE,
              exportPath="~/Dropbox/ZFAD/tmp/LMEreport.csv")

* `grporder`: put the group you want as reference group (e.g. wild-type or DMSO-treated) as the _first_ group.
* `exportPath`: full path to the statistics report we will write. You can call this file however you like but make sure it finishes with .csv.  

While `LMEreport()` run, it also prints summaries in Console, such as:

> #####################################  
Parameter activeboutSum  
\>>> Separating day and night datapoints  
[...]  
... NIGHT summary  
\>>> wt vs het = 4.561757 ± 1.676933   
\>>> wt vs hom = 5.764397 ± 2.029445   
\>>> Does group significantly affect this behavioural parameter? pval = 0.006698481 **  

Here, 4.561757 and 5.764397 are the slopes (magnitudes of the effect) and 1.676933 and 2.029445 is the error associated with each slope. As we are looking at parameter `activeboutSum`, you should read it as, for example: "during the night, each swimming bout of larvae from group hom moved in average 6 ± 2 extra pixels than the swimming bouts of larvae from group wt".  

All this information is recorded in the statistics reports (_LMEreport.csv_ in our command above). Have a look at the corresponding rows in the report:

| exps  | parameter | daynight | referenceGroup | beingGroup | LMEslope | LMEerror | pval | pvalsymbol | posthocval | posthocpvalsymbol   |
|:--------------------|:------------|:-----------|:-------|:-------|:-------| :-------|  :-------|   :-------|  :-------|  :-------|
| 220316_14 & 220316_15 | activeboutSum  | night  | wt   | het  | 4.56175675466967  |  1.67693318829172  |   0.00669848089039374    |   **    |    0.00747698513255208    |   **    |
| 220316_14 & 220316_15 | activeboutSum  | night  | wt   | hom  | 5.76439680434721  |  2.02944541645242  |   0.00669848089039374    |   **    |    0.00525474836388776    |   **    |

* `exps` lists the YYMMDD_BX of the experiments you analysed together.
* `LMEslope` is the effect of being from `beingGroup` compared to `referenceGroup`, here: "if I am from group hom, each of my swimming bouts moved 6 ± 2 more pixels compared to wt larvae".  
* `pval` is the p-value for the null hypothesis "group assignment has no effect on this behaviour parameter". Notice that this p-value is simply repeated in the two rows as it is not group-specific.
* `pvalsymbol`: ns for p > 0.05, * for p ≤ 0.05, ** for p ≤ 0.01, *** for p ≤ 0.001.
* `posthocpval` is the p-value of the post-hoc test. This p-value is specific to each comparison, i.e. there is one p-value for wt vs het and one p-value for wt vs hom.
* `posthocpvalsymbol`, same logic as `pvalsymbol` but for the post-hoc p-value.  

When we plotted the behaviour parameters (see **10– Plot every behaviour parameter**), the asterisks that get added when `statsOrNo=TRUE` are the `pvalsymbol`, i.e. they represent the p-value of the null hypothesis "group assignment has no effect on this behaviour parameter".  

And that is all for the minimal tutorial! The FramebyFrame package can do other things, such as analyse different experimental designs, calculate and plot behavioural fingerprints and more. Make sure to check the documentation to make full use of it.  

___

## R basics

Here is the minimum you need to know about R to get started.
> Did I forget something? Let me know and we will make it 1% easier for the next user!

* Download and install R & RStudio from https://posit.co/download/rstudio-desktop/. R is the coding language, RStudio is a fancy text editor to make it easier to write R code.

* Open RStudio and start a new script (File > New File > R Script or Cmd/Ctrl + ↑ + N). The panel that just opened is your script. This is where you write R code. The bottom is the Console, this where the computer answers. In the top-right is the Environment, this is where you keep stuff in memory.

* For example, try to write in your script:
      print("Hello World")
  Now to run the command, place your cursor at the end of the line or select the command and press Cmd/Ctrl + Enter.
  In the Console, you will see your command repeated, then the answer from the computer:
      > print("Hello World")
      [1] "Hello World"

* The computer does not see lines that start with `#`. Use that to write comments as notes to yourself or others. e.g.
      # this is a comment
      print("Hello World")  

* As you may notice, you need to indicate to R what is actual text (and not computer code) by using double quotes, like `"Hello World"`. One unit of text is called a string.

* You can give R a series of things at once, it is called a vector. In R it is written `c(thing1, thing2)`. Examples:
      c("ho", "hey")
      c(1, 2, 3)
      c("hey", 3)

* `NA` stands for "Not Available". It is a way of telling R "missing value".
* You will often need to give R a path to some file on your computer. You need to give it as text.  
Here is an example of a path on Mac: `"~/Desktop/myExperiment/210907_12_RAWs.csv"` (`~` means Home directory).  
Here is an example of a path on Windows: `"C:\\Desktop\\myExperiment\\210907_12_RAWs.csv"` (the double `\\` is an unfortunate R quirk).

* Writing complete paths manually is boring. Instead, when you open the quotes like `""`, press Tab and R will list the folder/files it currently sees. You can just pick in dropdown menu and press Tab repeatedly until you reach your destination. See below **R tips** for an even better solution. You can often use Tab for shortcuts so try regularly to see what it does.

## R tips

A few tips that may make the experience smoother.

* I almost never want to save what I have in Environment when closing RStudio, yet it constantly asks.  
To switch this off:  
Tools > Global Options > untick Restore .RData into workspace as startup  
Also change > Save workspace to .RData on exit to _Never_

* For paths, I recommend the `here` package. This is how it works: open RStudio, then File > New Project > Existing Directory > navigate to your experiment folder > Open > Create Project. This will create a .Rproj file in your folder.
Now install and load the `here` package:
      install.packages("here")
      library(here)
In Console, you should see: `> here() starts at /Users/.../myExperiment`. Basically, `here` sees your .Rproj file and will now start all the paths from there. This makes writing paths less painful. For example, instead of writing `"~/Desktop/myExperiment/210907_12_RAWs.csv"`, you can simply write `here("210907_12_RAWs.csv")`. Another advantage is that if you move your experiment folder or change its name, you will not need to update all the paths in your script. For example, imagine we moved the folder _myExperiment_ to _Documents_, we would need to update all our paths if we were using absolute paths. Instead, if we use relative paths (thanks to `here`), as long as you did not touch the .Rproj file everything is still working.

___

## FAQ

> _I use the Noldus from DanioVision (or else). Can I still use your amazing package?_  

Yes, it is possible. Get in touch with me. At least one person has used it successfully with data from DanioVision's Noldus, but the conversion is not yet integrated in the package.

> _I do not have a Zebralab .xls results file!_

No problem. All we need the Zebralab file for is to read the date and time when your experiment started. You can give that information manually to `vpSorter()` instead:

      vpSorter(ffDir=...,
               zebpath=NA,
               boxGen=...,
               twoBoxMode=...,
               boxnum=...,
               zt0=...,
               date0="07/09/2021",
               time0="15:34:05",
               dayduration=...)

Ideally, you should be precise at the second as FramebyFrame will use this to determine when the light transitions occured.  

> _The first timestamp in my Zebralab results file is wrong!_

Yes, this happens when running a Replay on Zebralab, for example. The software (stupidly) takes the computer clock time when you started the Replay, not the original start of the experiment. Follow instructions above to give the start date/time manually to `vpSorter()`.

___

## Recommended experimental design
> section under construction, do not read
