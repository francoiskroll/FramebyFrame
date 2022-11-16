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

___

## Minimal example

Impatient? Here is the quickest possible example of a FramebyFrame analysis. The package can do many other things, so make sure to check the full documentation once you got the gist of it.

I will assume you have R and RStudio installed. If not, you can follow e.g. https://rstudio-education.github.io/hopr/starting.html.  

#### 1– Get your frame-by-frame data from Zebralab

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


#### 2– Sort the frame-by-frame data

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


#### 2– Prepare your genotype map
This will assign each well to a group. We say 'genotype' as our experiments often look at larvae of different genotypes (e.g. wild-type vs knockout), but your groups can be anything, e.g. different concentrations of a drug.  

Grab the template which matches your plate format in _genotypeMap_templates/_ (find `Download` button).  
To edit the genotypeMap, follow the instructions written in the file.  
Let's say you called it _220531_14_genotypeMap.xlsx_.

#### 3– Generate the genotype file
We need a simple .txt file listing the wells belonging to each group. You can write this file manually (and skip making the genotypeMap above) but it is not a very rewarding task and is prone to errors. Instead, we can use the genotypeGenerator on our genotypeMap:  

    ## genotypeGenerator
    genotypeGenerator(plateMap="~/.../220531_14_genotypeMap.xlsx")

`genotypeGenerator` should write _220531_14genotype.txt_ in your experiment folder, which lists the number of the wells belonging to each group.  

Tip: it also generates _220531_14_README.txt_ which I find especially useful to quickly get the sample sizes (`N = `) when preparing figures.  

#### 4– Quality check: frame rate
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

![](tutorialPics/framerate.png?raw=true)

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

#### 5– Any aberrant larva we should be excluding?
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

#### 7– Activity trace by group
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

Important: the `bin_nsecs` setting impacts the unit of the Y axis. In example above: `bin_nsecs=1*60` means that each datapoint is the sum of Δ px in each 10-min bin, therefore the unit on the Y axis is sum of Δ px/10 min.  


#### 8– Sleep trace by group
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

Important: the `epo_min` setting impacts the unit of the Y axis. In example above: `epo_min=10` means that each datapoint is the total time spent asleep (in minutes) in each 10-minute epoch, therefore the unit on the Y axis is min/10 min.  

As before, not all possible settings are mentioned here. Read the full documentation to learn about those.  

#### 9– Calculate behaviour parameters
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

Calculating every parameter for two Zebraboxes at once takes about 50 min on my MacBook.  

`multiBehaviourParameter()` will write "behaviour tables" in a folder called _bhvparams/_, within your experiment folder. It writes one behaviour table per Zebrabox per parameter, so you expect 17 files for each Zebrabox.  

The name of each behaviour table is _parameter_YYMMDD_BX.csv_, e.g. _sleepHours_220531_14.csv_. The format is fairly intuitive, for example:  

| parameter           | date        | box     | fish     | grp     | night0    | day1    |  ...    |
|:--------------------|:------------|:-----------|:-------|:-------|:-------| :-------|  :-------|
| sleepHours | 220531    | 14   | f1      | wt     | 8.01    |  0.55    |   ...    |
| sleepHours | 220531    | 14   | f2      | ko     | 9.11    |  1.47    |   ...    |

The values depend on the parameter. In the example above, the parameter is sleepHours, so the values (8.01, 0.55, ...) represent total number of hours spent asleep during night0, day1, etc.  

#### 10– Plot every behaviour parameter

We can plot a grid of parameter scatterplot using:  

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

#### 11– Statistics on behaviour parameter
> under construction, do not read

As promised, here are the statistics. It uses linear mixed effects modelling.

Very briefly, my understanding is: we want to measure the effects of the groups on each behaviour parameter, this is the "fixed effect". However, a bunch of factors bias our data. We want to control for those, and we do not really care about quantifying their effect. These are the "random effects".  

> Any feedback is welcome, especially if you think I got something wrong.

___

## R basics

* script vs console
* environment
* comments
* vector with c()
* strings in quotes
* run some lines
* tab auto-complete
* paths
* NA

## R tips
> under construction, do not read

* here package  
* don't save .Rdata
* project
