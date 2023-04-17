# FramebyFrame R package

Francois Kroll, 2022 @Rihel lab, UCL.

FramebyFrame is an R package to analyse behavioural trackings of zebrafish larvae, mainly using the Zebrabox (Viewpoint) in "Quantization" mode.

Already using the Zebrabox? Read below (**Frame-by-frame manifesto**) why you should consider doing your analysis on the frame-by-frame data.

Not did your experiment yet? Make sure to read the section **Experimental design commandments** for some advice.  

[![alt text][1.2]][1] [@francois_kroll](https://twitter.com/francois_kroll)

:email: francois@kroll.be

<!-- icons with padding -->
[1.1]: http://i.imgur.com/tXSoThF.png (twitter icon with padding)

<!-- icons without padding -->
[1.2]: http://i.imgur.com/wWzX9uB.png (twitter icon without padding)

<!-- links to your social media accounts -->
[1]: https://twitter.com/francois_kroll


> —_I do not know anything about R so this package is not for me._    
I wrote everything with that in mind, I promise. The amount of R (or any coding language) you need to know in order to run a complete analysis is minuscule, and I wrote it all in section **R basics** below.

___

## Credit

FramebyFrame builds upon work from Rihel lab members/alumni & collaborators, primarily:

* Original work describing sleep/wake tracking in zebrafish larvae: [Rihel, Prober, Schier, 2010. _Methods in Cell Biology_](https://doi.org/10.1016/B978-0-12-384892-5.00011-6).

* MATLAB analyses written by Rihel & Prober labs, e.g. [Lee, Oikonomou, Prober, 2022. _Bio Protocols_](https://bio-protocol.org/e4313). The definitions of the sleep parameters were taken from there.

* Work on analysis of the frame-by-frame data: [Ghosh and Rihel, 2020. _eNeuro_](https://www.eneuro.org/content/7/4/ENEURO.0408-19.2020). The definitions of the "active bout" parameters were taken from there.

___

## Installation

Installation requires the package `devtools`. If you do not have it already, first run in R:

```r
install.packages("devtools")
```

Then:

```r
# load the package devtools
library(devtools)

# install the package FramebyFrame from the present repo
install_github("francoiskroll/FramebyFrame")
```

It may say:

> These packages have more recent versions available.  
It is recommended to update all of them.  
Which would you like to update?  
[...]

Answer `1` to update all.  

If it asks:
> _Do you want to install from sources the packages which need compiltation?_

Answer _No_.

Now to load the FramebyFrame package:

```r
# load FramebyFrame
library(FramebyFrame)
```

And you should be good to go!

To be clear, you do not need to re-install the package every time you open R. Next time, just load it with `library(FramebyFrame)`.

But I recommend that you run the installation again once in a while so I can update things.

___

## The frame-by-frame manifesto

Already using the Zebrabox or similar? Here is why you should consider doing the analysis on the frame-by-frame data.

#### How Zebralab actually works
At its core, the Zebrabox in "Quantization" mode records, at each frame transition and for each well, the number of pixels that changed grey pixel value above the Sensitivity threshold set by the user. The Sensitivity threshold is in grey pixel value (0 for black to 255 for white). On the new generation of Zebraboxes, we typically use Sensitivity = 20 when tracking 5–8 dpf larvae in 96-well plates.

Precisely, my understanding of how Zebrabox works is: at frame $f$, it asks: “in this well, how many pixels changed intensity between frame ${f−1}$ and frame $f$”. To decide whether _one_ pixel counts as having changed intensity it asks “did that pixel’s grey value change by more than _Sensitivity_”. For example, say the larva swam over pixel X at frame $f$. The grey value of pixel X was 220 (almost white) at frame ${f−1}$ and became 11 (almost black) at frame $f$ as the larva covered it. Therefore, pixel X changed intensity by ${220−11 = 209}$ grey values, which is above the _Sensitivity_ threshold. Pixel X is thus counted as having changed intensity at frame f. The number of pixels that changed intensity between two successive frames is called Δ pixel.

Therefore, the frame-by-frame data is, for each well, a long list of pixel counts. Each row representing the number of pixels which changed intensity vs. the previous frame. Something like:

| frame | Δ pixel |
|:---|:---|
|frame 1|0 px|
|frame 2|0 px|
|frame 3|12 px|
|frame 4|8 px|
|frame 5|0 px|


Looking at the larval behaviour in this way, zebrafish larvae are inactive the great majority of the time (i.e. most of the Δ pixels are 0) interrupted frequently by swimming bouts which appear as ‘peaks’ in the Δ pixel values, e.g. `0 0 0 0 0 0 2 4 11 18 20 5 0 0 0 0 0`…

#### What is the _middur_ parameter

Many users of the Zebrabox analyse the _middur_ parameter, typically binned in 1-minute epochs. _middur_ stands for "duration in mid-activity". Precisely, it is the number of seconds the larva spent in "mid-activity" during a given minute.

How does Zebralab (the software) calculates _middur_ exactly?

Let us assume the frame rate is 25 frames-per-second and the integration period is 1 minute. Zebralab takes the first 1 minute of frame-by-frame data, i.e. 25 frames-per-second × 60 seconds = 1500 frames or Δ pixel values. Of these 1500 frames, it counts those which were above or equal to the _Freezing_ threshold (set by the user, we use _Freezing_ = 3 Δ pixel) but below or equal to the _Burst_ threshold (set by the user, we use _Burst_ = 200 Δ pixel). This is the number of frames spent in "mid-activity", which Zebralab then converts into a "number of seconds in mid-activity" assuming 25 frames-per-second.

For example, of the first 1500 frames, 400 were below _Freezing_ (i.e. they were 0 or 1 or 2 Δ pixel) and 100 were above _Burst_ (i.e. they were 201 or 202 or … Δ pixel). We are left with 1000 frames, i.e. ⅔ of one minute. So, during the first minute, the larva spent _middur_ = 40 seconds active.

However, analysing the **frame-by-frame data** (i.e. the Δ pixels) is more accurate and more interesting than analysing the _middur_ datapoints. Here are a few reasons.

#### Sleep detection on the frame-by-frame data is more accurate

A bunch of labs use the Zebrabox to monitor sleep in zebrafish larvae. To detect sleep bouts, the routinely used definition is any period of inactivity longer than one minute. If you are analysing the _middur_ parameter and assuming you set the integration period to 1 minute, this translates to "at least one _middur_ datapoint = 0" (or < 0.1 is sometimes used).

Unfortunately, this approach misses many sleep bouts. Here is why.

The _middur_ algorithm runs on **non-overlapping** chunks of 1 minute of data (~ 1500 frames), i.e. the first minute runs from frame #1 to #1500, the second minute is from frame #1501 to #3000, etc. This is not ideal for detection of sleep bouts. Consider the following example: the larva stops moving at frame #1000 then starts moving again at frame #2900, for a total of 1900 inactive frames. Zebralab calculates the _middur_ value for the first minute (frame #1 to #1500): the larva was active during this period, as it only stopped moving at frame #1000. It then calculates the middur value for the second minute (frame #1501 to #3000): the larva was also active during this period, as it started moving at frame #2900. Result: we get two middur values > 0, i.e. no sleep is detected. But the larva was inactive for 1900 frames = 1.26 minutes, which should count as a sleep bout!

This is why analysing _middur_ datapoints binned in 1-minute epochs misses sleep bouts. For a typical 10-hr night, the _middur_ analysis:
* underestimates total sleep time by 1.2 ± 0.4 hr.
* underestimates the number of sleep bouts by 38 ± 16 sleep bouts.
* overestimates sleep bout duration by 0.4 ± 0.6 min.

This last point may be counterintuitive. Imagine a sleep bout that is exactly 1 minute. To detect this sleep bout, we would need the larva to fall asleep exactly at frame #1501 and wake up exactly at frame #3000, for example. This is very unlikely. Therefore, with the _middur_ analysis: the longer the sleep bout, the more likely it is detected. In other words, the detection is biased _against_ the detection of shorter sleep bouts, so it overestimates the typical sleep bout duration.

Why is working with the frame-by-frame data better? The FramebyFrame R package detects every sleep bout (period of > 1500 inactive frames) present in the data. In practice, at each frame, it sums the Δ pixels of the previous 1500 frames. Was there any positive Δ pixel in the previous 1500 frames? Then the sum will give some positive number. Were the previous 1500 Δ pixels all 0? Then ${0 + 0 + 0 + … = 0}$.  So, at each frame, we can ask: “prior to this exact frame, had the larva been inactive for a complete minute?”. If yes (the previous 1500 frames were all 0), then the larva had been asleep for exactly 1 minute at that frame. In other words, the larva fell asleep exactly 1 minute ago. We can go back to 1 minute ago and label all these frames as ‘asleep’ and continue until the larva woke up, i.e. until the next positive Δ pixel.

#### We can calculate more behavioural parameters on the frame-by-frame data
Zebrafish larvae make swimming bouts that last ~ 0.2 second. Using the 1-min binned data (the _middur_ parameter) is a bit like recording at 1 frame-per-minute: one cannot detect single swimming bouts at such slow recording speed. This is a shame, especially as you may already have the data on your drive. Indeed, studying the frame-by-frame data allows to describe the structure of single swimming bouts (see DOCUMENTATION, Behavioural parameters, Active bout parameters).

The _middur_ analysis is also blind to the actual number of pixels that changed intensity at each frame transition. For the _middur_ algorithm, a vigorous swimming bout which reached a whooping 100 Δ pixel is the same as a subdued movement which reached 9 Δ pixel, assuming both lasted the same duration. Therefore, using the _middur_ parameter, one can only describe activity in terms of time spent active but cannot describe the _intensity_ of this activity (i.e. how many pixels were moved). Accordingly, a larva which moved constantly but very calmly can, in theory, have the same _middur_ values as a larva which moved constantly and very ‘violently’ (e.g. had seizures). The _middur_ analysis cannot differentiate these two situations even though they are completely different biologically. In this example, we can use the FramebyFrame package to simply sum the Δ pixels, which will differentiate these two cases. We expect the first larva to spend all of its time active but have a low Δ pixel total, while the other larva would also spend all of its time active but its Δ pixel total would also be very high.

#### One slight point of caution
I can think of one potential drawback of using the frame-by-frame data: as it actually takes the Δ pixels into account, I would expect it to be more biased in situations where there is a difference in pigmentation or size between larvae. For example, say the mutant larvae are less pigmented than their wild-type siblings. In this case, you would expect the mutant larva to trigger fewer pixels when doing the exact same movement as the wild-type larva, i.e. you may underestimate the activity of the mutant larva. If you think this is the case in your experiment, give more weight to parameters which do not take the Δ pixel values into account, i.e. are given in a different unit than pixels. Those should be fairly robust to differences in size or pigmentation.

___

## Minimal tutorial

Ready? Here is the shortest possible tutorial of a FramebyFrame analysis. The package can do more than what is presented here, so make sure to check the full documentation once you got the gist of it.

I will assume you have R and RStudio installed. If not, have a look at section **R basics** below.

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

```r
## do not forget to load the package first...
library(FramebyFrame)

## vpSorter
vpSorter(ffDir="~/.../220531_14_15_rawoutput/",
         zebpath="~/.../220531_14_15_myexp.xls",
         boxGen=2,
         twoBoxMode=TRUE,
         boxnum=1,
         zt0="09:00:00",
         dayduration=14)
```


* `ffDir`: path to the folder with your raw .xls/xlsx files.  
* `zebpath`: path to the Zebralab results file (.xls/xlsx).  
* `boxGen`: 2 if you are using the newer version of Zebralab (post circa 2020), 1 for previous versions.
> Note sure which one? Open one of the raw .xls/xlsx files, are the Δ pixel values (typically in column _data1_) mostly 1s or mostly 0s? If mostly 1s: `boxGen=1`; if mostly 0s: `boxGen=2`
* `zt0`: Zeitgeber 0 (ZT0). What is the time of your sunrise?  
* `dayduration`: how long does the day last in your experiment? By day, we mean lights ON.  

These are only brief notes on the settings, please refer to the documentation for the full explanations, especially if you get stuck. 

> Are you getting _Error: vector memory exhausted (limit reached?)_ (or similar)? Please check Troubleshooting section in DOCUMENTATION.md.


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

> Getting stuck here? Viewpoint changes the format of the output files all the time, so it is much more likely I have not seen your particular format yet, rather than you doing anything wrong. Just send me the first 10 or so raw .xls/xlsx files on francois@kroll.be and I will update the package.

### 2– Prepare your genotype map
This will assign each well to a group. We say 'genotype' as our experiments often look at larvae of different genotypes (e.g. wild-type vs knockout), but your groups can be anything, e.g. different concentrations of a drug.  

Grab the template which matches your plate format in _genotypeMap_templates/_ (find `Download` button).  
To edit the genotypeMap, follow the instructions written in the file.  
Let's say you called it _220531_14_genotypeMap.xlsx_.

### 3– Generate the genotype file
We need a simple .txt file listing the wells belonging to each group. You can write this file manually (and skip making the genotypeMap above) but it is not a very rewarding task and is prone to errors. Instead, we can use the genotypeGenerator on our genotypeMap:  

```r
## genotypeGenerator
genotypeGenerator(plateMap="~/.../220531_14_genotypeMap.xlsx")
```

`genotypeGenerator` should write _220531_14genotype.txt_ in your experiment folder, which lists the number of the wells belonging to each group.  

Tip: it also generates _220531_14_README.txt_ which I find especially useful to quickly get the sample sizes (`N = `) when preparing figures.  

### 4– Quality check: frame rate
It is a good idea to check that the frame rate was fairly stable throughout our experiment.  

To plot instantaneous frame rates during our experiment:  

```r
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
```

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

```r
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
```

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

### 6– Activity trace by group
We are ready to draw our first nice plot! Let's start with activity:

```r
## activity trace by group
ggActivityTraceByGroup(ffpath="~/.../220531_14_RAWs.csv",
                       genopath="~/.../220531_14genotype.txt",
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
                       exportPath="~/.../activitybygroup.pdf",
                       width=75,
                       height=55)
```

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


### 7– Sleep trace by group
Now let's plot the sleep traces.  

```r
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
```

Some notes about the settings we have not encountered yet:
* `epo_min`: size of the epoch in minutes, e.g. `epo_min=10` means that each datapoint represent the total sleep (in minutes) in each 10-minute epoch.  
* `smoothOrNo`: whether to smooth the trace.  
* `smooth_npoints`: a larger number smoothes the trace more. Precisely: the size of the rolling average in number of epochs.  

Important: the `epo_min` setting determines the unit of the Y axis. In example above: `epo_min=10` means that each datapoint is the total time spent asleep (in minutes) in each 10-minute epoch, therefore the unit on the Y axis is min/10 min.  

As before, not all possible settings are mentioned here. Read the full documentation to learn about those.  

### 8– Calculate behaviour parameters
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

```r
## calculate behaviour parameters
multiBehaviourParameter(parameters="all",
                        ffpath="~/.../20531_14_RAWs.csv",
                        genopath="~/.../220531_14genotype.txt",
                        dayduration=14)
```

Did you run two Zebraboxes in parallel? I would recommend calculating the behaviour parameters for both Zebraboxes in one command, simply give the multiple `ffpath` and `genopath`. For example:

```r
## for two Zebraboxes at once
multiBehaviourParameter(parameters='all',
                        ffpath=c("~/.../220531_14_RAWs.csv",
                                 "~/.../220531_15_RAWs.csv"),
                        genopath=c("~/.../220531_14genotype.txt",
                                   "~/.../220531_15genotype.txt"),
                        dayduration=14)
```

Make sure that the `ffpath` and the `genopath` are given in the same order!  

Calculating every parameter for two Zebraboxes at once takes about 50 min on my MacBook 2017.  

`multiBehaviourParameter()` will write "behaviour tables" in a folder called _bhvparams/_, within your experiment folder. It writes one behaviour table per Zebrabox per parameter, so you expect 17 files for each Zebrabox.  

The name of each behaviour table is _parameter_YYMMDD_BX.csv_, e.g. _sleepHours_220531_14.csv_. The format is fairly intuitive, for example:  

| parameter           | date        | box     | fish     | grp     | night0    | day1    |  ...    |
|:--------------------|:------------|:-----------|:-------|:-------|:-------| :-------|  :-------|
| sleepHours | 220531    | 14   | f1      | wt     | 8.01    |  0.55    |   ...    |
| sleepHours | 220531    | 14   | f2      | ko     | 9.11    |  1.47    |   ...    |

The values depend on the parameter. In the example above, the parameter is sleepHours, so the values (8.01, 0.55, ...) represent total number of hours spent asleep during night0, day1, etc.  

### 9– Plot every behaviour parameter

We can plot a grid of parameter scatterplots using:  

```r
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
                keysOrNo=TRUE,
                statsOrNo=TRUE,
                ncol=5,
                nrow=4,
                width=500,
                height=230,
                exportPath="~/.../paramgrid.pdf")
```

You will see a bunch of statistics passing by in the Console, we will get to them in a minute.  

Some of the settings we have not encountered yet:
* `paDir`: path to the _bhvparams_ directory (folder). `ggParameterGrid()` will simply plot every behaviour table it finds in that folder.  
* `skipNight0`: whether to skip the first night. In the Rihel lab, we typically collect data for three nights and two days but skip the first night as habituation period.  
* `ynameOrNo`: whether to have Y axes as small sentence, e.g. "total sleep (hr)".
* `yunitOrNo`: instead, you may prefer having Y axes as just the unit, e.g. "hr".
* `xtextOrNo`: whether to write labels on the X axis of each plot. Make sure to at least use `xtextOrNo=TRUE` the first time so you understand the order of the groups on the X axis, then you can write them manually for the final figure if you prefer.
* `titleOrNo`: whether to have the parameter name as title of each plot.  
* `keysOrNo`: whether to add A, B, C, ... keys to label each panel.
* `statsOrNo`: whether to calculate the statistics and add the p-value asterisks on top of each plot. Read below about how these p-values are calculated.  

The parameter grid will not display in RStudio, do not be alarmed. It is relatively big so it slows things down. Instead, look directly at the pdf it generates.  

### 10– Sleep latency survival plot

Sleep latency is, for each larva, the amount of time before first sleep bout once lights switched off, i.e. how long each larva took to fall asleep.

In addition to its scatterplot drawn above, the sleep latency parameter can also be represented as a survival curve. The idea is: when lights just switched off (start of the night), 100% of the larvae did not sleep yet. As the night goes by, larvae start falling asleep one after the other. Each time a larva falls asleep for the first time, we say it "died" (not actually, but in survival statistics terms), so it is removed from the curve. The result is a survival curve when the proportion of larvae that did not sleep yet (Y axis) decreases in a step-wise fashion as the night goes by (X axis).

```R
## sleep latency survival plot
ggSleepLatencyGrid(pa="~/.../bhvparams/sleepLatency_220531_15.csv",
                   grporder=c('sorl1', 'scr'),
                   skipNight0=TRUE,
                   colours=c('#417dcd', '#697a87'),
                   xmaxh=3,
                   exportDir="~/.../plots/",
                   width=150,
                   height=70)
```

* `pa`: path to the sleep latency parameter table(s). Please give the path(s) to the .csv file(s) itself, not the _bhvparams_ directory. You can give multiple parameter tables here, e.g. `c("~/.../bhvparams/sleepLatency_220531_14.csv", "~/.../bhvparams/sleepLatency_220531_15.csv")`.
* `xmaxh`: where to stop the X axis, in number of hours after lights turn off. Best to decide this by trial and error.

For one experiment, `ggSleepLatencyGrid` will draw a horizontal grid, where each plot is one night.

It will also calculate survival statistics using a Cox Proportional-Hazards model. The output should look like:

> \>\>\> Cox Proportional-Hazards Model  
> 
> compared to reference group * sorl1 *,  
> 
> group scr is associated with...  
> Hazard Ratio =  1.802116  
> Hazard Ratio 95% confidence interval =  0.2107428  
> p-value =  0.005194907 **  
> i.e. at any time point, members of group scr were 80.21 % ± 21.07 % *more* likely to 'die' than members of group sorl1 ; pvalue = 0.005194907 ** 


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

```r
## LME stats
LMEreport(paDir="~/.../bhvparams/",
          grporder=c('wt', 'het', 'hom'),
          skipNight0=TRUE,
          exportPath="~/.../LMEreport.csv")
```

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

When we plotted the behaviour parameters (see **9– Plot every behaviour parameter**), the asterisks that get added when `statsOrNo=TRUE` are the `pvalsymbol`, i.e. they represent the p-value of the null hypothesis "group assignment has no effect on this behaviour parameter".  

### 12– Calculate a behavioural fingerprint

The "behavioural fingerprint" is a synthesised way of looking at the changes across all the behavioural parameters. For each unique parameter (e.g. number of sleep bouts at night), it is simply the *Z*-score of knockout larvae (or any other genotype/treatment group) from the controls' mean. Please refer to DOCUMENTATION for how exactly this is calculated.

We calculate the behavioural fingerprint with:

```r
## calculate behavioural fingerprint
calculateFingerprint(paDir="~/.../bhvparams/",
                     controlGrp='scr',
                     grporder=c('sorl1', 'scr'),
                     skipNight0=TRUE)
```

* `controlGrp`: whichever group you want to use as reference/baseline in your experiment. The fingerprint will represent the *Z*-scores from this reference. 

`calculateFingerprint(...)` will write _fingerprint.csv_ in the experiment folder.

You can give multiple *bhvparams/* directories to `calculateFingerprint(...)` in the format: `paDir=c("~/exp1/bhvparams/", "~/exp2/bhvparams/")`. The calculations will always use the controls within the same experiment (i.e. same YYMMDD_BX) for normalisation so this is equivalent to calculating fingerprints for each experiment separately, but it has the advantage to write a common _fingerprint.csv_ file.

### 13– Plot a behavioural fingerprint

We can give the _fingerprint.csv_ file to ggFingerprint(...) to plot:

```r
## plot behavioural fingerprint
ggFingerprint(fgp="~/.../fingerprint.csv",
              grporder=c('sorl1', 'scr'),
              controlGrp='scr',
              removeControl=TRUE,
              colours=c('#417dcd', '#a3bbdb'),
              legendOrNo=TRUE,
              xtextOrNo=TRUE,
              xParamNum=TRUE,
              ymin=-3,
              ymax=3,
              exportOrNo=TRUE,
              exportPath="~/Dropbox/FbyFdemo/plots/finger.pdf",
              width=200,
              height=100)
```

* `removeControl`: whether (TRUE) or not (FALSE) to plot the controls. Controls have all datapoints at *Z*-score = 0, as expected from calculating the mean of *Z*-scores.
* `xtextOrNo`: whether (TRUE) or not (FALSE) to write the names of the behavioural parameters as X axis labels.
* `xParamNum`: whether (TRUE) or not (FALSE) to write numbers instead of parameter names.

There are other ways to calculate/plot behavioural fingerprints, please refer to DOCUMENTATION for more details.

### 14– Compare behavioural fingerprints

Do you have multiple experiments you want to compare? For example replicate experiments of the same genotype. We can calculate the similarity between pairwise behavioural fingerprints and represent the results as a heatmap.

```r
ggPairwiseHeat(fgp="~/.../fingerprint.csv",
               simScore='cosine',
               grporder=c('sorl1', 'scr'),
               controlGrp='scr',
               minCol=NA,
               maxCol=NA,
               onlyHalf='upper',
               scoreSize=5,
               legendOrNo=TRUE,
               labelsOrNo=TRUE,
               width=100,
               height=100,
               exportPath="~/Dropbox/FbyFdemo/heat.pdf")
```

* `simScore`: choose one of three possible "similarity scores": `correlation` (Pearson correlation), `cosine` (cosine similarity), `euclidean` (Euclidean distance).
* `minCol`: colour for the start of the colour gradient, i.e. colour assigned to –1 when `correlation` or `cosine` is used or to 0 when `euclidean` is used.
* `maxCol`: colour for the end of the colour gradient, i.e. colour assigned to +1 when `correlation` or `cosine` is used or to the maximum value when `euclidean` is used.
* `onlyHalf`: as we are plotting a pairwise matrix, the heatmap is symmetrical across the diagonal, i.e. the similarity scores repeat each other on each side of the diagonal. If that is okay, you can omit this setting or give `NA`. If you prefer to plot only one side of the diagonal, you can choose between `upper` or `lower`, which will respectively plot the upper or lower half.
* `scoreSize`: font size for the similarity scores.

`ggPairwiseHeat(...)` will also return in Console the similarity score between fingerprints.  

> And that is all for the minimal tutorial! The FramebyFrame package can do other things, such as analyse different experimental designs, calculate and plot behavioural fingerprints and more. Make sure to check the documentation to make full use of it.  

___

## R basics

Here is the minimum you need to know about R to get started.
> Did I forget something? Let me know and we will make it 1% easier for the next user!

* Download and install R & RStudio from https://posit.co/download/rstudio-desktop/. R is the coding language, RStudio is a fancy text editor to make it easier to write R code.

* Open RStudio and start a new script (File > New File > R Script or Cmd/Ctrl + ↑ + N). The panel that just opened is your script. This is where you write R code. The bottom is the Console, this where the computer answers. In the top-right is the Environment, this is where you keep stuff in memory.

* For example, try to write in your script:

```r
print("Hello World")
```

  Now to run the command, place your cursor at the end of the line or select the command and press Cmd/Ctrl + Enter.
  In the Console, you will see your command repeated, then the answer from the computer:

      > print("Hello World")
      [1] "Hello World"

* The computer does not see lines that start with `#`. Use that to write comments as notes to yourself or others. e.g.

```r
# this is a comment
print("Hello World")
```

* As you may notice, you need to indicate to R what is actual text (and not computer code) by using double quotes, like `"Hello World"`. One unit of text is called a string.

* You can give R a series of things at once, it is called a vector. In R it is written `c(thing1, thing2)`. Examples:

```r
c("ho", "hey")
c(1, 2, 3)
c("hey", 3)
```

* `NA` stands for "Not Available". It is a way of telling R "missing value".
* A "logical" simply means `TRUE` or `FALSE`. In R, you write the full word in uppercase.
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

```r
install.packages("here")
library(here)
```

In Console, you should see: `> here() starts at /Users/.../myExperiment`. Basically, `here` sees your .Rproj file and will now start all the paths from there. This makes writing paths less painful. For example, instead of writing `"~/Desktop/myExperiment/210907_12_RAWs.csv"`, you can simply write `here("210907_12_RAWs.csv")`. Another advantage is that if you move your experiment folder or change its name, you will not need to update all the paths in your script. For example, imagine we moved the folder _myExperiment_ to _Documents_, we would need to update all our paths if we were using absolute paths. Instead, if we use relative paths (thanks to `here`), as long as you did not touch the .Rproj file everything is still working.

Next time you work on that analysis, double-click the .Rproj file to open RStudio and get started, not the .R script. This will ensure `here()` starts in your project folder.  

___

## FAQ

> _I use the Noldus from DanioVision (or else). Can I still use your amazing package?_  

Yes, it is possible. Get in touch with me. At least one person has used it successfully with data from DanioVision's Noldus, but the conversion is not yet integrated in the package. I could work on it if there is demand.

> _I do not have a Zebralab .xls results file!_

No problem. The only information we get from the Zebralab file is the date and time when your experiment started. You can give that information manually to `vpSorter()` instead:

```r
vpSorter(ffDir=...,
         zebpath=NA,
         boxGen=...,
         twoBoxMode=...,
         boxnum=...,
         zt0=...,
         date0="07/09/2021",
         time0="15:34:05",
         dayduration=...)
```

Ideally, you should be precise at the second as FramebyFrame will use this to determine when the light transitions occured.  

If the experiment crashed and did not generate a Zebralab results file, one source for the date0/time0 may be the .phc file, if you can find it in the folder. Open the file in a text editor (e.g. Notepad) and you should find the start date and time at the top. Make sure to give date0 as DD/MM/YYYY and time0 as HH:MM:SS (24-hr format, so e.g. 9 PM should be 21).

> _The first timestamp in my Zebralab results file is wrong!_

Yes, this happens when running a Replay on Zebralab, for example. The software (stupidly) takes the computer clock time when you started the Replay, not the original start of the experiment. Follow instructions above to give the start date/time manually to `vpSorter()`.

> _I do short experiments without any light transitions. Can I still use the package to analyse my data?_  

You can use some of it. When calculating behaviour parameters (`multiBehaviourParameter(...)`), please make use of the `woi` setting to define one or more time window(s) within your experiment (see DOCUMENTATION.md).

Currently `ggParameterGrid(...)` and the LME statistics unfortunately expect full days/nights, but it is certainly possible to make them work for short experiments. Get in touch with me so I have some motivation to work on this!

___

## Experimental design commandments

* _**I shall have my controls in the same Zebrabox and from the same clutch**_  
As you are guaranteed to have variability between clutches and likely have variability between Zebraboxes (e.g. light levels or temperature not being exactly equal) or between runs. In other words, you _**never**_ want to be in a situation where you are comparing e.g. WT clutch1 vs KO clutch2, or WT Zebrabox1 vs KO Zebrabox2. In this situation, you are almost _guaranteed_ to find a difference because of clutch-to-clutch variability or box-to-box variability. In this case, claiming that the animals' genotype contributes to the difference you observe is just wishful thinking. When doing experiments with stable knockout lines, this is why we track the offspring of an in-cross of heterozygous adults, i.e. the clutch is 25% homozygous (–/–), 50% heterozygous (+/–), 25% wild-type (+/+).  
More broadly, when analysing multiple experiments simultaneously, try to have every group represented in each Zebrabox.  

* _**I shall refrain from drawing important conclusions from a single clutch**_  
The variability between clutches can be _very_ important. Accordingly, drawing important conclusions from a single clutch is asking for future trouble, for example the phenotype not replicating in a more complex experiment. Try to track at least two clutches to control for biological (clutch-to-clutch) and technical (Zebrabox-to-Zebrabox or run-to-run) variability. Give all your replicates to the LME command and let it worry about it, I think this is a better solution than calculating statistics on experiments one by one.  

* _**I shall refrain from comparing absolute datapoints between clutches**_  
Again, the variability between clutches can be _very_ important, so making comparisons where both the group and the clutch/Zebrabox are different (e.g. wild-type from clutch #1 vs homozygous from clutch #2) is asking for wrong conclusions. It is still possible to do comparisons between clutches (e.g. is the effect size larger in clutch #1 vs clutch #2 ?), but you will need to normalise the datapoints to within-clutch and within-Zebrabox controls first, using for example _Z_-scores.  

* _**I shall randomise the well positions as best as possible**_  
Disclaimer: I do not know how important this is, but probably best to be safe.  
When you place the larvae in the wells, do you know which group each larva belongs to (e.g. injected vs uninjected or drug-treated vs DMSO)? If yes, then alternate the rows or columns to avoid edge effects (e.g. water from the well evaporates faster from the outer rows/columns). If you are tracking the offspring of a cross (e.g. heterozygous in-cross) and will genotype the larvae _after_ the experiment, then the wells are already randomised. Problem solved!  

* _**(optional) I shall track a single clutch in each Zebrabox**_  
Where clutch means offspring from the same parents and (ideally) mating event. The easiest experimental design is one where you have one clutch per Zebrabox/experiment. In other words, I recommend you do not mix multiple clutches in the same Zebrabox.  

**_exception 1_**: if you are lucky enough to have multiple Zebraboxes and a very large clutch, you can track the same clutch in two Zebraboxes. You should mention this when calculating the LME statistics so it can adapt the random effects accordingly (more details in DOCUMENTATION, below `LMEdaynight()`). For example:

```r
LMEreport(paDir=c(here('220906_run1', 'bhvparams'),
                  here('220912_run2', 'bhvparams')),
          sameClutch1=c('220906_01', '220906_02'),
          sameClutch2=c('220912_03', '220912_04'),
          grporder=c('wt', 'het', 'hom'),
          skipNight0=TRUE,
          exportPath=here('LMEreport.csv'))
```
means that experiments _220906_01_ + _220906_02_ tracked the same clutch #1 and that experiments _220912_03_ + _220912_04_ tracked the same clutch #2.

**_exception 2_**: alternatively, if you are unlucky and only have small clutches and one Zebrabox, you can track multiple clutches in the same Zebrabox (while keeping note of which larva is from which clutch!). To analyse that experimental design, the procedure is (assuming a single Zebrabox):  

– prepare one genotypeMap.xlsx for each clutch, leaving all the other wells as 'empty', e.g. you should make 230306_01_genotypeMapclutch1.xlsx and 230306_01_genotypeMapclutch2.xlsx.  
– run `genotypeGenerator(...)` once for each genotypeMap. You will likely need to rename the output files (especially genotype.txt) so they do not overwrite each other. For example:
```r
genotypeGenerator(plateMap="~/.../230306_01_genotypeMapclutch1.xlsx")
# rename output file 230306_01genotype.txt into 230306_01genotypeclutch1.txt
genotypeGenerator(plateMap="~/.../230306_01_genotypeMapclutch2.xlsx")
# rename output file 230306_01genotype.txt into 230306_01genotypeclutch2.txt
```  
– when calculating behaviour parameters, give one `ffpath` and all the `genopath` (one per clutch), for example:
```r
multiBehaviourParameter(parameters='all',
                        ffpath='~/.../230306_01_RAWs.csv',
                        genopath=c('~/.../230302_01genotypeclutch1.txt',
                                   '~/.../230302_14genotypeclutch2.txt'),
                        skipNight0=FALSE,
                        dayduration=14)
```
– the parameter tables (in folder _bhvparams/_) will now have a new column _clutch_. The clutch number simply corresponds to the order in which the genotype files were given (`genopath=` above), i.e. _clutch1_ are all larvae found in _230302_01genotypeclutch1.txt_, _clutch2_ are all larvae found in _230302_01genotypeclutch2.txt_.  
– run `ggParameterGrid()` and `LMEreport()` as usual. The formula of the linear mixed effects model will be adapted automatically to control for multiple clutches within the same box (you can read about this in DOCUMENTATION below `LMEdaynight()`).

___

## Version history

To see which version of FramebyFrame you have, run:
```r
packageVersion("FramebyFrame")
```

### v0.1.0
14/02/2023
First real version (arbitrary).

### v0.2.0
20/02/2023
* ggFingerprint() now allows multiple paths as input (`fgp`).
* Can give multiple bhvparams directories to calculateFingerprint().

### v0.3.0
16/03/2023  
Allows analysis of an experiment where there are multiple clutches in the same Zebrabox. Read below _Experimental design commandments_ and in DOCUMENTATION below _LMEdaynight()_ for details about how to proceed.

### v0.4.0
11/04/2023  
(Issuing as new version as it will affect slighly existing results, mainly of parameter sleep latency).  
Caught small inconsistency when detecting sleep bouts. For an empty well or if the larvae was asleep throughout the light transition, the first asleep frame was #1500 (assuming `zthr_min`=1 and framerate 25 fps). However, in this situation, first asleep frame should #1, as definition of sleep is retroactive, e.g. if at frame #1500, the larva had been asleep for 1 minute, this means the sleep bout started exactly one minute earlier, i.e. at frame #1. In this situation, the first asleep frame is now correctly marked as #1. See comments in function detectNaps() for more details.  
I think the main consequence of this change is on parameter sleepLatency. Previously, it was returning ~ 1.0 min for empty wells or if the larva was asleep during the transition. Now it returns 0.0 min (more precisely 0.00067 min, i.e. one frame in minute) which I think is more accurate.  

### v0.5.0
New settings `fainterExp` and `faintMax` for ggParameter/ggParameterGrid. See DOCUMENTATION.  

### v0.6.0
New setting `connectMean` for ggParameter/ggParameterGrid. See DOCUMENTATION. Might change default to TRUE once confirmed it behaves as expected.  