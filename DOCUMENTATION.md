# Documentation

Francois Kroll, 2022 @Rihel lab, UCL.

Full guide to the FramebyFrame R package.

Make sure to follow first the Minimal tutorial (in README) for a smoother introduction to the package.  

[![alt text][1.2]][1] [@francois_kroll](https://twitter.com/francois_kroll)

:email: francois@kroll.be

<!-- icons with padding -->
[1.1]: http://i.imgur.com/tXSoThF.png (twitter icon with padding)

<!-- icons without padding -->
[1.2]: http://i.imgur.com/wWzX9uB.png (twitter icon without padding)

<!-- links to your social media accounts -->
[1]: https://twitter.com/francois_kroll

## General

FramebyFrame assumes that your experiment started during a day.  

It also assumes that the total day + night duration is 24 hours, but it is flexible as to how long the day and night phase lasted.

In the description of settings for each function below, _Default is …_ means this is what the value will be if you do not mention this setting in your call.

For p-values,
*	\> 0.05 is ns
*	<= 0.05 is *
*	<= 0.01 is **
*	<= 0.001 is ***

Plotting functions start with _gg_, e.g. ggFramerate. This is because it uses a R package called `ggplot2` under the hood.

___

## Sorting the frame-by-frame data

### vpSorter(...)

Sorts the Δ pixel data from a bunch of raw .xls/xlsx files and generates files _RAWs.csv and _lights.csv.

**ffDir**: full path to folder which stores all the raw .xls/xlsx files from Zebralab's raw export.  

**zebpath**: full path to Zebralab .xls/xlsx results file. In practice, vpSorter only looks at the first row to obtain the start time/date of the experiment. Default is NA.  

**boxGen**: 2 if you are using the newer version of Zebralab (post circa 2020), 1 for previous versions. Default is 2.  

> Note sure which one? Open one of the raw .xls/xlsx files, are the Δ pixel values (typically in column _data1_) mostly 1s or mostly 0s? If mostly 1s: `boxGen=1`; if mostly 0s: `boxGen=2`. 

**twoBoxMode**: if two boxes ran in parallel, twoBoxMode=TRUE; if a single box ran at a time, twoBoxMode=FALSE. Precisely the question is: do the raw .xls/xlsx files contain data from two boxes or a single one? If you are unsure, you can open one of the .xls/xlsx files and look at the wells. Default is TRUE.  

**boxnum**: if ran two boxes in parallel, which box do you want to process? It can only be 1 or 2. Default is 1.  

**zt0**: what time do you want to take as reference Zeitgeber? Given as HH:MM:SS. This is typically the lights ON time. Default is '09:00:00'.  

**date0**: if you do not have a Zebralab results file or there is an issue with it, you can give the date at which the experiment started manually instead, in the format DD/MM/YYYY, e.g. `date0="17/05/2022"`. Default is NA.  

**time0**: if you do not have a Zebralab results file or there is an issue with it, you can give the time at which the experiment started manually instead, in the format HH:MM:SS, e.g. `time0="13:02:25"`. Default is NA.  

**dateformat**: the format in which the date is given in the Zebralab results file. You only need to mention this setting if you are in the US and the Zebralab results file gives date as MM/DD/YYYY, in which case please give `dateformat="MDY"`. Default is 'DMY'.  

**dayduration**: during your experiment, how long did the days (lights ON) last? FramebyFrame currently assumes 24-hr cycles, so e.g. if dayduration=14, it will understand this as: days last 14 hr, nights last 10 hr. Default is 14.  

**exportXlsOrNo**: you probably do not need to worry about this. TRUE will attempt to export all the .xls files after correcting the errors, essentially as if Zebralab did not make any error. Default is FALSE.

___

## Quality checks

### ggFramerate(...)

Plots instantaneous frame rates over the course of the experiment.

**ffpath**: full path to _RAWs.csv.  

**zebpath**: full path to Zebralab .xls results file.  

**subsample**: whether or not (TRUE or FALSE) to subsample the frame rates. It calculates instantaneous frame rate between every successive frame so it ends up with a lot of data points, subsample allow to not plot all of them. If use FALSE, you will have to wait for a bit. Default is TRUE.  

**subsample_by**: if subsampling, plot only every nth data point, thereby decreasing the total number of data points plotted by n. For example subsample_by=1000 will only plot every 1000th instantaneous frame rate, thereby 1000 times fewer points. Default is 1000.  

**xstart**: start of X axis, in hours since 9 AM of day0. Default is 0.  

**xstop**: end of X axis, in hours, in hours since 9 AM of day0. Default is 0, which plots the entire timecourse.  

**ymin**: start of Y axis, in frames-per-second. Default is 0.  

**ymax**: end of Y axis, in frames-per-second. Default is 30.  

**sunlines**: whether or not (TRUE or FALSE) to draw dashed vertical lines representing the light transitions.  

**dayduration**: how many hours do the days last in your experiment. Default is 14.  

**xname**: name of the X axis.  

**yname**: name of the Y axis.  

**exportOrNo**: whether or not (TRUE or FALSE) to export the plot to pdf. Default is FALSE.  

**width**: width of pdf in mm. Default is 75.  

**height**: height of pdf in mm. Default is 55.  

**exportPath**: full path to export file. It will create a pdf. Accordingly, exportPath must finish with .pdf.  

___

## Genotype file

### genotypeGenerator(...)

Generates a genotype file from a .xlsx plate map. We say "genotype" here as Rihel lab experiments often look at larvae of different genotypes, but groups can represent anything, e.g. different concentrations of a drug.  

To prepare the genotype map, download one of the templates in directory _genotypeMap_templates_ and follow the instructions there. Please do not use special characters such as µ in the group names.

**plateMap**: full path to .xlsx plate map (genotype map).  

___

## Timecourse plots

### ggActivityTraceGrid(...)

Plots a grid of single activity traces.  

ggActivityTraceGrid currently does not support making a legend. Accordingly, please confirm for a few wells that the colours are correctly matched with the groups.

**ffpath**: full path to _RAWs.csv.  

**genopath**: full path to genotype.txt.  

**zebpath**: full path to Zebralab .xls results file.  

**dayduration**: how many hours do the days last in your experiment. Default is 14.  

**smoothOrNo**: whether or not (TRUE or FALSE) to smooth the frame-by-frame data. Default is TRUE.  

**smooth_nsecs**: Number of seconds in rolling average during smoothing. Higher number gives a smoother trace. Setting will be ignored if smoothOrNo=FALSE. Default is 30\*60 = 1800 seconds = 30 minutes.  

**binOrNo**: whether or not (TRUE or FALSE) to bin the frame-by-frame data. Beware, not binning will take a while to plot. Default is TRUE.  

**bin_nsecs**: Number of seconds in bin for binning. Higher number gives fewer datapoints plotted. For example, 600 seconds (10 minutes) sums the data in 10-minute bins. Setting will be ignored if binOrNo=FALSE. Default is 10\*60 = 600 seconds = 10 minutes.  

**onlyWell**: do you only want to plot some specific well(s)? You can give them here as ‘fX’. Examples: onlyWell='f1' or onlyWell=c('f1', 'f5', 'f7'). You can give well(s) not present in the genotype file, it will plot them in light grey. Default is NA, which plots every well in the data, including those not mentioned in light grey.  

**tracecols**: one trace colour per group in genotype file. It understands a bunch of colour words like ‘red’ or ‘blue’ or you can give them as HEX codes (can use Eyedropper tool in Illustrator to get HEX colour code, for example). Give the colours in the order of the groups in the genotype file. For example, tracecols=c('#fcb505', '#78ac63'). Default is NA which uses default R colours.  

**linethick**: thickness of the trace. Default is 0.4.  

**ymin**: start of the Y axis. Default is 0.  

**ymax**: end of the Y axis. I recommend finding a suitable value by trial and error.

**xstart**: start of the X axis, in hours since 9 AM of day0. Default is 0.  

**xstop**: end of the X axis, in hours since 9 AM of day0. Default is 0, which means plot all the timecourse.  

**trimstart**: whether to trim some of the data at the start, in number of hours since 9 AM of day0. e.g. trimstart = 24 trims all the data before 9 AM night0 >> day1 transition (i.e. 24 hours after 9 AM day 0). It is slightly different than xstart because xstart leaves a little bit of the data before the start (try to see how it looks without trimming). If you want the trace to start at 24 hours (9 AM day1) sharp, use xstart=24 and trimstart=24 together. Default is 0, i.e. no trimming.  

**trimstop**: whether to trim some of the data at the end, in number of hours since 9 AM of day0. e.g. trimstart = 72 trims all the data after 9AM night2 >> day3 transition. (i.e. 72 hours after 9 AM day 0). It is slightly different than xstop because xstop leaves a little bit of the data after the stop (try to see how it looks without trimming). If you want the trace to stop at 72 hours (9 AM day3) sharp, use xstart=72 and trimstart=72 together.

Default is 0, i.e. no trimming. Default is 0, which means no trimming.  
Note, xstart/xstop & trimstart/trimstop are especially useful to align plots from different experiments, as you probably did not start/stop the experiments at exactly the same times.

**xmajorOrNo**: whether or not (TRUE or FALSE) to draw the major vertical grid lines in the background of the plot. Default is TRUE.  

**ymajorOrNo**: whether or not (TRUE or FALSE) to draw the major horizontal grid lines in the background of the plot. Default is TRUE.

**nightBgOrNo**: whether or not (TRUE or FALSE) to add grey backgrounds to represent the nights.  

**sunlinesOrNo**: whether or not (TRUE or FALSE) to draw vertical dashed lines representing the light transitions. You probably do not need these light transition lines if you are happy with the night backgrounds (when nightBgOrNo=TRUE, see above). However, if for whatever reason you want to add the night backgrounds yourself in Illustrator or else, I recommend exporting the plot once with sunlinesOrNo=TRUE so you can place your grey rectangles correctly, then return to R, export the plot without the sunlines (sunlinesOrNo=FALSE) and replace the file in your graphics software. Default is FALSE.  

**markTimes**: specific time(s) to mark as vertical dashed lines. Give them in the format YYYY-MM-DD HH:MM:SS. For example, markTimes=c('2022-01-25 11:40:00', '2022-01-26 12:40:00'). Lines will be in the same style as the sunlines (see setting sunlinesOrNo). To not mark any times, simply skip this setting or give NA. Default is NA.  

**ncol**: how many columns should the grid have?  

**nrow**: how many rows should the grid have?  

If you are plotting every well (so onlyWell is not mentioned or onlyWell=NA), then what I recommend is setting ncol and nrow to the number of columns/rows you have in your plate, that way the grid will represent your plate. For a 96-well plate: ncol=12 and nrow=8.  

**exportOrNo**: Whether or not (TRUE or FALSE) to export the plot. Default is TRUE.  

**exportPath**: full path to export file. It will create a pdf. Accordingly, exportPath must finish with .pdf.  

**width**: width of pdf in mm. Default is 75.  

**height**: height of pdf in mm. Default is 55.  

As above, you can set width and height to be same size or width:height ratio as your plate. For a 96-well plate: width=255 and height=171 work well and are exactly twice the dimensions of a real 96-well plate.  

### ggActivityTraceByGroup(...)

Plots binned & smoothed activity traces by group.

**ffpath**: full path to _RAWs.csv.

**genopath**: full path to genotype.txt.

**zebpath**: full path to Zebralab .xls results file.

**dayduration**: how many hours do the days last in your experiment. Default is 14.

**smoothOrNo**: whether or not (TRUE or FALSE) to smooth the frame-by-frame data. Default is TRUE.

**smooth_nsecs**: Number of seconds in rolling average during smoothing. Higher number gives a smoother trace.  
Setting will be ignored if `smoothOrNo=FALSE`. Default is 30\*60 = 1800 seconds = 30 minutes.

**binOrNo**: whether or not (TRUE or FALSE) to bin the frame-by-frame data. Beware, not binning will take a while to plot. Default is TRUE.

**bin_nsecs**: Number of seconds in bin for binning. Higher number gives fewer datapoints plotted. For example, 600 seconds (10 minutes) sums the data in 10-minute bins. Setting will be ignored if binOrNo=FALSE. Default is 10*60 = 600 seconds = 10 minutes.

**ribbon**: how ribbon (error bar around mean trace) should be calculated. Options are ‘sd’ for standard deviation or or ‘sem’ standard error of the mean.

**grporder**: do you have a preferred order for the groups (genotypes)? If yes, mention it here. If no, you can simply not mention this setting or give `grporder=NA`. You can exclude any group (genotype) by simply not mentioning it here. Default is NA, which plots all groups present in the data in the alphabetical order.

**onlyWell**: do you only want to plot some specific well(s)? You can give them here as ‘fX’. Examples: onlyWell='f1' or onlyWell=c('f1', 'f5', 'f7'). You can give well(s) not present in the genotype file, it will plot them in light grey and label them as ‘excluded’ in legend. Default is NA, which plots the larvae present in the genotype file.

**tracecols**: colours of the mean trace(s). If also giving grporder, give the colours in the same order. Default is NA which uses default R colours.

**ribboncols**: colours of the ribbon(s). If also giving grporder, give the colours in the same order. Default is NA which uses default R colours with 50% transparency.

**linethick**: thickness of the mean trace. Default is 0.4.

**xname**: name of the X axis.

**yname**: name of the Y axis.

**xtextOrNo**: whether or not (TRUE or FALSE) to write the units on the X axis. Default is TRUE.

**ytextOrNo**: whether or not (TRUE or FALSE) to write the units on the Y axis. Default is TRUE.

**ymin**: start of the Y axis. Default is 0.

**ymax**: end of the Y axis. I recommend finding a suitable value by trial and error.

**xstart**: start of the X axis, in hours since 9 AM of day0. Default is 0.

**xstop**: end of the X axis, in hours since 9 AM of day0. Default is 0, which means plot all the timecourse.

**trimstart**: whether to trim some of the data at the start, in number of hours since 9 AM of day0. e.g. trimstart = 24 trims all the data before 9 AM night0 >> day1 transition (i.e. 24 hours after 9 AM day 0). It is slightly different than xstart because xstart leaves a little bit of the data before the start (try to see how it looks without trimming). If you want the trace to start at 24 hours (9 AM day1) sharp, use xstart=24 and trimstart=24 together. Default is 0, i.e. no trimming.

**trimstop**: whether to trim some of the data at the end, in number of hours since 9 AM of day0. e.g. trimstart = 72 trims all the data after 9AM night2 >> day3 transition. (i.e. 72 hours after 9 AM day 0). It is slightly different than xstop because xstop leaves a little bit of the data after the stop (try to see how it looks without trimming). If you want the trace to stop at 72 hours (9 AM day3) sharp, use xstart=72 and trimstart=72 together. Default is 0, i.e. no trimming. Default is 0, which means no trimming.

Note, xstart/xstop & trimstart/trimstop are especially useful to align plots from different experiments, as you probably did not start/stop the experiments at exactly the same times.

**xmajorOrNo**: whether or not (TRUE or FALSE) to draw the major vertical grid lines in the background of the plot. Default is TRUE.

**ymajorOrNo**: whether or not (TRUE or FALSE) to draw the major horizontal grid lines in the background of the plot. Default is TRUE.

**nightBgOrNo**: whether or not (TRUE or FALSE) to add grey backgrounds to represent the nights.

**sunlinesOrNo**: whether or not (TRUE or FALSE) to draw vertical dashed lines representing the light transitions. You probably do not need these light transition lines if you are happy with the night backgrounds (when nightBgOrNo=TRUE, see above). However, if for whatever reason you want to add the night backgrounds yourself in Illustrator or else, I recommend exporting the plot once with sunlinesOrNo=TRUE so you can place your grey rectangles correctly, then return to R, export the plot without the sunlines (sunlinesOrNo=FALSE) and replace the file in your graphics software. Default is FALSE.

**markTimes**: specific time(s) to mark as vertical dashed lines. Give them in the format YYYY-MM-DD HH:MM:SS. For example, markTimes=c('2022-01-25 11:40:00', '2022-01-26 12:40:00'). Lines will be in the same style as the sunlines (see setting sunlinesOrNo). To not mark any times, simply skip this setting or give NA. Default is NA.  

**legendOrNo**: whether or not (TRUE or FALSE) to make the legend. I would recommend first generating the plot with the legend so you can check that the groups and colours are correctly matched. Default is TRUE.

**exportOrNo**: whether or not (TRUE or FALSE) to export the plot. Default is TRUE.

**exportPath**: full path to export file. It will create a pdf. Accordingly, exportPath must finish with .pdf.

**width**: width of pdf in mm. Default is 75.

**height**: height of pdf in mm. Default is 55.

#### Would you prefer plotting the timecourse yourself?
ggActivityTraceByGroup writes the binned & smoothed data in the same folder as the Zebralab .xls results file.

Filename will be _acttc_YYMMDD_BX_smoXXX_binXXX.csv_:
*	acttc stands for activity timecourse
* YYMMDD = experiment date (taken from filename of _RAWs.csv)
*	BX = box number (taken from filename of _RAWs.csv)
*	smo XXX = number of seconds of the rolling average for smoothing
*	bin XXX = number of seconds for binning

Format is columns = wells (f XX for e.g. fish 53) / rows = timepoints. Second row gives the genotype (group) of each well. If well was not mentioned in genotype file, group is written as ‘excluded’.

Units of datapoints depend on the binning setting, e.g. 10\*60 seconds = 10 minutes binning would give unit = sum px/10 min.

First three columns give time information:
*	fullts = full timestamps with date and clock time.
*	zhrs = number of hours since 9 AM on day0 (i.e. day when experiment was started).
*	exsecs = number of seconds since start of the experiment.

All timestamps represent the time of the last frame taken in the bin during binning. For example, if binning is set to 30 minutes, zhrs=9.5 would mean that this datapoint is the sum of the frames from zhrs 9.0 up to zhrs 9.5.

Note, some rows at the start may have been trimmed. This is for two reasons:
* smoothing calculates rolling averages of the previous `smooth_nsecs` seconds. Accordingly, it cannot start at the first frame. For example, if smoothing is set to 30*60 seconds = 30 minutes, the rolling average can only start at row (frame) 30 minutes * 60 seconds * 25 frames-per-second = 45,000th row/frame, i.e. the 45,499 rows (~ 30 minutes worth of frames) will be lost.
* additional rows may have been trimmed prior to binning to obtain a total number of rows divisible by the bin size.

Any trimming is done at the start of the timecourse as we do not usually analyse the first hours of tracking.

### ggSleepTraceByGroup(...)

Plots binned & smoothed sleep traces by group.

**ffpath**: full path to _RAWs.csv.

**genopath**: full path to genotype.txt.

**zebpath**: full path to Zebralab .xls results file.

**zthr_min**: stands for zzz threshold in minutes, i.e. minimum number of minutes of inactivity to call a period of inactivity a sleep bout. Default is 1.

**inaThr**: the maximum Δpixel to call a frame inactive during sleep bout detection. For example, inaThr=3 means “count any frame below or equal 3 Δpixel as inactive”. The setting is analogous to the Freezing threshold in Zebralab. I do not recommend changing this parameter without a good reason to do so. In my opinion, 1 minute of inactivity should genuinely mean 1 minute of inactivity, i.e. a continuous 1,500 frames which are all 0 Δ pixel. Having said that, if you used a low Sensitivity setting in Zebralab (< 20 on the newer Zebrabox), you may get some false positive Δ pixel detection. A good test is to scroll through the frame-by-frame data (in the _RAWs.csv) of empty wells. If there are some small scattered numbers (e.g. 1–3), it may be justified to set inaThr above 0, for example inaThr=3. Default is 0.

**epo_min**: stands for epoch in minutes, i.e. epoch duration to sum the data. Note, what you choose here will define the unit of the Y axis. For example, 10 minutes would give number of minutes asleep/10 minutes as Y axis unit. Default is 10.

**dayduration**: how many hours do the days last in your experiment. Default is 14.

**smoothOrNo**: whether or not (TRUE or FALSE) to smooth the sleep timecourse data. Note, smoothing is applied on the data after it is summed in epochs. Default is TRUE.

**smooth_npoints**: Number of datapoints in rolling average during smoothing. Higher number gives a smoother trace. Note, smoothing is applied on the data after it is summed in epochs, this is why the setting is in datapoints and not in seconds or minutes. For example, if epo_min=10 and smooth_npoints=5, it means the rolling average will calculate successive means of the previous 5 datapoints = 50 minutes. Setting will be ignored if smoothOrNo=FALSE. Default is 5.

**ribbon**: how ribbon (error bar around mean trace) should be calculated. Options are ‘sd’ for standard deviation or or ‘sem’ for standard error of the mean.

**grporder**: do you have a preferred order for the groups (genotypes)? If yes, mention it here. If no, you can simply not mention this setting or give grporder=NA. You can exclude any group (genotype) by simply not mentioning it here. Default is NA, which plots all groups present in the data in alphabetical order.

**onlyWell**: do you only want to plot some specific well(s)? You can give them here as ‘fX’. Examples: onlyWell='f1' or onlyWell=c('f1', 'f5', 'f7'). You can give well(s) not present in the genotype file, it will plot them in light grey and label them as ‘excluded’ in legend. Default is NA, which plots the larvae present in the genotype file.

**tracecols**: colours of the mean trace(s). If also giving grporder, give the colours in the same order. Default is NA which uses default R colours.

**ribboncols**: colours of the ribbon(s). If also giving grporder, give the colours in the same order. Default is NA which uses default R colours with 50% transparency.

**linethick**: thickness of the mean trace. Default is 0.4.

**xname**: name of the X axis.

**yname**: name of the Y axis.

**xtextOrNo**: whether or not (TRUE or FALSE) to write the units on the X axis. Default is TRUE.

**ytextOrNo**: whether or not (TRUE or FALSE) to write the units on the Y axis. Default is TRUE.

**ymin**: start of the Y axis. Default is 0.

**ymax**: end of the Y axis. I recommend finding a suitable value by trial and error.

**xstart**: start of the X axis, in hours since 9 AM of day0. Default is 0.

**xstop**: end of the X axis, in hours since 9 AM of day0. Default is 0, which means plot all the timecourse.

**trimstart**: whether to trim some of the data at the start, in number of hours since 9 AM of day0. e.g. trimstart = 24 trims all the data before 9 AM night0 >> day1 transition (i.e. 24 hours after 9 AM day 0). It is slightly different than xstart because xstart leaves a little bit of the data before the start (try to see how it looks without trimming). If you want the trace to start at 24 hours (9 AM day1) sharp, use xstart=24 and trimstart=24 together. Default is 0, i.e. no trimming.

**trimstop**: whether to trim some of the data at the end, in number of hours since 9 AM of day0. e.g. trimstart = 72 trims all the data after 9AM night2 >> day3 transition. (i.e. 72 hours after 9 AM day 0). It is slightly different than xstop because xstop leaves a little bit of the data after the stop (try to see how it looks without trimming). If you want the trace to stop at 72 hours (9 AM day3) sharp, use xstart=72 and trimstart=72 together. Default is 0, i.e. no trimming. Default is 0, which means no trimming.

Note, xstart/xstop & trimstart/trimstop are especially useful to align plots from different experiments, as you probably did not start/stop the experiments at exactly the same times.

**xmajorOrNo**: whether or not (TRUE or FALSE) to draw the major vertical grid lines in the background of the plot. Default is TRUE.

**ymajorOrNo**: whether or not (TRUE or FALSE) to draw the major horizontal grid lines in the background of the plot. Default is TRUE.

**nightBgOrNo**: whether or not (TRUE or FALSE) to add grey backgrounds to represent the nights.

**sunlinesOrNo**: whether or not (TRUE or FALSE) to draw vertical dashed lines representing the light transitions. You probably do not need these light transition lines if you are happy with the night backgrounds (when nightBgOrNo=TRUE, see above). However, if for whatever reason you want to add the night backgrounds yourself in Illustrator or else, I recommend exporting the plot once with sunlinesOrNo=TRUE so you can place your grey rectangles correctly, then return to R, export the plot without the sunlines (sunlinesOrNo=FALSE) and replace the file in your graphics software. Default is FALSE.

**markTimes**: specific time(s) to mark as vertical dashed lines. Give them in the format YYYY-MM-DD HH:MM:SS. For example, markTimes=c('2022-01-25 11:40:00', '2022-01-26 12:40:00'). Lines will be in the same style as the sunlines (see setting sunlinesOrNo). To not mark any times, simply skip this setting or give NA. Default is NA.

**legendOrNo**: Whether or not (TRUE or FALSE) to make the legend. I would recommend first generating the plot with the legend so you can check that the groups and colours are correctly matched. Default is TRUE.

**exportOrNo**: Whether or not (TRUE or FALSE) to export the plot. Default is TRUE.

**exportPath**: full path to export file. It will create a pdf. Accordingly, exportPath must finish with .pdf.

**width**: width of pdf in mm. Default is 75.

**height**: height of pdf in mm. Default is 55.

#### Would you prefer plotting the timecourse yourself?
ggSleepTraceByGroup writes the data (in number of minutes asleep/epoch, before any smoothing) in the same folder as the Zebralab .xls results file.

Filename will be _zzztc_YYMMDD_BX_epoXXX.csv_:
*	zzztc stands for sleep (zzz) timecourse
*	YYMMDD = experiment date (taken from filename of _RAWs.csv)
*	BX = box number (taken from filename of _RAWs.csv)
*	epo XXX = number of minutes in each epoch

Format is columns = wells (f XX for e.g. fish 53) / rows = timepoints. Second row gives the genotype (group) of each well. If a well was not mentioned in the genotype file, its group is written as ‘excluded’.
Units of datapoints depend on the binning setting, e.g. 10*60 seconds = 10 minutes binning would give unit = sum px/10 min.

First three columns give time information:
* fullts = full timestamps with date and clock time.
*	zhrs = number of hours since 9 AM on day0 (i.e. day when experiment was started).
*	exsecs = number of seconds since start of the experiment.

All timestamps represent the time of the last frame taken in the epoch. For example, if epoch is set to 10 minutes, zhrs=9.5 would mean that this datapoint is the sum of the frames from zhrs=9.33 (9.5 hrs – 0.17 hours) up to zhrs 9.5.

Note, some data at the start may have been trimmed prior to summing in epochs to obtain a total number of rows divisible by the epoch size.

Any trimming is done at the start of the timecourse as we do not usually analyse the first hours of tracking.


## Behavioural parameters

A behavioural parameter is a summary metric per larva per time window, e.g. how many hours larva #5 spent asleep during night2.

There are three categories of behavioural parameters:
* **Activity parameters**: any parameter that can be calculated on the Δpixel timecourse directly. The names of these parameters start with activity.

* **Active bout parameters**: parameters which are calculated on single swimming bouts (active bouts). For most of these parameters, the value per larva per time window represents the mean of all active bouts during that time window. The names of these parameters start with activebout.

* **Sleep parameters**: parameters which are calculated on sleep bouts. The names of these parameters start with sleep.

There are currently 17 behavioural parameters.

###### Activity parameters:
*	**activityPercentageTimeActive**: the percentage of time spent active per larva per time window. For example, larva #5 spent 10% during day2.

*	**activityTotalPx**: the total (sum) number of pixels moved per larva per time window. This parameter is given in millions (106) of pixels. For example, larva #7 moved 3 (× 106) pixels during day1.

*	**activitySunsetStartle**: each larva’s startle response at the day-to-night transition, defined as the maximum Δ px value found around the transition (as of v0.9.0, it looks from 1 min before the transition up to 1 min after the transition to be safe not to miss the real startle response in case the transition is not perfectly precise). Note, this parameter is only defined for nights. For example, the highest swimming bout of larva #9 during the first three seconds of night2 reached 50 pixels.  
Are you analysing your data by woi? activitySunsetStartle will return the maximum Δ px value from a bit before up to a bit after the first frame of the woi. You may need to think whether this has any meaning in the context of your experiment.  

*	**activitySlope**: the slope in the activity for one larva and one time window. For example, the activity of larva #7 decreased with a slope of −1,000 during day1.
In practice, the slope is calculated by linear regression of Y = frame-by-frame data summed in 10-minute bins and X = hours since the start of this window. This is because most frames are 0 Δpixel so calculating the linear regression on the ‘raw’ frame-by-frame data would not be meaningful. Consequently, you can read the example above as “each additional hour of day1 decreased the activity of larva #7 by 1,000 Δpixel/10 minute in average”.

*	**activityTransitionDelta**: for one larva, the difference in activity (frame-by-frame data summed in 10-minute bins) between the previous time window and this window. For example, the activity of larva #12 increased by 29,000 Δpixel from night1 to day2 (this would be the day2 datapoint). This is essentially the difference in heights between time windows on the summary activity plot. For days, this parameter is usually positive, as activity jumped from low during the previous night to high during that day. For nights, this parameter is usually negative, as activity dropped from high during the previous day to low during that night.
In practice, we first calculate, for one time window,  the linear regression of Y = frame-by-frame data summed in 10-minute bins and X = hours since the start of this window. Prior to linear regression, the first 10 minutes of Δpixel is trimmed to skip any strong effect of the light transition that would bias the linear regression, particularly to avoid the startle response at sunset. This also applies to parameter activitySlope. We obtain the ‘start intercept’, i.e. the actual intercept of the linear regression, which represents the activity at the start of the window. From the full formula, we can also calculate the ‘end intercept’, which represents the activity at the end of the window. Finally, we obtain the ‘transition delta’ for this larva and this time window by doing the following subtraction: transition delta = start intercept of current window − end intercept of previous window.

*	**activityFractalDim**: the fractal dimension of the activity trace for one larva and one time window. Fractal dimension essentially measures the ‘ruggedness’ of the activity trace on a scale from 1 to 2. A smooth trace has a low fractal dimension (e.g. 1.1); a rugged/chaotic trace has a high fractal dimension (e.g. 1.9).
In practice, we first trim the initial 10 minutes of Δpixel to skip any strong effect of the light transition, particularly to avoid the startle response at sunset. The data is then summed in one-second bins to reduce compute time. The binned data is then smoothed with a one-minute rolling average<sup>1</sup>. We then calculate the fractal dimension using the ‘boxcount’ method<sup>2</sup>.

> <sup>1</sup> This is to reduce slightly the ‘ruggedness’ of the trace. Without smoothing, the majority of fractal dimensions during the day are > 1.8, which may not leave much space for it to vary between larvae/genotypes as the theoretical maximum is 2.0.  

> <sup>2</sup> See https://www.mssanz.org.au/MODSIM97/Vol%201/Breslin.pdf for an accessible introduction. I tested the ‘variation’ method which is recommended in the article but the fractal dimension estimates made no sense. For example, they were not decreasing when I was making the activity trace smoother. The ‘boxcount’ method behaved intuitively, with a clear relationship between smoothing increasing and fractal dimension decreasing.

###### Active bout parameters:
*	**activeboutLength**: the mean of all active bout durations for one larva and one time window, in seconds. For example, the average active bout of larva #4 during day1 lasted 0.15 seconds.

*	**activeboutMean**: the mean of all active bout means for one larva and one time window. For example, the mean delta pixel during the average active bout of larva #2 during night2 was 10.8 pixels.

*	**activeboutSum**: the mean of all active bout sums for one larva and one time window. For example, the average active bout of larva #17 during night1 moved a total of 61.5 pixels.

*	**activeboutStd**: the mean of all active bout standard deviations for one larva and one time window. For example, the standard deviation of delta pixels during the average active bout of larva #11 during day1 was 11.1 pixels.

*	**activeboutMin**: the mean of all active bout minimums for one larvae and one time window. For example, the minimum delta pixel during the average active bout of larva #7 during day2 was 4.3 pixels.

*	**activeboutMax**: the maximum of all active bout minimums for one larvae and one time window. For example, the maximum delta pixel during the average active bout of larva #10 during day2 was 25.3 pixels.

*	**activeboutNum**: the total number of active bouts one larva performed during one time window. For example, larva #22 performed 34,736 swimming bouts (active bouts) during day1.

###### Sleep parameters:
*	**sleepHours**: the total number of hours one larva slept during one time window. It is 0 hours if the larva had no sleep bout. For example, larva #2 slept a total of 3.54 hours during night2.

*	**sleepNumNaps**: the total number of sleep bouts one larva had during one time window. Or in other words, the number of times that larva fell asleep during that time window. It is 0 if the larva had no sleep bout. For example, larva #21 had 134 sleep bouts during night1.

*	**sleepNapDuration**: the mean of all sleep bout durations for one larva during one time window. It is NA if the larva had no sleep bout, as we could not measure any nap duration. It can be NA for empty wells too. For example, the average sleep bout of larva #4 during night1 lasted 2.2 minutes.

*	**sleepLatency**: the duration before the first sleep bout of that window for one larva, in minutes. Or in other words, how long it took for that larva to fall asleep once this night or day started. It is NA (which will not appear in the ggParameter() scatter plot) if the larva had no sleep bout. For example, it took 14.2 minutes for larva #32 to have its first sleep bout when night1 started.

Any idea for a new behavioural parameter? Let me know.


### multiBehaviourParameter(...)

Calculates multiple behaviour parameters.

**parameters**: names of the parameters to be calculated. Options are: 'all' for every available parameter, 'activity' for all activity parameters, 'activebout' for all activebout parameters, ‘sleep’ for all sleep parameters, or a list of parameters e.g. `parameters=c('activityTotalPx', 'activeboutNum', 'sleepHours')`. See above for parameter names. You can also run `allparameters` in Console to get all the parameter names.

**ffpath**: full path(s) to _RAWs.csv.

**genopath**: full path(s) to genotype.txt. If giving multiple ffpaths, make sure to match the order, i.e. it will assume that the first genopath is the genotype file for the first ffpath, etc.

**zebpath**: full path(s) to Zebralab .xls results file. In the case of two Zebraboxes run in parallel, give the path to the common Zebralab .xls results file here. It is possible to calculate parameters for experiments not run in parallel, in which case you should give one zebpath for each ffpath, with orders matching.

**woi**: window(s) of interest. This is if you want to analyse specific window(s) of data, _instead_ of analysing days/nights. Give the time windows as: `c('start1', 'stop1', 'start2', 'stop2', ...)`, each *start* or *stop* timestamp is given in the format YYYY-MM-DD HH:MM:SS. Here _woi1_ will be from _start1_ until _stop1_; _woi2_ will be from _start2_ until _stop2_, etc. For example, `woi=c('2022-01-25 20:00:00', '2022-01-25 23:00:00', '2022-01-26 20:00:00', '2022-01-26 23:00:00')` will analyse two windows of interest: _woi1_ from 8 PM to 11 PM on 25/01/2022 (3 hours of data) and _woi2_ is from 8 PM to 11 PM on 26/01/2022 (3 hours of data). Not mentioning this setting or giving NA will analyse days/nights present in the data. Default is NA.

**zthr_min**: stands for zzz threshold in minutes, i.e. minimum number of minutes of inactivity to call a period of inactivity a sleep bout. This setting is only used for sleep parameters, it will simply be ignored when calculating an _activity_ or _activebout_ parameters. Default is 1.

**inaThr**: the maximum Δpixel to call a frame inactive during sleep bout detection. For example, inaThr=3 means “count any frame below or equal 3 Δpixel as inactive”. The setting is analogous to the Freezing threshold in Zebralab. I do not recommend changing this parameter without a good reason to do so. In my opinion, 1 minute of inactivity should genuinely mean 1 minute of inactivity, i.e. a continuous 1,500 frames which are all 0 Δ pixel. Having said that, if you used a low Sensitivity setting in Zebralab (< 20 on the newer Zebrabox), you may get some false positive Δ pixel detection. A good test is to scroll through the frame-by-frame data (in the _RAWs.csv) of empty wells. If there are some small scattered numbers (e.g. 1–3), it may be justified to set inaThr above 0, for example inaThr=3. Default is 0.

**dayduration**: how many hours do the days last in your experiment. Default is 14.

The function records in Environment<sup>1</sup> and saves in a folder called bhvparams<sup>2</sup> a “parameter table”. It is a fairly self-explanatory table storing the parameter calculation per larva per time window.

These tables (precisely the directory _bhvparams_) will be used as input for a series of commands.

> <sup>1</sup> Objects in Environment are listed in the top right corner of RStudio. You may need to stretch the first column to see the full name of each object.

> <sup>2</sup> Folder bhvparams is created (if it did not exist already) in the same folder as the Zebralab .xls results file. Find inside the parameter tables as _parameter_YYMMDD_BX_, e.g. _activityPercentageTimeActive_210913_12.csv_.

For two parallel experiments each of the format night0/day1/night1/day2/night2, calculating all parameters on my MacBook Pro 2017 takes ~ 1 hr. Most of it is spent on calculating the activebout parameters, e.g. activeboutSum alone takes ~ 7 minutes. The activity and sleep parameters are fast to calculate.

Calculations of activebout parameters often throw warnings, you can ignore them.

###### How are sleep parameters affected by the inaThr setting?
Again, I do not recommend using an inaThr other than 0, but if you are curious: I have tested on one of my experiments (for one night and N = 3 larvae) how gradually increasing inaThr from 0 to 10 affects each sleep parameter. Slope represents the linear regression of Y = parameter (e.g. total number of hours spent asleep during night2 for sleepHours) vs X = inaThr from 0 to 10.
* **sleepHours**: slope ~ 0.14, so increasing inaThr by 1 Δpixel overestimates total sleep time by an additional ~ 8.5 minutes.
* **sleepNumNaps**: negligible effect for 2 of the 3 larvae tested.
* **sleepLatency**: negligible effect.
* **sleepNapDuration**: slope ~ 0.07, so increasing inaThr by 1 Δpixel overestimates sleep bout duration by an additional ~ 4 seconds.

Note this conclusion is very likely to be dependent on the Sensitivity threshold used in Zebralab. Lowering Sensitivity makes the camera more sensitive. It may, for example, record positive Δpixels from empty wells. My experiment was recorded at Sensitivity = 20.


###### About calculating behaviour parameters within window(s) of interest
Parameters were originally defined and coded with complete days/nights in mind, not window(s) of interest of arbitrary lengths. Therefore, some parameters may not be directly suited to being calculated within window(s) of interest. Namely:

*	Calculating parameter **activityTransitionDelta** is not currently supported when analysing by window(s) of interest. The rationale for not calculating it is that windows of interest do not typically follow each other, in which case it does not make sense to calculate the ‘transition’. However, it could make sense to calculate it when the windows of interest directly follow each other, but it requires more work so let me know if it is ever needed.
*	**activitySunsetStartle**: it will return the maximum Δ px during the 10 seconds at the beginning of each window of interest. You can decide whether this is any interesting in the context of your experiment.
* **activitySlope**: the first 10 minutes of data is skipped prior to linear regression, then the data is summed in 10-minute bins. Accordingly, if the window of interest is only a few minutes long it will throw an error. If the window of interest is only a few tens of minutes long, it may return a result that is not particularly meaningful as the regression is calculated on only a few datapoints.


## Plots of behavioural parameters

### ggParameter(...)

Plots a scatter plot for one behavioural parameter, from one or multiple parameter tables. Whatever the settings used, one dot represents one larva during one time window.

**pa**: parameter table(s), given as Environment object(s) or full path(s) to .csv. If giving as Environment object(s), give the names as strings, e.g. `pa=c('pa1', 'pa2')`.  

**grporder**: do you have a preferred order for the groups (genotypes)? If yes, mention it here. If no, you can simply not mention this setting or give grporder=NA. You can exclude any group (genotype) by simply not mentioning it here. Default is NA, which plots all groups present in the parameter table(s) in alphabetical order.

**skipNight0**: whether or not (TRUE or FALSE) to plot night0’s datapoints. Mostly applies to the standard Rihel lab experiment night0/day1/night1/day2/night2. Default is FALSE.

**poolExp1**: a set of experiments to pool together. Give them as YYMMDD_BX, e.g. poolExp1=c('220706_16', '220706_17') will pool datapoints into one experiment called pool_1. Note, there is no normalisation of any kind happening. It will effectively behave as if the datapoints from the different experiments were from a single experiment, including when calculating the statistics. Use at your own risk. Default is NA.

**poolExp2**: another set of experiments to pool together in experiment pool_2. See comment above. Default is NA.

**poolExp3**: another set of experiments to pool together in experiment pool_3. See comment above. Default is NA.

**poolDayNight**: whether or not (TRUE or FALSE) to pool all the day datapoints together and all the night datapoints together, i.e. to lose the ‘time window’ resolution. For example, TRUE would pool all day1 and day2 datapoints on the X axis. When multiple experiments are plotted, TRUE keeps experiments separated. Default is FALSE.
onlyExp: do you want to plot only specific experiment(s) of the parameter tables you gave? Give them here as YYMMDD_BX. If you gave a single parameter table you can skip this setting or give NA. Default is NA, which plots every experiment it was given.

**onlyDayorNight**: do you want to plot only day datapoints or only night datapoints? Options are ‘day’ or ‘night’ or NA. This will keep time windows separated, so for the standard Rihel lab experiment ‘day’ here is equivalent to onlyWin=c('day1', 'day2') below. Most parameters vary a lot between day and night, so you may want one ‘day’ plot and one ‘night’ plot with different Y axes. Default is NA, which plots datapoints from days and nights.

**onlyWin**: do you want to plot only some time windows? Give them here, for example onlyWin='day1' or onlyWin=c('day1', 'night1'). Default is NA, which plots every time window available in the parameter table(s).

**colours**: colours for the groups (genotypes), either in the same order as grporder or in alphabetical order of the group names if not giving grporder. It understands a bunch of colour words like ‘red’ or ‘blue’ or you can give them as HEX codes (can use Eyedropper tool in Illustrator to get HEX colour code, for example). Default is NA, which will colour groups with default R colours, which actually look good.

**fainterExp**: whether or not TRUE or FALSE to colour replicate experiments with slightly fainter group colours to help distinguish them on the plot. Default is FALSE.  

**faintMax**: maximum faint ratio, from 0 to 1 (1.0 gives white). Only applies if `fainterExp=TRUE`. Essentially gives "how faint can we go" when colouring replicate experiments. For example, if there are 3 experiments and `faintMax=0.6`, dots from first experiment will be original colour; dots from second experiment will be 30% (0.3) fainter; dots from third experiment will be 60% (0.6) fainter. Default is 0.5.  

**ymin**: lowest point of the Y axis. Default is NA, which will do it automatically.

**ymax**: highest point of the Y axis. Default is NA, which will do it automatically.

**dotSize**: size of the dots. Default is 0.5.  

**violinWidth**: dots are arranged like they were inside a violin plot. What do you want the width of the violin to be? Default is 0.09, which makes a very narrow violin.  

**connectMean**: whether or not (TRUE or FALSE) to connect the mean crosses with a line. This can help guiding the reader towards the comparisons they should be making, and also illustrate the LME slope (a steeper line represents a larger effect size). Default is TRUE.  

**legendOrNo**: whether or not (TRUE or FALSE) to make the legend. I would recommend first generating the plot with the legend so you can check that the groups and colours are correctly matched. Default is FALSE.

**ynameOrNo**: whether or not (TRUE or FALSE) to write the name of the Y axis. It automatically writes a name depending on the parameter, e.g. “active bout mean (Δ px)”. Default is TRUE.

**yunitOrNo**: whether or not (TRUE or FALSE) to only write the unit of measure on the Y axis, e.g. Δ px for active bout length. It is essentially a simpler version of ynameOrNo. I would recommend yunitOrNo=TRUE if you have the parameter names as titles already (below), otherwise the Y axis name is redundant. Default is FALSE.  

**splitBy**: how to split the panel, can be `"window"`, which splits the panel by each day/night of each experiment, or `"experiment"`, which splits the panel by experiment Splitting the panel only means that the horizontal lines of the grid have breaks, it does not affect the order of the dots. Default is `'window'`.  

**titleOrNo**: whether or not (TRUE or FALSE) to write the parameter name as title of the plot.

**blankTitle**: you can probably skip, this is for internal use. TRUE (with titleOrNo=TRUE) writes a blank title. It helps to align the day and night plots properly in ggParameterGrid(). Default is FALSE.

**nightBgOrNo**: whether or not (TRUE or FALSE) to add a grey background when night datapoints are plotted. Currently, it only works if `onlyDayorNight='night'`. Default is TRUE.

**statsOrNo**: whether or not (TRUE or FALSE) to add ns/*/**/*** on top of the plot to indicate statistical significance between groups. This is calculated by LMEdaynight(), read there for details.

**silent**: you can skip, it is used internally to block it from printing the statistics results to Console. TRUE makes it silent, FALSE lets it print the summary. Default is FALSE.
xtextOrNo. whether or not (TRUE or FALSE) to write the text on the X axis. Default is TRUE.

**width**: width of pdf in mm.

**height**: height of pdf in mm.

**exportPath**: full path to export file. It will create a pdf. Accordingly, exportPath must finish with .pdf. You can skip giving this parameter entirely if you do not wish to export the plot.

I usually run two Zebraboxes in parallel with one clutch in each to serve both as technical (different Zebraboxes) and biological (different clutches) replicates. Accordingly, I like to plot the two clutches side to side to see whether the results are consistent. However, most parameters vary a lot between day and night so it is not always ideal to have the same Y axis for day and night. To have different Y axes, I usually give the two parameter tables (one for each Zebrabox/clutch) but generate a ‘day’ plot (onlyDayorNight='day') and a ‘night’ plot (onlyDayorNight='night') separately, i.e. two separate ggParameter() calls.


### ggParameterGrid(...)

Plots a grid of scatter plots, one per behavioural parameter. Whatever the settings used, one dot represents one larva during one time window.

ggParameterGrid() does not currently support parameters calculated on specific window(s) of interest. Let me know if needed.

**paDir**: directory that stores the parameter tables, typically called bhvparams. For example, `paDir=here('bhvparams/')`. You can have as many experiments/parameters as you want in this directory. You can also give multiple bhvparams directories, e.g. `paDir=c(here('220906_exp1/bhvparams/'), here('220906_exp2/bhvparams/'))`. It will import all the parameter tables it finds in these directories.

**statsReport**: whether or not (TRUE or FALSE) to write the report (.csv file) listing all the linear mixed effect models. If TRUE, it will be written as LMEreport.csv in the same folder as exportPath (below).

**grporder**: do you have a preferred order for the groups (genotypes)? If yes, mention it here. If no, you can simply not mention this setting or give grporder=NA. You can exclude any group (genotype) by simply not mentioning it here. Please read how this setting also affects the LME calculations under LMEdaynight(...) below. Default is NA, which plots all groups present in the parameter table(s) in alphabetical order.

**skipNight0**: whether or not (TRUE or FALSE) to plot night0’s datapoints. Mostly applies to the standard Rihel lab experiment night0/day1/night1/day2/night2. Please read how this setting also affects the LME calculations under LMEdaynight(...) below.. Default is FALSE.

**poolExp1**: a set of experiments to pool together. Give them as YYMMDD_BX, e.g. `poolExp1=c('220706_16', '220706_17')` will pool datapoints into one experiment called pool_1. Note, there is no normalisation of any kind happening. It will effectively behave as if the datapoints from the different experiments were from a single experiment, including when calculating the statistics. Use at your own risk. Default is NA.

**poolExp2**: another set of experiments to pool together in experiment pool_2. See comment above. Default is NA.

**poolExp3**: another set of experiments to pool together in experiment pool_3. See comment above. Default is NA.

**poolDayNight**: whether or not (TRUE or FALSE) to pool all the day datapoints together and all the night datapoints together, i.e. to lose the ‘time window’ resolution. For example, TRUE would pool all day1 and day2 datapoints on the X axis. When multiple experiments are plotted, TRUE keeps experiments separated. This setting does not affect the LME statistics. Default is FALSE.

**onlyExp**: do you want to plot only specific experiment(s) of the parameter tables you gave? Give them here as YYMMDD_BX. Did you pool experiments (i.e. used poolExp1 or poolExp2 or poolExp3 above)? Then you will need to give the experiment(s) you want to plot as pool_1 or pool_2 or pool_3, e.g. onlyExp=c('pool_1', 'pool_3'). If you gave a single parameter table you can skip this setting or give NA. Default is NA, which plots every experiment it was given.

**onlyDayorNight**: not currently supported. Let me know if needed.

**onlyWin**: do you want to plot only some time windows? Give them here, for example onlyWin='day1' or onlyWin=c('day1', 'night1'). Default is NA, which plots every time window available in the parameter table(s).

**colours**: colours for the groups (genotypes), either in the same order as grporder or in alphabetical order of the group names if not giving grporder. It understands a bunch of colour words like ‘red’ or ‘blue’ or you can give them as HEX codes (can use Eyedropper tool in Illustrator to get HEX colour code, for example). Default is NA, which will colour groups with default R colours, which actually look good.

**legendOrNo**: whether or not (TRUE or FALSE) to make the legend. I would recommend first generating the plot with the legend so you can check that the groups and colours are correctly matched. Default is TRUE.

**dotSize**: size of the dots in each plot. Default is 0.5.  

**connectMean**: whether or not (TRUE or FALSE) to connect the mean crosses with a line. This can help guiding the reader towards the comparisons they should be making, and also illustrate the LME slope (a steeper line represents a larger effect size). Default is FALSE.  

**xtextOrNo**: whether or not (TRUE or FALSE) to write the X axis tick labels.

**ynameOrNo**: whether or not (TRUE or FALSE) to write the name of the Y axis. It automatically writes a name depending on the parameter, e.g. “active bout mean (Δ px)”. Default is TRUE.

**yunitOrNo**: whether or not (TRUE or FALSE) to only write the unit of measure on the Y axis, e.g. Δ px for active bout length. It is essentially a simpler version of ynameOrNo. I would recommend yunitOrNo=TRUE if you have the parameter names as titles already (below), otherwise the Y axis name is redundant. Default is FALSE.

**titleOrNo**: whether or not (TRUE or FALSE) to write the parameter name as title of the plot. Default is TRUE.

**nightBgOrNo**: whether or not (TRUE or FALSE) to add a grey background to the night plot. Default is TRUE.

**keysOrNo**: whether or not (TRUE or FALSE) to add A, B, C, ... keys to the panels. Default is FALSE.

**statsOrNo**: whether or not (TRUE or FALSE) to add ns / * / ** / *** on top of the plot to indicate statistical significance between groups. This is calculated by LMEdaynight(), read there for details.

**ncol**: how many columns should the grid have?

**nrow**: how many rows should the grid have?

**width**: width of pdf in mm.

**height**: height of pdf in mm.

**exportPath**: full path to export file. It will create a pdf. Accordingly, exportPath must finish with .pdf. You can skip giving this parameter entirely if you do not wish to export the plot.


### ggSleepLatencySurvival(...)

Plots the sleep latency parameter for one time window in the style of a survival curve. At each timepoint (X), the proportion of larvae which did not yet have any sleep bout (Y). Or in other words, every time a larva falls asleep (‘dies’), it is removed from the curve. X axis starts with the start of the time window, e.g. sleep latency plot for night1 starts at 11 PM night1.

It also calculates a Cox Proportional-Hazards model for each experiment and time window and reports the results in Console. The formula looks like:

`coxph( Surv(times_of_first_nap, status) ~ group)`

The analysis was inspired by these tutorials:
*	how to prepare the data: http://www.sthda.com/english/wiki/survival-analysis-basics
*	how to calculate a Cox Proportional-Hazards model: http://www.sthda.com/english/wiki/cox-proportional-hazards-model
*	more details on how to interpret the Hazard Ratio: https://s4be.cochrane.org/blog/2016/04/05/tutorial-hazard-ratios/

Please be critical and check that the statistics seem to match what you can see with your eyes on the plots, especially regarding the direction of the effect (i.e. which group is falling asleep more quickly/more slowly than the other). Let me know if you think that something is wrong.

sleepLatency calculated on window(s) of interest is not currently supported. Let me know if needed.

**pa**: sleep latency parameter table(s), given as Environment object(s) or full path(s) to .csv.

**grporder**: do you have a preferred order for the groups (genotypes)? If yes, mention it here. If no, you can simply not mention this setting or give `grporder=NA`. You can exclude any group (genotype) by simply not mentioning it here. The order here will change which group is taken as reference in the Cox Proportional-Hazards Model, e.g. `grporder=c('wt', 'ko')` will give the results for KO in relation to WT, which is probably what you want. Now, the survival curves typically overlap and the order here will decide which group is plotted on top of the other(s). Accordingly, if you would like a different order for the plot than for the statistics, you can always generate the plot first then re-run the command to get the statistics to care about. Just remember that colours (below) are matched in the same order so I would strongly recommend having `legendOrNo=TRUE` in the process so you make sure you are interpreting the plots correctly. Default is NA, which will use alphabetical order.

**skipNight0**: whether or not (TRUE or FALSE) to plot night0’s sleep latency survival plot. Mostly applies to the standard Rihel lab experiment night0/day1/night1/day2/night2. Default is FALSE.
onlyDayorNight: do you want to plot the sleep latency survival plots of only the days or only the nights? Options are ‘day’ or ‘night’ or NA.

**onlyWin**: do you want to plot the sleep latency survival plots of only specific day(s) or night(s)? Give them here, for example onlyWin='day1' or onlyWin=c('day1', 'night1'). Default is NA, which makes the sleep latency survival plot for every time window available in the parameter table(s).

**colours**: colours for the groups (genotypes), either in the same order as grporder or in alphabetical order of the group names if not giving grporder. It understands a bunch of colour words like ‘red’ or ‘blue’ or you can give them as HEX codes (can use Eyedropper tool in Illustrator to get HEX colour code, for example). Default is NA, which will colour groups with default R colours, which actually look good.
legendOrNo: whether or not (TRUE or FALSE) to make the legend. I would recommend first generating the plot with the legend so you can check that the groups and colours are correctly matched. Default is TRUE.

**xtextOrNo**: whether or not (TRUE or FALSE) to write the units on the X axis. Default is TRUE.

**xnameOrNo**: whether or not (TRUE or FALSE) to write the name of the X axis. TRUE will write “hours since light transition”. Default is TRUE.

**ynameOrNo**: whether or not (TRUE or FALSE) to write the name of the Y axis. TRUE will write “% larvae which have not slept yet”. Default is TRUE.

**nightBgOrNo**: whether or not (TRUE or FALSE) to add a grey background when we are making the sleep latency survival plot for a night. Default is TRUE.

**xmaxh**: end of X axis, in hours after the light transition. Default is the total duration (in hours) of that time window, typically 14 hours if day, 10 hours if night.

**detailsOrNo**: whether or not (TRUE or FALSE) to print in Console the details of each Cox Proportional-Hazards model.

**exportOrNo**: you can skip, it is used internally to avoid exporting every single plot when making the grid in ggSleepLatencyGrid(). Whether or not (TRUE or FALSE) to export the plots. Default is TRUE.

**exportDir**: directory where we will export the sleep latency survival plots. It will make one plot for each experiment and each time window. The name of each plot will be YYMMDD_BX_sleepLatency_window_.pdf, e.g. 210913_12_sleepLatency_night1.pdf.

**width**: width of each pdf in mm.

**height**: height of each pdf in mm.

**dayduration**: how many hours do the days last in your experiment. Default is 14.

### ggSleepLatencyGrid(...)

Makes two grids: one with the day sleep latency survival plots and one with the night sleep latency survival plots. Within each grid, each column is one window and each row is one experiment. For example, the day grid could be structured as:

|      | Day1      | Day2     |
|:-----|:----------|:---------|
| **Exp1** | _plot_  | _plot_ |
| **Exp2** | _plot_  | _plot_ |

It simply calls ggSleepLatencySurvival() multiple times to build each grid. Read there for details. Most of the parameters below are the same.

sleepLatency calculated on window(s) of interest is not currently supported. Let me know if needed.

**pa**: sleep latency parameter table(s), given as Environment object(s) or full path(s) to .csv.
grporder: do you have a preferred order for the groups (genotypes)? If yes, mention it here. If no, you can simply not mention this setting or give grporder=NA. You can exclude any group (genotype) by simply not mentioning it here. The order here will change which group is taken as reference in the Cox Proportional-Hazards Model, e.g. grporder=c('wt', 'ko') will give the results for KO in relation to WT, which is probably what you want. Now, the survival curves typically overlap and the order here will decide which group is plotted on top of the other(s). Accordingly, if you would like a different order for the plot than for the statistics, you can always generate the plot first then re-run the command to get the statistics to care about. Just remember that colours (below) are matched in the same order so I would strongly recommend having legendOrNo=TRUE in the process so you make sure you are interpreting the plots correctly. Default is NA, which will use alphabetical order.

**skipNight0**: whether or not (TRUE or FALSE) to plot night0’s sleep latency survival plot. Mostly applies to the standard Rihel lab experiment night0/day1/night1/day2/night2. Default is FALSE.

**colours**: colours for the groups (genotypes), either in the same order as grporder or in alphabetical order of the group names if not giving grporder. It understands a bunch of colour words like ‘red’ or ‘blue’ or you can give them as HEX codes (can use Eyedropper tool in Illustrator to get HEX colour code, for example). Default is NA, which will colour groups with default R colours, which actually look good.

**legendOrNo**: whether or not (TRUE or FALSE) to make the legend. I would recommend first generating the plot with the legend so you can check that the groups and colours are correctly matched. Default is TRUE.

**xtextOrNo**: whether or not (TRUE or FALSE) to write the units on the X axis. Default is TRUE.

**xnameOrNo**: whether or not (TRUE or FALSE) to write the name of the X axis. TRUE will write “hours since light transition”. Default is TRUE.

**ynameOrNo**: whether or not (TRUE or FALSE) to write the name of the Y axis. TRUE will write “% larvae which have not slept yet”. Default is TRUE.

**nightBgOrNo**: whether or not (TRUE or FALSE) to add a grey background when we are making the sleep latency survival plot for a night. Default is TRUE.

**xmaxDay**: end of X axis to be used for the day plots, in hours after the light transition. Default is the total duration (in hours) of the day, typically 14 hours.

**xmaxNight**: end of X axis to be used for the night plots, in hours after the light transition. Default is the total duration (in hours) of the night, typically 10 hours.

**detailsOrNo**: whether or not (TRUE or FALSE) to print in Console the details of each Cox Proportional-Hazards model.

**exportOrNo**: you can skip, it is used internally to avoid exporting every single plot when making the grid in ggSleepLatencyGrid(). Whether or not (TRUE or FALSE) to export the plots. Default is TRUE.

**exportDir**: directory where we will export the grids. It will generally make two grids: sleepLatency_day.pdf and sleepLatency_night.pdf.

**width**: width of each pdf in mm. Default is 150.

**height**: height of each pdf in mm. Default is 100.

**dayduration**: how many hours do the days last in your experiment. Default is 14.

### LMEdaynight(...)
Runs linear mixed effects (LME) models as statistical tests on a behavioural parameter. Day data and night data are analysed separately. It prints in Console a summary in the form:

> reference group vs treatment group = slope ± error ; pval = … ns / * / ** / ***

Slope (i.e. the effect, read below) is given in relation to the first group, here wt. It is in the units of the behavioural parameter you are analysing.

For example, say we are testing if KO and WT have a different startle response as sunset. We get:
> summary NIGHT  
\>>> wt vs ko = 4.576645 ± 2.596402 ; pval = 0.07952819 ns

This means that, in average, ‘being WT’ increases your startle response compared to KO by 4.58 ± 2.66 Δ pixel (p-value = 0.079, i.e. non-significant).

Remember here that behavioural parameter means one value per larva per time window, e.g. mean sleep bout duration of larva #12 during night2. My broad understanding of linear mixed effect modelling is: fixed effects are the variables we want to study the impact of. Here, the fixed effect is the group assignment (genotype), as we want to know how the group affects the parameter. The random effects are the variables that may impact the outcome but that we are not interested in, so we want to control for them. Here, random effects are:
*	the experiment, i.e. the Zebrabox and the clutch. I am assuming here that you tracked a unique clutch in each Zebrabox. There is usually substantial variability between clutches, so we want to control for this. There may also be some technical variability between the Zebraboxes.
* the larva’s age. Typically, larvae are 5 dpf on night0, 6 dpf on day1 & night1, 7 dpf on day2 & night2. In practice, it just counts 0/+1/+2/… so it will still be correct even if your experiment was different (e.g. more or fewer days, or started at a different age). This is to control for development. For example, sleep is often slightly lower during night2 (at 7 dpf) so we want to control for this.
*	the larva’s ID (e.g. larva #12). This is to account for non-independence in the data. Indeed, each larva is typically sampled multiple times during the experiment. For example, larva #12 is sampled once on day1 and once on day2. Or in other words, day1 and day2 are not independent experiments as they include the same animals<sup>1</sup>.

> <sup>1</sup> Particularly useful here was: https://ourcodingclub.github.io/tutorials/mixed-models/, especially the bit below _Nested random effects_ about pseudoreplication.

Experiment (clutch/Zebrabox) and larva’s number are modelled as nested random effects (larva ID nested inside experiment). This somehow tells the model that we expect data points within each experiment (within the same clutch & within the same Zebrabox) to be more similar between each other than between larvae from different experiments. Then, within each experiment, we expect each larva to have more similar day1/day2 (or night1/night2) data points than if we were comparing between different larvae. The age of the larva is modelled as a crossed effect here.

The command to create the day or night model looks like<sup>2</sup>:

`lmer(parameter ~ group + (1|experiment/larva ID) + (1|larva age))`

The model provides the slope, i.e. the magnitude of the effect (e.g. in average, KO slept 30 min less than WT) and its standard error.

> <sup>2</sup> This is pretty much copying the example from https://ourcodingclub.github.io/tutorials/mixed-models/ below _Nested random effects_: leafLength ~ treatment + (1|Bed/Plant/Leaf) + (1|Season). Here, leafLength is the behaviour parameter, treatment is the group, Bed is the experiment, Plant is the larva’s ID, (skip Leaf), Season is the larva’s age.

We then create a ‘null’ model by omitting to tell the model about the fixed effect, i.e. the group assignment, as:

`lmer(parameter ~ 1 + (1|experiment/larva ID) + (1|larva age))`

We then compare the null model with the full model with a likelihood-ratio test, which provides the p-value<sup>3</sup>.

> <sup>3</sup> How to obtain a p-value is taken from the tutorial by Winter Bodo: https://doi.org/10.48550/arXiv.1308.5499.

This analysis was informed by these excellent tutorials:
* https://doi.org/10.48550/arXiv.1308.5499
* https://ourcodingclub.github.io/tutorials/mixed-models/

I would recommend reading there if you want to learn more or check my logic. Do let me know if you think I’m doing something wrong!

Using the LME model, we compare each treatment group vs the reference group using ‘estimated marginal means’ (package `emmeans` in R), essentially performing ‘post-hoc’ tests.

Here, you may think “*This is wrong! We can only do post-hoc tests if the overall ANOVA (here: linear mixed effects model) is significant*”. Well, I was raised believing this too, but these arguments seem reasonable to me: https://stats.stackexchange.com/questions/9751/do-we-need-a-global-test-before-post-hoc-tests.

So I decided to run the "post-hoc" tests regardless of the "ANOVA" results. You can use your own judgement when deciding what to do with them.

Note, these post-hoc p-values are not adjusted for multiple comparisons. Adjustment for multiple comparisons is usually context-dependent so it would be difficult to predict when and how to adjust. For example, you may only care about the WT vs HOM comparison and completely ignore the WT vs HET comparison, so why should you lose statistical power adjusting your p-value for a comparison you will not look at? Again, you can use your own judgement and adjust these p-values if needed. As you may notice, the comparisons between treatment groups (e.g. HET vs HOM) are not reported, which helps to keep the number of tests down.

Overall, I think the only situation where you could justify using directly the post-hoc p-value (i.e. without looking first at the overall ANOVA p-value) without any adjustment is when there is only one comparison of interest in your experiment. For example, you tracked WT, HET, HOM but only care about HOM vs WT. However, if both the HET vs WT and HOM vs WT comparisons are interesting to you, I think you should only look at the post-hoc p-values if the overall ANOVA is significant and/or (probably _and_) adjust the post-hoc p-values for multiple comparisons.

**EXCEPTION 1**  
If there is only one day or night in the data, then there is only datapoint per larva for a given parameter. In this situation, the random effect `larva age` is meaningless (all measurements were done at the same age) and the random effect `larva ID` does not group the datapoints in any way (as there is only one datapoint per larva). These random effects get dropped from the LME formula. For example,   
`lmer(parameter ~ group + (1|experiment/larva ID) + (1|larva age))`  
becomes  
`lmer(parameter ~ group + (1|experiment)`  
(see below if you also have only one experiment).  

**EXCEPTION 2**  
If you have only one experiment, one day or night, and one clutch, there is no random effects to give to the model. In this case, `LMEdaynight()` switches automatically to a simple linear regression, whose formula looks like:

`lm(parameter ~ group)`

In this case, the overall p-value is the p-value of the F-statistic. Note, this is _exactly_ the p-value of an ANOVA so you can report it that way (you can check this for yourself or read e.g. https://faculty.nps.edu/rbassett/_book/regression-with-categorical-variables.html). The only advantage of the linear regression (instead of doing directly an ANOVA) is to obtain an estimate of effect size. 

_WARNING_: the LME report generated by `LMEreport()` will still say LMEslope and LMEerror, but if you are in this situation, this is not correct. Those are calculated by a _linear regression_, not a linear mixed effects model. Report this correctly!

In this situation, the post-hoc p-values is the p-value for each slope from the linear regression. Each p-value is _exactly_ the p-value from a t-test beingGroup vs referenceGroup so you can report it that way (you can check this for yourself or see e.g. https://faculty.nps.edu/rbassett/_book/regression-with-categorical-variables.html).

**EXCEPTION 3**  
If you tracked the same clutch in two separate boxes, you should mention this in the `sameClutch` settings (see below). In this case, the LME formula looks like<sup>6</sup>:

`lmer(parameter ~ group + (1|clutch/experiment/larva ID) + (1|larva age))`

This allows the LME model to control separately for clutch-to-clutch variation (i.e. variation between biological replicates) and box-to-box variation (i.e. variation between technical replicates).

> <sup>6</sup> This is now exactly copying the formula: leafLength ~ treatment + (1|Bed/Plant/Leaf) + (1|Season) (see comment above). Indeed, fish (Leaf) is a subset of experiment (Plant) which is a subset of clutch (Bed).

The rest of the analysis is the same.

**EXCEPTION 4**
Did you track multiple clutches in the same box? Please read in README (below Experimental design commandments) how to proceed.

In that case, the LME formula looks like:  
`lmer(parameter ~ group + (1|clutch/fish) + (1|larva age))`

Currently it does not support a situation where one experiment/box has multiple clutches and another replicate experiment has only one clutch. Let me know if it is needed.

**pa**: parameter table(s), either as path(s) or as Environment object(s).

**grporder**: do you have a preferred order for the groups (genotypes)? If yes, mention it here. If no, you can simply not mention this setting or give grporder=NA. You can exclude any group (genotype) by simply not mentioning it here. Default is NA.

Giving all groups present in the parameter table(s) but in a different order will only modify the direction of the effect, i.e. the sign of the slope. For example, grporder=c('ko', 'wt') will give the effect of ‘being KO’, compared to WT (reference). Alternatively, grporder=c('wt', 'ko') will just change the sign of the slope because the results will represent the effect of ‘being WT’ compared to KO (reference), the error and the p-value will remain the same. If you have any doubt about the interpretation, just take the scatter plot of a parameter which has a fairly obvious difference between groups and look at which group is higher or lower.

Excluding groups may modify the slopes and errors, even for the same group-vs-group comparison. For example, say you first run LMEdaynight(...) on a parameter table that has three groups: wt, het, hom. You will get estimates for wt vs het and for het vs hom. Now you decide to exclude the het group by giving `grporder=c('wt', 'hom')`. The wt vs hom comparison is likely different than the one you first obtained when giving all three groups, which may be counterintuitive. This is because you are ‘hiding’ datapoints from the LME model when it models how the different random effects affect the data. Say there were N=25 wt, N=50 het, and N=25 hom. In the first case (all three groups), the LME model estimates how the random effects (e.g. each larva’s age) affect the data on N=100 larvae, while in the second case (only wt and het) it does so on N=50 larvae. Except if you have a good reason to completely exclude a group, I think it is preferable to give the largest possible sample size to the LME model so it models the random effects the best it can. Then you can decide to only report the comparison you/the readers care about. So, in the example: keep all three groups (`grporder=NA` or `grporder=c('wt', 'het', 'hom')`), but report only the wt vs hom estimates.

Finally, I am not certain what to recommend regarding p-values when excluding groups. Remember each p-value here answers the question “does the genotype (group) affect parameter X”, more precisely it is the probability of being wrong when excluding the null hypothesis which is “the genotype (group) has no effect on parameter X”. When there are two groups, the p-value necessarily represents the comparison between the two groups. However, when there are more than two groups, it is more akin to the interpretation of an ANOVA, i.e. “do genotypes have an effect?”. So it will depend on what your claim is. In the example, you could also try with and without the het group and see if you reach the same conclusion(s) regarding significance.

**skipNight0**: whether or not (TRUE or FALSE) to remove the night0 datapoints before making the model. Default is FALSE. Mostly applies to standard Rihel lab experiment night0 / day1 / night1 / day2 / night2. In this case, note that keeping night0 datapoints would make the day results and the night results less readily comparable. Indeed, the night model would be calculated on three windows while the day model would be calculated on two windows. I am not certain what the precise effect of this would be but the statistical power would almost certainly be different.

**silent**: you can skip, it is used internally to make it not print anything to Console, i.e. TRUE makes it silent, FALSE lets it print the summaries.

**detailsOrNo**: whether or not (TRUE or FALSE) to print in Console the details of each LME model.


### LMEreport(...)

Calculates linear-mixed effects (LME) statistics and write results in a statistics report.

Please refer to the documentation of LMEdaynight(...) above for explanations about the LME model.

**paDir**: directory that stores the parameter tables, typically called bhvparams. For example, `paDir=here('bhvparams/')`. You can have as many experiments/parameters as you want in this directory. You can also give multiple bhvparams directories, e.g. `paDir=c(here('220906_exp1/bhvparams/'), here('220906_exp2/bhvparams/'))`. It will import all the parameter tables it finds in these directories.

**poolExp1**: a set of experiments to pool together. Give them as YYMMDD_BX, e.g. `poolExp1=c('220706_16', '220706_17')` will pool datapoints into one experiment called pool_1. Note, there is no normalisation of any kind happening. It will effectively behave as if the datapoints from the different experiments were from a single experiment, including when calculating the statistics. Use at your own risk. Default is NA.

**poolExp2**: another set of experiments to pool together in experiment pool_2. See comment above. Default is NA.  

**poolExp3**: another set of experiments to pool together in experiment pool_3. See comment above. Default is NA.  

**sameClutch1**: a set of experiments which tracked the same clutch. Please read above _If you tracked the same clutch in two separate boxes [...]_.  

**sameClutch2**: another set of experiments which tracked the same clutch.  

**sameClutch3**: another set of experiments which tracked the same clutch.  

**grporder**: please give the reference/control group first, then the other groups in your preferred order. For example, `grporder=c('wt', 'het', 'hom')` or `grporder=c('DMSO', 'dose1uM', 'dose100uM')`. Try _not_ to exclude groups here. Please read more details about the rationale under LMEdaynight(...).  

**skipNight0**: whether or not (TRUE or FALSE) to remove the night0 datapoints before making the model. Default is FALSE. Mostly applies to standard Rihel lab experiment night0 / day1 / night1 / day2 / night2. In this case, note that keeping night0 datapoints would make the day results and the night results less readily comparable. Indeed, the night model would be calculated on three windows while the day model would be calculated on two windows. I am not certain what the precise effect of this would be but the statistical power would almost certainly be different.  

**silent**: you can skip, it is used internally to make it not print anything to Console, i.e. TRUE makes it silent, FALSE lets it print the summaries. Default is FALSE.  

**detailsOrNo**: whether or not (TRUE or FALSE) to print in Console the details of each LME model. Default is FALSE.  

**exportPath**: full path to export file. It will create a csv. Accordingly, exportPath must finish with .csv.  


### calculateFingerprint(...)
Calculates a behavioural fingerprint for a larva or clutch.

Let’s take a simple example to explain what the code does. Say you have an experiment with N=40 controls and N=20 mutants and two days: day1 and day2; and two nights: night1 and night2. For each parameter, we want to calculate by how much mutants deviate from controls (we assume `singleFish=FALSE` and `avgDayNight=TRUE` below). Let’s start with e.g. parameter activeboutLength; the process is:

1. Calculate the mean and standard deviation of activeBoutLength of the N=40 controls during day1, e.g. 0.3 ± 0.1 sec.  
From each mutant’s larva activeBoutLength during day1, calculate Z-score from controls. Say mutant larva f5’s activeBoutLength during day1 is 0.5 sec. Z-score formula is:  
```math
Z_{day1} = {x-μ_{con} \over σ_{con}}
```
Where x is f5’s activeBoutLength, i.e. 0.5 sec; $μ_{con}$ is the mean of controls, i.e. 0.3 sec.; $σ_{con}$ is the standard deviation of controls, i.e. 0.1. So:
```math
Z_{day1} = {0.5-0.3 \over 0.1} = 2
```


2. Calculate the mean and standard deviation of activeBoutLength of the N=40 controls during day1, e.g. 0.3 ± 0.1 sec.


3. Now we repeat for day2, obtaining e.g. $Z_{day2} = 3$.  


4. To get a common Z-score for day activeboutLength of larvae f5, we simply calculate the average of both Z-score day1 and Z-score day2:
```math
Z_{day} = { 2 + 3 \over 2} = 2.5
```

5. We repeat the process for every mutant larva to obtain N=20 day activeboutLength Z-scores.

6. To summarise these 20 Z-scores, we calculate the mean and standard error of the mean (SEM) of the 20 day activeboutLength Z-scores. The mean of the Z-scores tells us by how much mutant larvae deviate from controls during the day for the parameter activeboutLength. Say it’s +2.3; it means: in average, mutant larvae have activeboutLength that are 2.3 standard deviations greater than controls.

The process is the same for nights and for each behavioural parameter.

**paDir**: directory that stores the parameter tables, typically called bhvparams. For example, `paDir=here('bhvparams/')`. You can have as many experiments/parameters as you want in this directory. You can also give multiple bhvparams directories, e.g. `paDir=c(here('220906_exp1/bhvparams/'), here('220906_exp2/bhvparams/'))`. It will import all the parameter tables it finds in these directories.

**controlGrp**: name of control group, e.g. ‘wt’. Does the name of the control group change between experiments? For example it is ‘control’ in one experiment but ‘wt’ in another. That is fine, you can give multiple control groups, for example controlGrp=c('control', 'wt').

**mergeExp1**: a set of experiments to merge, each written as YYMMDD_BX. For example, mergeExp1=c('220906_14', '220906_15'). Important note: this pools Z-scores normalised to controls, not raw datapoints. Steps 1–6 (see process above) are executed for each experiment, then the Z-scores for the experiments to merge are pooled to make them look as if they were generated during a single experiment. Step 7 is then executed on the pool of Z-scores. Experiments will be merged into one experiment called “mergeExp_1'' in the results. Default is NA.

**mergeExp2**: a second set of experiments to merge. See above. Experiments will be merged into one experiment called “mergeExp_2” in the results. Default is NA.

**mergeExp3**: a third set of experiments to merge. See above. Experiments will be merged into one experiment called “mergeExp_3” in the results. Default is NA.

**singleFish**: where or not (TRUE or FALSE) to calculate single larva fingerprints. This essentially makes the code stop before executing the last step 7 (see above) so you get for each larva and each parameter (e.g. activeBoutLength during the day) its Z-score normalised to controls.

**grporder**: do you have a preferred order for the groups (genotypes)? If yes, mention it here. If no, you can simply not mention this setting or give grporder=NA. You can exclude any group (genotype) by simply not mentioning it here. Default is NA, which keeps all groups.

**skipNight0**: whether or not (TRUE or FALSE) to remove the night0’s datapoints prior to calculating the fingerprint. Mostly applies to the standard Rihel lab experiment night0/day1/night1/day2/night2. Default is FALSE.

**avgDayNight**: whether or not (TRUE or FALSE) to average, for each parameter and each larva, its days datapoints together and its nights datapoints together prior to calculating the fingerprint. This will affect what each unique parameter is in the fingerprint: if TRUE, parameters are e.g. day_sleepHours and night_sleepHours; if FALSE, parameters are e.g. day1_sleepHours, day2_sleepHours, night1_sleepHours, night2_sleepHours. In other words, do you want to keep individual day/night resolution or not?

About **mergeExp1–3** settings: this can be useful if you tracked a single clutch into multiple boxes to boost sample sizes. As we are pooling Z-scores to controls within each box, this should in theory control for technical variability between boxes. A benefit of pooling is to avoid creating “fake replicates”. Indeed, keeping boxes separate can make it look like you had more replicates than in reality (the technical replicates should probably not count).

### ggFingerprint(...)

**fgp**: object or full path to _fingerprint.csv_, created by calculateFingerprint(...). You can overlay multiple fingerprints by giving multiple paths here, e.g. `fgp=c("'~/.../fingerprintExp1.csv", "fingerprintExp2.csv")`.  

**lmePath**: full path to a .csv LME report file created by `LMEreport()`. It will be used to add asterisks on top of the fingerprint to mark the significant parameters. This only works if you are plotting a single group (from one or multiple experiments) in the fingerprint plot, e.g. the homozygous fingerprint of two replicate experiments. If you do not want these asterisks, simply skip the setting or turn to NA: `lmePath=NA`. Default is NA.  

**metric**: `mean` or `median`. I briefly experimented with defining the fingerprint as median ± MAD of all *Z*-scores instead of mean ± SEM (see step 6 above). This caused several issues, for example the median of all *Z*-scores is not 0 so the controls' fingerprint was not at 0 anymore. Overall I do not think this was a good idea, but it is left here if ever useful. Default is `mean`.

**grporder**: do you have a preferred order for the groups (genotypes)? If yes, mention it here. If no, you can simply not mention this setting or give `grporder=NA`. You can exclude any group (genotype) by simply not mentioning it here. Default is NA, which keeps all groups.

**controlGrp**: name of control group, e.g. ‘wt’. Does the name of the control group change between experiments? For example it is ‘control’ in one experiment but ‘wt’ in another. That is fine, you can give multiple control groups, for example `controlGrp=c('control', 'wt')`.

**removeControl**: whether or not (TRUE or FALSE) to remove the controls' fingerprint(s). Every point of the controls' fingerprint(s) should be at 0 ± SEM, by definition of the Z-scores. Setting to TRUE first can be a good way to check this and understand the plot. Default is FALSE. Not mentioning the control group(s) in `grporder` is another way of excluding them from the plot.  

**onlyExp**: if you do not wish to plot every experiment present in the data, you can list the only experiments you want to plot here in the format YYMMDD_BX, e.g. `onlyExp=c('220601_16', '220601_17')`.  

**removeParam**: if you do not wish to plot every behaviour parameter present in the data, you can list some parameters to exclude here, e.g. `removeParam=c('activeboutStd', 'sleepLatency')`.  

**colours**: colour of each fingerprint. The order will follow the chronological order of the experiments (their YYMMDD), and within each experiment the order of the groups (genotypes), given as `grporder` or in alphabetical order if `grporder` is not given. For example, say you are plotting experiment 220601_16 and 220609_16 and each have groups hom & het, the order in which to give the colours is: 220601_16_het, 220601_16_hom, 220601_16_het, 220601_16_hom. In practice, you can simply keep `legendOrNo=TRUE` (see below) and do by trial-and-error. Note, R understands a bunch of colour words like ‘red’ or ‘blue’ or you can give them as HEX codes (can use Eyedropper tool in Illustrator to get HEX colour code, for example). Default is NA, which will give default R colours, which actually look alright.  

**connectOrNo**: whether or not (TRUE or FALSE) to connect the dots in the fingerprint plot. I find that the lines help visually tracking each fingerprint. However, connecting the behaviour parameters together does not have any meaning, hence the option to delete these lines. Default is TRUE.  

**legendOrNo**: whether or not (TRUE or FALSE) to make the legend. I would recommend first generating the plot with the legend so you can check that the groups and colours are correctly matched. Default is TRUE.  

**ynameOrNo**: whether or not (TRUE or FALSE) to write the name of the Y axis. TRUE writes "deviation from controls (z-score)" as Y axis name. Default is TRUE.  

***ytextOrNo**: whether or not (TRUE or FALSE) to write the units on the Y axis.  

**xtextOrNo**: whether or not (TRUE or FALSE) to write labels on the X axis. What is written depends on `xParamNum` below. Default is TRUE.  

**xParamNum**: whether or not (TRUE or FALSE) to write the parameter numbers on the X axis, typically 1, 2, 4, ... for day; then 1, 2, 3, ... for night (day skips two parameters: activitySunsetStartle and sleepLatency). FALSE with `xtextOrNo=TRUE` will write the parameter names instead. Default is FALSE.  

**nightBgOrNo**: whether or not (TRUE or FALSE) to add a grey background behind the fingerprint of the night parameters. Default is TRUE.  

**ymin**: start of Y axis, in z-scores.  

**ymax**: end of Y axis, in z-scores.  

**dotSize**: size of the dots ± range (to represent SEM). Default is 0.2.  

**lineSize**: size (thickness) of the lines connecting the parameters. Default is 0.2.  

**asteriskSize**: size of the asterisks taken from the LME report. Default is 3.  

**exportOrNo**: whether or not (TRUE or FALSE) to export to plot to a .pdf file. Default is TRUE.  

**exportPath**: full path to export file. It will create a pdf. Accordingly, exportPath must finish with .pdf.  

**width**: width of the pdf in mm. Default is 150.  

**height**: height of the pdf in mm. Default is 100.  


### ggPairwiseHeat(...)

Calculates the similarity between pairwise behavioural fingerprint and represent the results as a heatmap.

* **fgp**: object or full path to _fingerprint.csv_, created by calculateFingerprint(...).  

* **metric**: `mean` or `median`. I briefly experimented with defining the fingerprint as median ± MAD of all *Z*-scores instead of mean ± SEM (see step 6 above). This caused several issues, for example the median of all *Z*-scores is not 0 so the controls' fingerprint was not at 0 anymore. Overall I do not think this was a good idea, but it is left here if ever useful. Default is `mean`.  

* **simScore**: one of three possible "similarity scores": `correlation` (Pearson correlation), `cosine` (cosine similarity), or `euclidean` (Euclidean distance).  

* **grporder**: do you have a preferred order for the groups (genotypes)? If yes, mention it here. If no, you can simply not mention this setting or give grporder=NA. You can exclude any group (genotype) by simply not mentioning it here. Default is NA, which keeps all groups.  

* **removeControl**: whether (TRUE) or not (FALSE) to remove the controls' fingerprints. I think you should always use TRUE here as the controls' fingerprints are always 0 by definition. Default is TRUE.  

* **controlGrp**: name of control group, e.g. ‘wt’. Does the name of the control group change between experiments? For example it is ‘control’ in one experiment but ‘wt’ in another. That is fine, you can give multiple control groups, for example controlGrp=c('control', 'wt').  

* **minCol**: colour for the start of the colour gradient, i.e. colour assigned to –1 when `correlation` or `cosine` is used or to 0 when `euclidean` is used.  

* **maxCol**: colour for the end of the colour gradient, i.e. colour assigned to +1 when `correlation` or `cosine` is used or to the maximum value when `euclidean` is used.  

* **onlyHalf**: as we are plotting a pairwise matrix, the heatmap is symmetrical across the diagonal, i.e. the similarity scores repeat each other on each side of the diagonal. If that is okay for you, you can omit this setting or give `NA`. If you prefer to plot only one side of the diagonal, you can choose between `upper` or `lower`, which will respectively plot the upper or lower half only.  

* **scoreSize**: font size for the similarity scores.  

* **legendOrNo**: whether or not (TRUE or FALSE) to make the legend. I would recommend first generating the plot with the legend so you can check that the groups and colours are correctly matched. Default is TRUE.  

* **labelsOrNo**: whether or not (TRUE or FALSE) to write the X and Y axis labels.  

* **width**: width of pdf in mm. Default is 250.  

* **height**: width of pdf in mm. Default is 200.  

* **exportPath**: full path to export file. It will create a pdf. Accordingly, exportPath must finish with .pdf.  

### appendRAWs(...)

If tracking was interrupted (for example because of a powershut) and quickly re-started, this function allows to append the two RAWs.csv files into one. Naturally, it will not magically recover the missing frames, so there will be a gap in the data, but the appended RAWs.csv is compatible with other functions.

Note, it will generate a YYMMDD_BX_lights.csv file using the filename of the first RAWs.csv (first ffpath given). For example, if user gives `ffpaths=c('~/.../230307_17_Pt1_RAWs.csv',
                     '~/.../230308_17_Pt2_RAWs.csv')`, it will generate a file called _230307_17_lights.csv_. This is likely to overwrite the original YYMMDD_BX_lights.csv.  

* **ffpaths**: full paths to the two _RAWs.csv files to append. Make sure to give them in chronological order.  

* **exportPath**: full path of RAWs.csv to export to, e.g. `exportPath='~/.../230307_17_RAWsappend.csv'`  

* **dayduration**: day duration in hours. Default is 14.  


### rawToMiddur(...)

Converts frame-by-frame data (a RAWs.csv file) into middur data (a middur.csv file).  

* **ffpath**: full path to _RAWs.csv.  

* **zebpath**: full path to Zebralab .xls results file.  

* **freezing**: minimum Δ pixel for a frame to be counted as active. Any Δ pixel below the freezing threshold is considered inactive. Default is 3.  

* **burst**: minimum Δ pixel for a frame to be counted as active. Any Δ pixel above the burst threshold is considered inactive. Default is 200.  

* **exportOrNo**: whether or not (TRUE or FALSE) to export the middur data to a csv file. TRUE writes a file called YYMMDD_BX_middur.csv in the ffpath folder. Default is FALSE. 


### adjustPixel(...)

Decreases or increases all Δ px data of a group of larvae by a given ratio to cancel (as best as possible) effect of smaller/bigger or darker/fainter pigmentation. It will output a file _YYMMDD_BX_RAWsadjusted.csv_. Obviously editing the raw data comes with some risks, so please only use the function if you suspect that keeping the raw data with the potential detection bias is worse than the small artefacts adjustPixel may cause (see below). Finally, I would recommend you compare results before and after adjusting the Δ px data, for example by comparing fingerprints (`ggFingerprint`) or parameter grid (`ggParameterGrid`) plots.  

Function will print in Console the result of a t-test comparing the maximum sunset startle response across nights of `grpL` vs `grpS` (see below) to help you decide whether adjusting the Δ px is worth the effort.  

Beware, adjusting the data will make you lose parameter activitySunsetStartle. We use it to calculate the scaling ratio (see setting `scale` below) so the procedure cancels any difference in that parameter. Additionally, it seems to create an artefact in parameter activeboutMin. Please ignore/delete these two parameters from any analysis that uses the adjusted data as input. If you create a fingerprint using `ggFingerprint`, you can make use of the `removeParam` setting, i.e. you probably want: `removeParam=c('activitySunsetStartle', 'activeboutMin')`. Alternatively, you can simply delete these parameter tables.  

**ffpath**: full path to _RAWs.csv.

**genopath**: full path to genotype.txt.

**grpL**: which group of larvae do you suspect has a larger size/darker pigmentation? Larvae of this group will get their Δ px data downscaled. Currently the function does not support adjusting the Δ px data of multiple groups, let me know if needed.   

**grpS**: the group of larvae to compare `grpL` to, i.e. the group with the smaller size/fainter pigmentation in comparison.  

**scale**: scaling ratio. For example, `scale=0.9` will multiply every Δ px datapoints of larvae of group `grpL` by 0.9 (i.e. reduce them a bit). If you do not give this setting (or `scale=NA`), adjustPixel will analyse parameter activitySunsetStartle as a proxy for the size/darkness of each larva. The logic is that the startle response at lights OFF should be fairly close to the maximum number of pixels each larva can move in a single frame, which should be directly proportional to its size/darkness. To get as close as possible to the maximum number of pixels each larva can move, adjustPixel takes the maximum startle response across all nights available in the data. For example, say larva #5 moved 55 px at the start of night0, 73 px at the start of night1, 65 at the start of night2; adjustPixel keeps 73 px at the maximum startle response for this larva. The scale setting is then calculated as the ratio of the means (mean of all maximum startle response of larvae from grpS / mean of all maximum startle response of larvae from grpS), which essentially says how much fainter/smaller the larvae of grpS are compared to the larvae of grpL.  

**round**: multiplying by `scale` likely give a decimal number, but Δ px values should be integers. Do you want to round them `up` (e.g. 0.9 becomes 1) or `down` (e.g. 0.9 becomes 0)? Default is `down`, which you should probably use too. My logic is that a partial pixel does not exist, if it is below detection it would not be counted, so I think we should round down.  

# Troubleshooting

Here are leads for troubleshooting. If this does not help in your case, let me know where you are stuck by raising an issue (`Issues` at the top > `New issue`) or get in touch with me.

> Error in grDevices::pdf(file = filename, ..., version = version) :   
  cannot open file '...pdf'

Are you on Windows? Do you have the plot you are trying to overwrite open? On Windows, it seems like R cannot overwrite a PDF that is already open. Close Acrobat Reader (or else) and run the command again.  
If that was not the solution, check carefully that the `exportPath` is correct. Does the folder you are trying to write into exist?

> Error in data.table::fwrite(paL[[i]], file = paste0(beforeLastSlash(ffpath[1]),  :
  Permission denied: 'D:/.../.csv'. Failed to open existing file for writing. Do you have write permission to it? Is this Windows and does another process such as Excel have it open?  

Error is self-explanatory here. On Windows, R cannot overwrite a CSV that is open, for example in Excel. Close Excel (or else) and run the command again.

> Error: vector memory exhausted (limit reached?)  

Are you on a Mac? Please follow [this answer on StackOverflow](https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached).

Are you on a Windows? Please try the following:

1- Increase memory in R settings file

* Open Notepad as administrator
* File > Open..., file C: / Program Files / R / R-xxx / etc / Rprofile.site (click _All files_ on bottom right if you cannot see it)
* Add line:
`invisible(utils::memory.limit(128000))`
* Close RStudio and start again. 
* Once started again, you can run `memory.limit()` in Console to check it got the setting correctly.
* Note, if multiple versions of R co-exist on your computer, make sure you change the `Rprofile.site` file of the correct one (folder name xxx above should be different). If you do not know which R version you are using in RStudio, run in Console `version`.

2- Increase allowed size of page file

From what I understand this is to allow more "virtual" RAM, i.e. data actually being written on the disk but treated as RAM by computer.

* Control Panel > System and Security > System > Change Settings (right) > Advanced tab > Performance: Settings... > Virtual Memory: Change...
* Untick _Automatically[...]_
* For both D: drive & C: drive (or whichever you have), tick Custom size, Initial size = 16 / Maximum size = 64000
* Click _Set_, then restart computer

Please let me know if those instructions do not help.

> R Session Aborted. R encountered a fatal error.

Try following the instructions above. If that does not help, check how much space you have on your hard drive. If you do not have a lot (e.g. less than 20 Gb), there is a good chance this is the cause of the problem. Try to free > 50 Gb. On macos (and probably on Windows too), you can see the free space on your hard drive going down when R runs an intensive operation (Apple logo > About This Mac > Storage). So the more space there is, the more R can use to store temporary files.

> Errors/warnings related to "cairo", such as _failed to load cairo DLL_

Installing `xquartz` on your computer seems to fix this issue. On Mac, if you have Homebrew installed: `brew install --cask xquartz` in Terminal. I found that installing latest release of XQuartz (https://www.xquartz.org/index.html) like you would for any other app works too.  