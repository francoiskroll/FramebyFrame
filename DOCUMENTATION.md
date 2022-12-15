# FramebyFrame R package: Documentation
> in construction

Francois Kroll, 2022 @Rihel lab, UCL.

[![alt text][1.2]][1] [@francois_kroll](https://twitter.com/francois_kroll)

:email: francois@kroll.be

<!-- icons with padding -->
[1.1]: http://i.imgur.com/tXSoThF.png (twitter icon with padding)

<!-- icons without padding -->
[1.2]: http://i.imgur.com/wWzX9uB.png (twitter icon without padding)

<!-- links to your social media accounts -->
[1]: https://twitter.com/francois_kroll

Full guide to the FramebyFrame R package.

Make sure to follow first the Minimal tutorial (in README.md) for a smooth introduction to the package.  


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

nightBgOrNo: whether or not (TRUE or FALSE) to add grey backgrounds to represent the nights.  

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
legendOrNo: Whether or not (TRUE or FALSE) to make the legend. I would recommend first generating the plot with the legend so you can check that the groups and colours are correctly matched. Default is TRUE.

**exportOrNo**: Whether or not (TRUE or FALSE) to export the plot. Default is TRUE.

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

*	**activitySunsetStartle**: each larva’s startle response at the day-to-night transition, defined as the maximum delta pixel value during the three seconds after lights switch OFF. Note, this parameter is only defined for nights. For example, the highest swimming bout of larva #9 during the first three seconds of night2 reached 50 pixels.

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
