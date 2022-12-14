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
**trimstop**: whether to trim some of the data at the end, in number of hours since 9 AM of day0. e.g. trimstart = 72 trims all the data after 9AM night2 >> day3 transition. (i.e. 72 hours after 9 AM day 0). It is slightly different than xstop because xstop leaves a little bit of the data after the stop (try to see how it looks without trimming). If you want the trace to stop at 72 hours (9 AM day3) sharp, use xstart=72 and trimstart=72 together. Default is 0, i.e. no trimming. Default is 0, which means no trimming.  
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

ggActivityTraceGrid currently does not support making a legend. Accordingly, please confirm for a few wells that the colours are correctly matched with the groups.
