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

#### ggFramerate

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
