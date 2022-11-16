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

Fire up RStudio. See also **R tips** below if you want to a nicer experience.  

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

`ffDir`: path to the folder with your raw .xls files.  
`zebpath`: path to the Zebralab results file (.xls).  
`boxGen`: 2 if you are using the newer version of Zebralab (post circa 2020), 1 for previous versions.  
`zt0`: Zeitgeber 0 (ZT0). What is the time of your sunrise?  
`dayduration`: how long does the day last in your experiment? By day, we mean lights ON.  

These are only brief notes on the settings, please refer to the documentation for the full explanations, especially if you get stuck.  

`vpSorter` should write _220531_14_RAWs.csv_ in your experiment folder. The format is fairly simple:  

| fullts      | zhrs        | exsecs | f1 | f2 | ... |
| ----------- | ----------- | ------- | -------| | |
| 2022-05-31 14:08:12| 5.136678 |    0.040393   | 0 | 29 | ... |
| 2022-05-31 14:08:12| 5.136689 |    0.079975   | 3 | 19 | ... |

Each row is one frame. Each column is one well, plus a few timestamp columns at the start.

`fullts`: full clock date/time  
`zhrs`: number of hours since ZT0 on the day you started the experiment (day0). ZT0 always occured _before_ you started the experiment.  
`exsecs`: number of seconds since experiment started. If you recorded at 25 frames-per-second, you expect roughly: 0.04, 0.08, etc.  
`f1`: data for well #1 (f for fish).  
`f2`: data for well #2.  
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
                exportPath='~/.../framerate.pdf')

![](tutorialPics/framerate.pdf?raw=true)

Every command that generates a plot starts with `gg` (it stands for "grammar of graphics", if you were curious).  

Some minimal explanations of the important settings:  

`ffpath`: full path to the RAWs.csv file.  

Plotting every instantaneous frame rates would take a while and is completely unnecessary, so instead we only plot a subset of them:  
`subsample`: whether to plot only a subset of the instantaneous frame rates (`TRUE` strongly recommended).  
`subsample_by`: e.g. 1000 means we only plot every 1000th instantaneous frame rate.  

`exportOrNo`: whether to export the plot to a .pdf file.  
`width`: width of the plot pdf in mm.  
`height`: height of the plot pdf in mm.  
`exportPath`: where/under which name do you want to save the plot? Note, it has to finish by .pdf.  

___

## R basics
> under construction, do not read

* script vs console
* environment
* comments
* vector with c()
* strings in quotes
* run some lines
* tab auto-complete
* paths

## R tips
> under construction, do not read

* here package  
* don't save .Rdata
* project
