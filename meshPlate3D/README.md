# about the 3D-printed 96-well mesh-bottom plate

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />The model **meshPlate.3mf** is released under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>.

But feel free to get in touch about commercial use!

**meshPlate.3mf** is the 3D model of the 96-well mesh-bottom plate used in Kroll et al., 2023. It was created by Eirinn Mackay.

## some printing instructions

* The model itself has a solid base which is 0.4 mm thick (i.e. two layers at 0.2 mm layer height). The trick is to tell the slicing software to use 0 bottom layers instead of the default 4. It will then use the infill pattern to print the bottom. Ideally, this should only occur in the middle area, not the border of the plate. To achieve this, we used features of PrusaSlicer to apply different printing preferences to different parts of an object. In theory, one should be able to open the .3mf file in PrusaSlicer (open as a 'project') and it should just work.

* When opened in PrusaSlicer, select the 'Generic-Slab' (right panel) to adjust the infill density for the part of the model which overlaps the generic slab. A higher infill density will make the mesh tighter. Infill density of 55% seems to work well. A first prototype at infill 45% was loose enough to allow some larvae to escape. When you slice the object in PrusaSlicer, the mesh should be clearly visible, as in the screenshot below.

Feel free to get in touch if need any help with printing.

![alt text](https://github.com/francoiskroll/FramebyFrame/blob/main/meshPlate3D/prusa_screenshot.png?raw=true)