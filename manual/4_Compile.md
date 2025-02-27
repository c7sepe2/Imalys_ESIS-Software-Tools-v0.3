## Compile	

**Select, translate and stack bands or images**

*Compile* is the primary command to organize images and prepare them for further processing.

```
IMALYS [compile]
…
compile
	search = ~/ESIS/results/*2022*.tif
	period = 20220501 – 20220731
	frame = ~/ESIS/frames/c4738.gpkg
	crsystem = 32632
	pixel = 30
	target = Leipzig_2022
```

Most of all *Imalys* commands need one image stored at the working directory as an input. The *compile* command translates all selected images to a common format, stacks them to a multi layer image, clips them to the given *frame* and stores the stack as *compile* at the working directory. The compilation ensures that all selected bands share the same format, projection, pixel size and frame. 

The images can be selected directly by a filename (*select*) or a *search* string with wild chars ('?','*'). Select can be repeated as often as necessary. Each uncompressed image can be used as an input. If a projection *crsystem* and a *pixel* size is passed, the result is converted to the given system, otherwise the properties of the first image will be used. The selection can be reduced further to a specified time *period* and clipped to a common *frame*. Regions outside of he *frame* will be filled with nodata. Each selected image will cover individual bands at the result. If two images share exactly the same acquisition date, they are merged automatically to one image. Different images can be merged with the [reduce](5_reduce.md) command. 

To process time-dependent statistics, the acquisition date of the images must be known. For images from external sources the date [YYYYMMDD] must be added at the end of the file name.  

------

### Example

This first example stacks all images taken between May and July. The images are taken from the working directory. As no target name is given the result is named “compile” and stored at the working directory.

The second example stacks all images taken at 2022 from the “results” directory and cuts them to the frame “c4738” and stored them to the working directory. The result is called "Leipzig_2022". The “TIFF” format is converted to “ENVI” during the stacking process. 

-----

### Select

**Select images by their filename**

```
IMALYS [compile]
…
compile
	select = ~/ESIS/results/BRD_NirV_20230801-20231031.tif
	...
```

*Select* will assign a single image. The *select* parameter can be repeated as often as necessary. Images at the working directory can be called only by their filename. *Select* can be combined with *search* and *period*. 

------

### Search

**Select images using a search string**



```
IMALYS [compile]
…
compile
	search = ~/ESIS/results/*2022*.tif
	...
```

A *search* string using system wildchars ('*','?') is used to select appropriate images. Variable and fixed parts of the filenames can be combined as needed. Images at the working directory can be called only by their filename. The *search* parameter can be repeated as often as necessary. *Search* can be combined with *select* and *period*.

------

### Period

**Select images of a given time period**

```
IMALYS [compile]
…
compile
	search = ~/ESIS/results/*2022*.tif
	period = 20220501 – 20220731
	...
```

*Period* reduces the file list of *select* or *search* to images that match the specified period. The acquisition date must be given as YYYYMMDD (Y=Year, M=Month, D=Day). As not all image formats save an acquisition date, the date must be provided at the end of the file name. Use [import](3_Import.md) to get appropriate names.

------

### Frame

**Cut the result to a given polygon**

```
IMALYS [compile]
…
compile
	search = ~/ESIS/results/*2022*.tif
	period = 20220501 – 20220731
	frame = ~/ESIS/frames/c4738.gpkg
	...
```

If the selected images have different coverages or the result should be restricted to a specified area the *frame* parameter will clear all image parts outside of the passed *frame*. The *frame* can take any shape. The passed geometry is automatically reprojected to the passed *crsystem* and *pixel* size. If no *crsystem* is given all images are adjusted to the first image in the list.

------

### Crsystem, Pixel 

**Reproject images and define a pixel size**

```
IMALYS [compile]
…
compile
	search = ~/ESIS/results/*2022*.tif
	period = 20220501 – 20220731
	frame = ~/ESIS/frames/c4738.gpkg
	crsystem = 32632
	pixel = 30
	...
```

To use different images of the same region or to adopt images to given maps the projection and the pixel size can be changed. The process depends on the image type. Maps and classified images are reprojected using the nearest neighbor principle. All other images are interpolated by a bicubic convolution process.

*crsystem* expects an EPSG number as new projection. *Pixel* changes the pixel size during the reprojection process. *Crsystem* and *pixel* can be used also if only the pixel size is to be changed. 

------

### Target

**Assign a filename for the compiled image stack**

```
IMALYS [compile]
…
compile
	search = ~/ESIS/results/*2022*.tif
	period = 20220501 – 20220731
	frame = ~/ESIS/frames/c4738.gpkg
	crsystem = 32632
	pixel = 30
	target = Leipzig_2022
```

The *compile* command saves its result as "compile" at the working directory. If the result of the *compile* process should be preserved or two *compile* commands should be given in succession the default name can be changed. Final results can be stored with the [export](11_Export.md) command at a different place. 

------

[Previous](3_Import.md)	–	[Index](README.md)	–	[Next](5_Reduce.md)