**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## *Compile:* Select, translate and stack bands or images

*Compile* is the primary command to organize images and prepare them for further processing.

Most of all *Imalys* commands need one image stored at the working directory as an input. The *compile* command translates all selected images to a common format, stacks them to a multi layer image, clips them to the given *frame* and stores the stack as *compile* at the working directory. The compilation ensures that all selected bands share the same format, projection, pixel size and frame. 

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

*This example stacks all images of the results directory taken between May and July 2022 and cuts them to the frame “c4738”. The result is set to UTM zone 32 (crsystem = 32632) and the pixel size to 32 meters. The result is called "Leipzig_2022" and stored at the working directory.*

------

The images can be selected directly by a filename (*select*) or a *search* string with wild chars ('?','*'). Select can be repeated as often as necessary. Each uncompressed image can be used as an input. If a projection *crsystem* and a *pixel* size is passed, the result is converted to the given system, otherwise the properties of the first image will be used. The selection can be reduced further to a specified time *period* and clipped to a common *frame*. Regions outside of he *frame* will be filled with nodata. Each selected image will cover individual bands at the result. If two images share exactly the same acquisition date, they are merged automatically to one image. Different images can be merged with the [reduce](5_reduce.md) command. 

To process time-dependent statistics, the acquisition date of the images must be known. For images from external sources the date [YYYYMMDD] must be added at the end of the file name.  

-----

### *Select* a single images by a filename

```
IMALYS [compile]
…
compile
	select = ~/ESIS/results/BRD_NirV_20230801-20231031.tif
	...
```

*Select* will assign a single image. The *select* parameter can be repeated as often as necessary. Images at the working directory can be called only by their filename. *Select* can be combined with *search* and *period*. 

------

### *Search:* Select images using a search string



```
IMALYS [compile]
…
compile
	search = ~/ESIS/results/*2022*.tif
	...
```

A *search* string using system wildchars ('*','?') is used to select appropriate images. Variable and fixed parts of the filenames can be combined as needed. Images at the working directory can be called only by their filename. The *search* parameter can be repeated as often as necessary. *Search* can be combined with *select* and *period*.

------

### *Period:* Select images by a time period

```
IMALYS [compile]
…
compile
	search = ~/ESIS/results/*2022*.tif
	period = 20220501 – 20220731
	...
```

*Period* reduces the file list of a *search* to images that match the specified period. The acquisition date must be passed as YYYYMMDD - YYYYMMDD (Y=Year, M=Month, D=Day). As not all image formats save an acquisition date, the date must be provided at the end of the file name. Use [import](3_Import.md) to get appropriate names.

------

### *Crsystem [EPSG], Pixel [meters]:* Reproject images and define a pixel size

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

To use different images of the same region or to adopt images to given maps the projection and the pixel size can be changed. *crsystem* expects an EPSG number as new projection. *Pixel* expects a number in meters for the new pixel size. The process depends on the image type. Maps and classified images are reprojected using the nearest neighbor principle. All other images are interpolated by a bicubic convolution process. *Crsystem* and *pixel* can be used also if only the pixel size is to be changed. 

------

### *Frame:* Cut the result to a polygon

```
IMALYS [compile]
…
compile
	search = ~/ESIS/results/*2022*.tif
	period = 20220501 – 20220731
	frame = ~/ESIS/frames/c4738.gpkg
	...
```

If the selected images have different coverages or the result should be restricted to a specified area the parameter *frame* can restrict the result to a specific area. Frame must be passed as a polygon (geometry).

*Frame* spans a rectangle that contains all points of the *frame* and only returns the areas that lie within this rectangle. The *frame* can take any shape. With *clip = true* option, all areas outside the *frame* can be set to NoData. The geometry of the *frame* is automatically reprojected to the coordinate system (*crsystem*) of the images. If no *crsystem* is given all images are adjusted to the first image in the list.

------

### *Clip:* Mask the passed *frame*

**only together with *frame***

```
IMALYS [compile]
…
compile
	search = ~/ESIS/results/*2022*.tif
	period = 20220501 – 20220731
	frame = ~/ESIS/frames/c4738.gpkg
	clip = true
	...
```

*Clip* restricts the visible image to the transferred *frame*. The option *clip = true* overwrites all pixels outside the *frame* with NoData. 

------

### *Target:* Assign a filename for the compiled image stack

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

If the result of the *compile* command should be preserved or two *compile* commands should be given in succession the default name can be changed. 

The *target* parameter saves the result of the *compile* command at the working directory with the passed name. No path name needs to be specified for the working directory. Final results can be stored with the [export](11_Export.md) command at a different place. 

------

## *Names:* Add filenames as layer names

[Top](4_Compile.md)