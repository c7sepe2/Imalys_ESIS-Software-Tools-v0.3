**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## 2 Seamless image products

Satellite image data are shipped in compressed archives, the tracks are cut into tiles, different bands are stored to different files and the image values support compression. The [import](../manual/3_Import.md) and the [catalog](../manual/2_Catalog.md) commands are designed to collect the necessary data, check their quality and return calibrated images within a given frame. The [compile](../manual/4_Compile.md) command is to store all images with a common projection, pixel size and file format and arrange different parts of a larger image to a seamless result.

![](../images/T2_Spain.png)

*Selection points and image data for entomological research at the north of Madrid; Sensor: Sentinel-2; Bands: 8-4-3 (Infrared); Date: 12 June 2021; Calibration: TOA Reflectance; Section: Test area; Source: ESA*

------

The [import](../manual/3_Import.md) command uses a lot of parameters to return most appropriate images. *Select* accepts the archive name(s), *frame* cuts the scene to a given region, *period* selects the acquisition time, *factor* and *offset* transfer the result into calibrated values and finally *quality* rejects images with more than the given partition of pixels with low quality. Except of *select* or *search*, none of these parameters are mandatory. 

-----

### 2a Extract, calibrate and crop images

The first tutorial is to import 6 optical bands of 4 Landsat summer images, cut them to the frame of a Region Of Interest (ROI) and calibrate the results to Top Of Atmosphere (TOA) reflectance.

The necessary **archives** are preselected using a mask and then further restricted by parameters. Only the time period *period* is used in this case (see [import](../manual/3_Import.md)). Individual archives can be selected if a complete name is entered as “search.” 

The *frame* parameter cuts out the **ROI**. *Imalys* expects a vector file to define the ROI. The result will be a rectangle including all points of the given geometry. *Select* without *frame* imports the whole tile of approximal 37.000 km².

With the *bands* parameter, *import* extracts the specified bands. The identifiers correspond to the names assigned by the provider. They appear at the very end of the file names in the archive. Without *bands*, *import* would extract all entries in the archive. With “bands = .TIF”, it would extract all image data. This will be rarely necessary and the calibration can depend on the wavelength.

To compare images or calculate indices the values of the archived images must be *calibrated*. TOA reflectance is the most common format, radiation [W/m²] is another important option. The calibration parameters are part of the image metadata. As an example, *factor* and *offset* for TOA reflectance are given for the optical bands of Landsat 5-9 and Sentinel 2:

| Sensor                | Factor      | Offset |
| :-------------------- | ----------- | ------ |
| Landsat 5,7,8,9       | 2.75e-5     | -0.2   |
| Landsat B10 (thermal) | 3.418023e-3 | 149.0  |
| Sentinel-2            | 1.0e-4        | 0      |

For image analysis, it is important whether a pixel is undefined or has a value of zero. Undefined pixels are defined as NoData (1/0) in *Imalys*. Almost all providers use zero as NoData. For indices or elevation models, zero can be a valid value. NoData should be set as early as possible.

```
IMALYS [tutorial 2a]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
import
	search = ~/ESIS/archives/LC08_L2SP_193026_2022*.tar
	period = 20220501 - 20220731
	frame = ~/ESIS/frames/bounding-box.shp
	bands = B2, B3, B4, B5, B6, B7
	factor = 2.75e-5
	offset = -0.2
	nodata=0
```

tutorial 2a shows, how the [import](../manual/3_Import.md) command selects (*search*) four Landsat-8 OLI images, extracts them from the tar-archive, cuts them with *frame* to the extend of the *bounding-box.shp*, extracts six optical *bands* of the OLI sensor from each archive, calibrates the values with *factor* and *offset* to TOA reflectance and stores the result as multilayer images at the working directory *~/.imalys*. 

The result names are composed of the sensor type (“LS”), the tile ID (‘193026’) and the date of recording, e.g. “20220515”. The date is encoded as YYYYMMDD with Y=Year, M=Month, D=Day.

The *home* process is necessary for each process chain (see [prepare](1_Prepare))

------

### 2b Enhance image quality

A main problem of satellite images are dropouts due to cloud coverage and other image failures. Less noticeable but of similar importance is the seasonal change of the landscape features. Each image is a snapshot. Dropouts and changes can be controlled by collecting information from the same region at different acquisition times. Tutorial 2b shows how to extract a seamless image with typical values out of a couple of patchy images.

![Munich B654 summer](../images/T2_Munich_summer.png)

*Infrared RGB composit of the city of Munich (Germany) ⬥ Sensor: Landsat 8/9 ⬥ Bands: 6-5-4 (Infrared)  ⬥ Date: Mai – July 2022 ⬥ Calibration: TOA Reflectance ⬥ Source: USGS*

------

[Reduce](../manual/5_Reduce.md) with *execute = bestof* uses the median of all selected images to return the most common value for each pixel in an image stack. The result is a “typical” value for a short time period. Almost everywhere clouds are random in time. If the majority of each pixel in an image stack is clean, the most common value is free of clouds and cloud shadows. To get an impression how the process works, compare the imported images from October, September and August (tutorial 2d) with the result of the *bestof* process. 

Except for [import](../manual/3_Import.md) and [compile](../manual/4_Compile.md), all Imalys commands expect data that has been saved in the working directory by *compile* or other *Imalys* commands. 

```
IMALYS [tutorial 2b]
home
	directory = ~/.imalys
	clear = false
	log = ~/ESIS/results
compile
	search = LS*.hdr
	crsystem = 32632
	pixel = 30
reduce
	select = compile
	execute = bestof	
	retain = bands
export
	select = bestof
	target = ~/ESIS/results/bestof_20220501-20220731.tif
```

Tutorial 2b uses the results of tutorial 2a. The default working directory *~/.imalys* is assigned by the [home](../manual/1_Home.md) command but not cleared to use the imported images for subsequent processes.

The [compile](../manual/4_Compile.md) command is mandatory if different images should be compared or processed together. In this case the *search* parameter selects all Landsat images at the working directory. Because *compile* can read any image format, the name of the metadata must be specified here to select the ENVI format. *Compile* also controls and reprojects the images if necessary. 

The [reduce](../manual/5_Reduce.md) command transforms the compiled stack to a new image. Using the *retain = bands* parameter *bestof* reduces a multi image stack to one multiband image with the same bands or colors as one of the original images. The *bestof* process tries to return the most significant content of the different bands. *Bestof* depends on the extended image metadata that [compile](../manual/4_Compile.md) provides.

The [export](../manual/11_Export.md) process transfers the image into a Geo-TIFF and stores the result to a freely selected place. The new image format is defined by the extension. No extension will select the ENVI labeled format as it is used at the working directory.

The results of the [compile](../manual/4_Compile.md) and the [reduce](../manual/5_Reduce.md) commands can be called without a path name or an extension. If no path is given, *Imalys* looks at the working directory.

------

### 2c Import of selected tiles

If contiguous images of areas larger than one (Landsat) tile are required, or if the selected area lies on a tile boundary, the tile IDs of Landsat and Sentinel-2 can be specified directly using *tiles*. 

```
IMALYS [process chain 2c]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
import
	search = ~/ESIS/archives/LC0*.tar
	period = 20220801 - 20221031
	tiles = 193026, 193027
	frame = ~/ESIS/frames/bounding-box.shp
	bands = B2, B3, B4, B5, B6, B7
	factor = 2.75e-5
	offset = -0.2
	nodata=0
```


Similar to Tutorial 2a, Tutorial 2c selects all archives in the “archives” directory, selects Landsat bands 2 to 7, crops the result to the area of “bounding-box.shp,” calibrates to TOA reflectance and sets zero as NoData.

Unlike Tutorial 2a, Tutorial 2c extracts the months August to October (period = 20220801 - 20221031) and selects tiles no. 193026 and 193027. Tile 193027 is located south of Munich and is necessary if the city and district of Munich are to be mapped. Tile 193027 is not included in the dataset on Zenodo. 

Tutorial 2c shows how individual tiles can be selected from a larger collection of archives. [Import](../manual/3_Import.md) ignores the missing tile at the "archives" directory. Missing tiles can be downloaded from the [USGS](https://earthexplorer.usgs.gov/) at any time.

------

### 2d Select and reduce a short time course

Tutorial 2a shows how summer images of Munich are extracted, while Tutorial 2b reduces them to a typical scene. Tutorial 2c combines both processes for the autumn period and directly exports a typical autumn scene.

Unlike in summer, the autumn images are heavily affected by clouds. With *bestof*, it is still possible to obtain a seamless image that only lacks data in one location in the west of Munich.

```
IMALYS [process chain 2d]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
import
	search = ~/ESIS/archives/LC0*.tar
	period = 20220801 - 20221031
	frame = ~/ESIS/frames/bounding-box.shp
	bands = B2, B3, B4, B5, B6, B7
	factor = 2.75e-5
	offset = -0.2
	quality = true
	nodata=0
compile
	search = LS*.hdr
	crsystem = 32632
	pixel = 30
	frame = ~/ESIS/frames/bounding-box.shp
reduce
	select = compile
	execute = bestof
	retain = bands
export
	select = bestof
	target = ~/ESIS/results/bestof_20220801-20221031.tif
```

Tutorial 2d shows how *import*, *median*, and *export* can be combined into a process chain. *Import* extracts with *period* the images from August to October, selects with *bands* the six optical bands, calibrates them with *factor* and *offset* to TOA reflectance and sets the value zero in the images to NoData. 

*Compile* combines the extracted bands, checks formats and projection and generates the necessary metadata. *Crsystem* and *pixel* initialize the control functions of *compile* and automatically change the projection of the images if they do not match the specifications. *Frame* delineates a defined region of interest. If the image has been reprojected, the frame is necessary to compare the result with other images.

*Reduce* generates a 6-band image with typical values for the autumn period from the result of *compile*.

*Export* saves the result in a freely selectable format.

All processes that require the date of the recordings assume that the date is in the format YYYYMMDD at the end of the file name (Y=Year, M=Month, D=Day).

------

[Top](2_Images.md)