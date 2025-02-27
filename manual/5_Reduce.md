## Reduce	

**reduce image dimension by pixel based processing**

The *reduce* module summarizes all processes that reduce the number of bands in an image. Use *select* to select an image from the working directory. The *execute* parameter selects one of the processes. Some of them need additional parameters. *Target* can be used to rename the result. The default is named as the used process.

[TOC]

*Reduce* includes 17 different processes. The *mean*, *medium*, *range*, *minimum* and *maximum* processes are calculated as expected. They can be used for quality enhancement. The *bestof* process is a mixture of them that tries to select the most reliable combination of several images. Using *retain = bands* the result of each band is calculated separately. *Calculate* and *formula* allow free band arithmetics. The vegetation indices  *NIRV*, *NDVI*, *EVI* and *LAI* are proxies for biomass and mainly the metabolism rate of green plants. The statistical processes *variance* and *regression* return the expected values. *Weight* returns the sum of all bands.

*Retain* can be used with all processes except *principal*. If a stack of multispectral images is selected, *retain* selects weather the input is reduced to one multispectral image or to a time series with one band for each date. 

*Reduce* accepts more than one *execute* call for the same image. All results are stored at the working directory and named as the process. 

---

### Bands (Parameter) #

**Band selection under NIRV, NDVI, EVI, LAI**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = NIRV
	bands = B3:B4
```

For the vegetation indices *NIRV, NDVI, EVI* or *LAI*, the consecutive band number for the red and infrared bands must be specified. The counting starts with 'B1' for the first band in the image. For a Landsat image with all 6 spectral bands (0.45 µm - 2.3 µm) the third and fourth band (*B3:B4*) is needed. For a Sentinel-2 image with all 4 high-resolution bands (0.44 µm - 0.83 µm) also *B3:B4* must be passed.

------

### Bestof (Process) #

**Automatically choose the most appropriate generalization**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = bestof
```

The process returns an optimized image from one or more (multispectral) images with lesser quality. The typical import is a short time course. Holes in the input images must be coded as nodata. *Bestof* relies on a quality indicator taken during the [import](3_Import.md) process. It also works if all image disturbances are set to nodata. If three or more images exceed the minimum quality restraints the process returns the median of all passed pixels. If two images of equal quality are available, the process returns the mean of the pixel values. If the overall quality differs for more than 20%, only the better image is returned. The three options are evaluated individually for each pixel. If only one image is available, there is no choice.

---

### Brightness (Process) #

**First principal component of all bands**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = brightness
	retain = time
```

*Brightness* uses the first principal component of all bands as a measure of the common brightness of the selected image. If the acquisition date is preserved by *retain = time*, the result consist of a time line with one band per acquisition date. 

​	![image-20240319183231922](../images/5_brigthness.png.png)	v: values; i: items; 

Result: [0 … positive values]

The process will convert negative values into positive ones.

------

### Calculate (Process) #

**Band arithmetic (+, -, *, /, <, >)** 

```
IMALYS [reduce]
…
reduce
	select = compile
	excute = calculate
	formula = B3 / B1 – 0.45
```

*Calculate* can apply the four basic arithmetic operations to every pixel in the image. In addition, there are the logical queries greater than (>) and less than (<) which are translated as '1' for “yes” and '0' for “no”. The formulas can contain numbers that act as constants. Individual bands are selected by a 'B' followed by a digit. '1' denotes the first band.

*Calculate* translates the *formula* strictly from left to right. Parentheses or the “dot before dash” rule in algebra are not supported. In the example above, *calculate* first determines the ratio between band 3 and  1 and then subtracts 0.45. 

---

### Count (Parameter) #

**Image dimensions after principal component rotation**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = principal
	count = 3
```

The *count* parameter restricts the *principal* component rotation to *count* steps. Without *count* the result of the rotations has one dimension less than the source. *Count* can only used together with *principal*.

---

### Execute (Command) #

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = brightness
```

*Execute* selects one of the possible processes under *reduce*. There are currently 17 different processes implemented. In this example *brightness* is selected. Some processes require parameters. *Execute* can be repeated as often as necessary to get different results for the same image.

------

### Formula (Command) #

**Operators (+, -, *, /, <, >) and band IDs (B1, B2, …) under calculate**

```
IMALYS [reduce]
…
reduce
	select = compile
	excute = calculate
	formula = B3 / B1 – 0.45
```

*Formula* denotes an algebraic expression that is applied by *calculate* to all pixels in the image. The expression can consist of band numbers (here 'B3' and 'B1' for bands 3 and 1), arithmetic operators for the four basic arithmetic operations ('+', '-', '*', '/'), logical operators ('>' and '<' for “greater than” and “less than”) and constants (here '0.45').

------

### Maximum (Process) #

**Highest value of all pixel in an image stack**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = maximum
```

The *maximum* process returns the highest value of all bands in a layer stack. *Maximum* is calculated for each pixel individually. Nodata pixels are ignored. 

------

### Mean (Process) #

**Arithmetic mean of all bands**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = mean
```

The *mean* process returns the arithmetic mean of all image bands provided. *Mean* is calculated for each pixel individually. If the *retain* option is used, a stack of multispectral images can be reduced either to one multispectral image or a time course of brightness bands (see *retain*).

​	![image-20240319183835174](../images/5_mean.png)	v: values; i: items; n: item count

Result: [negative values … positive values]

---

### Median (Process) #

**Most common value for each pixel from a stack of bands**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = median
	retain = bands
```

The *median* is defined as the value in the middle of a sorted list and reflects the most common value of each pixel in a stack of bands or images. The calculation is repeated for each pixel individually. If the *retain* option is used (as above) a stack of multispectral images is reduced to one multispectral image of most common values for each band. The *retain = time* option will return a multiband image with one *brightness* layer for each acquisition date.

The *median* process can mask rare values. Clouds or smoke will disappear if more than the half of all pixels show undisturbed values.

------

### Minimum (Process) #

**Lowest value of all pixels in an image stack**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = minimum
```

The minimum process returns the lowest value of each pixel in a layer stack.  *Minimum* is calculated for each pixel individually. Nodata pixels are ignored. 

---

### NDVI, NIRV, EVI, LAI (Process) #

**Normalized vegetation index (NDVI)**
**Near infrared vegetation index (NIRV)**
**Enhanced vegetation index (EVI)**
**Leaf cover per area (LAI)** (deactivated)

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = NIRV
	bands = B3:B4
```

The example returns the NIRV plant metabolism index using band 3 as red and band 4 as near infrared values. The band selection must be passed with *bands* and the band numbers as shown above.

NIRV: ![image-20240319182841085](../images/5_NirV.png)	N: Near infrared value; R: Red band value

Result: [0 … 1]

The *LAI* parameter gives the proportion of leaf surface compared to the ground surface covered by the plants. The LAI was introduced as a proxy for field work. Estimated LAI values by means of estimating the solar induced chlorophyll fluorescence (SIF) seem to return minor quality.

More than 20 different vegetation indices are described¹, some of them only differ in details². The NIRV index tries to quantify the photosynthetically active radiation (PAR) as a measure of plant metabolism³ that might be most important for the evaluation of environmental services. The LAI seems to be very context dependent and is temporarily deactivated.

Most vegetation indices are calculated as the normalized difference between the near infrared and the visible red bands. The NIRV definition shows a better mapping at sparsely vegetated areas. 

------

### Overlay (Process) #

**Overwrite values in a layer stack**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = overlay
```

Overlay superimposes the transferred bands in the given order. The last band has the highest priority. Gaps (NoData) remain empty until a band contains valid values. 

---

### Principal (Process) #

**Principal component rotation**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = principal
	count = 3
```

The example extracts the first *count* principal components from a n-dimensional image. 

The *principal* component rotation tries to extract the most significant image property (overall brightness) from the passed image, stores the result as a new band, deletes the result from the source and repeats the procedure with the remainder for *count* times. In many cases the information content of the source can be concentrated to only a few result bands. *Count* must be lower than the number of bands. 

Def:	![image-20240319183231922](../images/5_brigthness.png.png)	v: values; i: items; iterated "count" times

Result: [0 … positive values]

---

### Range (Process) #

**Absolute difference between the highest and the lowest value**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = range
```

*Range* returns the difference between the highest and lowest value for each pixel in a layer stack. The result is always positive. 

Def: [highest value – lowest value]

Result: [0 … positive values]

------

### Regression (Process) #

**Regression based on standard deviation**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = regression
```

The *regression* process returns the regression of individual pixels for all bands in the source. *Regression* tries to use the temporal distance of the recordings from the time stamps at the end of the filenames. If the images are arranged with the [compile](4_Compile.md) module, the time stamps are added to the band names of the images. Otherwise an equal distance is assumed. Using the *retain = bands* option the process returns a multispectral image of regressions for each band.

​	![image-20240319181602245](../images/5_regression.png)	t: time; v: values; i: items; n: item count

Result: [negative values … positive values]

Since not all image formats register the acquisition date, the date [as YYYYMMDD] must added at the end of the filename. Using the [import](3_Import.md) command the acquisition date is added properly. 

---

### Retain (Command) #

**decide if spectral or temporal characteristics are preserved**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = »processes under reduce«
	retain = »time | bands«
```

*Retain* can be used with each process under *reduce* except the *principal* component rotation. If *retain* is not used, *reduce* combines all selected bands to one result band. If a stack of multispectral images is selected, *retain = time* generates a multispectral image with one band per acquisition date. The resulting bands reflect the result of the selected process for each acquisition date. *Retain = bands* instead combines all selected images to one multispectral image with all bands of the source images. The selected process is executed for each band separately. 

»process under reduce« must be exchanged by a defined process name like *regression*. *retain=time* or *retain=bands* are alternatives. Retain will only work properly if the selected images are combined with the [compile](4_Compile.md) module. 

---

### Select (Command) #

**Mark an image of the working directory to be processed**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = »processes under reduce«
```

All processes under reduce need one multiband image as input. Different bands or images should be combined with the [compile](4_Compile.md) module. [Compile](4_Compile.md) checks image properties and adds the required metadata. »process under reduce« must be exchanged by a defined process name like *regression*. 

------

### Target (Command) #

**Rename the result of the last process**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = »processes under reduce«
	target = »filename«
```

The *target* option renames the result of the last process. The new name is restricted to the working directory and needs no path. If several processes (*execute =*) are called together, only the last result will be modified. Choose the [export](11_Export.md) command to store one or more results at a different place.  »process under reduce« must be exchanged by a defined process name like *regression*. »filename« must be exchanged by the new name.

---

### Variance (Process) #

**Variance based on standard deviation**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = variance
```

The *variance* process determines the variance of individual pixels based on a standard distribution for all individual bands in the source image. If the *retain = bands* or the *retain = time* option is used (see *retain*) the result is a multiband image of variances. Otherwise the result is a single band of the variance of all bands.

​	![image-20240323125742064](../images/5_variance.png)	v: values; i: items; n: item count

Result: [0 … positive values]

------

### Weight (Process) #

**Global sum of all bands in the layer stack**

```
IMALYS [reduce]
…
reduce
	select = compile
	execute = weight
```

*Weight* returns the sum of all values from all bands of the layer stack for each pixel. Unlike *brightness*, *weight* sums arithmetically. Negative values reduce the result.

---

[Previous](4_Compile.md)	–	[Index](README.md)	–	[Next](6_Kernel.md)
