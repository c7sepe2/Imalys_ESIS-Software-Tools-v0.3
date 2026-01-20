**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## 7 Process Chain Variables

The process chain can use variables instead of parameters. A “$” sign followed by a single number [0 … 9] will be interpreted as a variable. Variables can be used for different purposes:

- Long filenames can be substituted by a short expression
- All input parameters for a given process chain can be concentrated at the beginning of the process chain
- Parameters can be passed as a list. The list will repeat the process chain for each entry without manual interaction
- Image selection can depend on variable parameters

-----

### 7a Define variables

Variables must be defined under [replace](../manual/12_Replace.md) at the very beginning of the process chain. Each occurance of the variable will be exchanged by the defined string. Variables and constant letters can be mixed as desired.

```
IMALYS [Tutorial 7a]
replace
	$1 = 2022
	$2 = 0501
	$3 = 0731
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
import
	search = ~/ESIS/archives/$1/LC0*.tar
	period = $1$2 - $1$3
	...
```

will replace the last two lines with

```
	...
import
	search = ~/ESIS/archives/2022/LC0*.tar
	period = 20220501 - 20220731
	...
```

Lesson 7a shows how a variable definition under *replace* id formatted and variables are exchanged with their values. The example is taken from tutorial 2d. Each variable of the whole process chain is replaced in the same way.

------

### 7b Repeat a process chain

Variables can also be used to repeat a whole process chain with different inputs and produce corresponding outputs without further interaction. In this case the variables have to be passed as a comma separated list, included in curly brackets. The process chain will be repeated for each entry in he list. If more than one variable is to be repeated, the lists must contain the same number of entries. Other variables will be exchanged with the same value at all repetitions.

```
IMALYS [Tutorial 7b]
replace
	$1 = { 2020, 2021, 2022, 2023, 2024 }
	$2 = 0501
	$3 = 0731
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
import
	search = ~/ESIS/archives/$1/LC0*.tar
	period = $1$2 - $1$3
	...
```

For the first year of the variable definition of $1 under *replace* the variables under *import* will be replaced as shown below:

```
...
import
	search = ~/ESIS/archives/2020/LC0*.tar
	period = 20200501 - 20200731
	...
```

Tutorial 2b shows how a whole process chain can be repeated for several times with changing values. Imalys runs the whole process chain for each year defined as »$1« under *replace*. The example shows only the beginning of the process chain of [tutorial 2d](2_Images.md).

------

### 7c Concatenate and repeat a process chain

Process chains can be combined as needed. In this example 10 different optimized image products of five different years and two different seasons are produced with the same process chain. 

```
IMALYS [process chain 7c]
replace
	$1 = { 2020, 2021, 2022, 2023, 2024 }
	$2 = 0501
	$3 = 0731
	$4 = 0801
	$5 = 1031
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
# ------------------------------
# summer
# ------------------------------
import
	search = ~/ESIS/archives/$1/LC0*.tar
	period = $1$2 - $1$3
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
	target = ~/ESIS/results/bestof_$1$2-$1$3.tif
# ------------------------------
# autumn
# ------------------------------
import
	search = ~/ESIS/archives/$1/LC0*.tar
	period = $1$4 - $1$5
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
	target = ~/ESIS/results/bestof_$1$4-$1$5.tif
```

Tutorial 7c shows how to concatenate and repeat a process chain different years and seasons. The *search* under [compile](../manual/4_Compile.md) specifies the year, two calls of almost the same process chain differentiate between summer and autumn. The '#' sign is used to include comments. Except the different years, the example is taken from [tutorial 2d](2_Images.md). **The example assumes that image data of the five years are stored in the "archives" folder. These images are NOT included in the tutorial data!** 

------

[Top](7_Variables.md)

