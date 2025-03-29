**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## 7 Process Chain Variables

The process chain can use variables instead of parameters. A “$” sign followed by a single number will be interpreted as a variable. Variables can be used for different purposes:

- Long filenames can be substituted by a short expression
- All input parameters for a given process chain can be concentrated at the beginning of the process chain
- Parameters can be passed as a list. The list will repeat the process chain for each entry without manual interaction
- Image selection can depend on variable parameters

-----

### 7a Define variables

Variables must be defined under [replace](../manual/12_Replace.md) at the beginning of the process chain. Each occurance of the variable will be exchanged by the defined string. Variables and constant letters can be mixed as desired.

```
IMALYS [example 7a]
replace
	$1 = c7106
	$2 = Leipzig
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results/$1_$2
```

will replace the last line with

```
	...
	log = ~/results/c7106_Leipzig
```

Tutorial 7a shows how a variable definition under *replace* is exchanged in the last command line of the example. Each variable of the whole process chain is replaced in the same way.

------

### 7b Repeat a process chain

Variables can also be used to repeat a whole process chain with different inputs and produce corresponding outputs without further interaction. In this case the variables have to be passed as a comma separated list. The process chain will be repeated for each entry in he list. If more than one variable is to be repeated, the lists must contain the same number of entries. Other variables will be exchanged with the same value at all repetitions.

```
IMALYS [example 7b]
replace
	$1 = c3542, c3546, c4738, c6730
	$2 = Berlin-West, Berlin-Mitte, Leipzig, Nürnberg
	$3 = summer
home
	directory = ~/.imalys
	clear = false
	log = ~/ESIS/results
compile
	search = ~/ESIS/results/$1_$2_$3*.tif
reduce
	select = compile
	execute = bestof
	preserve = bands
export
	select  =  bestof
	target  =  ~/ESIS/results/$1_$2_$3_2020-2024.tif
```

Tutorial 7b shows how to repeat a process chain for 4 different cities. The *search* under [compile](../manual/4_Compile.md) specifies the city (name) and the season (summer) but not the year. **The example assumes that image data for the four cities are stored in the *results* folder. These data are NOT included in the tutorial data!** [Reduce](../manual/5_Reduce.md) merges the different years to one multispectral image and [export](../manual/11_Export.md) saves them with a new name.

------

### 7c Combine database and variables

A process chain can include repetition and several processes. In this case different archives are extracted using a *database* and the result is combined to a timeline over 5 years with images of two different seasons.

```
IMALYS [example 7c]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
replace
	$1 = c7106
	$2 = Leipzig
	$3 = 2020, 2021, 2022, 2023, 2024
import
	database = ~/ESIS/archives/bonds.csv
	frame = ~/ESIS/frames/$1.gpkg
	quality = 0.7
	bands = _B2, _B3, _B4, _B5, _B6, _B7
	factor = 2.75e-5
	offset = -0.2
compile
	search = LC0*.hdr
	period = $30501-$30730
reduce
	select = compile
	execute = bestof
	preserve = bands
export
	select = bestof
	target = ~/ESIS/results/$1_$2_$3_summer.tif
compile
	search = LC0*.hdr
	period = $30801-$31031
reduce
	select = compile
	execute = bestof
	preserve = bands
export
	select = bestof
	target = ~/ESIS/results/$1_$2_$3_autumn.tif
```

Tutorial 7c shows how to create two seasonal image products from archive data of 5 years. *Database* under [import](../manual/3_Import.md) selects all archives that cover the *Leipzig* map sheet at least partly and show less than 30% cloud cover (*quality = 0.7*). All optical *bands* are selected and calibrated to TOA reflectance (*factor*, *offset*). 

The *search* command under [Compile](../manual/4_Compile.md) selects all image scenes with "LC0" at the beginning of the filename regardless of their acquisition date. This includes Landsat-8 ("LC08") and Landsat-9 ("LC09") images. *Period* reduces the result for summer (20200501-20200731). The year is due to the variable *$3* with *2020* in the first run. [Reduce](../manual/5_Reduce.md) combines the images to one multispectral result and [export](../manual/11_Export.md) stores them under a new name. 

The whole process is repeated for the *autumn* season (20200801-20201031) within the process chain. The variable list *$3 = 2020, 2021,…* iterates the process chain for the years 2020 to 2024.

[Top](7_Variables.md)

