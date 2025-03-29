**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

[TOC]

------

## 1 Prepare the tutorial files

### 1a Download the tutorial data

- Create a new directory *ESIS* at your home directory
- download the image and vector data from [zenodo](https://zenodo.org/records/11097359) 
- extract the ZIP archive to the new *ESIS* directory 

The examples at the tutorial will refer to this data set. If you prefer to install the tutorial to another directory, you will have to modify the path names at the examples.

***Imalys has been significantly changed with version 0.3. The examples in the *results* chapter of the tutorial data match the tutorial of the previous version. New parameters and improved algorithms may lead to different results using the current version. This applies in particular to classification results and details in the shape and size of the zones.***

------

### 1b Install executable files

All commands and processes of *Imalys* are compiled into one executable program *xImalys*. *xImalys* does not need to be installed. It is sufficient to copy [xImalys](../executables) to your *usr/local/bin* directory. You will need administrative rights to copy it. The *usr/local/bin* directory is not included into the package system.

 - download *xImalys* from the *binaries* folder
 - copy *xImalys* to your /usr/local/bin directory

```
sudo cp ~/downloads/xImalys usr/local/bin
```

If you prefer to install the executable files to a subdirectory of your */usr/local/bin* do not forget to extend your environment for the selected path.

Imalys uses the *GDAL library* of the [Open Source Geospatial Foundation](https://www.osgeo.org/) for a lot of processes. This library must be installed under */usr/bin*. For many Linux distributions this is already the case. Alternatively GDAL can be installed from [GitHub](https://github.com/OSGeo/GDAL). If you run QuantumGis the GDAL library is already installed.

-----

### 1c Run a process chain

The executable file *xImalys* must be called as a command in a shell or terminal. The only parameter is the filename of a *process chain*. The *process chain* is a text file (hook) containing commands and parameters (see [manual](../manual/0_Execute.md)).

```
xImalys »path_to_process-chain_file«
```

-----

### 1d Initialize Imalys processes

To simplify long process chains, all commands and parameters are given in one text file or hook. The first entry is always the [home](../manual/1_Home.md) command with information about the working directory and where to store the logs.

```
IMALYS [process chain 1d]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/results
```

The [home](../manual/1_Home.md) command and *IMALYS* at the beginning of the first line is mandatory for each process chain. All examples will use the default working directory *~/.imalys*. The directory can be cleared by *clear=true* at the beginning of the process chain. The *log* directory *~/ESIS/results* is assigned to store messages and metadata. Each process and many sub-steps return messages about the progress of the processing. We recommend to store these files together with the results.

-----

### 1e General syntax of the process chain

The *process chain* passed to *xImalys* must contain commands and their parameters. The commands can be combined as necessary but the internal logic of the process chain is up to the user. The *home* command to create or assign a working directory at the beginning of the process chain is mandatory. 

```
IMALYS [tutorials 3a]
home
	directory = ~/.imalys
	log = ~/ESIS/results
compile
	search = LC0*.hdr
	period = 20220801-20221031
reduce
	select = compile
	execute = bestof
	retain = bands
reduce
	select = bestof
	execute = NirV
	execute = NDVI
	formula = B3:B4
export
	select = NirV
	target = ~/ESIS/results/NirV.tif
export
	select = NDVI
	target = ~/ESIS/results/NDVI.tif
```

*Example of a process chain. The syntax is explained in detail at [indices](3_Indices.md).*

------

Each command and each parameter needs a separate line. A single word in one line is interpreted as a command. Lines with a “=” character are interpreted as a “parameter = value” pair. Parameters always have a preset. It is changed by the entry in the text. Only the *select* or *search* parameter to assign an appropriate input for each command is mandatory. 

Everything after the "#" character is interpreted as a *comment* and ignored until the end of the line.

The working directory was implemented to enable the rapid processing of data that may only be available through a service or a slow connection. It should be directly accessible. The process chain is bound to the working directory. It can be created or emptied at the beginning of the chain and stores all intermediate results. Each instance of *Imalys* needs its own working directory. 

Besides the [export](../manual/11_Export.md) command each process stores the results to the working directory. The file name of the result is the same as the command name. If the results are transferred to tables, the process name also serves as a field name. Therefore command names are short and can have a much wider meaning in general usage.

The result names can be changed using the *target* parameter. As Imalys must be able to run without manual control existing files will be overwritten without warning. All images are stored as raw binary with ENVI header. *Imalys* thus complies with the requirements of the European Space Agency (ESA). Geometries use the WKT-format.

[Top](1_Prepare.md)
