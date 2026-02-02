**[Home](../README.md) «» [Manual](../manual/README.md) «» [Tutorial](../tutorial/README.md) «» [Background](../background/README.md) «» [Source](../source)**

------

## Install executable files

All commands and processes of *Imalys* are compiled into one executable file [xImalys](../../binaries). *xImalys* does not need to be installed. It is sufficient to copy *xImalys* to your *usr/local/bin* directory. You will need administrative rights to copy it. The *usr/local/bin* directory is not included into the package system.

 - download *xImalys* from the *binaries* folder
 - copy *xImalys* to your /usr/local/bin directory

```
sudo cp ~/downloads/xImalys usr/local/bin
```

If you prefer to install the executable files to a subdirectory of your */usr/local/bin* do not forget to extend your environment for the selected path.

Imalys uses the *GDAL library* of the [Open Source Geospatial Foundation](https://www.osgeo.org/) for a lot of processes. This library must be installed under */usr/bin*. For many Linux distributions the GDAL library is installed by default. Alternatively GDAL can be installed from [GitHub](https://github.com/OSGeo/GDAL). If you run QuantumGis the GDAL library is installed in any case.

## Run Imalys

Imalys is designed to translate traits defined for ESIS into standard and well-defined data products. To this end, Imalys contains commands and functions that allow extensive analysis and transformation of image and vector data, as in a GIS system. However, Imalys is not controlled via a user interface but with console commands. 

Imalys was designed to run long process chains without manual intervention and to automatically repeat the results with changing input data, so that large data sets can be processed in manageable sections. This is only possible if the necessary steps are coded. We have endeavored to keep this code very simple and manageable, even though the underlying processes are complex.

*Imalys* must be called using a shell or terminal. To simplify the call, all commands and parameters for an entire process chain are passed by one text file. The call consist of the executable file *xImalys* and the name of the text file (process chain) containing commands and parameters. 

```
~$ xImalys ~/ESIS/Hooks/Munich
```

The example calls *Imalys* with the process chain *Munich*.

The *Imalys* process chain is controlled by 12 commands and their parameters. The commands in the process chain also serve as filename of the results and as field names in tables. Therefore the command names are short and can have a much wider meaning in general usage. 

Each command needs a separate line. Each parameter must be given as a “name = value” pair according to a dictionary. Some parameters run subprocesses others assign values. Everything after the "#" sign is ignored. The following chapters explain each command and parameter and give hints for their application. The background of the processes and dependencies among them is explained at the [background](../background/README.md) section. 

A process chain could look like this:

```
IMALYS [process chain "Munich"]
home
	directory = ~/.imalys
	clear = true
	log = ~/ESIS/Results	
#	––––––––––––––––––––––––––––––––––––––––
#	comment ...
#	––––––––––––––––––––––––––––––––––––––––
compile
	search = ~/ESIS/Takes/LC*_193026_2025*.tif
reduce
	select = compile
	execute = bestof
	retain = bands
export
	select = bestof
	target = ~/ESIS/Results/Munich_2025.tif
```

The example combines all Landsat image tiles "193026" of the year *2025* stored at "ESIS/Takes" into one optimized multispectral image "Munich_2025. The city of Munich is located at the Landsat tile 193026. *Execute = bestof* calculates a result image with the most typical colors for the selected time period (see [reduce](5_Reduce.md)). 

The working directory is set to default (*directory = ~/.imalys*), the directory is emptied at the beginning (*clear = true*), the logs (*log =* …) and the results (*target =* …) are saved at the same place (*~/ESIS/results*).

------

[Top](1_Execute.md)
