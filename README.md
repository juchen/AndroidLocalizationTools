# android-localization-tool

## Description

* `to_xlsx`: Find all `strings.xml` files and convert them into an excel file.
* `merge_xlsx`: Use a specified `xlsx` file to update the content of all `strings.xml` files.

These two applications have to be run in the directory `src/main/res` of an android project.

## Installation

The project is supposed to be built in
[Stack](https://docs.haskellstack.org/en/stable/README/ "The Haskell Tool
Stack") the haskell package tool.

Stack can be installed simply by running the command

```
$ curl -sSL https://get.haskellstack.org/ | sh
```

For more details, refer to the [official installation
guide](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Once you have the Stack installed. Clone this project and then just

```
$ cd android-localization-tool
$ stack build
```

to build. If the build is a success, find the executables in

```
.stack-work/install
```

Or, you can install the executables to your local bin path by

```
$ stack install
```


## Usage

Remember to change directory to `your-project/src/main/res` before running the commands

```
Usage: merge_xlsx <xlsx file>

	Update all strings.xml file according to the specified xlsx file.
	Need to be run in the src/main/res/ directory.
```

And

```
Usage: to_xlsx <xlsx file>

	Collect strings.xml files and generate an excel file.
	Has to be run in the src/main/res/ directory.
```

## Caution

1. All the cells in the `xlsx` file must have their format being "plain text",
   otherwise, the tools won't recognize them and will treat them as "Nothing".

2. Only translatable `<string>`s will be exported to the `xlsx` file. `<string>`s with
   `translatable == "false"` and `<string-array>`s are not exported.

3. Only translatable `<string>`s will be updated. `<string>`s with `translatable == "false"` and `<string-array>`
   will not be updated no matter whether there are corresponding entries in the `xlsx` file.

4. Any entry that is not used to update a `strings.xml` file will be appended to the `strings.xml` file.

5. An XML `<string>` element will be removed if there is a corresponding entry in the `xlsx` file and 
   the content is empty, except for the default `values/strings.xml`, in which a `<string>` element with empty
   content is kept.



