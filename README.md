# android-localization-tool

## Description

`to_xlsx`: Find all `strings.xml` files and convert them into an excel file.
`merge_xlsx`: Use a specified `xlsx` file to update the content of all `strings.xml` files.

These two applications have to be run in the directory `src/main/res` of an android project.

## Usage

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

2. Only translatable `string`s will be exported to the `xlsx` file. `strings`s with
   `translatable == "false"` and `string-array`s are not exported.

3. Only translatable `string`s will be updated. `strings`s with `translatable == "false"`
   will not be updated no matter whether there are corresponding entries in the `xlsx` file.

4. Any entry that is not used to update a `strings.xml` file will be appended to the `strings.xml` file.

5. An XML `string` element will be removed if there is a corresponding entry in the `xlsx` file and 
   the content is empty.

6. An XML `string` element will empty content will be exported as an entry with empty content in the `xlsx` file.
   This file will remove the XML `string`  element if used to update `strings.xml` files.
   So, XML `string` elements with empty contents should be voided.


