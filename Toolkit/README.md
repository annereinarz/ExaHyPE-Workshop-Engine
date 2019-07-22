# ExaHyPE Toolkit v2

This is a new and slim Toolkit for ExaHyPE, written in Python and using JSON as the
specification file language.

The need for that was finally raised by memory limitations of SableCC, as discussed
in https://gitlab.lrz.de/exahype/ExaHyPE-Engine/issues/237

To use it:

```
./Toolkit/toolkit.sh MySpecFile.exahype2
```

Or

```
python3 Toolkit/exahype/toolkit MySpecFile.exahype2
```

## Requirements/Dependencies

* Python3
* jsonschema (pip3 install jsonschema OR apt-get install python-json-schema-validator)
* jinja2     (pip3 install jinja2)

Dependencies of jinja2/jsonschema are usually installed automatically when
using pip3. Otherwise, manually install the following python modules

* markupsafe
* attr
* pyrsistent 
* six

## Local Installation of Dependencies

In case you cannot use pip3 to install the required python modules,
`Submodules/updateSubmodules.sh` offers to download them for you.

## Installation of Dependencies on SuperMUC

On SuperMUC, accessing github.com is only possible via SSH tunneling a la

```
ssh -R 19489:github.com:9418 <Your Login>@<SuperMUC Login Node>
```
(Do not change the second port. More details: https://www.lrz.de/services/software/programmierung/git/)

You additionally have to modify the `GITHUB` url in file `Toolkit2/toolkit.sh`.
If you use the same ports as above, you just need to comment in the line
```
GITHUB=git://localhost:19489
```
