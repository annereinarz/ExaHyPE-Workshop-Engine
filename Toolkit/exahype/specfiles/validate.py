# Python3

"""
Validator for JSON specfiles ("specfiles 2.0"), using the a JSON-Schema library

Test it like this:

> testfile = json.load(open("../../examples/EulerFlow-example.exahype2", "r"))
> enriched_with_defaults = validate(testfile)

It will raise an Exception if the file is not valid.
"""

# python-included ("batteries")
import sys, json, pathlib

# add path to dependencies
from .configuration import Configuration
sys.path.insert(1, Configuration.pathToJSONSchema)
sys.path.insert(1, Configuration.pathToAttr)       #jsonschema dependency
sys.path.insert(1, Configuration.pathToPyrsistent) #jsonschema dependency
sys.path.insert(1, Configuration.pathToSix)        #pyrsistent dependency
from jsonschema import Draft4Validator, validators, validate
# https://pypi.org/project/jsonschema/
# Current stable version is 2.6, version 3.0 brings Draft6Validator,
# but that's still alpha.

schema = json.load(pathlib.Path(__file__).parent.joinpath(Configuration.pathToSchema).open("r",encoding="utf-8"))

def extend_with_default(validator_class):
  """
  JSON-Schema Validator which sets the default values.
  This is a bit experimental but in genreal it is good practise to have config
  files which only describe the deviation from reasonable default values.
  
  Code comes from https://python-jsonschema.readthedocs.io/en/latest/faq/#why-doesn-t-my-schema-s-default-property-set-the-default-on-my-instance
  """  
  validate_properties = validator_class.VALIDATORS["properties"]

  def set_defaults(validator, properties, instance, schema):
    for property, subschema in properties.items():
      if "default" in subschema:
        instance.setdefault(property, subschema["default"])

    for error in validate_properties(
      validator, properties, instance, schema,
    ):
      yield error

  return validators.extend(
    validator_class, {"properties" : set_defaults},
  )

SimpleValidator = Draft4Validator # should try to use Draft6Validator if available...
ExtendingValidator = extend_with_default(Draft4Validator)

def get_validator(set_defaults=True):
  validator = ExtendingValidator if set_defaults else SimpleValidator
  return validator(schema)

def validate(python_structure, set_defaults=True):
  """
  Validate a nested dict/list structure. If you want to validate a JSON file,
  load it with json.load() first.
  """
  # python_structure is modified by reference, unfortunately. Should probably
  # do a deep copy before.
  get_validator(set_defaults).validate(python_structure)
  
  # mutually exclusives terms tests
  if "solvers" in python_structure:
    for solver in python_structure["solvers"]:
      pass
        # if "aderdg_kernel" in solver:
        #    if "flux" in solver["aderdg_kernel"]["terms"] and "viscous_flux" in solver["aderdg_kernel"]["terms"]:
        #        raise ValueError("flux and viscous-flux are mutually exclusive")
        # if "fv_kernel" in solver:
        #    if "flux" in solver["fv_kernel"]["terms"] and "viscous_flux" in solver["fv_kernel"]["terms"]:
        #         raise ValueError("flux and viscous-flux are mutually exclusive")
  
  
  return python_structure
