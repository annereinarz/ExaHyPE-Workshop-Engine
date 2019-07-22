import fnmatch
import os

from collections import OrderedDict

class BadSpecificationFile(Exception):
    pass

class ToolkitHelper:

    @staticmethod
    def find(parent_directory,file_glob):
        """
        Searches `parent_directory` recursively for files
        matching `file_glob`.

        Returns a list of paths to files 
        matching `file_glob`

        NOTE: Function works for Python 2.2 and above.
        Starting with Python 3.5, more elegant solutions are available.
        """
        matches = []
        for root, dirnames, filenames in os.walk(parent_directory):
            for filename in fnmatch.filter(filenames, file_glob):
                matches.append(os.path.join(parent_directory, filename))
        return matches


    @staticmethod
    def parse_variables(solver,field):
        """
        Parse the 'variables' parameter and similarly
        structured parameters ('material_parameters','global_observables')
        """
        if field in solver:
            if type(solver[field]) is int:
                return [OrderedDict([("name", "Q"), ("multiplicity", solver[field]), ("offset", 0)])]
            else: # is list
                offset = 0
                listOfVariables = []
                for item in solver[field]:
                    if type(item) is str:
                        listOfVariables.append(OrderedDict([("name", item), ("multiplicity", 1), ("offset", offset)]))
                        offset += 1
                    else:
                        multiplicity = item["multiplicity"]
                        listOfVariables.append(OrderedDict([("name", item["name"]), ("multiplicity", multiplicity), ("offset", offset)]))
                        offset += multiplicity
                return listOfVariables
        else:
            return [OrderedDict([("name", "Q"), ("multiplicity", 0), ("offset", 0)])]


    @staticmethod
    def variables_to_str(solver,field):
        """
        Represent a variables field as string.
        """
        if field in solver:
            if type(solver[field]) is int:
                return str(solver[field])
            else:
                return ", ".join([item["name"]+" : "+str(item["multiplicity"]) for item in solver[field]])
        else:
            return "0"


    @staticmethod
    def count_variables(variables_as_list):
        """
        Sum up the multiplicities.
        """
        number = 0;
        for variable in variables_as_list:
            number += variable["multiplicity"]
        return number
