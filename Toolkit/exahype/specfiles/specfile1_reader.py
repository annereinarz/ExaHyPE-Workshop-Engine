#!/usr/bin/env python3

import sys
import configparser
import re
import json
import collections
import logging

class SpecFile1ParserError(RuntimeError): 
    def __init__(self,message):
        super().__init__(message)

##
# A reader for the original ExaHyPE specification file format
# 
# Sample usage:
#```
# r = SpecFile1Reader()
# my_json_obj = r.read()
# my_json_file_content = json.dumps(my_json_ob)
#```
class SpecFile1Reader():
    def __init__(self, log=None):
        """
        Initialize with some logging target. If none given, will setup an own one
        """
        if not log:
            logging.basicConfig(format="%(filename)s:%(lineno)s(%(funcName)s):%(levelname)s %(message)s")
            log = logging.getLogger(__name__)
            #log.addHandler(logging.StreamHandler()) # to stderr
            log.setLevel(logging.INFO)
        self.log = log

    ##
    # Converts the original ExaHyPE spec file into an ini file.
    #
    # In the process, it performs the following changes
    # * group 'exahype-project'            -> 'project'
    # * group 'global-optimisations' -> 'optimisations'
    # * removes all 'const' tokens
    # * replaces dashes (-) in group names and keys of options by underscore (_)
    #
    # @note the SableCC parser for the original spec file did ignore whitespaces.
    #             We are thus free to place any whitespaces where it makes our work easier.
    # 
    # @return tuple containing the spec file converted to INI format (str),
    #                    the number of solvers (int) and the number of plotters per solver (list of int)
    def spec_file_1_to_ini(self,spec_file_1):
        solver=0
        plotter=[]
        plotter.append(0)
        spec_file_1_ini = ""
        reads_multiline_comment = 0
        reads_singline_comment  = False
        in_solver = False
        for line in spec_file_1:
            reads_singline_comment = line.strip().startswith("//") or line.strip().startswith("#")
            reads_multiline_comment += 1 if line.strip().startswith("/*") else 0
            if reads_multiline_comment==0 and not reads_singline_comment:
                m_project = re.match(r"^\s*exahype-project\s+(\w+)",line)
                m_group   = re.match(r"^\s*(computational-domain|shared-memory|distributed-memory|(global-)?optimisation|profiling)\s*(//.+)?$",line)
                m_solver  = re.match(r"^\s*solver\s+([^\s]+)\s+(\w+)",line)
                m_plotter = re.match(r"^\s*plot\s+([^\s]+)(\s+(\w+))?",line)
                if m_project:
                    spec_file_1_ini += "[project]\n"
                    spec_file_1_ini += "project_name="+m_project.group(1)+"\n"
                elif m_group and not in_solver:
                    spec_file_1_ini += "[%s]" % m_group.group(1).replace("-","_").replace("global_","")+"\n"
                elif m_solver:
                    in_solver = True
                    spec_file_1_ini += "[solver%d]" % (solver)    +"\n"
                    spec_file_1_ini += "solver_type="+m_solver.group(1)+"\n" # will be replaced later on
                    spec_file_1_ini += "name="+m_solver.group(2)+"\n"
                elif re.match(r"^\s*end\s+solver",line):
                    in_solver = False
                    solver+=1
                    plotter.append(0)
                elif m_plotter:
                    spec_file_1_ini += "[solver%dplotter%d]\n" % (solver,plotter[solver])
                    spec_file_1_ini += "type="+m_plotter.group(1)+"\n"
                    if m_plotter.group(2) is None:
                        raise SpecFile1ParserError("Could not infer plotter name from tokens '%s'. Is it missing?" % line.strip())
                    spec_file_1_ini += "name="+m_plotter.group(2)+"\n"
                    plotter[solver]+=1
                elif line.strip().startswith("end "):
                    pass
                else:
                    m_option = re.match(r"\s*((\w|-)+)\s*(const)?\s*=(.+)",line)
                    if m_option:
                        option = m_option.group(1)
                        spec_file_1_ini += option.replace("-","_")+"="+m_option.group(4).strip()+"\n"
                        if option=="kernel":
                            raise SpecFile1ParserError("Found legacy option 'kernel const'. Please split it into ['type const','terms const','optimisation const']. Further ensure")
                        if option=="limiter-kernel":
                            raise SpecFile1ParserError("Found legacy option 'kernel const'. Please split it into ['limiter-type const','limiter-terms const','limiter-optimisation const']")
                    else: # multiline options need to be indented
                        spec_file_1_ini += "    "+line.strip()+"\n"
            reads_multiline_comment -= 1 if line.strip().endswith("*/") else 0 
        return (spec_file_1_ini, solver, plotter[0:-1])
    
    ##
    # Tries to convert certain options to the expected type.
    # Returns string if it fails
    def convert(self,option,value):
        integers=[\
            "dimension",\
            "time_steps",\
            "buffer_size",\
            "timeout",\
            "node_pool_timeout",
            "max_forks_at_once",\
            "cores",\
            "measure_cell_processing_times_iter",\
            "outside_cells",\
            "outside_cells_left",\
            "order",\
            "patch_size",\
            "halo_cells",\
            "maximum_mesh_depth",\
            "dmp_observables",\
            "steps_till_cured",\
            "helper_layers",\
            "thread_stack_size",\
            "scale_bounding_box_multiplier",\
            "max_mesh_setup_iterations"\
        ]
        numbers=[\
            "end_time",\
            "maximum_mesh_size",\
            "time",\
            "repeat",\
            "fuse_algorithmic_steps_rerun_factor",\
            "fuse_algorithmic_steps_diffusion_factor",\
            "time_step_batch_factor",\
            "double_compression",\
            "dmp_relaxation_parameter",\
            "dmp_difference_scaling"\
        ]
        try:
            if option in integers:
                return int(value)
            elif option in numbers:
                return float(value)
            else:
                return value 
        except:
            return value # let the JSON schema validation deal with the error handling

    ## 
    # Convert ini file as created by method `spec_file_1_to_ini` to nested list of dict - dist of list structure.
    def convert_to_dict(self,config,n_solvers,n_plotters):
        # root node
        root = "project"
        context = collections.OrderedDict()
        for option in config.options(root):
            context[option.replace("-","_")] = config.get(root, option)
        # other sections
        for section in config.sections():
            if not section.startswith("solver") and not section==root:
                context[section] = collections.OrderedDict()
                for option in config.options(section):
                        context[section][option.replace("-","_")] = self.convert(option, config.get(section, option))
        # solvers
        context["solvers"]=[]
        for i in range(0,n_solvers):
            solver="solver%d" % i
            context["solvers"].append(collections.OrderedDict())
            for option in config.options(solver):
                context["solvers"][i][option.replace("-","_")] = self.convert(option, config.get(solver,option))
            # plotters
            context["solvers"][i]["plotters"]=[]
            for j in range(0,n_plotters[i]):
                plotter="solver%dplotter%d" % (i,j)
                context["solvers"][i]["plotters"].append(collections.OrderedDict())
                for option in config.options(plotter):
                    context["solvers"][i]["plotters"][j][option] = self.convert(option, config.get(plotter,option))
        return context
    
    ##
    # TODO
    def map_computational_domain(self,domain):
        for option in ["offset","width"]:
            token = domain.pop(option)
            domain[option]=[]
            for value in token.split(","):
                domain[option].append(float(value))
    ##
    # TODO
    def map_distributed_memory(self,distributed_memory):
        distributed_memory["load_balancing_type"] = distributed_memory.pop("identifier").replace("_load_balancing","")
        distributed_memory["buffer_size"]         = distributed_memory.pop("buffer_size")
        # configure
        if "configure" in distributed_memory:
            configure = distributed_memory.pop("configure").replace("{","").replace("}","")
            required = ["ranks-per-node"]
            required_found = [] 
            for token in configure.split(","):
                token_s = token.strip()
                m_ranks_per_node          = re.match(r"ranks-per-node:([0-9]+)",token_s) # '-' since original values; only keys have been modified
                m_primary_ranks_per_node  = re.match(r"primary-ranks-per-node:([0-9]+)",token_s)
                m_node_pool_strategy      = re.match(r"(fair|FCFS|sfc-diffusion)",token_s)
                m_load_balancing_strategy = re.match(r"(hotspot|greedy-naive|greedy-regular)",token_s)
                
                found_token = False
                if token_s=="virtually-expand-domain":
                    distributed_memory["scale_bounding_box"] = True # might be better placed into the optimisation section
                    found_token = True
                if m_ranks_per_node:
                    distributed_memory["ranks_per_node"] = int(m_ranks_per_node.group(1))
                    found_token = True
                    required_found.append("ranks-per-node")
                if m_primary_ranks_per_node:
                    distributed_memory["primary_ranks_per_node"]=int(m_ranks_per_node.group(1))
                    found_token = True
                if m_node_pool_strategy:
                    distributed_memory["node_pool_strategy"]=m_node_pool_strategy.group(1).replace("-","_")
                    found_token = True
                if m_load_balancing_strategy:
                    distributed_memory["load_balancing_strategy"]=m_load_balancing_strategy.group(1).replace("-","_")
                    found_token = True
                if not found_token and len(token_s):
                    raise SpecFile1ParserError("Could not map value '%s' extracted from option 'distributed-memory/configure'. Is it spelt correctly?" % token_s)
        for param in required:
            if param not in required_found:
                raise SpecFile1ParserError("Could not find required parameter '{}:<number>' in 'distributed-memory/configure' section.".format(param))
                 
    ##
    # TODO
    def map_shared_memory(self,shared_memory):
        shared_memory["autotuning_strategy"]=shared_memory.pop("identifier")
        # configure
        if "configure" in shared_memory:
            configure = shared_memory.pop("configure").replace("{","").replace("}","")
            for token in configure.split(","):
                token_s = token.strip()
                found_token = False
                m_background_job_consumers = re.match(r"background-tasks:([0-9]+)",token_s)
                if m_background_job_consumers:
                    shared_memory["background_job_consumers"] = int(m_background_job_consumers.group(1))
                    found_token = True
                if re.search(r"manual-pinning",configure)!=None:
                    shared_memory["manual_pinning"] = True
                    found_token = True
                if token_s in ["no-invade", "analyse-optimal-static-distribution-but-do-not-invade", "occupy-all-cores", "invade-between-time-steps", "invade-throughout-computation", "invade-at-time-step-startup-plus-throughout-computation"]:
                    shared_memory["invasion_strategy"] = token_s.replace("-","_")
                    found_token = True
                if not found_token and len(token_s):
                    raise SpecFile1ParserError("Could not map value '%s' extracted from option 'shared-memory/configure'. Is it spelt correctly?" % token_s)
    ##
    # Converts the kernel terms of a solver entry in the original spec file
    #
    # @return tuple consisting of parsed context and the number of point sources
    def map_kernel_terms(self,kernel_terms):
        n_point_sources = 0
        context = []
        for token in kernel_terms.split(","):
            token_s         = token.strip()
            found_token = False
            for term in ["flux","source","ncp","pointsources","materialparameters","viscousflux"]:
                if token_s.startswith(term):
                    context.append(term.\
                            replace("pointsources",      "point_sources").\
                            replace("viscousflux",       "viscous_flux").\
                            replace("materialparameters","material_parameters"))
                    if term=="pointsources":
                        try:
                            n_point_sources = int(token_s.split(":")[-1])
                            found_token = True
                        except:
                            raise SpecFile1ParserError("Number of point sources could not be parsed in original ExaHyPE specification file (is: '%s'. expected: 'pointsources':<int>)!" % token_s)
                    elif token_s==term:
                        found_token = True
            if not found_token:
                raise SpecFile1ParserError("Could not map value '%s' extracted from option 'terms' (or 'limiter-terms'). Is it spelt correctly?" % token_s)
            
        return (context,n_point_sources)
    
    ##
    # Converts the "optimisation" string of a ADER-DG and Limiting-ADERDG solver found 
    # in the original spec file
    def map_aderdg_kernel_opts(self,kernel_opts):
        context = collections.OrderedDict()
        stp     = "space_time_predictor"
        opt     = "optimised_terms"
        opt_dbg = "optimised_kernel_debugging"
        context[stp]=collections.OrderedDict()
        context[opt]=[]
        context[opt_dbg]=[]
        for token in kernel_opts.split(","):
            token_s = token.strip() 
            found_token=False
            for term in ["generic","optimised","user"]:
                if token_s==term:
                    context["implementation"]=term
                    found_token=True
            if token_s=="patchwiseadjust":
                context["adjust_solution"]="patchwise"
                found_token=True
            if token_s=="usestack":
                context["allocate_temporary_arrays"]="stack"
                found_token=True
            for term in ["fusedsource","fluxvect","fusedsourcevect","ncpvect","materialparametersvect"]:
                if token_s==term:
                    context[opt].append(term.\
                            replace("fluxvect",               "flux_vect").\
                            replace("fusedsourcevect",        "fusedsource_vect").\
                            replace("materialparametersvect", "material_parameters_vect").\
                            replace("ncpvect",                "ncp_vect"))
                    found_token=True
            for term in ["converter","flops"]:
                if token_s==term:
                    context[opt_dbg].append(term)
                    found_token=True
            for term in ["cerkguess","notimeavg","maxpicarditer","split_ck"]:
                if token_s.startswith(term):
                    mappedTerm = term.replace("maxpicarditer","fix_picard_iterations")
                    context[stp][mappedTerm]=True
                    found_token=True
            if not found_token:
                raise SpecFile1ParserError("Could not map value '%s' extracted from option 'optimisation'. Is it spelt correctly?" % token_s)
        return context
        
    ##
    # Converts the "optimisation" string of a Finite-Volumes and the "limiter-optimisation" string of Limiting-ADERDG solver 
    # found in the original spec file
    def map_fv_kernel_opts(self,kernel_opts):
        context = collections.OrderedDict()
        for token in kernel_opts.split(","):
            token_s = token.strip()
            found_token = False
            if token_s in ["generic","optimised","user"]:
                context["implementation"]=token_s
                found_token = True
            if token_s=="patchwiseadjust":
                context["adjust_solution"]="patchwise"
                found_token = True
            if token_s=="usestack":
                context["allocate_temporary_arrays"]="stack"
                found_token = True
            if not found_token:
                raise SpecFile1ParserError("Could not map value '%s' extracted from option 'optimisation' (or 'limiter-optimisation'). Is it spelt correctly?" % token_s)
        return context
    
    ##
    # TODO
    def map_variables(self,variables):
        if re.match("\s*([0-9]+)\s*",variables):
            return int(variables.strip())
        else:
            result=[]
            for token in variables.split(","):
                token_s=token.strip()
                m = re.match("([^\s,]+)\s*:\s*([^\s,]+)",token_s)
                if m:
                    multiplicity = None
                    try:
                        multiplicity = int(m.group(2)) 
                    except:
                        multiplicity = m.group(2)
                        
                    result.append(collections.OrderedDict([ ( "name",m.group(1) ), ( "multiplicity", multiplicity ) ])) 
                else:
                    raise SpecFile1ParserError("variables|parameters|global_observables: Token '%s' does not have structure '<string>:<integer>'." % token_s)
            if result:
                return result
            else:
                return [variables]
    
    ##
    # TODO
    def map_constants(self,constants):
        result = collections.OrderedDict()
        for token in constants.split(","):
            token_s=token.strip()
            m = re.match("([^\s,]+)\s*:\s*([^\s,]+)",token_s)
            if m:
              try:
                  result[m.group(1)]=int(m.group(2))
              except:
                  try:
                      result[m.group(1)]=float(m.group(2))
                  except:
                      if re.match(r"^(True|Yes|On)$", m.group(2)):
                          result[m.group(1)] = True
                      elif re.match(r"^(False|No|Off)$", m.group(2)):
                          result[m.group(1)] = False
                      else: # just store the string
                          result[m.group(1)]=m.group(2)
            else:
                raise SpecFile1ParserError("constants|select: Token '%s' does not have structure '<string>:<something>'." % token_s)
        if result:
            return result
        else:
            return [constants]
        
    ##
    # Post processes result of `convert_to_dict`, i.e. 
    # changes the structure of the dict.
    #
    # This allows it to be passed to the jinja2 schema.
    #
    # @param context the specfile as dict
    # 
    def map_options(self,context):
        # paths, optimisation, distributed_memory, shared_memory
        context["paths"]=collections.OrderedDict()
        context.move_to_end("paths",last=False)        # put on top
        context.move_to_end("project_name",last=False) # put on top
        for option in list(context.keys()):
            if option in ["log_file","peano_kernel_path","peano_toolbox_path","exahype_path","output_directory","plotter_subdirectory"]:
                context["paths"][option] = context.pop(option)
        self.map_computational_domain(context["computational_domain"])
        for section in ["optimisation","profiling","distributed_memory","computational_domain"]:
            if section in context:
                for option in context[section]:
                    if context[section][option] in ["on","off"]:
                        context[section][option]=False if context[section][option]=="off" else True
        if "distributed_memory" in context:
            self.map_distributed_memory(context["distributed_memory"])    
        if "shared_memory" in context:
            self.map_shared_memory(context["shared_memory"])
        # solvers
        for i,solver in enumerate(context["solvers"]):
            # type
            old_type = solver["type"]
            solver["type"]=solver.pop("solver_type")
            solver.move_to_end("type",last=False)    # put on top
            
            # kernels
            n_point_sources = 0
            aderdg_kernel_type, aderdg_kernel_terms, aderdg_kernel_opts    = "", "", ""
            fv_kernel_type, fv_kernel_terms, fv_kernel_opts    = "", "", ""
            # aderdg 
            if solver["type"] in ["Limiting-ADER-DG","ADER-DG"]:
                solver["aderdg_kernel"]=collections.OrderedDict()
                if "language" in solver:
                    solver["aderdg_kernel"]["language"]=solver.pop("language")
                if "type" in solver:
                    aderdg_kernel_type  = old_type
                if "terms" in solver:
                    aderdg_kernel_terms = solver.pop("terms")
                if "optimisation" in solver:
                    aderdg_kernel_opts    = solver.pop("optimisation")
                    # type
                for token in aderdg_kernel_type.split(","):
                    token_s = token.strip() 
                    if token_s in ["linear","nonlinear"]:
                        solver["aderdg_kernel"]["nonlinear"]=token_s=="nonlinear"
                    if token_s in ["Legendre","Lobatto"]:
                        solver["aderdg_kernel"]["basis"]=token_s
                # terms
                result, n_point_sources = self.map_kernel_terms(aderdg_kernel_terms)
                solver["aderdg_kernel"]["terms"]=result
                solver["point_sources"]=n_point_sources
                # opts
                solver["aderdg_kernel"].update(self.map_aderdg_kernel_opts(aderdg_kernel_opts))

            
            # limiter
            if solver["type"]=="Limiting-ADER-DG":
                solver["limiter"]=collections.OrderedDict()
                for option in [ "dmp_observables", "dmp_relaxation_parameter", "dmp_difference_scaling", "helper_layers", "steps_till_cured" ]:
                    if option in solver:
                        solver["limiter"][option] = solver.pop(option)
                if "implementation" in solver["aderdg_kernel"]:
                    solver["limiter"]["implementation"]=solver["aderdg_kernel"]["implementation"]
                
                solver["fv_kernel"]=collections.OrderedDict()
                if "limiter_language" in solver:
                    solver["fv_kernel"]["language"]=solver.pop("limiter_language")
                if "limiter_type" in solver:
                    fv_kernel_type    = solver.pop("limiter_type") 
                if "terms" in solver["aderdg_kernel"]:
                    solver["fv_kernel"]["terms"]=solver["aderdg_kernel"]["terms"] # copy ADER-DG terms
                if "limiter_optimisation" in solver:
                    fv_kernel_opts    = solver.pop("limiter_optimisation")
                if "limiter_slope_limiter" in solver:
                    solver["fv_kernel"]["slope_limiter"]=solver.pop("limiter_slope_limiter")

            
            # fv
            if solver["type"]=="Finite-Volumes":
                solver["fv_kernel"]=collections.OrderedDict()
                if "language" in solver:
                    solver["fv_kernel"]["language"]=solver.pop("language")
                if "type" in solver:
                    fv_kernel_type    = old_type
                if "terms" in solver:
                    fv_kernel_terms = solver.pop("terms")
                if "slope_limiter" in solver:
                    solver["fv_kernel"]["slope_limiter"]=solver.pop("slope_limiter")
                # fv terms
                result, n_point_sources = self.map_kernel_terms(fv_kernel_terms)
                solver["fv_kernel"]["terms"]=result
                if "optimisation" in solver:
                    fv_kernel_opts    = solver.pop("optimisation")
           
            if solver["type"]=="Limiting-ADER-DG" or\
               solver["type"]=="Finite-Volumes":
                # fv type
                fv_schemes = ["godunov","musclhancock","robustmusclhancock"]
                for token in fv_kernel_type.split(","):
                    token_s = token.strip() 
                    if token_s in fv_schemes:
                        solver["fv_kernel"]["scheme"]=token_s
                    else:
                        raise SpecFile1ParserError("FV type must be one one of: '{}'. It is '{}'.".format(", ".join(fv_schemes),token_s))
                # fv opts
                if "fv_kernel" in solver: 
                    solver["fv_kernel"].update(self.map_fv_kernel_opts(fv_kernel_opts))
            
            # variables, parameters, and more
            solver["variables"]=self.map_variables(solver.pop("variables"))
            if "parameters" in solver:
                solver["material_parameters"]=self.map_variables(solver.pop("parameters"))
            if "global_observables" in solver:
                solver["global_observables"]=self.map_variables(solver.pop("global_observables"))
            if "constants" in solver:
                solver["parameters"]=self.map_constants(solver.pop("constants"))
            
            # plotters
            solver["plotters"] = solver.pop("plotters") # put at end
            for j,plotter in enumerate(solver["plotters"]):
                plotter["variables"]=self.map_variables(plotter.pop("variables"))
                if "select" in plotter:
                    plotter["select"]=self.map_constants(plotter.pop("select"))
        
        return context
    
    ##
    # @return dictionary storing spec file 1 content expressed
    # in terms of a spec file 2 JSON object
    #
    # Sample usage:
    #```
    # r = SpecFile1Reader()
    # my_json_obj = r.read()
    # my_json_file_content = json.dumps(my_json_ob)
    # ```
    def read(self,file_handle):
        spec_file_1 = file_handle.readlines()
        return self.read_lines(spec_file_1)

    def read_string(self, document_as_string):
        return self.read_lines(document_as_string.split("\n"))
    
    def read_lines(self, spec_file_1):
        """
        spec_file_1: A list of strings (one line per list element)
        """
        self.log.info("Converting legacy specification file (%d lines) to INI file... " % len(spec_file_1))
        (spec_file_1_ini, n_solvers, n_plotters) = self.spec_file_1_to_ini(spec_file_1)
        self.log.info("OK")
        self.log.debug("INI file is:\n" + str(spec_file_1_ini))
       
        self.log.debug("Number of solvers found in legacy specification file: {}\n".format(n_solvers))
 
        config = configparser.ConfigParser(delimiters=('='))
        try:
            config.read_string(spec_file_1_ini)
        except configparser.Error as e:
            raise SpecFile1ParserError(e)

        self.log.info("Mapping INI structure to JSON input object... ")
        result=self.map_options(self.convert_to_dict(config,n_solvers,n_plotters))
        self.log.info("OK")
        self.log.debug("JSON input object is: \n" + json.dumps(result, indent=2, sort_keys=True))
        return result 

    @classmethod
    def cli(cls, parser=None):
        """
        A super light command line frontend using argparse
        """
        import argparse, json
        if not parser:
            parser = argparse.ArgumentParser(
                description="A lightweight for the old original ExaHyPE specification file format to the new JSON format",
                epilog="See http://www.exahype.eu and the Guidebook for more help."
            )

        parser.add_argument("-v", "--verbose", action="store_true", help="Show all info messages and stack traces")
        parser.add_argument("-c", "--compact", action="store_false", help="Print output compact in a single line (default is pretty print)")

        parser.add_argument('specfile', nargs="?", type=argparse.FileType('r'), default=sys.stdin, help="The specification file to read in (*.exahype1), default stdin")
        parser.add_argument('jsonfile', nargs="?", type=argparse.FileType('w'), default=sys.stdout, help="The output file to write to (*.exahype2), default stdout")

        args = parser.parse_args()

        logging.basicConfig(format="%(filename)s:%(levelname)s %(message)s")
        log = logging.getLogger(cls.__name__)
        #log.addHandler(logging.StreamHandler()) # to stderr

        if args.verbose:
            log.setLevel(logging.DEBUG)

        reader = cls(log)

        try:
            json_specfile = reader.read(args.specfile)
        except SpecFile1ParserError as e:
            if args.verbose:
                log.exception(e)
            else:
                log.fatal(str(e))
            sys.exit(-1)

        json_specfile_as_string = json.dumps(
            json_specfile,
            sort_keys = True,
            indent = 4 if args.compact else None
        )

        print(json_specfile_as_string, file=args.jsonfile)


if __name__ == '__main__':
    SpecFile1Reader().cli()
