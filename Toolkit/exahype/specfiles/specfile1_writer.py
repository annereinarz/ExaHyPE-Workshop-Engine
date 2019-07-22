#!/usr/bin/env python3
#

"""
A writer for the original ExaHyPE specification file.

The code is extracted and modified from the mexa code.

--> nothing more has yet beend one
"""

# add path to dependencies
from .configuration import Configuration
sys.path.insert(1, Configuration.pathToJinja2)
sys.path.insert(1, Configuration.pathToMarkupsafe)
import jinja2

class specfile1_writer:
	"""
	Generates an ExaHyPE specification file. Based on the queried root, the input
	has to follow a shape defined by the used jinja template file.
	"""
	
	default_root = 'exahype' # the root container in a mexa file where the hierarchy begins
	
	# Inclusion of solver constants (aka simulation parameters) and plotter select statements
	# (aka plotter configuration) into ExaHyPE specfiles:
	#  - embedded: Failsafe, encodes a whole mexafile in a discrete and easily embeddable string
	#  - referenced: Reference to an external mexa file (something we don't want to do in practise)
	#  - adapted: Try to generate redable specfile constants. Reparsing can be inaccurate, i.e.
	#    strings are not correctly escaped.
	parameter_styles = ['embedded', 'adapted', 'referenced']
	default_parameter_style = 'embedded'
	
	# the internal backref key used
	backref_key = "$path"
	
	@classmethod
	def arguments(cls, parser):
		io_mexafile.arguments(parser, add_root=False)
		parser.add_argument('--root', default=cls.default_root, help='Root container in the mexa file where the hierarchy begins')
		parser.add_argument('--debug', action='store_true', default=False, help='Print debug output')
		parser.add_argument('--parameter-style', choices=cls.parameter_styles, default=cls.default_parameter_style, help='Method to note constants/simulation parameters in specfile')

	def __init__(self, root, debug, parameter_style, *args, **kwargs):
		super(mexafile_to_exahype_specfile,self).__init__(*args, **kwargs)
		self.root = root
		self.debug = debug
		self.parameter_style = parameter_style
		self.tplfile = './exa-specfile-tpl.exahype'
		#self.tplfile = './debug-project.jinja'
		self.exahype_base = self.mf.query(root)
		self.native = self.exahype_base.tree(backref=True, backref_key=self.backref_key)
		self.write(self.exaspecfile(mf_tree=self.native))
	
	def encode_parameters(self, path):
		ret = OrderedDict() # is a k:v list in any case
		
		join_multiline = False # allow the parameter string to cover multiple lines
		join_kv_tpl = "%s:%s"  # how to merge key and value
		
		
		ret['mexa/ref'] = path
		ret['mexa/style'] = self.parameter_style
		if self.parameter_style == 'referenced':
			ret['mexa/filename'] = 'PUT-MEXA-FILENAME-HERE' # todo
		elif self.parameter_style == 'embedded':
			# embed the configuration as a string, suitable for the specfile constants
			# parameters
			mf = self.mf.query(path).remove_prefix(path)
			encoding = 'quotedprintable'
			ret['mexa/encoding'] = encoding
			ret['mexa/content'] = encoded_mexafile.dump(mf, encoding=encoding)
		elif self.parameter_style == 'adapted':
			# adapt the simple mexa file content to the specfile config tokens.
			mf = self.mf.query(path).remove_prefix(path)
			
			# Strings must be treated with special care. They must not whitespace other "weird"
			# characters. Therefore, they are encoded.
			encoding = 'quotedprintable'
			encode_str = encoded_mexafile.relaxed_quoted_printable
			ret['mexa/encoding'] = encoding
			join_multiline = True
			join_kv_tpl = "%s: %s" # Allow some whitespace
			
			# in the ExaHyPE toolkit grammar, certain identifiers and RHS values generate problems
			# if they are in the token list, i.e. they are reserved expressions in the language.
			# We can extract these reserved tokens from the SableCC grammar for the specfile:
			#   cat exahype.grammar  | grep -E "^\s*token_" | grep "=" | cut -d'=' -f2 | tr -d "'; " | paste -d" " -s
			reserved_tokens = "end exahype-project const peano-kernel-path peano-toolbox-path exahype-path output-directory architecture log-file computational-domain dimension width offset maximum-mesh-size maximum-mesh-depth time-stepping end-time solver ADER-DG Finite-Volumes Limiting-ADER-DG variables parameters naming-scheme constants order patch-size kernel type terms optimisation kernel limiter-type limiter-optimisation language limiter-language dmp-observables dmp-relaxation-parameter dmp-difference-scaling steps-till-cured helper-layers plot variable time repeat output select shared-memory cores properties-file identifier distributed-memory configure buffer-size timeout master-worker-communication neighbour-communication global-optimisation fuse-algorithmic-steps fuse-algorithmic-steps-factor timestep-batch-factor skip-reduction-in-batched-time-steps disable-amr-if-grid-has-been-stationary-in-previous-iteration double-compression spawn-double-compression-as-background-thread profiling profiler metrics deep-profiling profiling-output likwid_inc likwid_lib ipcm_inc ipcm_lib"
			reserved_token_list = reserved_tokens.split()

				
			for l,r,op,src in mf:
				assert op=='equals'
				assert not isa(r,symbol), "Expect symbols to be removed"
				assert not isa(r,lang.evalstr), "Expect RHS strings to be evaluated."
				
				if isa(r, [str,unicode]):
					re = encode_str(r)  # in the specfile language, strings are not enclosed in " or '
				elif isa(r, bool):
					re = 'on' if r else 'off'
				else:
					re = str(r) # hope that this are only numbers.
				
				identifier = l.canonical()
				
				# at the moment, warn only. Proper escaping is subject to future extension of this code.
				# TODO: Shouldn't these check wether "in reserved_token_list" instead of "in reserved_tokens"?
				if identifier in reserved_tokens:
					warn("Warning: Key '%s' is a reserved token in the ExaHyPE toolkit. Will probably generate problems. You should probably switch to parameter_style=embedded" % identifier)
				if re in reserved_tokens:
					warn("Warning: Value '%s' (at key %s) is a reserved token in the ExaHyPE toolkit. Will probably generate problems. You should probably switch to parameter_style=embedded" % (re,identifier))
				
				ret[identifier] = re
		else:
			raise ValueError("Parameter style not understood: %s" % self.parameter_style)
		
		# join ret to a specfile config string
		joinstr = ",\n" if join_multiline else ","
		return joinstr.join([ "%s:%s" % (k,v) for k,v in ret.iteritems() ])
	
	def exaspecfile(self, mf_tree):
		"Return an ExaHyPE specfile which corresponds to these data"
	
		# assuming we only have one exahype project in ctx
		ctx = {'exahype_projects': mf_tree }
		
		# replace True and False by "on" and "off"
		exaBool = { True: "on", False: "off" }
		replBool = lambda item: exaBool[item] if isa(item,bool) else item
		ctx = mapComplex(replBool, ctx)
		
		# for debugging:
		if self.debug:
			pprint.pprint(ctx)
		
		mexa_path = self.mf.resolve_symbol("mexa")
		
		
		jinja_env = jinja2.Environment(
			loader=jinja2.FileSystemLoader(mexa_path),
			#undefined=jinja2.StrictUndefined
			undefined=(jinja2.DebugUndefined if self.debug  else jinja2.StrictUndefined)
		)
		
		# provide further jinja functions:
		# w decorators: https://stackoverflow.com/a/47291097
		
		def jinja_filter(func):
			jinja_env.filters[func.__name__] = func
			return func
		
		@jinja_filter
		def tolist(adict):
			"""
			Mexa always stores dict. To treat a dictionary as a list, use this. It
			It basically works like the .values() call. An alternative is to
			loop like "for k,v in thedict" in jinja. The nice feature of this
			function is that it removes the backrefs.
			"""
			# former:
			## return adict.values()
			# instead, filter out the $path backref
			return [ adict[k] for k in sorted(adict.keys()) if k != self.backref_key ]
		
		@jinja_filter
		def asfloat(something):
			"""
			Returns a scalar or vector thing casted as a float string. This is sometimes
			important when ExaHyPE expects it to be clearly defined as a float.
			"""
			if isa(something, int):
				return float(something)
			elif isa(something, list):
				return map(float, something)
			else:
				raise ValueError("The asfloat filter can only be used on scalar and vector values. '%s' given." % something)
		
		@jinja_filter
		def dimlist(comp_domain, field):
			"Compute the ExaHypE computational domain string (width_x, width_y, width_z?) "
			fieldnames = [field+"_"+i for i in "xyz"]
			fieldnames = fieldnames[:comp_domain['dimension']]
			fields = [str(comp_domain[fn]) for fn in fieldnames]
			return ", ".join(fields)
		
		@jinja_filter
		def count_variables(variable_list):
			"""Count the variables in a list 'x,y,z' or 'x:1,y:5,z:17' to 3 or 23, respectively"""
			if not isa(variable_list, [str,unicode]):
				return str(variable_list) ## whatever!
			matches = re.findall(r"([a-zA-Z-_]+)(?:[:](\d+))?", variable_list)
			return sum([1 if count == "" else int(count) for label,count in matches ])
		
		@jinja_filter
		def resolve_path(node, prepend='/'):
			"""
			Lookup the tree_backref_native inserted backreference and embed it.
			In order to have this be understood as a file path by the ExaHyPE specfile parser,
			prepend a slash.
			"""
			# print out the query xpath (backref)
			backref_key = "$path"
			if backref_key in node:
				return prepend + node[backref_key]
			else:
				raise ValueError("Missing backref key '%s' in node '%s'" % (backref_key, str(node)))
			
		
		@jinja_filter
		def link_in_list(node):
			symb = resolve_path(node)
			return self.encode_parameters(symb)
		
		@jinja_filter
		def as_float(txt):
			"""
			Ensure a number does look like a float (2.) instead of an int (2).
			This is what the ExaHyPE parser is sensitive on...
			"""
			return str(float(txt))
		
		# function (not a filter):
		jinja_env.globals['error'] = lambda msg: raise_exception(ValueError("Template stopped: "+msg))
		
		#try:
		return jinja_env.get_template(self.tplfile).render(ctx)
		# without exception chaining, we loose the stack trace:
		#except Exception as e:
		#	print "Debuggin context:"
		#	pprint.pprint(ctx)
		#	print "Exception:"
		#	print str(e)
		#	raise e	
