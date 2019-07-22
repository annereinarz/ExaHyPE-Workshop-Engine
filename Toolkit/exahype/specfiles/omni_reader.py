#!/usr/bin/env python3

"""
An "omni-reader" for trying to read different kind of file formats.
"""

import logging, os
from collections import OrderedDict
get_filename_extension = lambda filename: os.path.splitext(filename)[1]

# We work with two major exceptions: ParserError, a generalized
# decoder error and ImportError, if a library was not available
class ParserError(RuntimeError): pass

class OmniReaderError(RuntimeError): pass

readers = OrderedDict()
reader_by_extension = {}

# somehow this whole registration thing is defunct. Should have class context
# but python does not like me.

def register_reader(format_name, extensions=None):
	global reader_by_extension
	if isinstance(extensions, list):
		for e in extensions:
			reader_by_extension[e] = format_name
	if isinstance(extensions, str):
		reader_by_extension[extensions] = format_name
	
	def impl(func):
		readers[format_name] = func
		return func
	return impl

class OmniReader:
	any_format_name = "any"
	
	def __init__(self, log=None):
		if not log:
			logging.basicConfig(format="%(filename)s:%(lineno)s(%(funcName)s):%(levelname)s %(message)s")
			log = logging.getLogger()
			#log.addHandler(logging.StreamHandler()) # to stderr
			log.setLevel(logging.INFO)
		self.log = log.getChild(__name__)
		self.log.info("%d file formats registered: %s" % (len(readers), ",".join(readers.keys())))
	
	@classmethod
	def available_readers(cls):
		return [cls.any_format_name] + list(readers.keys())
	
	@register_reader("json", extensions=[".json"])
	def read_json(self, document_as_string):
		"""
		JSON fails pretty quickly thanks to it's strictness
		"""
		import json
		#import ipdb; ipdb.set_trace()
		try:
			return json.loads(document_as_string)
		except ValueError as e:
			raise ParserError(e)

	@register_reader("hjson", extensions=[".hjson"])
	def read_hjson(self, fp):
		"""
		Read human readable JSON, https://hjson.org/
		Python implementation: https://github.com/hjson/hjson-py
		
		Same usage as the json module.
		"""
		import hjson # pip install hjson
		try:
			hijson.loads(document_as_string)
		except hijson.HjsonDecodeError as e:
			raise ParserError(e)
		
	@register_reader("exahype-v1", extensions=[".exahype"])
	def read_specfile1(self, document_as_string):
		"""
		This reader also prefers to deal with file handles
		"""
		from .specfile1_reader import SpecFile1Reader # local directory, requires no extra libraries
		from .specfile1_reader import SpecFile1ParserError # local directory, requires no extra libraries
		try:
			rd = SpecFile1Reader(self.log)
			return rd.read_string(document_as_string)
		except SpecFile1ParserError as e:
			raise ParserError(e)
		
	@register_reader("mexa", extensions=[".mexa", ".mexahype"])
	def read_mexa(self, document_as_string):
		import mexa # local directory, but requires networkx
		try:
			mf = mexa.mexafile.from_string(document_as_string)
			return mf.tree()
		except ValueError as e:
			raise ParserError(e)
		

        # TODO disabled until we decide how to deal with YAML
        #@register_reader("yaml", extensions=[".yaml"])
	def read_yaml(self, document_as_string):
		"""
		TODO:
		Note that YAML is extremely relaxed and will read almost any file and will certainly
		produce nonsense output which subsequently fails the schema. Therefore, we should have some
		early heuristics to detect if the output is even close to how a spec file should look like.
		"""
		import yaml
		try:
			return yaml.load(document_as_string)
		except yaml.YAMLError as e:
			raise ParserError(e)

	def read_omni(self, document_as_string, filename=None):
		"""
		Tries to read anything, without any preference, by order.
		"""
		testable = []
		missing_libs = []
		for format_name, format_func in readers.items():
			try:
				self.log.info("Trying to read file format %s" % format_name)
				structure = format_func(self, document_as_string)
				self.log.info("Success reading file format %s" % format_name)
				return structure
			except ImportError as e:
				# library is not installed
				self.log.info("Cannot check file format %s because neccessary library is not installed. The missing library is: %s" % (format_name, str(e)))
				self.log.info("Will silently ignore this problem")
				missing_libs.append(format_name)
				pass
			except ParserError as e:
				# this is most likely not a file of that type
				self.log.info("The input file is certainly not written in the format %s as a ParserError occured: %s" % (format_name, str(e)))
				testable.append(format_name)
				pass
		
		if filename:
			ext = get_filename_extension(filename)
			self.log.warning("All file formats tested failed at reading the given file %s." % filename)
			if ext in reader_by_extension:
				file_format = reader_by_extension[ext]
				self.log.warning("In order to exit with a useful error message, I will exhibit the errors occuring while parsing the file as %s, the format which was guessed from the filename extension %s." % (ext, file_format))
				return self.read(document_as_string, required_file_format = file_format)
			else:
				self.log.error("Extension %s cannot even be associated to a file format. These extensions are registered: %s" % (ext, reader_by_extension))
		else:
			self.log.error("Since no filename information is available, cannot make any more educated guess about which parser error to pass to the user.")
				
		raise OmniReaderError("File could not be understood at all. I could successfully test the file formats %s but was missing libraries to test the formats %s. In order to find syntax errors in your files, first fix the file format your file assumably has." % (str(testable), str(missing_libs)))

	def read(self, document_as_string, required_file_format=None, filename=None):
		if not required_file_format or required_file_format == OmniReader.any_format_name:
			self.log.info("No specific file format requested, trying all available file formats in order.")
			return self.read_omni(document_as_string, filename=filename)
		elif required_file_format in self.available_readers():
			try:
				self.log.info("Trying to read file as %s" % required_file_format)
				structure = readers[required_file_format](self, document_as_string)
				return structure
			except ImportError as e:
				self.log.error("Cannot check file format %s because neccessary library is not installed. The missing library is: %s" % (required_file_format, str(e)))
				raise e
			except ParserError as e:
				self.log.error("The input file is not a proper %s file, a parser error occured: %s" % (required_file_format, str(e)))
				raise e	
		else:
			raise OmniReaderError("Invalid input file format: %s." % required_file_format)
