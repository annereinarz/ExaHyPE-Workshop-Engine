#!/usr/bin/python
# Regular Python 2

"""
This is the mexa implementation for the ExaHyPE Toolkit.

Mexa is a kind of non-turing-complete programming language to compose hierarchical
configuration files. Stripping the logic, it is also a simple markup language for
hierarchical configuration files. Last but not least, it is an idiom about how
configuration should be done.

This code is a regular python2 (should(!) be compatible with python3) program with
minimal dependencies (networkx) in a single file. It also ships a rich command line
interface (CLI) but is primarily supposed to be used from other python code.

For more details about the Mexa language see the README file or the repository
http://bitbucket.org/svek/mexa

Written in 2017 by SvenK for ExaHyPE.
"""

# dependency
import networkx as nx


# batteries:
import os, re, sys, ast, inspect, argparse, base64, pprint, subprocess, collections, types, operator
from collections import namedtuple, OrderedDict, defaultdict
from argparse import Namespace as namespace
from itertools import izip, islice, product
from functools import wraps
#from future.utils import raise_from # no batteries

# helpers and shorthands:
def warn(msg):
	print(msg, file=sys.stderr)

baseflags = re.IGNORECASE
match = lambda pattern,string,flags=0: re.match(pattern,string,baseflags+flags)
# unpack a tuple
unpack = lambda f: lambda p: f(*p)
# unpack a namespace
unpack_namespace = lambda f: lambda ns: f(**vars(ns))
# The identity function. Don't mix up with pythons internal "id"
idfunc = lambda x: x
# A no-operation function which just slurps arguments.
noop = lambda *args, **kwargs: None
# A nicer functional list access thing
first = lambda x: x[0]
last = lambda x: x[-1]
tuplize = lambda x: (x,)
# Removes all None from a list
removeNone = lambda l: [ x for x in l if x is not None ]
removeFalse = lambda l: [ x for x in l if x ]
# removes the list wrapper if it contains only one item
unlistOne = lambda l: l[0] if len(l)==1 else l
# Gives the list of duplicates from a list
dupes = lambda a: [x for n, x in enumerate(a) if x in a[:n]]
# Insert list b at position i in list a, replacing element a[i]
replace_with_list_at = lambda i, a, b: a[:i]+b+a[i+1:]
# Remove common prefix of two strings, i.e. remove_prefix("abcdef","abc")=="def"
remove_comstr_prefix = lambda text, prefix: text[text.startswith(prefix) and len(prefix):]
# flatten a 2d list
flatten2d = lambda l: [item for sublist in l for item in sublist]
# potentially flatten a 2d list, accept heterogeneous input such as [1,[2,3],4]
potentialFlatten2D = lambda l: flatten2d([ i if isa(i,collections.Iterable) and not isa(i,types.StringTypes) else [i] for i in l ])
# unique items
unique = lambda l: list(set(l))
# unique items while preserve the order
def unique_preserve(seq):
	seen = set()
	seen_add = seen.add
	return [x for x in seq if not (x in seen or seen_add(x))]
# invert dictionary
invdict = lambda d: {v: k for k, v in d.iteritems()}
# an own isninstance function which also allows mapping if types is a list.
# Reads "is a" as the Perl isa function. Mimics isinstance.
def isa(obj, types):
	if isinstance(types, list):
		return any([isa(obj, t) for t in types])
	return isinstance(obj, types)
# Gives an iterator over a dict or list
anykey = lambda obj: obj.keys() if isa(obj,dict) else xrange(len(obj))
# walks a complex dict or list structure
def mapComplex(func, node):
	for key in anykey(node):
		item = node[key]
		if isa(item,dict) or isa(item,list):
			mapComplex(func, item)
		else:
			node[key] = func(item)
	return node
# remove elements from list by indexlist
withoutIndices = lambda lst, indices: [i for j, i in enumerate(lst) if j not in indices]
# raise an exception. Useful from lambdas.
def raise_exception(e):
	raise e
# returns string until first occurance of character, not including the character
untilCharacter = lambda txt, char: first(txt.partition(char))
#
def window(seq, n=2):
	"Returns a sliding window (of width n) over data from the iterable"
	"   s -> (s0,s1,...s[n-1]), (s1,s2,...,sn), ...                   "
	it = iter(seq)
	result = tuple(islice(it, n))
	if len(result) == n:
		yield result
	for elem in it:
		result = result[1:] + (elem,)
		yield result
#
class NamedFunction:
	"Readable names for functions"
	def __init__(self, name, f):
		self.f = f
		self.name = name
	def __call__(self, *args, **kwargs):
		return self.f(*args, **kwargs)
	def __repr__(self):
		return self.name

### end helpers

class term:
	# a unique thing (string). For nxNetwork graph
	# Only one term is equal to all others: The None or "" term
	@classmethod
	def _incrCounter(cls):
		if not hasattr(cls, 'uniq_counter'):
			cls.uniq_counter = 0
		cls.uniq_counter += 1
		return cls.uniq_counter
	def __init__(self,value=None, addr_instance=None):
		"""
		Get a new term. This will be an unique object, i.e. term("foo") != term("foo").
		In order to address an exixting term (mostly for debugging purposes), you can
		write term("foo", 7) if your existing term("foo") has a counter==7.
		"""
		self.value = value
		if addr_instance == None:
			self.counter = self.__class__._incrCounter() if value else 0
		else:
			self.counter = addr_instance
	def isRoot(self):
		return not bool(self.value)
	def copy(self):
		# make a new instance of term which is unrelated to this one.
		return term(self.value)
	def __repr__(self):
		return "term(%s,%d)"%(repr(self.value),self.counter) if self.value else "root"
	def __eq__(self,other):
		#return not self.value and not other.value
		return isa(other,term) and self.counter == other.counter and self.value == other.value
	def __hash__(self):
		return hash((self.value,self.counter))

# define the root symbol for convenience
term.root = term()

class symbol:
	"""
	A symbol is an hierarchical identifier, similar to symbols in LISP and Mathematica.
	Symbols are the atoms of the mexa language.
	Symbols are treated as immutable: There is no method to change them after construction.
	In Mexa, symbols always appear as the LHS of an relation (operation).
	"""

	def __init__(self,name=''):
		""""
		Creates a symbol from a name such as Foo/Bar/Baz or a path such
		as ['Foo','Bar','Baz']. Without argument, this gives the root symbol
		symbol().
		Since a symbol is immutable, we can store a string and list version at the
		same time for lookup efficiency.
		"""
		if name == None: name = '' # whatever
		if isa(name,(list,tuple)):
			self._path = map(str.lower, map(str.strip, name))
		elif isa(name, symbol):
			self._path = name._path # kind of copy constructor, but only references to other list.
		else:
			name = str(name).lower() # in order to get case-insensitive
			self._path = map(str.strip, re.split(lang.symb_split_or, name))
		# in case of roots and merged paths and so on
		self._path = removeFalse(self._path)
		# make the path immutable
		self._path = tuple(self._path)
		# Compute a string representation, useful for path computations
		self._canonical = "/".join(self._path)
		
	def canonical(self):
		"Canonical string representation with a specific seperator"
		return self._canonical
	def __repr__(self):
		return "symbol(%s)" % self.canonical()
	def isRoot(self):
		return len(self._path) == 0
	
	def base(self): # first non-root element of path
		return symbol(self._path[0]) if not self.isRoot() else symbol()
	def except_base(self): # everything below the root, except the first non-root element
		return symbol(self._path[1:]) if len(self._path)>1 else symbol()
	def node(self): # last element of path
		if self.isRoot():
			return self
		return symbol(self._path[-1])
	def parent(self):
		if self.isRoot():
			raise ValueError("Symbol is already root")
		return symbol(self._path[:-1])
	def ancestors(self):
		"""
		Lists all parenting symbols, for instance for /a/b/c it is the
		list [/, /a, /a/b].
		"""
		return [symbol(self._path[:i]) for i,_ in enumerate(self._path)]
	def as_str_tuple(self, include_root=False):
		# Returns the (immutable) tuple of strings holding the elements of the path
		return ("",)+self._path if include_root else self._path
	def parts_as_term(self):
		"""
		Like ancestors, just weird. For instance /a/b/c it is
		[root, term('a'), term('b'), term('c')]
		"""
		return [term()] + [term(p) for p in self._path]
	def node_as_term(self):
		if self.isRoot():
			return term()
		return term(self._path[-1])
	def base_as_term(self):
		return term(self._path[0])
	
	def add_prefix(self, other, inplace=False):
		"""
		Returns a new symbol wich is prefixed by other, i.e.
		>>> a, b = symbol("a"), symbol("b")
		>>> print a.add_prefix(b)
		"b/a"
		"""
		if not isa(other,symbol): other = symbol(other)
		newpath = other._path + self._path;
		if inplace:
			self._path = newpath
			return self
		else:	return symbol(newpath)
	
	def sub(self, other):
		"""
		A more readable version:
		>>> a, b = symbol("a"), symbol("b")
		>>> print a.sub("b")
		"a/b"
		"""
		if not isa(other,symbol): other = symbol(other)
		return symbol(self._path + other._path)
	
	def remove_prefix(self, other, inplace=False):
		"""
		Returns a new symbol which has removed the common prefix. I.e.
		>>> ab, a = symbol("a/b"), symbol("a")
		>>> print ab.remove_prefix(a)
		"b"
		"""
		if not isa(other,symbol): other = symbol(other)
		newpath = remove_comstr_prefix(self.canonical(), other.canonical())
		if inplace:
			self._path = newpath
			return self
		else:	return symbol(newpath)

	# allow symbols to be dict keys:
	def __hash__(self):
		return hash(tuple(self._path))
	def __eq__(self, other):
		return isa(other,symbol) and self.canonical() == other.canonical()
	def __lt__(self, other): # sortable
		return self.canonical() < other.canonical()


class source:
	"""
	A source is a list of where something comes from.
	Sources just have to yield strings when asked for. That's it.
	"""
	def __init__(self, src=None):
		if isa(src, source):
			self.sources = src.sources
		elif isa(src, list): # shall not catch sourceline instances
			self.sources = src
		elif src==None:
			self.sources = ["unknown"]
		else:
			self.sources = [src]
	def __repr__(self): # todo: make nicer
		return "%s(%s)" % (self.__class__.__name__,self.sources_as_str())
	def add_source(self, src):
		self.sources += [src]
	def sources_as_str(self):
		if len(self.sources)==1:
			return self.sources[0]
		else:
			# TODO: Make this nicer
			return " -> ".join(map(str, self.sources))

source.unknown = source()

class sourceline(namedtuple('sourceline', 'fname linenum text')):
	"""
	Sourceline represents the source of something. It is a key class to allow
	transparent traceback of operations back to their source.
	"""
	
	def __repr__(self):
		return '%s:%s' % (self.fname, self.linenum)
	def verbose(self):
		return '%s line %s: `%s`' % (self.fname, self.linenum, self.text.strip())

	@classmethod
	def from_python(cls):
		"Create a sourceline instance from a python caller position."
		previous_frame = inspect.currentframe().f_back
		(filename, line_number, function_name, lines, index) = inspect.getframeinfo(previous_frame)
		return cls(filename, line_number, lines[0])
	
	class sourcemap:
		"""
		Allows to map an iterable with a given source name to inject sourceline objects.
		"""
		def __init__(self,iterable,source_name=None):
			self.iterable = iterable
			# todo: test with StringIO
			if not source_name:
				try:
					self.source_name = iterable.name
				except AttributeError:
					self.source_name = str(iterable) # hopefully short description
			else:
				self.source_name = source_name
		def iter(self):
			"Returns an iterator over the file"
			offset = 1 # python starts with 0 for line counting but humans start with 1
			return (sourceline(self.source_name,linenum+offset,text) for linenum, text in enumerate(self.iterable))
		def map(self,func):
			return map(func, self.iter())


# Regexps for defining the language
class lang:
	symbols = { '=': 'equals', '<=': "subsets", '<<': "include", '+=': "append" }
	symbol_alternatives = { '=': 'equals', '<': 'subsets', '<=': 'subsets', '<<': 'include', '+=': 'append' }
	primitives = [int,float,str,unicode,bool]
	opor = "|".join(map(re.escape, symbol_alternatives.keys())) # regex detecting operators
	symb_split = ("::", "/")
	symb_split_or = "|".join(map(re.escape, symb_split))
	symb = r"[a-z_][a-z_0-9:/]*" # regex defining a LHS symbol
	comment = r"#" # comment character
	linecomment = r"\s*(?:" + comment + r".*)?$" # allows a comment until end of line
	stringvarchar = "@" # variable escape character
	stringvarsimple = stringvarchar + "([a-z_0-9]+)" # only alphanumeric
	stringvarcomplex = stringvarchar + "\{("+symb+")\}"

	# rhs parsing:
	astring = r"(?:" + r'"([^"]*)"'+'|'+r"'([^']*)'" + ')'
	anum = r"([-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?)" # floats/ints; cf https://docs.python.org/3/library/re.html#simulating-scanf
	yes = r"^(Yes|True|On)"
	no = r"^(No|False|Off)"
	
	class evalstr:
		def __init__(self, text):
			self.text = text
		def __call__(self, variable_resolver):
			replmatch = lambda matchobj: variable_resolver(matchobj.group(1))
			text = re.sub(lang.stringvarsimple, replmatch, self.text, count=0, flags=baseflags)
			text = re.sub(lang.stringvarcomplex, replmatch, text, count=0, flags=baseflags)
			return lang.rhs2python(text, context=variable_resolver)
		def __repr__(self):
			return "%s(%s)" % (self.__class__.__name__, self.text)

	@classmethod
	def num2py(cls, text):
		try:
			return int(text)
		except ValueError:
			try:
				return float(text)
			except ValueError:
				return text # string fallback
	
	@classmethod
	def rhs2python(cls, text, context=None):
		"""
		Parses a right hand side into int, float, boolean, symbol, whatever.
		This does return functions in some cases. They are supposed to be executed with
		an operations instance then for variable substitution.
		If this returns a list or if the returned and subsequently evaluated function
		returns a list, it is subject to the caller to deal with this list accordingly.
		
		Note that this parser covers the requirements by the language but does not fit them
		1:1. Especially, it may allow certain syntax which is not part of the language.
		Feel free to improve the parser; the focus of this implementation lies in resolving
		variables, thought, while staying simple and without external dependencies.
		"""
		text = text.strip()
		
		# as a kind of preprocessor, support "@foo"-like variables. In case they
		# are detected (we uglily try to detect them not in comments), we return
		# instead a promise function for later evaluation with a correct variable
		# context.
		text_decommented = first(text.partition(lang.comment))
		if re.search(lang.stringvarsimple, text_decommented) or re.search(lang.stringvarcomplex, text_decommented):
			#import ipdb; ipdb.set_trace()
			# if we already have an operations context, directly evaluate the variables.
			# Otherweise, return the closure.
			promise = lang.evalstr(text)
			return promise(context) if context else promise

		# throw a number of regexps on the values to determine what they are.
		
		# "strings" or 'strings'. Caveat: Treat comments correctly in strings like "foo#bar".
		mstr = re.match(lang.astring + lang.linecomment, text, re.IGNORECASE)
		if mstr:
			return first(removeNone(mstr.groups())) # for whatever reason, mstr.group() treats "' wrong.
		# numbers. cf. 
		mnum = re.match(lang.anum + lang.linecomment, text, re.IGNORECASE)
		if mnum:
			num = mnum.groups()[0]
			return lang.num2py(num)
	
		# since our boolean types also cast as lang.symb, we disallow nodes with these names.
		# Yes/No/True/False/On/Off are thus the only reserved keywords in the language.
		if re.match(lang.yes+lang.linecomment, text, re.IGNORECASE):
			return True
		if re.match(lang.no+lang.linecomment, text, re.IGNORECASE):
			return False
		
		# A symbol or a list of symbols.
		symbs = re.match(r"^("+lang.symb+")(\s+"+lang.symb+")*"+lang.linecomment, text, re.IGNORECASE)
		# If only one symbol is detected, return the symbol. Otherwise
		# return the list of symbols which has to be understood correctly.
		if symbs:
			return unlistOne(map(symbol, removeNone(symbs.groups())))
		
		# A list of numbers. The list follows python syntax: (1,2,3) or [1,2,3]
		list_open_bracket = r"[(\[]"
		list_close_bracket = r"[)\]]"
		list_sep = "," # comma seperates values
		nums = re.match(list_open_bracket +"("+ "(?:" + lang.anum + "\s*"+list_sep+"\s*)+" + lang.anum +")"+ list_close_bracket + lang.linecomment, text, re.IGNORECASE)
		if nums:
			bracket_content = first(nums.groups())
			all_numbers = re.findall(lang.anum, bracket_content)
			return map(lang.num2py, all_numbers)
		
		
		# this does not work because of
		
		"""
		"""
		
		return text
		"""
		
		# We use python's parser for both understanding the data structure
		# and removing pythonic line comments such as "#".
		try:
			typed = ast.literal_eval(text)
			if isa(typed, lang.primitives):
				return typed
			elif isa(typed,[tuple,list]):
				# We said we support lists.
				return list(typed)
			else:
				# we probably have a created a weird type, for instance we
				# parsed a dictionary, which we do not want to support.
				# Instead, thus, return a text. This may not correctly strip comments,
				# thought.
				return text
		except (ValueError, SyntaxError):
			# This is most likely a string or something weird
			

			
			return text
		return text
		"""
	
	@classmethod
	def rhs2string(cls, rhs):
		if isa(rhs,symbol):
			return rhs.canonical()
		if isa(rhs,lang.evalstr):
			return '"%s"' % rhs.text
		elif isa(rhs, [str,unicode]):
			return '"%s"' % rhs
		if isa(rhs,lang.primitives):
			return str(rhs)
		else:
			raise ValueError("Bad RHS, cannot transform safely to text: %s" % str(rhs))
	
	@classmethod
	def Rel_from_textline(cls, srcline): # was textline2edge
		"""
		Parses a single line. Input is a sourceline object. You can test lines quickly with
		> line2operation(sourceline.from_unknown("foo=10"))
		"""
		if re.match(r"^"+lang.comment+"|^\s*$", srcline.text): # comments
			return None

		parts = re.match(r"^(?P<lhs>(?:"+lang.symb+"|))\s*(?P<op>(?:"+lang.opor+"))\s*(?P<rhs>.+)\s*$", srcline.text, re.IGNORECASE)
		if not parts:
			raise ValueError("Don't understand line %d in %s: '%s'"%(srcline.linenum,srcline.fname,srcline.text))

		# split lhs into name path
		name = symbol(parts.group('lhs'))
		# parse rhs as symbol or whatever
		value = lang.rhs2python(parts.group('rhs'))
		op = lang.symbol_alternatives[parts.group('op')]

		return Rel(name, value, op, srcline)
	
	@classmethod
	def Rel_to_textline(cls, rel):
		return "%s %s %s" % (rel.l.canonical(), invdict(lang.symbols)[rel.op], cls.rhs2string(rel.r))

class Rel:
	def __init__(self, l, r, op, src=None):
		self.l = l # typically a symbol. May also be a term in some contexts.
		self.r = r # typically something or also a symbol
		self.op = op
		self.src = source(src)
	def __repr__(self):
		return "%s(%s, %s)" % (self.op, self.l, self.r) # omit src for the time being

	def evaluate_rhs(self, *arg):
		if callable(self.r):
			self.r = self.r(*arg) # was self.r.value for some reason
		return self.r
	
	def add_prefix(self, path): # to be updated or removed
		"Return a new operation where lhs is prefixed with path"
		return self.__class__( self.l.add_prefix(path), self.r, self.op, self.src)
	
	def remove_prefix(self, path): # to be updated or removed
		"Return a new operation list where each lhs has a common prefix with path removed"
		return self.__class__( self.l.remove_prefix(path), self.r, self.op, self.src)
	
	# list or tuple idiom
	def __len__(self):
		return 4
	def __iter__(self):
		yield self.l
		yield self.r
		yield self.op
		yield self.src

class mexagraph:
	# the graph has two types of edges
	P = 'path'
	E = 'extends' # or inherits
	
	def __init__(self, edges):
		self.G = nx.DiGraph()
		# add a root
		self.G.add_node(term.root)
		self.from_mexafile(edges)
	def __repr__(self):
		return '%s(%s)' % (self.__class__.__name__, pprint.pformat(self.G.edges(data=True),width=1))
	
	def insert_path(self, path, starting_from=term.root, src=None):
		"""
		Insert edges for a  symbol path, i.e. /a/b/c gets a-[path]->b-[path]->c
		Returns the last edge target which was inserted.
		"""
		if not isa(path,symbol): path = symbol(path)
		if path.isRoot():
			return starting_from
		head, tail = path.base().canonical(), path.except_base()
		child = None
		for existing_child, attr in self.G[starting_from].iteritems():
			if attr['op'] == self.P:
				if existing_child.value == head:
					child = existing_child
					break
		# if did not found the segment, create it:
		if not child:
			child = term(head)
			self.G.add_edge(starting_from, child, op=self.P, src=src)
		return self.insert_path(tail, child, src=src)

	def get_path_down(self, left, starting_from=term(), silent_failure=False): # the original get_path
		"""
		Resolve symbol -> term.
		starting from some term (default: root).
		Resolves down the graph.
		Neccessary because term() instances are unfindable by definition.
		Returns None if nothing found, raises exception if asked for.
		Symbol is always supposed to be rootet at starting_from.
		-> works!
		"""
		if not isa(left,symbol): left = symbol(left)
		if left.isRoot(): # we found our element
			return starting_from
		first = left.base().canonical()
		for right, attr in self.G[starting_from].iteritems():
			if attr['op'] == self.P:
				if right.value == first:
					#print("Looking for %s, Found %s" % (left,first))
					return self.get_path_down(left.except_base(), right, silent_failure=silent_failure)
				else:
					#print("No success: %s != %s" % (right,first))
					pass
		#print("Looked for %s (%s), found nothing" % (left,first))
		if not silent_failure:
			raise ValueError("Symbol %s not found in graph, searching from %s." % (left,starting_from))
		
	def get_path_up(self, symb, right, include_start=True, stack=tuple(), disallow_first_parent=True):
		"""
		Resolves symbol -> [term], starting from some term.
		# Resolve from a term to a symbol by going back the path
		# symb: Symbol to look for, e.g.  foo/bar
		# right: Term where to start looking at, i.e. biz in bla/boo/biz
		# stack: Internal stack to avoid loops
		# In this example, foo/bar could be found at bla/foo/bar or bla/boo/foo/bar
		# or even bla/boo/biz/foo/bar.
		
		It always returns a list. Elements of this list are tuples.
		First value is target symbol, second value is the stack i.e. path from
		starting symbol to final value (neccessary for understanding double resolutions).
		Ideally, only one list element is returned and the resolution is trivial.
		"""
		
		if not isa(symb,symbol): symb = symbol(symb)
		ret = []
		# test from right itself
		if include_start:
			resolving_term = self.get_path_down(symb, starting_from=right, silent_failure=True)
			if resolving_term:
				ret.append( resolving_term )
		# check in parents of right
		for left, attr in self.G.pred[right].iteritems():
			resolving_term = self.get_path_down(symb, starting_from=left, silent_failure=True)
			if resolving_term:
				if disallow_first_parent:
					# we are in a situation where a/b = b and we are on a. Do not allow
					# a self-reference from b<->b.
					continue
				else:
					ret.append(resolving_term)
			else:
				if left in [rel.l for rel in stack]:
					# run into an equality a-[equals]->b, b-[equals]->a. Do not follow.
					# Equality loops are there by design.
					continue
				else:
					# traverse over the edge
					newstack = stack + tuplize(Rel(left, right, attr['op'], attr['src']))
					ret += self.get_path_up(symb, right=left, stack=newstack, disallow_first_parent=False)
		return unique_preserve(removeFalse(ret))
	
	# currently UNUSED.
	def resolve_symbol(self, symb, start, silent_failure=False):
		"""
		Returns a symbol to a term.
		"""
		res = self.get_path_up(symb, right=start, include_start=True)
		if len(res) == 1:
			return first(res)
		elif len(res) == 0:
			if silent_failure:
				return None
			else:
				raise ValueError("Symbol %s not found around term %s" % (symb,start))
		else: # len(res)>1
			raise ValueError("Found multiple candidates for %s around term %s: %s" % (symb,start,res))

	def from_mexafile(self, edges):
		"""
		Adds edges to the graph. The rules are:
		  * paths are resolved into the graph and get path edges
		  * equalities and subsets are represented as extends edges
		  * all other operations are not touched and go throught the processing.
		That is, you should make sure if you have operations such as "include" and "append",
		parse them before.
		"""
		
		for l,r,op,src in edges:
			assert isa(l,symbol)
			l = self.insert_path(l, src=src)
			
			needspatch = False
			if isa(r,symbol):
				oldr = r # for debugging
				target = self.get_path_up(r, l, include_start=False) # check if variable exists
				if len(target) == 1:
					r = first(target) # link to existing
				elif len(target) == 0:
					r = self.insert_path(r, src=src) # insert new node
				else: # len(target) > 1:
					# multiple targets exist. This is bad.
					raise ValueError("Found multiple candidates for %s around term %s: %s" % (r,l,target))
			# => allows full relative variable addressation.
			# => problem: Resolving variables may not yet take future relations into account.
			#    to encounter the problem, the graph would have needed to be setup in a
			#    iterative process with correction steps.

				assert l != r, "Produced weird graph because l=%s, oldr=%s, new r=%s for op=%s\n" % (l, oldr, r, op)
				needspatch = True
			
			# map equals and subsets together
			if op == 'equals':
				self.G.add_edge(l, r, op=self.E, src=src, patched=not needspatch)
				self.G.add_edge(r, l, op=self.E, src=src, patched=not needspatch) # TODO: Should comment the source.
			elif op == 'subsets':
				self.G.add_edge(l, r, op=self.E, src=src, patched=not needspatch)
			else:
				self.G.add_edge(l, r, op=op, src=src)
				
		def equivalent_adjacency_iter(l, stack=tuple()):
			# A version of self.G[l].iteritems() which also returns all edges
			# from the equivalent nodes
			for r, attr in self.G[l].iteritems():
				if attr['op'] == self.E and not r in stack:
					newstack = stack + (r,) # avoid loops
					for r, attr in equivalent_adjacency_iter(r, stack=newstack):
						yield (r, attr)
				yield (r, attr)
		
		# correction step: inherit equivalence.
		while not all([attr['patched'] for l,r,attr in self.G.edges(data=True) if attr['op']==self.E]):
			for l, r, attr in self.G.edges(data=True):
				if attr['op'] == self.E and not attr['patched']:
					#for (ln, lattr), (rn, rattr) in product(self.G[l].iteritems(), self.G[r].iteritems()):
					for (ln, lattr), (rn, rattr) in product(equivalent_adjacency_iter(l), equivalent_adjacency_iter(r)):
						if lattr['op'] == self.P and rattr['op'] == self.P \
						   and isa(ln, term) and isa(rn,term) \
						   and ln != rn and ln.value == rn.value \
						   and not self.G.has_edge(ln, rn):
							self.G.add_edge(ln, rn, op=self.E, src=attr['src'], patched=False) # TODO: Comment source
					# the l-[eq]->r edge is now patched.
					attr['patched'] = True
	
	# graph to mexa: Without evaluation of any edges
	def to_mexafile(self, left=term.root, left_path=symbol()):
		#assert isa(root, term)
		ret = []
		for right, attr in self.G[left].iteritems():
			#print("%s,%s" % (right,attr))
			if attr['op'] == self.P:
				right_path = symbol(right.value).add_prefix(left_path)# if isa(right,term) else str(right))
				#print("At %s, %s: Visiting %s, %s" % (left, left_path, right, right_path))
				ret += self.to_mexafile(right, right_path)
			else:
				#print("%s: Attr is %s" % (right, attr))
				if isa(right, term):
					right = self.get_path_down(right) # TODO: Resolve the term -> symbol here.
				ret += [ Rel(left_path, right, attr['op'], attr['src']) ]
		return mexafile(ret)
		#return map(unpack(node_to_mexafile), )
		
	# check for undefined symbols
	def check_undef(self, symb=term.root):
		# undefined symbols are defined simply as: A path leaf which has no outoing equalities.
		#[x for x in self.G.nodes_iter() if self.G.out_degree(x)==0 ]
		pass
		# TOD BE DONE.
		
	def evaluate_to_mexafile(self, left=term.root, stack=tuple(), max_rec=20, sort_by='input'):
		"""
		Correctly resolves all equalities and directed equalities to an oplist aka a mexafile.
		Current Limitations:
		  * Code ignores loops in the equality graph, thus it cannot detect
		    missing definitions. This should be done independently by check_undef.
		"""
		if max_rec == 0:
			raise ValueError("self.Path is too deep. at %s, stack=%s" % (left,stack))
		ret = []
		# this is an absurd hack which uses the unique item numbering for restoring the
		# order of the edges in the mexafile (ie. give them out in the order they were read in).
		if sort_by == 'input':
			### Sort by order of input:
			sort_edges = unpack(lambda itm,attr: itm.counter if isa(itm,term) else itm)
		elif sort_by == 'name':
			### Sort by symbol name:
			sort_edges = unpack(lambda itm,attr: itm.value if isa(itm,term) else itm)
		else:
			raise ValueError("sort_by=%s not understood, only input/name allowed."%sort_by)
		
		for right, attr in sorted(self.G[left].iteritems(), key=sort_edges):
			if attr['op'] == self.P:
				# put onto the stack:
				newstack = stack + tuplize(Rel(left, right, self.P, attr['src']))
				#right_path = symbol(right.value).add_prefix(left_path)
				#print("At %s, %s: Visiting %s, %s" % (left, left_path, right, right_path))
				ret += self.evaluate_to_mexafile(right, newstack, max_rec=max_rec-1, sort_by=sort_by)
			elif attr['op'] == self.E:
				if isa(right, term):
					if right in [rel.r for rel in stack]: # WAS rel.l
						# run into an equality a-[equals]->b, b-[equals]->a. Do not follow.
						# Equality loops are there by design.
						continue
					else:
						# follow the equals with same path
						newstack = stack + tuplize(Rel(left, right, self.E, attr['src']))
						ret += self.evaluate_to_mexafile(right, newstack, max_rec=max_rec-1, sort_by=sort_by)
				else:
					# make an attribute node.
					newstack = stack + tuplize(Rel(left, right, self.E, attr['src']))
					pathlst = [ rel.r.value for rel in newstack if rel.op == self.P ]
					srclst  = [ rel.src     for rel in newstack if rel.op == self.E and not rel.l.isRoot() ]
					#print("At left=%s, newstack=%s I composed path=%s, srclist=%s" % (left,newstack,pathlst,srclst))
					ret += [ Rel(symbol(pathlst), right, "equals", source(srclst)) ] # could also use "define"
			else:
				# TODO: Should instead let all other operations pass.
				raise ValueError("At l=%s,stack=%s, Operation not known: rattr=%s" % (left,stack,attr))
		#if len(ret) == 0:
			# no childs given at this node = undefined!
		#	raise ValueError("Term l=%s,stack=%s lacks a definition\n"%(left,stack))
		return mexafile(ret)
		#return map(unpack(node_to_mexafile), )
		
	def toFile(self, base_filename):
		"""
		Quickly write graph to a file. For more extensive usage examples, see the CLI version
		below.
		"""
		dotfilename = base_filename+".dot"
		imgfilename = base_filename+".png"
		nx.nx_agraph.write_dot(self.G, dotfilename)
		retcode = subprocess.call(["dot", "-Grankdir=LR", "-Tpng", dotfilename, "-o", imgfilename])
		print("Wrote GraphViz output to %s, invoked `dot`, produced %s with exit code %d." % (dotfilename, imgfilename, retcode))
	
	def to_agraph(self):
		"""
		Return a pygraphviz instance of the graph.
		"""
		return nx.nx_agraph.to_agraph(self.G)

class mexafile:
	"""
	Current mexafile
	"""
	
	def __init__(self, ops=None):
		if not ops:
			self.ops = list()
		elif isa(ops,mexafile):
			self.ops = ops.ops # reference
		else:
			self.ops = ops # reference
		# we do not call evaluate() here, do this manually.
		
	def __repr__(self):
		return '%s(%s)' % (self.__class__.__name__, pprint.pformat(self.ops,width=1))
	def __iter__(self):
		return iter(self.ops)
	def __len__(self):
		return len(self.ops)

	def add_source(self, something): # chainable
		for op in self.ops:
			op.src.add_source(something)
		return self
	
	def graph(self):
		# There is no mechanism to keep ops and graph in sync.
		if not hasattr(self, '_graph'):
			self._graph = mexagraph(self)
		return self._graph
	
	@classmethod
	def from_filehandle(cls, fh):
		# The ordered list of operations as they appear in the file
		return cls(removeNone(sourceline.sourcemap(fh).map(lang.Rel_from_textline)))
		# Prepend the list with the rootbase
		# self.ops = self.rootbase + self.ops # no more.

	@classmethod
	def from_filename(cls, fname):
		"Create an instance from a filename instead of filehandle"
		with open(fname, 'r') as fh:
			return cls.from_filehandle(fh)
	
	@classmethod
	def from_string(cls, document_as_string):
		# hack, abuse the point that fh is translated anyway to list(fh)
		return cls.from_filehandle(document_as_string.split("\n"))
	
	@classmethod
	def from_textlines(cls, oplines, source_name="unoriginated textlines"):
		"""
		* List of single lines: Are parsed, source_name used as source name.
		oplines must be iterable (i.e. list of strings or an open file handle).
		"""
		if not isa(oplines, list):
			oplines = [oplines]
		oplist = removeNone(sourceline.sourcemap(oplines, source_name).map(lang.Rel_from_textline))
		return cls(oplist)
	
	@classmethod
	def from_dict(cls, something):
		"""
		Create an instance of a mexafile from a nexted dict/list python structure.
		"""
		raise NotImplementedError("To be done")
	
	def query(self, root='', inplace=False, exclude_root=False):
		"""
		Get the assignment tree based on some root (which may be string, list, symbol).
		Returns a new operations instance.
		Maintains the order of operations as they appear.
		"""
		if not isa(root,symbol): root = symbol(root)
		# Search for all symbols which are *below* the root
		# on symbols:
		# tree = { lhs : rhs for lhs,rhs in self.symbols.iteritems() if root in lhs.ancestors() }
		# on the oplist:
		ret = [ op for op in self.ops 
			if root in op.l.ancestors() # root="foo", include "foo/bar" and "foo/bar/baz"
			or (root == op.l and not exclude_root)  # root="foo", include "foo" itself.
		]
		if inplace:
			self.ops = ret
		else:
			return mexafile(ret)
		
	def tree(self, root='', symbol_resolver=None, backref=False, backref_key="$path"):
		"""
		Like query, but will result in a nested dictionary structure instead of an oplist.
		
		\option root: A symbol instance.
		\option symbol_resolver: Not needed any more!
		\option backref: Insert the full path into every dictionary.
		"""
		oplist_absolute = self.query(root) # with absolute paths
		oplist_relative = oplist_absolute.remove_prefix(root) # relative paths to root
		# outline is a list with the general structure of the tree, especially parents come before
		# childs so we can make a dict tree out of it.
		outline = sorted(unique(flatten2d([op.l.ancestors() for op in oplist_relative])))
		
		# setup the tree outline (nodes)
		tree = {}
		for sym in outline:
			subtree = tree
			for symp in sym.as_str_tuple():
				if not symp in subtree:
					subtree[symp] = {}
				subtree = subtree[symp]

		# fill the tree with leafs
		for abs_op,op in izip(oplist_absolute,oplist_relative):
			if op.l.isRoot():
				tree = op.r
			else:
				# This reads as following:
				# when op.l = symbol(solvers/solver/constants/initialdata/name)
				# then op.l.parent() = symbol(solvers/solver/constants/initialdata)
				# then op.l.parent().as_str_tuple() =  ('solvers', 'solver', 'constants', 'initialdata')
				# then parent = tree['solvers']['solver']['constants']['initialdata']
				parent = reduce(dict.get, op.l.parent().as_str_tuple(), tree) 
				leaf_name = op.l.node().canonical()
				parent[leaf_name] = op.r
				
				
				### Should be done somewhere else
				### if backref and not backref_key in parent:
					# we do *not* put the parent in a symbol_resolver but instead
					# use the canonical description.
					# Note that the absolute path is the original one in the defining file,
					# not taking into account the actual mapping (inclusion) of the path.
				### 	parent[backref_key] = c
				
		# fill in backrefs if neccessary
		if backref:
			def visit(node, path):
				if backref_key in node:
					# we have a problem: The backref key is not unique but the data
					# allready use it.
					raise ValueError("backref_key=%s already taken in %s" % (backref_key, node))
				else:
					node[backref_key] = path.canonical()
				
				for k, v in node.iteritems():
					if isa(v, dict):
						visit(v, path.sub(k))
			visit(tree, path=symbol(root))

		return tree
	

	def evaluate_symbol(self, symb, evaluator, inplace=True, eliminate=True, max_rec=10):
		"""
		Evaluate a questioned symbol, where symbol is a class, for instance `append`.
		Returns new operations object or does the evaluation inplace.
		"""
		ret_oplist = []
		for rel in self.ops:
			if rel.op == symb:
				new_oplist = evaluator(rel, self)
				ret_oplist += new_oplist
			else:
				ret_oplist.append(rel) # pass throught
		
		if eliminate:
			remaining = mexafile([ rel for rel in ret_oplist if rel.op == symb ])
			if any(remaining):
			# There are still instances of symbol in the oplist and we were
			# asked to eliminate all of them. Recursively call ourselves,
				# expecting that they vanish.
				if max_rec == 0:
					raise ValueError("While trying to evaluate %s, reached maximum number of iterations. The user probably included cyclic links. These symbols remain: %s" % (str(symb),str(remaining)))
				return self.evaluate_symbol(symb, evaluator, inplace=inplace, eliminate=eliminate, max_rec=max_rec-1)
		
		if inplace:
			self.ops = ret_oplist
		else:
			return mexafile(ret_oplist)
		
	def evaluate_all_symbols(self, evaluator, inplace=True):
		ret = evaluator(self)
		if inplace:
			self.ops = ret
		else:
			return mexafile(ret)
	
	def evaluate(self, inplace=True, resolve_equalities=True):
		"""
		The evaluation is two-place: First, there is a element-local replacement step.
		Second, there is a global variable resolving step, using the graph.
		"""
		
		def include(op, operations):
			# include a file
			fname = op.evaluate_rhs(self.resolver_for(op))
			if not isa(fname, [str,unicode]):
				raise ValueError("For file inclusion, only strings are supported. In %s" % str(op))
			return mexafile.from_filename(fname).add_prefix(op.l).add_source(op.src)

		def append(op, operations):
			# This algorithm reads as rule: "a/b += c/d" => "a/b/d = c/d"

			if isa(op.r,list):
				# we do support multiple RHS values, i.e. a syntax like
				#  a += b c d  <=> equals(a, sourced([b,c,d], ...))
				return flatten2d([append(Rel(op.l, op_ri,'append',op.src), operations) for op_ri in op.r])
			
			# name of the node to create below the lhs.
			if isa(op.r, symbol):
				name = op.r.node()
			else:
				# a dumb way to come up with a number:
				# TODO: This is really dumb as the numbering is natural, i.e. we don't
				#       know the total length of the list (TODO there is certainly a simple way to obtain it)
				#       and therefore have either to fallback to natural sorting
				#       (n0, n1, n2, ... n9, n10, n11, ... n99, n100, n101, ...)
				#       instead of the buggy n0, n10, n11, ..., n100, n101, ..., n1, n2, ...
				#       or to rely on a hardcoded maximum where bugs arise, for instance
				#       n000, n001, n002, ... n999, n1000, n1001, ...
				#       here, with n%30i we choose the later and cross our hands that this
				#       bug will be noticed when it appears...
				#
				# Improvement: To make this nicer for vectors of length 2 (3), there one should
				#       fall back to a naming x, y, z, or i, j, k.
				#
				# Comment for obtaining the vector length: This is probably a "cleanup step"
				#       which should be done after the very end of the evaluation. Therefore,
				#       "counting names" should be marked and then beautified at the end.
				#
				if not hasattr(operations, 'append_counter'):
					operations.append_counter = defaultdict(lambda: 0)
				i_op_node = operations.append_counter[op.l]
				name = "n%03i" % i_op_node
				operations.append_counter[op.l] += 1
				# import pdb; pdb.set_trace()
				
			return mexafile([ Rel(symbol(name).add_prefix(op.l), op.r, 'equals', op.src) ])
		
		def prepare_equal(op, operations):
			if isa(op.r, list):
				# lists get created by rhs2python i.e. with something like a = (1,2,3)
				def listToAppends(i,vi): #for i,vi in enumerate(op.r.value):
					if isa(vi,lang.primitives):
						#return Rel(symbol("l%d"%i).add_prefix(op.l), vi, 'equals', op.src.add_source("list expansion"))
						return Rel(op.l, vi, 'append', op.src.add_source("list expansion"))
					else:
						raise ValueError("Found illegal non-primitive value in RHS of assignment operation: %s. You probably wanted to use the append += operation."%str(op))
				return map(unpack(listToAppends), enumerate(op.r))
			else:
				return [op]
		
		def equalities(operations): # caveat, this is global
			# resolves = and <=
			return operations.graph().evaluate_to_mexafile().ops
		
		self.evaluate_symbol('include', include, inplace=inplace)
		self.evaluate_symbol('equals', prepare_equal, eliminate=False, inplace=inplace)
		self.evaluate_symbol('append', append, inplace=inplace)
		if resolve_equalities:
			self.evaluate_all_symbols(equalities, inplace=inplace)
		
		# + evaluate strings?
		
		# as a last step, should look for inconsistencies or check whether all data
		# have correctly been evaluated.

	def resolve_symbol(self, varname, src=source.unknown):
		"""
		Looks up the value of a variable such as "foo" or "foo/bar" or "foo::bar::baz"
		in the symbols dictionary. In case of errors, src is spilled out.
		"""
		sv = symbol(varname)
		for rel in self.ops:
			if rel.l == sv:
				return rel.r
		raise ValueError("Variable '%s' not defined but used in %s" % (varname,src))
	
	def resolver_for(self, rel):
		"Returns a function for resolving a variable. Used for the evalstr() instances."
		return lambda varname: self.resolve_symbol(varname, src=rel.src) # what about passing "rel.r" for local variables?

	def add_prefix(self, path):
		"Return a new operations list where each lhs is prefixed with path"
		if not isa(path,symbol): path = symbol(path)
		return mexafile([ op.add_prefix(path) for op in self.ops ])
	
	def remove_prefix(self, path):
		"Return a new operation list where each lhs has a common prefix with path removed"
		if not isa(path,symbol): path = symbol(path)
		return mexafile([ op.remove_prefix(path) for op in self.ops ])

	def toPlain(self):
		return "\n".join([ lang.Rel_to_textline(rel) for rel in self ])

###
### CLI and application interface.
###

class mexa_cli(object):
	"""
	This is the command line interface to the gmexa code. The interface is structured
	into sub commands (similar to git). Call the individual sub commands in order to
	learn how they are used.
	The CLI is backed up by a class structure which shares code, such as a common
	input/output engine for all sub commands.
	"""
	
	command_registration = {} # maps string -> class
	subparsers_dest = 'command'
	
	def __init__(self):
		self.parser = argparse.ArgumentParser(description=self.__doc__, epilog=__doc__)
		
		subparsers = self.parser.add_subparsers(
			dest=self.subparsers_dest,
			title="subcommands",
			#description="valid subcommands",
			help="Individual meaning:")
		
		for cmd, cls in self.command_registration.iteritems():
			subargs = subparsers.add_parser(cmd, help=cls.__doc__, description=cls.__doc__)
			cls.arguments(subargs) # ask class for setting arguments
			subargs.add_argument("--pdb", action='store_true', help="Start Python debugger on error")

		args = vars(self.parser.parse_args())

		if args['pdb']:
			try:
				self.run(args)
			except:
				# Real PDB interface
				import pdb, traceback, sys
				type, value, tb = sys.exc_info()
				traceback.print_exc()
				pdb.post_mortem(tb)

				# interactive command line instead
				if False:
					import traceback, sys, code
					type, value, tb = sys.exc_info()
					traceback.print_exc()
					last_frame = lambda tb=tb: last_frame(tb.tb_next) if tb.tb_next else tb
					frame = last_frame().tb_frame
					ns = dict(frame.f_globals)
					ns.update(frame.f_locals)
					code.interact(local=ns)
		else:
			self.run(args)

	def run(self, args):
		"""
		args being a Python dictionary
		"""
		target_cls = self.command_registration[ args[self.subparsers_dest] ]
		self.job = target_cls(**args)


	@classmethod
	def register_subcommand(cls, subcommand_name):
		def register(the_cls): # @wraps breaks this.
			cls.command_registration[subcommand_name] = the_cls
			return the_cls
		return register

class io_mexafile(object):
	env_default_key = "env"
	evaluate_actions = {
		'all': lambda self: self.mf.evaluate(),
		'basic': lambda self: self.mf.evaluate(resolve_equalities=False),
		'none': noop
	}
	evaluate_actions_default = 'all'
	
	def __init__(self, infile, outfile, root="", evaluate='all', env=False, env_root=None, add=None, **ignored_kwargs):
		self.outfile = outfile
		self.mf = mexafile.from_filehandle(infile)
		
		# environment injection
		if env:
			env_root = symbol(self.env_default_key if not env_root else env_root)
			env_source = "Environment injection"
			env2rel = lambda k,v: Rel(env_root.sub(k), v, "equals", env_source)
			env_ops = map(unpack(env2rel), os.environ.iteritems())
		else:
			env_ops = []
		
		# Arbitrary command injection
		if add:
			mf_cmd = mexafile.from_textlines(add, source_name="Command line input")
			mf_cmd_ops = mf_cmd.ops
		else:
			mf_cmd_ops = []
		
		self.root = root
		if root:
			self.mf.query(root, inplace=True)
		
		# appending stuff to any file, as a root
		global_mexa_information = [
			# mexa = path to the current script directory
			Rel(symbol("mexa"), os.path.dirname(os.path.realpath(__file__)), "equals", sourceline.from_python()),
		]
		# -> should move to include file lookup paths instead.
		
		self.mf.ops = global_mexa_information + mf_cmd_ops + self.mf.ops + env_ops
		
		self.evaluate = evaluate
		self.evaluate_actions[evaluate](self)
		
		#print mf.toPlain()
		#mf.toGraph("graph")
		
		self.output_is_stdout = (self.outfile == sys.stdout)
	
	@classmethod
	def arguments(cls, parser, add_root=True, allow_generation=True):
		group = parser.add_argument_group('input/output arguments')
		group.add_argument('--infile', nargs='?', type=argparse.FileType('r'), default=sys.stdin,
			metavar='FILENAME', help="Input file to read. If no file is given, read from stdin.")
		group.add_argument('--outfile', nargs='?', type=argparse.FileType('w'), default=sys.stdout,
			metavar='FILENAME', help="Output file to write to. If no file is given, write to stdout.")
		if add_root:
			group.add_argument('--root', default='', metavar="/some/root", help='Queried root container')

		if allow_generation:
			gengroup = parser.add_argument_group('on-the-fly input arguments')
			gengroup.add_argument('--env', action='store_true',
				help="Add environment variables. They are inserted as strings below a root key (see --env-root).")
			gengroup.add_argument('--env-root', metavar="/some/env/root",
				help="Environment variables injection point. Defaults to " +cls.env_default_key)
			gengroup.add_argument('--add', action='append', metavar='"foo=\'bar\'"',
				help="Add any kind of mexa expression to the input. Mind the special treatment of your shell when it comes to whitespace and especially quotes. Add one expression with one --add argument each.")
		     
		
	def write(self, data):
		self.outfile.write(data)


@mexa_cli.register_subcommand('plain')
class plain_mexafile(io_mexafile):
	"""
	Prints the interpreted mexa file (pass-throught). This can either serve as syntax
	check or also invoke the evaluation which yields a simple mexa file with only
	assignments left.
	"""	
	def __init__(self, evaluate, *args, **kwargs):
		#import ipdb; ipdb.set_trace()
		super(plain_mexafile,self).__init__(evaluate=evaluate, *args, **kwargs)
		self.write("# evaluated = %s\n" % (str(self.evaluate)))
		self.write(self.mf.toPlain())
		self.write("\n")
		
	@classmethod
	def arguments(cls, parser):
		io_mexafile.arguments(parser)
		group = parser.add_argument_group('plain output arguments')
		group.add_argument('--evaluate', choices=cls.evaluate_actions, default=cls.evaluate_actions_default, help='Fully resolve equalities, just evaluate basic syntactic sugar or no evaluation at all.')

@mexa_cli.register_subcommand('graph')
class graph_mexafile(io_mexafile):
	"""
	Writes the graph of the mexafile (currently: DOT).
	tbd: Should support multiple file formats, such as DOT but also others
	"""
	# dot file formats we want to support here, taken from
	# help(pygraphviz.AGraph.draw)
	formats = [ \
		'canon', 'cmap', 'cmapx', 'cmapx_np', 'dia', 'dot',
		'fig', 'gd', 'gd2', 'gif', 'hpgl', 'imap', 'imap_np',
		'ismap', 'jpe', 'jpeg', 'jpg', 'mif', 'mp', 'pcl', 'pdf',
		'pic', 'plain', 'plain-ext', 'png', 'ps', 'ps2', 'svg',
		'svgz', 'vml', 'vmlz', 'vrml', 'vtx', 'wbmp', 'xdot', 'xlib'
	]
	default_format = 'plain'
	
	def __init__(self, dot=False, format=default_format, *args, **kwargs):
		super(graph_mexafile,self).__init__(*args, **kwargs)
		
		G = self.mf.graph().G
		A = nx.nx_agraph.to_agraph(G)
		
		if dot:
			if self.output_is_stdout:
				raise ValueError("Please specify a base output filename with --outfile.")
			base_filename = self.outfile.name
			dotfilename = base_filename+".dot"
			imgfilename = base_filename+"."+format
			nx.nx_agraph.write_dot(G, dotfilename)
			retcode = subprocess.call(["dot", "-Grankdir=LR", "-T"+format, dotfilename, "-o", imgfilename])
			print("Wrote GraphViz output to %s, invoked `dot`, produced %s with exit code %d." % (dotfilename, imgfilename, retcode))
		else:
			self.write( A.to_string() )
		
	@classmethod
	def arguments(cls, parser):
		io_mexafile.arguments(parser)
		group = parser.add_argument_group('graph output arguments')
		group.add_argument('--dot', action='store_true', help='Call dot on the output. Works only if you do not write to stdout but a file instead')
		group.add_argument('--format', choices=cls.formats, default=cls.default_format, help="Format to call dot with")
		#group.add_argument('--evaluate', choices=cls.evaluate_actions, default=cls.evaluate_actions_default, help='Fully resolve equalities, just evaluate basic syntactic sugar or no evaluation at all.')
		#group.add_argument('--graph', default=False, action='store_true', help='Plot a graph')

@mexa_cli.register_subcommand('shell')
class shell_mexafile(io_mexafile):
	"""
	Writes the graph of the mexafile (currently: DOT).
	tbd: Should support multiple file formats, such as DOT but also others
	
	You are given the two variables
	  mf: The Mexafile object
	  cli: The command line interface object
	to play with.
	
	Note: The shell command does not allow you to stream input data to stdin/stdout
	without hazzle. Instead, use --infile. Example usage:
	
	$ exa mexa shell --infile <(echo 'foo = (0,1,0)' )
	Python 2.7.12 ...
	> print mf
	mexafile([
		equals(symbol(foo/n0), 0),
		equals(symbol(foo/n1), 1)
	])
	...
	"""

	def __init__(self, *args, **kwargs):
		#import ipdb; ipdb.set_trace()
		super(shell_mexafile,self).__init__(*args, **kwargs)
		
		# expose variables to the shell
		cli = self
		mf = self.mf

		try:
			# show fancy IPython console
			from IPython import embed
			embed()
		except ImportError:
			# show standard python console
			import readline, code
			variables = globals().copy()
			variables.update(locals())
			shell = code.InteractiveConsole(variables)
			shell.interact()
		
	@classmethod
	def arguments(cls, parser):
		io_mexafile.arguments(parser)
		group = parser.add_argument_group('shell output arguments')
		#group.add_argument('--evaluate', choices=cls.evaluate_actions, default=cls.evaluate_actions_default, help='Fully resolve equalities, just evaluate basic syntactic sugar or no evaluation at all.')
		#group.add_argument('--graph', default=False, action='store_true', help='Plot a graph')


@mexa_cli.register_subcommand('encode')
class encoded_mexafile(io_mexafile):
	"""
	Gives the string of an encoded mexafile. Several encodings are possible. An encoded
	string can be embedded into other files or file formats more easily.
	"""
	def __init__(self, encoding, header, *args, **kwargs):
		#import ipdb; ipdb.set_trace()
		super(encoded_mexafile,self).__init__(*args, **kwargs)
		self.write(self.dump(self.mf, encoding, header))
	
	@staticmethod
	def quoted_printable(s, escape='%'): # helper
		"""
		Returns a quoted printable version of the string s with escape character %, no maximum line length
		(i.e. everything in a single line) and everything non-alphanumeric replaced.
		"""
		# sourcecode inspired by quopri, https://github.com/python/cpython/blob/2.7/Lib/quopri.py
		HEX = '0123456789ABCDEF'
		quote = lambda c: escape + HEX[ord(c)//16] + HEX[ord(c)%16] # quote a single character
		needsquote = lambda c: not ('0' <= c <= '9' or 'a' <= c <= 'z' or 'A' <= c <= 'Z') # Whether character needs to be quoted
		return "".join([ quote(c) if needsquote(c) else c for c in s])
	
	@staticmethod
	def relaxed_quoted_printable(s, escape='%'):
		"""
		Version of quoted printable which allows more special characters
		which are probably accepted by the toolkit.
		"""
		HEX = '0123456789ABCDEF'
		quote = lambda c: escape + HEX[ord(c)//16] + HEX[ord(c)%16] # quote a single character
		needsquote = lambda c: not (c in '_-' or '0' <= c <= '9' or 'a' <= c <= 'z' or 'A' <= c <= 'Z') # Whether character needs to be quoted
		return "".join([ quote(c) if needsquote(c) else c for c in s])
	
	encodings = {
		# base64 is the well known base64
		'base64': lambda f: base64.b64encode(f),
		# base16 is only the hex
		'base16': lambda f: base64.b16encode(f),
		# urlencode/quoted printable:
		'quotedprintable': lambda f: encoded_mexafile.quoted_printable(f)
	}

	default_encoding = 'quotedprintable'
	default_prepend_header = "##MEXA-simple configuration file"
	
	@classmethod
	def arguments(cls, parser):
		io_mexafile.arguments(parser)
		group = parser.add_argument_group('encoding output arguments')
		group.add_argument('--encoding', choices=cls.encodings.keys(), default=cls.default_encoding, help='Output encoding')
		group.add_argument('--header', default=cls.default_prepend_header, help='First line to add to output')
	
	@classmethod
	def dump(cls, mf, encoding=None, header=None):
		"""
		Return an encoded mexa file. Several encodements are available, see the
		encodings table.
		@arg mf: Mexafile instance
		@arg encoding: A value in encodings
		@arg header: A single header line (incl. comment sign) to be prepended before the file
		@arg simple_mexa: Whether to create simple mexa output
		"""
		if not encoding: encoding = cls.default_encoding
		if not header: header = cls.default_prepend_header
		fcontent = header + "\n" + mf.toPlain()
		return cls.encodings[encoding](fcontent)

@mexa_cli.register_subcommand('export')
class structured_mexafile(io_mexafile):
	"""
	Exports the hierarchical key-value parameter structure into various standard
	file formats. As they are typically more expressive than the mexa format,
	different style decisions are possible such as the representation as nested
	dictionaries or tags versus a linearization with fully qualified path names.
	"""
	
	native_styles = {
		'tree': lambda mexa: mexa.tree(), # graph as nested dictionary
		'tree-backref': lambda mexa: mexa.tree(backref=True), # including full paths (backreferences)
		'linear': lambda mexa: collections.OrderedDict([ (op.l.canonical(), op.r) for op in mexa ]), # all ops in a linear list
	}
	
	formats = [ 'json', 'yaml', 'xml' ]
	
	default_style = 'tree'
	default_format = 'json'
	
	@classmethod
	def arguments(cls, parser):
		io_mexafile.arguments(parser)
		parser.add_argument('--style', choices=cls.native_styles.keys(), default=cls.default_style, help='Data layout requested (semantics)')
		parser.add_argument('--format', choices=cls.formats, default=cls.default_format, help='Data format to use (structure)')


	def __init__(self, style, format, *args, **kwargs):
		super(structured_mexafile,self).__init__(*args, **kwargs)
		self.style = style
		self.native = self.native_styles[style](self.mf)
		outstr = getattr(self, format)()
		self.write(outstr)
		
	def json(self):
		"""
		Translate the oplist to a more human readable one, omitting all the classy
		meta data. In Json without hierarchy.
		"""
		
		# JSON pointer reference. Currently unused.
		json_pointer = lambda irhs: { '$ref': '#/'+irhs.canonical() }
		
		import json # batteries included 
		return json.dumps(self.native)

	def yaml(self):
		# placeholder
		yaml_pointer = lambda irhs: { 'YAML-REF': 'towards->'+irhs.canonical() }
		
		# this has to be installed
		import yaml
		return yaml.dump(self.native, default_flow_style=False)

	def xml(self):
		# xml dumps straight the classy structure
		# This is extremely verbose and more suitable as a tech-demo
		# Could also implement a hierarchy writer here
		
		# -> xml currently supports only the linear style!!
		
		# lxml and xml.sax are included in python (batteries)
		from lxml import etree
		from xml.sax.saxutils import escape as xml_escape
		
		root = etree.Element(self.__class__.__name__)
		root.set('style', self.style) # actually, the style is ignored here.
		root.set('query_root', self.root)
		
		def visit(obj, parent, tagname=None):
			if not tagname: tagname = obj.__class__.__name__
			me = etree.SubElement(parent, tagname)
			
			if type(obj) == list:
				for oi in obj:
					visit(oi, me)
			elif hasattr(obj, '_asdict'): # namedtuples instable API gives OrderedDict
				for namedtuple_fieldname, value in obj._asdict().iteritems():
					visit(value, etree.SubElement(me, namedtuple_fieldname))
			elif isinstance(obj, symbol):
				for p in obj.path:
					etree.SubElement(me, 'pathseg').text = p
			else:
				me.text = xml_escape(str(obj)).strip()

		#if self.style == 'linear':
		#	for k in self.native:
		
		# todo: repair the output layout.
		visit(self.native, root)
		return etree.tostring(root, pretty_print=True)

@mexa_cli.register_subcommand('specfile')
class mexafile_to_exahype_specfile(io_mexafile):
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
		
		# this has to be installed:
		import jinja2
		
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
		#	print("Debuggin context:")
		#	pprint.pprint(ctx)
		#	print("Exception:")
		#	print str(e)
		#	raise e	

if __name__ == "__main__":
	cli = mexa_cli()
