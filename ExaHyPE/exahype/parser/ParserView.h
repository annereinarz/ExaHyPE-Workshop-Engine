/**
 * This file is part of the ExaHyPE project.
 * Copyright (c) 2016  http://exahype.eu
 * All rights reserved.
 *
 * The project has received funding from the European Union's Horizon
 * 2020 research and innovation programme under grant agreement
 * No 671698. For copyrights and licensing, please consult the webpage.
 *
 * Released under the BSD 3 Open Source License.
 * For the full license text, see LICENSE.txt
 **/

#ifndef EXAHYPE_PARSER_ParserView_H
#define EXAHYPE_PARSER_ParserView_H

namespace exahype {
namespace parser {
class Parser;
class ParserView;
}
}

#include <iostream>

#include <map>
#include <vector>
#include <utility> // pair
#include <istream>
#include <ostream>

#include "peano/utils/Globals.h"
#include "tarch/la/Vector.h"
#include "tarch/logging/Log.h"

/**
 * View on the parser
 *
 * An instance of this class is a parser view. While the parser sees the
 * whole specification file, a view only 'sees' the fragment that is
 * specific to one solver. As such, we do pass it to solvers (that hold
 * constants) and then allow these solvers to read their data from the
 * config file.
 *
 * From the user's point of view, this class provides access to key-value
 * pairs. If you have an instruction alike
 * <pre>
 *   constants         = {rho:0.4567,gamma:-4,alpha:4.04e-5,file:output}
 * </pre>
 * in your
 *
 * @author Tobias Weinzierl
 */
class exahype::parser::ParserView {
private:
  static tarch::logging::Log _log;
  const exahype::parser::Parser* _parser;
  std::string _basePath;

public:
  ParserView(const exahype::parser::Parser* parser, std::string basePath);

  /** These two functions had to be introduced since ParserView
   *  was quickly introduced to all the plotters. It is in principle
   *  not something one really wants -.-
   **/
  
  // This is certainly not something we want, but copying Parser is cheap
  // and basePath is a string, so it's not too bad.
  ParserView(const ParserView& b) = default;
  
  ParserView() : _parser(nullptr), _basePath("") {}
  
  std::string getPath(const std::string& key) const;
  
  /**
   * An empty ParserView is either one constructed like ParserView() or
   * refering to a Parser object which holds no subdata at all.
   **/
  bool isEmpty() const;

  /**
   * You may use keys without a value. This operation allows you to check
   * whether there are such keys. Furthermore, you might use this guy as
   * a preamble to the other getters.
   */
  bool hasKey(const std::string& key) const;

  /**
   * Please ensure that isValueValidXXX holds before you invoke this
   * operation.
   */
  bool getValueAsBool(const std::string& key) const;

  /**
   * Please ensure that isValueValidXXX holds before you invoke this
   * operation.
   */
  int getValueAsInt(const std::string& key) const;

  /**
   * Please ensure that isValueValidXXX holds before you invoke this
   * operation.
   */
  double getValueAsDouble(const std::string& key) const;

  /**
   * Please ensure that isValueValidXXX holds before you invoke this
   * operation.
   */
  std::string getValueAsString(const std::string& key) const;
  
  bool getValueAsBoolOrDefault(const std::string& key, bool default_value) const;
  int getValueAsIntOrDefault(const std::string& key, int default_value) const;
  double getValueAsDoubleOrDefault(const std::string& key, double default_value) const;
  std::string getValueAsStringOrDefault(const std::string& key, std::string default_value) const;

  /**
   * Returns the contents of this ParserView as a map, i.e. the keys
   * mapped to the values which remain as strings (uncasted to their
   * native description, i.e. bool,int,double).
   *
   * @see getAllAsOrderedMap()
   **/
  std::map<std::string, std::string> getAllAsMap() const;

  /**
   * Returns the contents of this ParserView as an "ordered map", i.e
   * a vector of key -> value pairs.
   *
   * @see getAllAsMap()
   **/
  std::vector< std::pair<std::string, std::string> > getAllAsOrderedMap() const;

  bool isValueValidBool(const std::string& key) const;
  bool isValueValidInt(const std::string& key) const;
  bool isValueValidDouble(const std::string& key) const;
  bool isValueValidString(const std::string& key) const;

  /**
   * Access the Parser. Can be helpful for obtaining information beyond the
   * solver constants which are represented by this object.
   **/
  const exahype::parser::Parser& getParser() const;

  /**
   * Returns a short string representation of this ParserView. It mentions the
   * Parser Filename as well as the ParserView's solver name.
   **/
  std::string toString() const;

  /**
   * \see toString()
   **/
  void toString(std::ostream& out) const;
  
  /**
   * Not a short but a long string
   **/
  std::string dump(const std::string path="") const;
};

#endif
