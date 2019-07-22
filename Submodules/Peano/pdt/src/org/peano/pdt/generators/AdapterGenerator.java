package org.peano.pdt.generators;

import org.peano.pdt.analysis.DepthFirstAdapter;

import org.peano.pdt.node.*;


public class AdapterGenerator extends DepthFirstAdapter {
  private org.peano.pdt.generators.DirectoryGenerator _directoryGenerator;
  private org.peano.pdt.TranslationTable              _translationTable;
  private java.util.Vector<String>                    _mappings;
  private java.util.Vector<String>                    _fullQualifiedMappings;
  private java.util.Vector<String>                    _mappingsPath;
  /**
   * We need this one only for security checks
   */
  private java.util.Set<String>                       _handledAdapters;
  private String                                      _currentAdapterName;
 
  
  private int                                         _numberOfErrors;

  public AdapterGenerator(
    org.peano.pdt.generators.DirectoryGenerator directoryGenerator,
    org.peano.pdt.TranslationTable              translationTable
  ) {
    _directoryGenerator = directoryGenerator;
    _translationTable   = translationTable;

    _handledAdapters    = new java.util.HashSet<String>();
    _numberOfErrors     = 0;
  }

  
  public boolean wasSuccessful() {
    return _numberOfErrors==0;
  }
  
  /**
   * We clear the tables (_mappings, e.g.), befill them upon call backs. 
   * outAAdapter then created the actual adapter.
   */
  public void inAAdapter(AAdapter node) {
    _mappings              = new java.util.Vector<String>();
    _fullQualifiedMappings = new java.util.Vector<String>();
    _mappingsPath          = new java.util.Vector<String>();

    _currentAdapterName = node.getName().getText().trim();
	
	if (_handledAdapters.contains(_currentAdapterName)) {
      System.err.println( "\nERROR: Adapter " + _currentAdapterName + " is specified twice. You are not allowed to have two adapters with the same name" );
      _numberOfErrors++;
	}
    else {
      _handledAdapters.add( _currentAdapterName );
	}
  }

  
  public void inAPredefinedUseMapping(APredefinedUseMapping node)
  {
    String  adapterType = node.getName().toString().trim();
    String  adapterName = _currentAdapterName + "2" + adapterType + "_" + _mappings.size();

    if (_mappings.contains(adapterName)) {
      System.err.println( "\nWARNING: Mapping " + adapterName + " is used twice in adapter " + _currentAdapterName + ". Are you sure this is on purpose?" );
    }
	_mappings.add( adapterName );
	
	_fullQualifiedMappings.add( "adapters::" + adapterName );
	_mappingsPath.add( "adapters/" + adapterName );

    java.util.ArrayList<String> paramList = new java.util.ArrayList<String>();
    java.util.ArrayList<String> valueList = new java.util.ArrayList<String>();
    
    for (int i=0; i<node.getParameters().size(); i++) {
      paramList.add( "PARAM" + i );
      valueList.add( node.getParameters().get(i).toString().trim() );
    }

    _translationTable.setThisTypenameToAdapterTypename(adapterName);
    
    _translationTable.convertTemplateFile( 
      adapterType + "Header.template",
      _directoryGenerator.getAdaptersDirectory() + "/" + adapterName + ".h",
      paramList,
      valueList,
      true,
      false
    );
    _translationTable.convertTemplateFile( 
      adapterType + "Implementation.template",
      _directoryGenerator.getAdaptersDirectory() + "/" + adapterName + ".cpp",
      paramList,
      valueList,
      true,
      false
    );
  }

  
  public void inAUserdefinedUseMapping(AUserdefinedUseMapping node)
  {
    String  adapterType = node.getName().toString().trim();

    if (_mappings.contains(adapterType)) {
      System.err.println( "\nWARNING: Mapping " + adapterType + " is used twice in adapter " + _currentAdapterName + ". Are you sure this is on purpose?" );
    }

	_mappings.add( adapterType );
	_fullQualifiedMappings.add( "mappings::" + adapterType );
	_mappingsPath.add( "mappings/" + adapterType );
  }

  
  /**
   * Has to be in the out as I wanna get informed about all predefined adapters used
   * as well, i.e. we have to do this throughout the backtracking.
   */
  public void outAAdapter(AAdapter node) {
    _translationTable.setThisTypenameToAdapterTypename( _currentAdapterName, _mappings, _fullQualifiedMappings, _mappingsPath );

    _translationTable.convertTemplateFile( 
      "AdapterHeader.template",
      _directoryGenerator.getAdaptersDirectory() + "/" + _currentAdapterName + ".h",
      true, true
    );
    _translationTable.convertTemplateFile( 
      "AdapterImplementation.template",
      _directoryGenerator.getAdaptersDirectory() + "/" + _currentAdapterName + ".cpp",
      true, true
    );
  }
}
