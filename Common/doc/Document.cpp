// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TCommon.h"
#include "Document.h"
#include "TInterpreter.h"
#include "FileSystem.h"

USING_NAMESPACE_FIVEL
using namespace model;

//=========================================================================
//  Document Layout
//=========================================================================
//  A document currently has the following directory structure:
//
//    MyDocument/
//      Runtime/   - Program-independent Scheme runtime files
//      Fonts/     - Program-independent fonts
//      Script/    - Program-specific scripts
//      baseq2/    - Program-specific Quake 2 data (optional)
//      Graphics/  - Program-specific graphics
//      Media/     - Program-specific audio/video (optional)
//      Data/      - Saved user data
//      data.tam   - Saved XML data
//
//  This will change as we revamp the engine.  For now, we can only open
//  one document per application.  This should also change.


//=========================================================================
//  Document Format
//=========================================================================

enum {
    CURRENT_FORMAT = 0,
    COMPATIBLE_BACK_TO = 0,
    EARLIEST_READABLE = 0
};

const static ModelFormat gTamaleFormat("TamaleProgram", CURRENT_FORMAT,
									   COMPATIBLE_BACK_TO);


//=========================================================================
//  Document Methods
//=========================================================================

Document::Document(const std::string &inDirectory)
    : Model(gTamaleFormat)
{
	FileSystem::SetBaseDirectory(inDirectory);
    SaveAs(FileSystem::Path("data.tam").ToNativePathString().c_str());
	TInterpreterManager::GetInstance()->BeginScript();
}

Document::Document(const std::string &inDirectory, Flag inOpen)
    : Model(gTamaleFormat, EARLIEST_READABLE,
			FileSystem::Path("data.tam").ToNativePathString().c_str())
{
	FileSystem::SetBaseDirectory(inDirectory);
	TInterpreterManager::GetInstance()->BeginScript();    
}

Document::~Document()
{
    
}

