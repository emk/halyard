// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

// WARNING!  This file lives in Common/, but because it requires all sorts
// of exciting platform headers, it's actually built as part of the
// platform-specific engines.  This is a wart.

// Under MSVC++, you'll need to turn off precompiled headers for this file
// by selecting it in the file view, chosing "Settings" from the context
// menu, then "C/C++" > "Precompiled Headers" > "Not using precompiled
// headers".  This allows us to compile this code without begining the
// file with #include "stdafx.h", which won't work on the Mac.

// Needed for RegisterQuickTimePrimitives.
#include "TCommon.h"
#include "TPrimitives.h"
#include "TQTPrimitives.h"

// Need to implement the primitives.
#include <string>
#include <QTML.h>

USING_NAMESPACE_FIVEL


//=========================================================================
//  RegisterQTPrimitives
//=========================================================================
//  Install our portable primitive functions.  These are only required by
//  our old 5L interpreter, not by more reasonable languages.

void FIVEL_NS RegisterQuickTimePrimitives()
{
	REGISTER_5L_PRIMITIVE(QTComponentVersion);
}


//=========================================================================
//  Primitive Implementations
//=========================================================================

//-------------------------------------------------------------------------
// (QTComponentVersion type:STRING subtype:STRING)
//-------------------------------------------------------------------------
// Get the version of the QuickTime component specified by TYPE and
// SUBTYPE.  This allows us to check the versions of our video
// codecs.
//
// TYPE and SUBTYPE are four-character, case-sensitive strings.

DEFINE_5L_PRIMITIVE(QTComponentVersion)
{
	long version = 0;
	OSType type, subtype;
	ComponentInstance ci = NULL;
	OSErr err = noErr;

	// Get our type & subtype.
	std::string type_str, subtype_str;
	inArgs >> type_str >> subtype_str;
	if (type_str.length() != 4 || subtype_str.length() != 4)
	{
		gLog.Caution("QTComponent type and subtype must be four characters.");
		goto done;
	}
	
	// Convert them to OSType values.  We use << to avoid endianness problems.
	type = (type_str[0] << 24 | type_str[1] << 16 |
			type_str[2] << 8 | type_str[3]);
	subtype = (subtype_str[0] << 24 | subtype_str[1] << 16 |
			   subtype_str[2] << 8 | subtype_str[3]);
	
	// Open the component.
	ci = ::OpenDefaultComponent(type, subtype);
	if (!ci)
	{
		gLog.Log("Can't open component %s/%s",
				 type_str.c_str(), subtype_str.c_str());
		goto done;
	}

	// Get the version number.
	version = ::GetComponentVersion(ci);
	if (::GetComponentInstanceError(ci) != noErr)
	{
		gLog.Log("Can't get component version for %s/%s",
				 type_str.c_str(), subtype_str.c_str());
		version = 0;
	}
	
	// Close the component.
	err = ::CloseComponent(ci);
	if (err != noErr)
		gLog.Log("Can't close component %s/%s",
				 type_str.c_str(), subtype_str.c_str());

done:
	::SetPrimitiveResult(version);
}
