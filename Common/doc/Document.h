// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Document_H
#define Document_H

#include "Model.h"

class FIVEL_NS TamaleProgram;

BEGIN_NAMESPACE_FIVEL

//////////
// This class represents a Tamale document.  It should have no dependencies
// on the GUI--it's a pure, portable Model class.
//
class Document : public model::Model {
    DISABLE_COPY_AND_ASSIGN(Document);

	static std::string SetBaseAndGetFilePath(const std::string &inDirectory);

public:
	enum Flag { OPEN };

	//////////
	// Create a new document in the specified directory, and make
	// sure that all the appropriate support files are already in place.
	// This should eventually create all the support files, but we're
	// not that clever yet.
	//
    Document(const std::string &inDirectory);

	//////////
	// Open the document in the specified directory.
	//
    Document(const std::string &inDirectory, Flag inOpen);
    ~Document();

	//////////
	// A more-precisely-typed version of GetRoot.
	//
	FIVEL_NS TamaleProgram *GetTamaleProgram();
};

END_NAMESPACE_FIVEL

#endif // Document_H
