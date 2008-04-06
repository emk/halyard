// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#ifndef Document_H
#define Document_H

#include "FileSystem.h"
#include "Model.h"

BEGIN_NAMESPACE_HALYARD

class HalyardProgram;

//////////
/// This class represents a Halyard document.  It should have no dependencies
/// on the GUI--it's a pure, portable Model class.
///
class Document : public model::Model, boost::noncopyable {
	static std::string SetBaseAndGetFilePath(const std::string &inDirectory);

	void CheckStructure();
	void CheckDirectory(FileSystem::Path inPath);
	void CheckFile(FileSystem::Path inPath);

public:
	enum Flag { OPEN };

	//////////
	/// Create a new document in the specified directory, and make
	/// sure that all the appropriate support files are already in place.
	/// This should eventually create all the support files, but we're
	/// not that clever yet.
	///
    Document(const std::string &inDirectory);

	//////////
	/// Open the document in the specified directory.
	///
    Document(const std::string &inDirectory, Flag inOpen);
    ~Document();

	//////////
	/// A more-precisely-typed version of GetRoot.
	///
	Halyard::HalyardProgram *GetHalyardProgram();
};

END_NAMESPACE_HALYARD

#endif // Document_H
