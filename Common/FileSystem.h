// -*- Mode: C++; tab-width: 4; -*-

#ifndef FileSystem_H
#define FileSystem_H

#include <iostream>
#include <string>
#include <list>

//////////
// Portable interface to various filesystem functions.  Over time,
// it might be good to refactor more code into this library.
//
namespace FileSystem {

	//////////
	// A file-system related error. 
	// TODO - Refactor and combine with Typography::Error?
	//
	class Error {
		int mErrorCode;
		string mErrorMessage;

	public:
		Error(int inErrorCode);
		
		int GetErrorCode() const { return mErrorCode; }
		string GetErrorMessage() const { return mErrorMessage; }

		friend std::ostream &operator<<(std::ostream &out, const Error &error);
	};
	
	//////////
	// A portable representation of a pathname.  Not yet complete.
	//
	class Path {
		std::string mPath;

	public:
		//////////
		// Create a path to the current working directory.
		//
		Path();

		//////////
		// Create a path from a string.
		// TODO - Specify string format.
		// TODO - Check for trailing directory separator.
		// 
		Path(const std::string &inPath);
		
		//////////
		// Return the extension of the last element of the path, if
		// it appears to have one.  Otherwise, return "".
		//
		std::string GetExtension() const;

		//////////
		// Change the extension of a path, or add one if none exists.
		//
		// [in] inNewExtension - The extension to use.
		// [out] return - A path with the new extension.
		//
		Path ReplaceExtension(std::string inNewExtension) const;

		//////////
		// Does this path point to an actual object on disk?
		//
		bool DoesExist() const;

		//////////
		// Does this path point to a file on disk?
		//
		bool IsRegularFile() const;

		//////////
		// Does this path point to a directory on disk?
		//
		bool IsDirectory() const;

		//////////
		// Read in all the entries from a directory.
		//
		std::list<std::string> GetDirectoryEntries() const;

		//////////
		// Delete the file pointed to by this path.
		//
		void DeleteFile() const;

		//////////
		// Add a component to the end of the path (as though the
		// the path were a directory).
		//
		// [in] inFileName - The component to add to the path.
		// [out] return - The path plus the new component.
		//
		Path AddComponent(const std::string &inFileName) const;

		//////////
		// Add a "parent directory" component to the end of
		// the path.  This may have exciting behavior in the
		// presence of symlinks.
		// 
		Path AddParentComponent() const;
		
		//////////
		// Convert a Path object into a local path string.  This isn't
		// entirely correct on the Macintosh (where two files can have the
		// same pathname if two hard drives have the same name), but we
		// need it to interface to third-party libraries that assume paths
		// are unique.
		//
		std::string ToNativePathString () const;

		//////////
		// Compare two paths for equality.  This is a dumb test--it doesn't
		// know anything about simplifying paths, and it doesn't look at
		// the disk.  It just does a memberwise comparison.  It isn't
		// good for much besides the test suites, actually.
		//
		friend bool operator==(const Path& inLeft, const Path& inRight);
	};

	//////////
	// Set the base directory for the application.  Defaults to
	// the current working directory.  Used by GetFontDirectory,
	// GetFontFilePath, etc.
	//
	void SetBaseDirectory(const Path &inDirectory);

	//////////
	// Return the current base directory for this application.
	//
	Path GetBaseDirectory();

	//////////
	// Get the directory 5L uses to store fonts.  (Eventually there
	// will be more of these functions, and we might combine them
	// into one function with a string argument.)
	//
	inline Path GetFontDirectory()
	    { return GetBaseDirectory().AddComponent("Fonts"); }

	//////////
	// Given a file name, return a path pointing to a file with
	// that name in the font directory.
	//
	inline Path GetFontFilePath(std::string inFontFileName)
	    { return GetFontDirectory().AddComponent(inFontFileName); }
};

#endif // FileSystem_H
