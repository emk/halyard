// -*- Mode: C++; tab-width: 4; -*-

#include "TCommon.h"

#include <algorithm>
#include <fstream>

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

#if FIVEL_PLATFORM_WIN32
#	include <windows.h>
#	define S_ISREG(m) ((m)&_S_IFREG)
#	define S_ISDIR(m) ((m)&_S_IFDIR)
#elif FIVEL_PLATFORM_MACINTOSH
#	include <dirent.h>
#	include <unistd.h>
#elif FIVEL_PLATFORM_OTHER
#	include <sys/types.h>
#	include <dirent.h>
#	include <unistd.h>
#else
#	error "Unknown platform."
#endif

#include "FileSystem.h"
#include "TLogger.h"

using namespace FileSystem;


//=========================================================================
//  Error Methods
//=========================================================================

Error::Error(int inErrorCode)
{
	// THREAD - Use strerror because strerror_r appears broken on some
	// platforms (include Linux?).
	SetErrorMessage(strerror(inErrorCode));
	SetErrorCode(inErrorCode);
}

// Call this function before making a system call which sets errno.
// This will zero any pre-existing errno value, and warn the programmer
// about it.
static void ResetErrno()
{
	if (errno != 0)
		FIVEL_NS gDebugLog.Caution("Unexpected errno = %d", errno);
	errno = 0;
}

// Call this function *after* making a system call which sets errno.
// This will reset errno, and if errno is set, will throw an error.
static void CheckErrno()
{
	// Surprisingly, this function is threadsafe on most platforms.
	// 'errno' isn't really a variable; it's a magic pre-processor
	// define that accesses thread-local state.
	if (errno != 0)
	{
		int temp = (errno);
		errno = 0;
		throw Error(temp);
	}
}


//=========================================================================
//  Path Methods
//=========================================================================

#if FIVEL_PLATFORM_WIN32
#	define PATH_SEPARATOR '\\'
#elif FIVEL_PLATFORM_MACINTOSH
#	define PATH_SEPARATOR ':'
#elif FIVEL_PLATFORM_OTHER
#	define PATH_SEPARATOR '/'
#else
#	error "Unknown platform."
#endif

#if (FIVEL_PLATFORM_WIN32 || FIVEL_PLATFORM_OTHER)

Path::Path()
	: mPath(".")
{
	// All done!
}

Path::Path(const std::string &inPath)
	: mPath(std::string(".") + PATH_SEPARATOR + inPath)
{
	ASSERT(inPath.find(PATH_SEPARATOR) == std::string::npos);
}

#elif FIVEL_PLATFORM_MACINTOSH

Path::Path()
	: mPath(":")
{
	// All done!
}

Path::Path(const std::string &inPath)
	: mPath(std::string(":") + inPath)
{
	ASSERT(inPath.find(PATH_SEPARATOR) == std::string::npos);
}

#else
#	error "Unknown platform!"
#endif // FIVEL_PLATFORM_*

static std::string::size_type find_extension_dot(const std::string &inPath)
{
	// Make sure we only find '.' characters in the filename, not in
	// the names of any parent directories.
	std::string::size_type dotpos = inPath.rfind('.');
	std::string::size_type seppos = inPath.rfind(PATH_SEPARATOR);
	if (dotpos == std::string::npos || dotpos > seppos)
		return dotpos;
	else
		return std::string::npos;
}

std::string Path::GetExtension() const
{
	std::string::size_type dotpos = find_extension_dot(mPath);
	if (dotpos == std::string::npos)
		return std::string("");
	std::string extension = mPath.substr(dotpos + 1);
	std::transform(extension.begin(), extension.end(), extension.begin(), tolower);
	return extension;
}

Path Path::ReplaceExtension(std::string inNewExtension) const
{	
	std::string::size_type dotpos = find_extension_dot(mPath);
	std::string without_extension;
	if (dotpos == std::string::npos)
		without_extension = mPath;
	else 
		without_extension = mPath.substr(0, dotpos);
	Path newPath;
	newPath.mPath = without_extension + "." + inNewExtension;
	return newPath;
}

#if FIVEL_PLATFORM_MACINTOSH

//////////
// The Metrowerks Standard Library (MSL) contains a weird stat() function.
// When called on a non-existant file, 'stat' returns an error code of -1,
// but fails to set ENOENT.  We attempt to patch around this.
//
static int mac_stat(const char *inFileName, struct stat *outInfo)
{
	int result = stat(inFileName, outInfo);
	if (result < 0 && errno == 0)
		errno = ENOENT;
	return result;
}

#define stat(x,y) mac_stat(x,y)

#endif // FIVEL_PLATFORM_*

bool Path::DoesExist() const
{
	struct stat info;

	int result = stat(ToNativePathString().c_str(), &info);
	if (result >= 0)
	{
		return true;
	}
	else if (result < 0 && errno == ENOENT)
	{
		errno = 0;
		return false;
	}
	else
	{
		throw Error(errno);
	}

	ASSERT(false);
	return false;	
}

bool Path::IsRegularFile() const
{
	struct stat info;
	ResetErrno();
	stat(ToNativePathString().c_str(), &info);
	CheckErrno();
	return S_ISREG(info.st_mode) ? true : false;
}

bool Path::IsDirectory() const
{
	struct stat info;
	ResetErrno();
	stat(ToNativePathString().c_str(), &info);
	CheckErrno();
	return S_ISDIR(info.st_mode) ? true : false;
}

#if FIVEL_PLATFORM_WIN32

std::list<std::string> Path::GetDirectoryEntries() const
{
	// Allocate some storage.
	std::list<std::string> entries;	

	// Create a Windows file search object.  We should never get an empty
	// directory because of the "." and ".." entries.
	WIN32_FIND_DATA find_data;
	HANDLE hFind = ::FindFirstFile((ToNativePathString() + "\\*").c_str(),
								   &find_data);
	if (hFind == INVALID_HANDLE_VALUE)
		throw Error("Can't open directory"); // TODO - GetLastError()

	// Make sure we close our WIN32_FIND_DATA correctly.
	try
	{
		do
		{
			// Add our directory entry to the list.
			std::string name = find_data.cFileName;
			if (name != "." && name != "..")
				entries.push_back(name);
		} while (::FindNextFile(hFind, &find_data));

		// Check for any errors reading the directory.
		if (::GetLastError() != ERROR_NO_MORE_FILES)
			throw Error("Error reading directory"); // TODO - GetLastError()
	}
	catch (...)
	{
		::FindClose(hFind);
		throw;
	}
	if (!::FindClose(hFind))
		throw Error("Can't close directory"); // TODO - GetLastError()

	return entries;
}

#elif (FIVEL_PLATFORM_MACINTOSH || FIVEL_PLATFORM_OTHER)

std::list<std::string> Path::GetDirectoryEntries() const
{
	ResetErrno();
	DIR *dir = opendir(ToNativePathString().c_str());
	CheckErrno();

	std::list<std::string> entries;	
	for (struct dirent *entry = readdir(dir);
		 entry != NULL; entry = readdir(dir))
	{
		// Be careful to skip useless magic entries when running
		// on Unix.
		std::string name = entry->d_name;
		if (name != "." && name != "..")
			entries.push_back(name);
	}
	CheckErrno();

	closedir(dir);
	CheckErrno();

	return entries;
}

#else 
#	error "Unknown platform."
#endif // FIVEL_PLATFORM_*

void Path::RemoveFile() const
{
	ResetErrno();
	remove(ToNativePathString().c_str());
	CheckErrno();
}

#if (FIVEL_PLATFORM_WIN32 || FIVEL_PLATFORM_OTHER)

Path Path::AddComponent(const std::string &inFileName) const
{
	ASSERT(inFileName != "." && inFileName != "..");
	ASSERT(inFileName.find(PATH_SEPARATOR) == std::string::npos);
	Path newPath;
	newPath.mPath = mPath + PATH_SEPARATOR + inFileName;
	return newPath;
}

Path Path::AddParentComponent() const
{
	Path newPath;
	newPath.mPath = mPath + PATH_SEPARATOR + "..";
	return newPath;	
}

#elif FIVEL_PLATFORM_MACINTOSH

static const std::string ensure_trailing_colon(const std::string &inString)
{
	// Path names always contain at least ':'.
	ASSERT(inString.end() > inString.begin());
	if (*(inString.end() - 1) == ':')
		return inString;
	else
		return inString + ':';
}

Path Path::AddComponent(const std::string &inFileName) const
{
	ASSERT(inFileName.find(PATH_SEPARATOR) == std::string::npos);
	Path newPath;
	newPath.mPath = ensure_trailing_colon(mPath) + inFileName;
	return newPath;
}

Path Path::AddParentComponent() const
{
	Path newPath;
	newPath.mPath = ensure_trailing_colon(mPath) + PATH_SEPARATOR;
	return newPath;	
}

#else 
#	error "Unknown platform."
#endif // FIVEL_PLATFORM_*

std::string Path::ToNativePathString () const
{
	return mPath;
}

void Path::RenameFile(const Path &inNewName) const
{
	ASSERT(!inNewName.DoesExist());

	ResetErrno();
	rename(ToNativePathString().c_str(),
		   inNewName.ToNativePathString().c_str());
	CheckErrno();	
}

void Path::ReplaceWithTemporaryFile(const Path &inTemporaryFile) const
{
	ASSERT(inTemporaryFile.DoesExist());

	if (DoesExist())
		RemoveFile();
inTemporaryFile.RenameFile(*this);
}

#if FIVEL_PLATFORM_WIN32 || FIVEL_PLATFORM_OTHER

void Path::CreateWithMimeType(const std::string &inMimeType)
{
	std::ofstream file(ToNativePathString().c_str());
	file.close();
}

#elif FIVEL_PLATFORM_MACINTOSH

#include <TextUtils.h>
#include <Files.h>
#include <Script.h>
#include <Resources.h>

#define TEXT_PLAIN_TYPE ('TEXT')
#ifdef DEBUG
	// Developers want text files to open in a real editor...
#	define TEXT_PLAIN_CREATOR ('R*ch')
#else
	// ...but users may have nothing better than TeachText.
#	define TEXT_PLAIN_CREATOR ('ttxt')
#endif // DEBUG

// This function taken from the MacUtils.cpp file in the old
// Mac engine.
static bool PathToFSSpec(const char *inPath, FSSpec *inSpec)
{
	Str255		thePath;
	OSErr		err;
	bool		retValue = true;	// cbo - why is it not returning noErr?
	
	strcpy((char *) thePath, inPath);
	c2pstr((char *) thePath);
	
	if ((err = ::FSMakeFSSpec(0, 0, thePath, inSpec)) == noErr)
		retValue = true;
		
	return (retValue);
}

void Path::CreateWithMimeType(const std::string &inMimeType)
{
	// We could be really classy and call Internet Config to map the MIME
	// type to a creator/type pair.  But this will work for now.
	if (inMimeType == "text/plain")
	{
		FSSpec spec;
		if (PathToFSSpec(ToNativePathString().c_str(), &spec))
		{
			::FSpCreateResFile(&spec, TEXT_PLAIN_CREATOR,
							   TEXT_PLAIN_TYPE, smRoman);
			if (ResError() == noErr)
				return;
		}
	}
	
	std::ofstream file(ToNativePathString().c_str());
	file.close();
}

#else
#	error "Unknown FiveL platform."
#endif // FILEL_PLATFORM_*

bool FileSystem::operator==(const Path& inLeft, const Path& inRight)
{
	return (inLeft.mPath == inRight.mPath);
}


//=========================================================================
//  Base Directory Methods
//=========================================================================

// THREAD - Global variable.
static Path gCurrentBaseDirectory = Path();

void FileSystem::SetBaseDirectory(const Path &inDirectory)
{
	gCurrentBaseDirectory = inDirectory;
}

Path FileSystem::GetBaseDirectory()
{
	return gCurrentBaseDirectory;	
}
