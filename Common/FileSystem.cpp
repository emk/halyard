// -*- Mode: C++; tab-width: 4; -*-

#include <algorithm>

#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>

#include "TCommon.h"
#include "FileSystem.h"

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

std::ostream &FileSystem::operator<<(std::ostream &out, const Error &error)
{
	out << "FileSystem::Error (" << error.GetErrorCode() << "): "
		<< error.GetErrorMessage();
}

// Call this function before making a system call which sets errno.
// This will zero any pre-existing errno value, and warn the programmer
// about it.
static void ResetErrno()
{
	ASSERT(errno == 0); // TODO - Log a warning instead of failing.
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

#if FIVEL_PLATFORM_WINDOWS
#	define PATH_SEPARATOR '\\'
#elif FIVEL_PLATFORM_MACINTOSH
#	define PATH_SEPARATOR ':'
#else
#	define PATH_SEPARATOR '/'
#endif

Path::Path()
	: mPath(".")
{
	// All done!
}

Path::Path(const std::string &inPath)
	: mPath("./" + inPath)
{
	ASSERT(inPath.find(PATH_SEPARATOR) == std::string::npos);
}

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
	transform(extension.begin(), extension.end(), extension.begin(), tolower);
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
		Error(errno);
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
	return S_ISREG(info.st_mode);
}

bool Path::IsDirectory() const
{
	struct stat info;
	ResetErrno();
	stat(ToNativePathString().c_str(), &info);
	CheckErrno();
	return S_ISDIR(info.st_mode);
}

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
		string name = entry->d_name;
		if (name != "." && name != "..")
			entries.push_back(name);
	}
	CheckErrno();

	closedir(dir);
	CheckErrno();

	return entries;
}

void Path::DeleteFile() const
{
	ResetErrno();
	remove(ToNativePathString().c_str());
	CheckErrno();
}

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

std::string Path::ToNativePathString () const
{
	return mPath;
}

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
