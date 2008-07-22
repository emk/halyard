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

#define _CRT_SECURE_NO_DEPRECATE (1)

// See ScriptEditorDB.cpp for details on boost headers (sob).
#define BOOST_ALL_NO_LIB 1
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#undef BOOST_ALL_NO_LIB

#include "CommonHeaders.h"
#include "TTemplateUtils.h"
#include "TInterpreter.h"

#include <fstream>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

#if APP_PLATFORM_WIN32
#	include <windows.h>
#	define S_ISREG(m) ((m)&_S_IFREG)
#	define S_ISDIR(m) ((m)&_S_IFDIR)
#elif APP_PLATFORM_MACINTOSH || APP_PLATFORM_OTHER
#	include <sys/types.h>
#	include <dirent.h>
#	include <unistd.h>
#else
#	error "Unknown platform."
#endif

#include "FileSystem.h"
#include "TLogger.h"

using namespace Halyard;
using namespace FileSystem;
namespace fs = boost::filesystem;


//=========================================================================
//  Error Methods
//=========================================================================

Error::Error(const char *inErrorFile, int inErrorLine, int inErrorCode)
	: TException(inErrorFile, inErrorLine)
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
		Halyard::gDebugLog.Caution("Unexpected errno = %d", errno);
	errno = 0;
}

// Call this function *after* making a system call which sets errno.
// This will reset errno, and if errno is set, will throw an error.
static void CheckErrno(const char *inFile, int inLine)
{
	// Surprisingly, this function is threadsafe on most platforms.
	// 'errno' isn't really a variable; it's a magic pre-processor
	// define that accesses thread-local state.
	if (errno != 0)
	{
		int temp = (errno);
		errno = 0;
		throw Error(inFile, inLine, temp);
	}
}

#define CHECK_ERRNO() CheckErrno(__FILE__, __LINE__)


//=========================================================================
//  Path Methods
//=========================================================================

#if APP_PLATFORM_WIN32
#	define PATH_SEPARATOR '\\'
#elif APP_PLATFORM_MACINTOSH || APP_PLATFORM_OTHER
#	define PATH_SEPARATOR '/'
#else
#	error "Unknown platform."
#endif

Path::Path()
	: mPath(fs::current_path().native_directory_string())
{
	// All done!
}

Path::Path(const std::string &inPath)
	: mPath(fs::current_path().native_directory_string() +
	        PATH_SEPARATOR + inPath)
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
	extension = Halyard::MakeStringLowercase(extension);
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

	ResetErrno();
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
		throw Error(__FILE__, __LINE__, errno);
	}

	ASSERT(false);
	return false;	
}

bool Path::IsRegularFile() const
{
	struct stat info;
	ResetErrno();
	stat(ToNativePathString().c_str(), &info);
	CHECK_ERRNO();
	return S_ISREG(info.st_mode) ? true : false;
}

bool Path::IsDirectory() const
{
	struct stat info;
	ResetErrno();
	stat(ToNativePathString().c_str(), &info);
	CHECK_ERRNO();
	return S_ISDIR(info.st_mode) ? true : false;
}

#if APP_PLATFORM_WIN32

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
		throw Error(__FILE__, __LINE__, "Can't open directory"); // TODO - GetLastError()

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
			throw Error(__FILE__, __LINE__, "Error reading directory"); // TODO - GetLastError()
	}
	catch (...)
	{
		::FindClose(hFind);
		throw;
	}
	if (!::FindClose(hFind))
		throw Error(__FILE__, __LINE__, "Can't close directory"); // TODO - GetLastError()

	return entries;
}

#elif (APP_PLATFORM_MACINTOSH || APP_PLATFORM_OTHER)

std::list<std::string> Path::GetDirectoryEntries() const
{
	ResetErrno();
	DIR *dir = opendir(ToNativePathString().c_str());
	CHECK_ERRNO();

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
	CHECK_ERRNO();

	closedir(dir);
	CHECK_ERRNO();

	return entries;
}

#else 
#	error "Unknown platform."
#endif // APP_PLATFORM_*

void Path::RemoveFile() const
{
	ResetErrno();
	remove(ToNativePathString().c_str());
	CHECK_ERRNO();
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

void Path::RenameFile(const Path &inNewName) const
{
	ASSERT(!inNewName.DoesExist());

	ResetErrno();
	rename(ToNativePathString().c_str(),
		   inNewName.ToNativePathString().c_str());
	CHECK_ERRNO();	
}

void Path::ReplaceWithTemporaryFile(const Path &inTemporaryFile) const
{
	ASSERT(inTemporaryFile.DoesExist());

	if (DoesExist())
		RemoveFile();
    inTemporaryFile.RenameFile(*this);
}

void Path::CreateWithMimeType(const std::string &inMimeType)
{
    // This used to do something special on MacOS 9 and earlier.
	std::ofstream file(ToNativePathString().c_str());
	file.close();
}

bool FileSystem::operator==(const Path& inLeft, const Path& inRight)
{
	return (inLeft.mPath == inRight.mPath);
}

Path Path::NativePath(const std::string &inPath)
{
	CHECK(inPath != "", "Path cannot be an empty string");
	Path result;

#if APP_PLATFORM_OTHER || APP_PLATFORM_MACINTOSH

	CHECK(inPath.size() > 0 && inPath[0] == PATH_SEPARATOR,
		  ("\'" + inPath + "\' does not begin with a slash").c_str());
	if (inPath.size() > 1 && inPath[inPath.length()-1] == PATH_SEPARATOR)
		result.mPath = inPath.substr(0, inPath.length() - 1);
	else
		result.mPath = inPath;

#elif APP_PLATFORM_WIN32

	if (inPath.size() >= 2 &&
		inPath[inPath.length()-1] == PATH_SEPARATOR &&
		inPath[inPath.length()-2] != PATH_SEPARATOR)
	{
		result.mPath = inPath.substr(0, inPath.length() - 1);
	}
	else
	{
		result.mPath = inPath;
	}

#else
#	error "Unknown platform."
#endif // FILEL_PLATFORM_*

	return result;
}


//=========================================================================
//  Base Directory Methods
//=========================================================================

// THREAD - Global variables.
static Path gCurrentBaseDirectory = Path();
static Path gAppDataDirectory = Path();
static Path gAppLocalDataDirectory = Path();
static std::string gScriptsDirectoryName = "Scripts";
static std::string gScriptDataDirectoryName = "";

Path FileSystem::SetBaseDirectory(const Path &inDirectory)
{
    // Convert our path to an absolute path.
    fs::path path(inDirectory.ToNativePathString(), fs::native);
    fs::path completed(fs::complete(path, fs::current_path()));
    Path base(Path::NativePath(completed.native_directory_string()));

    // Sanity-check our path, store it, and return it.
	CHECK(base.IsDirectory(),
          ("\'" + base.ToNativePathString() +
           "\' is not a valid directory").c_str());
	gCurrentBaseDirectory = base;
	return base;
}

Path FileSystem::SetBaseDirectory(const std::string &inDirectory)
{
	Path base = Path::NativePath(inDirectory);
    return SetBaseDirectory(base);
}

Path FileSystem::GetBaseDirectory()
{
	return gCurrentBaseDirectory;	
}

void FileSystem::SetAppDataDirectory(const std::string &inDirectory) {
    Path data = Path::NativePath(inDirectory);
	CHECK(data.IsDirectory(),
		  ("\'" + inDirectory + "\' is not a valid directory").c_str());
	gAppDataDirectory = data;
}

Path FileSystem::GetAppDataDirectory() {
    return gAppDataDirectory;
}

void FileSystem::SetAppLocalDataDirectory(const std::string &inDirectory) {
    Path data = Path::NativePath(inDirectory);
	CHECK(data.IsDirectory(),
		  ("\'" + inDirectory + "\' is not a valid directory").c_str());
	gAppLocalDataDirectory = data;
}

Path FileSystem::GetAppLocalDataDirectory() {
    return gAppLocalDataDirectory;
}

void FileSystem::SetScriptDataDirectoryName(const std::string &inName) {
    gScriptDataDirectoryName = inName;
}

Path FileSystem::GetScriptDataDirectory() {
    ASSERT(gScriptDataDirectoryName != "");
    return GetAppDataDirectory().AddComponent(gScriptDataDirectoryName);
}

Path FileSystem::GetScriptLocalDataDirectory() {
    ASSERT(gScriptDataDirectoryName != "");
    return GetAppLocalDataDirectory().AddComponent(gScriptDataDirectoryName);
}

Path FileSystem::GetScriptTempDirectory() {
    ASSERT(!TInterpreterManager::IsInRuntimeMode());
    Path result(GetBaseDirectory().AddComponent("temp"));
    fs::path fs_temp(result.ToNativePathString(), fs::native);
    fs::create_directory(fs_temp);
    return result;
}

Path FileSystem::ResolveFontPath(const std::string &inRelPath) {
    fs::path fontdir(FileSystem::GetFontDirectory().ToNativePathString(),
                     fs::native);
    fs::path resolved;
    if (inRelPath == "") {
        resolved = fontdir;
    } else {
        resolved = fs::complete(inRelPath, fontdir);
    }

    // TODO - This always calls native_file_string, even when it should call
    // native_directory_string.  Since we're not running on VMS (or
    // something even more outlandish), this shouldn't give us any problems.
    return Path::NativePath(resolved.native_file_string());
}

void FileSystem::SetScriptsDirectoryName(const std::string &inName) {
    gScriptsDirectoryName = inName;
}

std::string FileSystem::GetScriptsDirectoryName() {
    return gScriptsDirectoryName;
}

FileSystem::Path FileSystem::GetScriptsDirectory() {
    return GetBaseDirectory().AddComponent(gScriptsDirectoryName);
}


//=========================================================================
//  Miscellaneous Utility Methods
//=========================================================================

void FileSystem::ExistenceCheck(const Path &inPath, const bool &inShouldBeDir)
{
	if (inShouldBeDir)
	{
		if (!inPath.DoesExist() || !inPath.IsDirectory())
		{
			Halyard::
			gLog.FatalError("Halyard was unable to open the directory \"%s\".  "
							"Please make sure Halyard is running in the "
							"correct directory, and that all source files are "
							"available.",
							inPath.ToNativePathString().c_str());
		}
	}
	else 
	{	
		if (!inPath.DoesExist() || !inPath.IsRegularFile())
		{
			Halyard::
			gLog.FatalError("Halyard was unable to open the file \"%s\".  "
							"Please make sure Halyard is running in the "
							"correct directory, and that all source files are "
							"available.", 
							inPath.ToNativePathString().c_str());
		}
	}	
}
