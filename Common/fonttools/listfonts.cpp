// -*- Mode: C++; tab-width: 4; -*-

#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>
#include <list>

#include <assert.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>

#include "Typography.h"

#define ASSERT(x) assert(x)

#define STRERROR_BUFF_SIZE (1024)

using namespace Typography;

void HandlePosixError ()
{
	char buffer[STRERROR_BUFF_SIZE];
	strerror_r(errno, buffer, STRERROR_BUFF_SIZE);
	std::cerr << "listfonts: error: " << buffer << endl;
	exit(1);
}

std::string FileExtension (const std::string &inFileName)
{
	std::string::size_type dotpos = inFileName.rfind('.');
	if (dotpos == std::string::npos)
		return std::string("");
	std::string extension = inFileName.substr(dotpos + 1);
	transform(extension.begin(), extension.end(), extension.begin(), tolower);
	return extension;
}

std::string FileWithoutExtension (const std::string &inFileName)
{
	std::string::size_type dotpos = inFileName.rfind('.');
	if (dotpos == std::string::npos)
		return inFileName;
	return inFileName.substr(0, dotpos);
}

std::string FileReplaceExtension (const std::string &inFileName,
								  const std::string &inFileExtension)
{
	return FileWithoutExtension(inFileName) + "." + inFileExtension;
}

bool IsFontFile(const std::string &inFileName)
{
	std::string extension = FileExtension(inFileName);
	return (extension == "pfb" || extension == "pcf" || extension == "ttf");
}

std::list<std::string> ReadDirectory (std::string inDirName)
{
	DIR *dir = opendir(inDirName.c_str());
	if (dir == NULL)
		HandlePosixError();

	std::list<std::string> entries;	
	for (struct dirent *entry = readdir(dir);
		 entry != NULL; entry = readdir(dir))
	{
		if (entry->d_name != "." && entry->d_name != "..")
			entries.push_back(entry->d_name);
	}
	if (errno)
		HandlePosixError();

	if (closedir(dir) < 0)
		HandlePosixError();

	return entries;
}

bool IsRegularFile (const string &inFileName)
{
	struct stat info;
	if (stat(inFileName.c_str(), &info) < 0)
		HandlePosixError();
	return S_ISREG(info.st_mode);
}

bool IsDirectory (const string &inFileName)
{
	struct stat info;
	if (stat(inFileName.c_str(), &info) < 0)
		HandlePosixError();
	return S_ISDIR(info.st_mode);
}

bool FileExists (const string &inFileName)
{
	struct stat info;

	int result = stat(inFileName.c_str(), &info);
	if (result >= 0)
		return true;
	else if (result < 0 && errno == ENOENT)
		return false;

	HandlePosixError();
	ASSERT(false);
	return false;	
}

class AvailableFace {
	Library    *mLibrary; // TODO - No ownership, document.
	std::string mFileName;
	std::string mMetricsFileName;
	bool        mHasMetrics;

	int         mSize;
	std::string mFamilyName;
	std::string mStyleName;
	bool        mIsBold;
	bool        mIsItalic;

public:
 	enum { kAnySize = 0 };
	static const string kNoMetrics;

	AvailableFace(Library *inLibrary,
				  string inFileName,
				  string inMetricsFileName = kNoMetrics);
	
	int    GetSize() const { return mSize; }
	string GetFamilyName() const { return mFamilyName; }
	string GetStyleName() const { return mStyleName; }
	bool   IsBold() const { return mIsBold; }
	bool   IsItalic() const { return mIsItalic; }
	bool   IsScalable() const { return GetSize() == kAnySize; }

	Face   OpenFace(int inSize) const;
};

const string AvailableFace::kNoMetrics = "";

AvailableFace::AvailableFace(Library *inLibrary,
							 string inFileName,
							 string inMetricsFileName /* = kNoMetrics */)
	: mLibrary(inLibrary),
	  mFileName(inFileName),
	  mMetricsFileName(inMetricsFileName),
	  mHasMetrics(inMetricsFileName != kNoMetrics)
{
	// Open up our face file.
	FT_Face face;
	Error::CheckResult(FT_New_Face(*mLibrary, mFileName.c_str(), 0, &face));
				
	try
	{
		// Extract some useful information about the face.
		mFamilyName = face->family_name;
		mStyleName = face->style_name;

		// Extract style flags from the face.
		mIsBold = (face->style_flags & FT_STYLE_FLAG_BOLD) ? true : false;
		mIsItalic = (face->style_flags & FT_STYLE_FLAG_ITALIC) ? true : false;

		// Fix the style flags a of a few commonly-used fonts which we
		// believe to be bogus.  Feel free to add more.  Yes, font drivers
		// often need to special case particular fonts; it's ugly.
		if (mFamilyName == "URW Gothic L" &&
			(mStyleName == "Demi" || mStyleName == "Demi Oblique"))
			mIsBold = true;

		// Figure out the font's size.
		if (FT_IS_SCALABLE(face))
			mSize = kAnySize;
		else
		{
			// Search for a fixed size we can use.
			bool found_size = false;
			for (FT_Int i = 0; i < face->num_fixed_sizes; i++) {
				if (face->available_sizes[i].height == 
					face->available_sizes[i].width)
				{
					found_size = true;
					mSize = face->available_sizes[i].height;
					break;
				}
			}

			// If we didn't find any sizes, get cranky.
			if (!found_size)
				throw Error(FT_Err_Invalid_File_Format);
		}
	}
	catch (...)
	{
		FT_Done_Face(face);
		throw;
	}
	Error::CheckResult(FT_Done_Face(face));
}

Face AvailableFace::OpenFace(int inSize) const
{
	ASSERT(inSize != kAnySize && inSize > 0);
	ASSERT(mSize == kAnySize || mSize == inSize);
	if (mHasMetrics)
		return Face(*mLibrary, mFileName.c_str(),
					mMetricsFileName.c_str(), inSize);
	else
		return Face(*mLibrary, mFileName.c_str(), NULL, inSize);
}

/*
class FamilyStyle {
	
};
*/

int main (int argc, char **argv)
{
	try
	{
		Library library;
		
		std::list<std::string> entries = ReadDirectory("../Fonts");
		for (std::list<std::string>::iterator iter = entries.begin();
			 iter != entries.end(); iter++)
		{
			std::string filename = *iter;
			if (IsRegularFile("../Fonts/" + filename) && IsFontFile(filename))
			{
				// Look for a metrics file.
				std::string metrics = FileReplaceExtension(filename, "afm");
				if (!FileExists("../Fonts/" + metrics))
					metrics = AvailableFace::kNoMetrics;

				// Use FreeType to load the face.
				AvailableFace face(&library, "../Fonts/" + filename, metrics);
									
				// Display some useful information about the face.
				std::cout << filename;
				if (metrics != AvailableFace::kNoMetrics)
					std::cout << "/" << metrics;
				std::cout << ": " << face.GetFamilyName() << " ("
						  << face.GetStyleName() << ") ";
				if (face.GetSize() == AvailableFace::kAnySize)
					std::cout << "ANY ";
				else
					std::cout << face.GetSize() << " ";
				if (face.IsBold())
					std::cout << "B";
				if (face.IsItalic())
					std::cout << "I";
				std::cout << endl;
			}
		}
	}
	catch (...)
	{
		std::cerr << "An error occurred." << endl;
		return 1;
	}

	return 0;
}
