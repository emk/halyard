// -*- Mode: C++; tab-width: 4; -*-

#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>
#include <list>
#include <map>

#include <assert.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>

#include "Typography.h"
#include "FileSystem.h"

#define ASSERT(x) assert(x)

#define STRERROR_BUFF_SIZE (1024)

using namespace Typography;

bool IsFontFile(const FileSystem::Path &inPath)
{
	std::string extension = inPath.GetExtension();
	return (extension == "pfb" || extension == "pcf" || extension == "ttf");
}

class AvailableFace {
	Library    *mLibrary; // TODO - No ownership, document.
	std::string mFileName;

	int         mSize;
	std::string mFamilyName;
	std::string mStyleName;
	bool        mIsBold;
	bool        mIsItalic;

public:
 	enum { kAnySize = 0 };

	AvailableFace(Library *inLibrary, const string &inFileName);

	int    GetSize() const { return mSize; }
	string GetFamilyName() const { return mFamilyName; }
	string GetStyleName() const { return mStyleName; }
	bool   IsBold() const { return mIsBold; }
	bool   IsItalic() const { return mIsItalic; }
	bool   IsScalable() const { return GetSize() == kAnySize; }

	Face   OpenFace(int inSize) const;

	static void WriteSerializationHeader(std::ostream &out);
	static void ReadSerializationHeader(std::istream &in);

				AvailableFace(std::istream &in);
	void        Serialize(std::ostream &out) const;
};

AvailableFace::AvailableFace(Library *inLibrary, const string &inFileName)
	: mLibrary(inLibrary), mFileName(inFileName)
{
	// Open up our face file.
	FT_Face face;
	std::string path =
		FileSystem::GetFontFilePath(mFileName).ToNativePathString();
	Error::CheckResult(FT_New_Face(*mLibrary, path.c_str(), 0, &face));
				
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

	FileSystem::Path path = FileSystem::GetFontFilePath(mFileName);
	std::string file = path.ToNativePathString();
	FileSystem::Path metrics_path = path.ReplaceExtension("afm");
	std::string metrics_file = metrics_path.ToNativePathString();

	if (metrics_path.DoesExist())
		return Face(*mLibrary, file.c_str(), metrics_file.c_str(), inSize);
	else
		return Face(*mLibrary, file.c_str(), NULL, inSize);
}

void AvailableFace::ReadSerializationHeader(std::istream &in)
{
	// Check our header information.
	string filetype, vers_label;
	int version;
	in >> filetype >> vers_label >> version >> ws;
	if (!in || filetype != "facecache" || vers_label != "vers" || version != 1)
		throw Error(Error::kOtherError); // TODO - Error message
	
	// Discard our human-readable comment line.
	string junk;
	std::getline(in, junk);
	if (!in)
		throw Error(Error::kOtherError); // TODO - Error message		
}

void AvailableFace::WriteSerializationHeader(std::ostream &out)
{
	out << "facecache vers 1" << endl
		<< "FILE|FAMILY|STYLE|SIZE|IS BOLD|IS ITALIC"
		<< endl;
}

AvailableFace::AvailableFace(std::istream &in)
{
	// Read in our individual fields.
	std::string has_metrics, size, is_bold, is_italic;
	std::getline(in, mFileName, '|');
	std::getline(in, mFamilyName, '|');
	std::getline(in, mStyleName, '|');
	std::getline(in, size, '|');
	std::getline(in, is_bold, '|');
	std::getline(in, is_italic);
	if (!in)
		throw Error(Error::kOtherError); // TODO - Error message.

	// Needed so eof() will return true after last record.
	// XXX - Will cause problems if font names begin with spaces.
	in >> std::ws; 
	
	// Convert a few numeric values.  Use ternary operator to
	// convert booleans so MSVC++ doesn't whine at us.
	mSize       = atoi(size.c_str());
	mIsBold     = atoi(is_bold.c_str()) ? true : false;
	mIsItalic   = atoi(is_italic.c_str()) ? true : false;
}

void AvailableFace::Serialize(std::ostream &out) const
{
	// XXX - This will fail if any of our strings contain '|'.
	out << mFileName << '|' << mFamilyName << '|' << mStyleName << '|'
		<< mSize << '|' << mIsBold << '|' << mIsItalic << endl;
}

class FaceSizeGroup {
	std::map<int,AvailableFace> mAvailableFaces;
	std::map<int,Face> mFaces;

public:
	FaceSizeGroup() {}
	
	void AddAvailableFace(const AvailableFace &inFace);
	Face GetFace(int inSize);

	void Serialize(std::ostream &out) const;
};

void FaceSizeGroup::AddAvailableFace(const AvailableFace &inFace)
{
	int size = inFace.GetSize();
	if (mAvailableFaces.find(size) != mAvailableFaces.end())
		throw Error(Error::kOtherError); // TODO - Error message
	mAvailableFaces.insert(std::pair<int,AvailableFace>(size, inFace));
}

Face FaceSizeGroup::GetFace(int inSize)
{
	// First, look for an already instantiated face.
	std::map<int,Face>::iterator foundFace = mFaces.find(inSize);
	if (foundFace != mFaces.end())
		return foundFace->second;
	
	// Next, look for either (1) an available face in the exact size or
	// (2) an available face which can be displayed at any size.
	std::map<int,AvailableFace>::iterator found = mAvailableFaces.find(inSize);
	if (found == mAvailableFaces.end())
		found = mAvailableFaces.find(AvailableFace::kAnySize);

	// If we *still* don't have a face, give up.  If we were feeling
	// very ambitious, we could look for the nearest size and use that.
	if (found == mAvailableFaces.end())
		throw Error(Error::kOtherError); // TODO - Error message

	// Open the face, remember it, and return it.
	Face face = found->second.OpenFace(inSize);
	mFaces.insert(std::pair<int,Face>(inSize, face));
	return face;
}

void FaceSizeGroup::Serialize(std::ostream &out) const
{
	for (std::map<int,AvailableFace>::const_iterator iter =
			 mAvailableFaces.begin();
		 iter != mAvailableFaces.end(); ++iter)
		iter->second.Serialize(out);
}

enum FaceStyle {
	kRegularFaceStyle = 0,
	kBoldFaceStyle = 1,
	kItalicFaceStyle = 2,
	kBoldItalicFaceStyle = kBoldFaceStyle | kItalicFaceStyle
};

class Family {
	string        mFamilyName;

	FaceSizeGroup mRegularFaces;
	FaceSizeGroup mBoldFaces;
	FaceSizeGroup mItalicFaces;
	FaceSizeGroup mBoldItalicFaces;

public:
	Family(const string &inFamilyName) : mFamilyName(inFamilyName) {}

	void AddAvailableFace(const AvailableFace &inFace);
	Face GetFace(FaceStyle inStyle, int inSize);

	void Serialize(std::ostream &out) const;
};

void Family::AddAvailableFace(const AvailableFace &inFace)
{
	ASSERT(mFamilyName == inFace.GetFamilyName());

	// Store the face in the appropriate group.
	if (inFace.IsBold() && inFace.IsItalic())
		mBoldItalicFaces.AddAvailableFace(inFace);
	else if (inFace.IsBold())
		mBoldFaces.AddAvailableFace(inFace);
	else if (inFace.IsItalic())
		mItalicFaces.AddAvailableFace(inFace);
	else
		mRegularFaces.AddAvailableFace(inFace);
}

Face Family::GetFace(FaceStyle inStyle, int inSize)
{
	// We use an elaborate system of recursive fallbacks to find
	// an appropriate face.
	switch (inStyle)
	{
		case kRegularFaceStyle:
			// Fallback: Regular -> Error
			return mRegularFaces.GetFace(inSize);

		case kBoldFaceStyle:
			// Fallback: Bold -> Regular -> Error
			try { return mBoldFaces.GetFace(inSize); }
			catch (...) { return GetFace(kRegularFaceStyle, inSize); }

		case kItalicFaceStyle:
			// Fallback: Italic -> Regular -> Error
			try { return mItalicFaces.GetFace(inSize); }
			catch (...) { return GetFace(kRegularFaceStyle, inSize); }

		case kBoldItalicFaceStyle:
			// Fallback: BoldItalic -> Bold -> Italic -> Regular -> Error
			try { return mBoldItalicFaces.GetFace(inSize); }
			catch (...)
			{ 
				try { return mBoldFaces.GetFace(inSize); }
				catch (...) { return GetFace(kItalicFaceStyle, inSize); }
			}

		default:
			// Illegal style codes!
			throw Error(Error::kOtherError); // TODO - Add error message.
	}
	ASSERT(false);
	return *(Face*) NULL; // This code should NEVER get run.
}

void Family::Serialize(std::ostream &out) const
{
	mRegularFaces.Serialize(out);
	mBoldFaces.Serialize(out);
	mItalicFaces.Serialize(out);
	mBoldItalicFaces.Serialize(out);
}

class FamilyDatabase {
	std::map<string,Family> mFamilyMap;

public:
	FamilyDatabase() {}

	void AddAvailableFace(const AvailableFace &inFace);
	Face GetFace(const string &inFamilyName, FaceStyle inStyle, int inSize);

	FamilyDatabase(std::istream &in);
	void Serialize(std::ostream &out) const;
};

void FamilyDatabase::AddAvailableFace(const AvailableFace &inFace)
{
	string family_name = inFace.GetFamilyName();
	std::map<string,Family>::iterator found = mFamilyMap.find(family_name);
	if (found == mFamilyMap.end())
	{
		mFamilyMap.insert(std::pair<string,Family>(family_name,
												   Family(family_name)));
		found = mFamilyMap.find(family_name);
		ASSERT(found != mFamilyMap.end());
	}
	found->second.AddAvailableFace(inFace);
}

Face FamilyDatabase::GetFace(const string &inFamilyName,
							 FaceStyle inStyle, int inSize)
{
	std::map<string,Family>::iterator found = mFamilyMap.find(inFamilyName);
	if (found != mFamilyMap.end())
		return found->second.GetFace(inStyle, inSize);
	else
		throw Error(Error::kOtherError); // TODO - Add error message.
}

FamilyDatabase::FamilyDatabase(std::istream &in)
{
	AvailableFace::ReadSerializationHeader(in);
	while (!in.eof())
		AddAvailableFace(AvailableFace(in));
}

void FamilyDatabase::Serialize(std::ostream &out) const
{
	AvailableFace::WriteSerializationHeader(out);
	for (std::map<string,Family>::const_iterator iter = mFamilyMap.begin();
		 iter != mFamilyMap.end(); ++iter)
		iter->second.Serialize(out);	
}

#include <strstream>

static string get_string(std::ostrstream &stream)
{
	// Go through the foolish new rigamarole for extracting a string.
	// We must unfreeze the stream before we exit this function, or
	// we'll leak memory.
	stream.freeze(1);
	try
	{
		string str(stream.str(), stream.pcount());
		stream.freeze(0);
		return str;
	}
	catch (...)
	{
		stream.freeze(0);
		throw;
	}
}

int main(int argc, char **argv)
{
	try
	{
		Library library;
		FamilyDatabase familyDatabase;
		
		FileSystem::SetBaseDirectory(FileSystem::Path().AddParentComponent());

		FileSystem::Path fontdir = FileSystem::GetFontDirectory();
		std::list<std::string> entries = fontdir.GetDirectoryEntries();
		for (std::list<std::string>::iterator iter = entries.begin();
			 iter != entries.end(); iter++)
		{
			FileSystem::Path file = fontdir.AddComponent(*iter);
			if (file.IsRegularFile() && IsFontFile(file))
			{
				// Use FreeType to load the face.
				AvailableFace face(&library, *iter);

				// Store the face in our database.
				familyDatabase.AddAvailableFace(face);
			}
		}

		Face f = familyDatabase.GetFace("Times",
										kBoldItalicFaceStyle,
										12);
		familyDatabase.Serialize(std::cout);

		ostrstream outstream;
		familyDatabase.Serialize(outstream);
		string outstring = get_string(outstream);
		istrstream instream(outstring.c_str());
		FamilyDatabase database2(instream);
		ostrstream outstream2;
		database2.Serialize(outstream2);
		ASSERT(outstring == get_string(outstream2));
	}
	catch (...)
	{
		std::cerr << "An error occurred." << endl;
		return 1;
	}

	return 0;
}
