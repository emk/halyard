// -*- Mode: C++; tab-width: 4; -*-

#include <algorithm>
#include <fstream>

#include "FileSystem.h"
#include "ImlUnit.h"

using namespace FileSystem;

extern void test_FileSystem (void);

void test_FileSystem (void)
{
	// Test the base directory.
	TEST(GetBaseDirectory() == Path());
	SetBaseDirectory(Path().AddParentComponent());
	TEST(GetBaseDirectory() == Path().AddParentComponent());
	SetBaseDirectory(Path());

	// Test file extensions.
	TEST(Path("foo.bar").GetExtension() == "bar");
	TEST(Path("FOO.BAR").GetExtension() == "bar");
	TEST(Path("foo.baz").GetExtension() == "baz");
	TEST(Path("foo.moby").GetExtension() == "moby");
	TEST(Path("foo.").GetExtension() == "");
	TEST(Path("foo").GetExtension() == "");
	TEST(Path("foo.bar").AddComponent("sample").GetExtension() == "");

	// Test changing file extensions.
	TEST(Path("foo.bar").ReplaceExtension("baz") == Path("foo.baz"));
	TEST(Path("foo.baz").ReplaceExtension("bar") == Path("foo.bar"));
	TEST(Path("foo.").ReplaceExtension("txt") == Path("foo.txt"));
	TEST(Path("foo").ReplaceExtension("txt") == Path("foo.txt"));
	TEST(Path("foo.bar").AddComponent("sample").ReplaceExtension("txt") ==
		 Path("foo.bar").AddComponent("sample.txt"));

	// Test our stat functions.
	TEST(Path("nosuch").DoesExist() == false);
	TEST_EXCEPTION(Path("nosuch").IsRegularFile(), Error);
	TEST_EXCEPTION(Path("nosuch").IsDirectory(), Error);
	TEST(Path("FileSystemTests.cpp").DoesExist() == true);
	TEST(Path("FileSystemTests.cpp").IsRegularFile() == true);
	TEST(Path("FileSystemTests.cpp").IsDirectory() == false);
	TEST(GetFontDirectory().DoesExist() == true);
	TEST(GetFontDirectory().IsRegularFile() == false);
	TEST(GetFontDirectory().IsDirectory() == true);

	// List the contents of a directory.
	std::list<std::string> entries = GetFontDirectory().GetDirectoryEntries();
	TEST(std::find(entries.begin(), entries.end(), "README.txt") !=
		 entries.end());

	// Test file deletion.
	Path deltest("deltest.txt");
	TEST(deltest.DoesExist() == false);
	ofstream deltest_stream(deltest.ToNativePathString().c_str());
	deltest_stream.close();
	TEST(deltest.DoesExist() == true);
	deltest.DeleteFile();
	TEST(deltest.DoesExist() == false);	

	// Make sure the directory isn't contaminated with magic Unix entries.
	TEST(std::find(entries.begin(), entries.end(), ".") == entries.end());
	TEST(std::find(entries.begin(), entries.end(), "..") == entries.end());

	// Do some tricky path manipulation.
	Path tricky = GetFontDirectory().AddParentComponent();
	TEST(tricky.AddComponent("FileSystemTests.cpp").DoesExist());

#if FIVEL_PLATFORM_WINDOWS
#	error "Not yet implemented for Windows."
#elif FIVEL_PLATFORM_MACINTOSH
#	error "Not yet implemented for Macintosh."
#else FIVEL_PLATFORM_OTHER
	
	// Test conversion to native path strings.
	TEST(Path().ToNativePathString() == ".");
	TEST(Path().AddComponent("foo").ToNativePathString() == "./foo");
	TEST(Path().AddParentComponent().ToNativePathString() == "./..");
	TEST(Path().AddParentComponent().AddComponent("f").ToNativePathString() ==
		 "./../f");
	TEST(Path("foo").ToNativePathString() == "./foo");
	TEST(GetBaseDirectory().ToNativePathString() == ".");
	TEST(GetFontDirectory().ToNativePathString() == "./Fonts");
	TEST(GetFontFilePath("README.txt").ToNativePathString() ==
		 "./Fonts/README.txt");

#endif // FIVEL_PLATFORM_*
}
