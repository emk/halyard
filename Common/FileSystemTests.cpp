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

#include "CommonHeaders.h"

#include <fstream>

#include "FileSystem.h"
#include "ImlUnit.h"

using namespace FileSystem;

extern void test_FileSystem (void);

void test_FileSystem (void)
{
#if FIVEL_PLATFORM_WIN32

	// Test conversion to native path strings.
	TEST(Path().ToNativePathString() == ".");
	TEST(Path().AddComponent("foo").ToNativePathString() == ".\\foo");
	TEST(Path().AddParentComponent().ToNativePathString() == ".\\..");
	TEST(Path().AddParentComponent().AddComponent("f").ToNativePathString() ==
		 ".\\..\\f");
	TEST(Path("foo").ToNativePathString() == ".\\foo");
	TEST(GetBaseDirectory().ToNativePathString() == ".");
	TEST(GetFontDirectory().ToNativePathString() == ".\\Fonts");
	TEST(GetFontFilePath("README.txt").ToNativePathString() ==
		 ".\\Fonts\\README.txt");
	TEST(Path("f").AddParentComponent().AddComponent("g").ToNativePathString()
		 == ".\\f\\..\\g");

#elif FIVEL_PLATFORM_MACINTOSH

	// Test conversion to native path strings.
	TEST(Path().ToNativePathString() == ":");
	TEST(Path().AddComponent("foo").ToNativePathString() == ":foo");
	TEST(Path().AddParentComponent().ToNativePathString() == "::");
	TEST(Path().AddParentComponent().AddComponent("f").ToNativePathString() ==
		 "::f");
	TEST(Path("foo").ToNativePathString() == ":foo");
	TEST(GetBaseDirectory().ToNativePathString() == ":");
	TEST(GetFontDirectory().ToNativePathString() == ":Fonts");
	TEST(GetFontFilePath("README.txt").ToNativePathString() ==
		 ":Fonts:README.txt");
//	TEST(Path("f").AddParentComponent().AddComponent("g").ToNativePathString()
//		 == ":g");

#pragma ANSI_strict off
#warning "Macintosh path manipulation still has problems."

#elif FIVEL_PLATFORM_OTHER
	
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
	TEST(Path("f").AddParentComponent().AddComponent("g").ToNativePathString()
	     == "./f/../g");

#else
#	error "Unknown platform."
#endif // FIVEL_PLATFORM_*

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

	// Make sure the directory isn't contaminated with magic Unix entries.
	TEST(std::find(entries.begin(), entries.end(), ".") == entries.end());
	TEST(std::find(entries.begin(), entries.end(), "..") == entries.end());

	// Test file deletion.
	Path deltest("deltest.txt");
	TEST(deltest.DoesExist() == false);
	std::ofstream deltest_stream(deltest.ToNativePathString().c_str());
	deltest_stream.close();
	TEST(deltest.DoesExist() == true);
	deltest.RemoveFile();
	TEST(deltest.DoesExist() == false);

	// Test file renaming.
	Path renametest1("renametest1.txt");
	Path renametest2("renametest2.txt");
	TEST(renametest1.DoesExist() == false);
	TEST(renametest2.DoesExist() == false);
	std::ofstream renametest_stream(renametest1.ToNativePathString().c_str());
	renametest_stream.close();
	TEST(renametest1.DoesExist() == true);
	TEST(renametest2.DoesExist() == false);
	renametest1.RenameFile(renametest2);
	TEST(renametest1.DoesExist() == false);
	TEST(renametest2.DoesExist() == true);
	renametest2.RemoveFile();

	// Test file replacement.
	Path replace_orig("replace_orig.txt");
	Path replace_temp("replace_temp.txt");
	TEST(replace_orig.DoesExist() == false);
	TEST(replace_temp.DoesExist() == false);
	std::ofstream orig_stream(replace_orig.ToNativePathString().c_str());
	orig_stream << "OriginalData" << std::endl;
	orig_stream.close();
	std::ofstream temp_stream(replace_temp.ToNativePathString().c_str());
	temp_stream << "NewData" << std::endl;
	temp_stream.close();
	TEST(replace_orig.DoesExist() == true);
	TEST(replace_temp.DoesExist() == true);
	replace_orig.ReplaceWithTemporaryFile(replace_temp);
	TEST(replace_orig.DoesExist() == true);
	TEST(replace_temp.DoesExist() == false);
	std::ifstream new_stream(replace_orig.ToNativePathString().c_str());
	std::string new_contents;
	new_stream >> new_contents;
	new_stream.close();
	TEST(new_contents == "NewData");
	replace_orig.RemoveFile();

	// Test file creation.
	Path fake_txt("fake.txt");
	TEST(!fake_txt.DoesExist());
	fake_txt.CreateWithMimeType("text/plain");
	TEST(fake_txt.DoesExist());
	fake_txt.RemoveFile();
	Path fake_jpg("fake.jpg");
	TEST(!fake_jpg.DoesExist());
	fake_jpg.CreateWithMimeType("image/jpeg");
	TEST(fake_jpg.DoesExist());
	fake_jpg.RemoveFile();

	// Do some tricky path manipulation.
#if !FIVEL_PLATFORM_MACINTOSH
	Path tricky = GetFontDirectory().AddParentComponent();
	TEST(tricky.AddComponent("FileSystemTests.cpp").DoesExist());
#else
#pragma ANSI_strict off
#warning "Macintosh path manipulation still has problems."
#endif
}
