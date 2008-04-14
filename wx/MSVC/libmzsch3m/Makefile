# This file is intended for use with Microsoft's nmake.  It can be called
# from a Visual Studio 'makefile' project to build PLT Scheme.
#
# This is not a very clever Makefile, especially with respect to the 'cd'
# command.  If any nmake experts would like to improve this, it would be
# greatly appreciated.

all: stage2

# Since PLT doesn't give us a decent 'make clean' target, we ask git to do
# the dirty work for us.  The command 'git clean -Xfd' will delete all
# files and directories stored under libs/plt that are covered by an ignore
# rule.
clean:
	cd ..\..\..\libs\plt
	git clean -Xfd
	cd ..\..\wx\MSVC\libmzsch3m

stage1:
	cd ..\..\..\libs\plt\src\worksp\mzscheme
	devenv mzscheme.sln /build Release
	cd ..\mred
	devenv mred.sln /build Release
	cd ..\..\..\..\..\wx\MSVC\libmzsch3m

stage2: stage1
	cd ..\..\..\libs\plt\src\worksp\gc2
	..\..\..\mzschemecgc -r make.ss
	cd ..\..\..\..\..\wx\MSVC\libmzsch3m
	copy ..\..\..\libs\plt\lib\libmzsch3mxxxxxxx.dll ..\..\..\Win32\Bin
