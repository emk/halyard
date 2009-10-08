// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#include <stdio.h>
#include <windows.h>
#include <boost/filesystem.hpp>
#include <boost/format.hpp>

#include "CommandLine.h"
#include "UpdateInstaller.h"
#include "LogFile.h"
#include "Interface.h"

using namespace boost::filesystem;
using boost::format;

void LaunchProgram(bool update_succeeded, size_t argc, const char **argv) {
    if (argc > 3) {
        // If we're running on Vista, we'll have elevated privileges, and
        // possibly by running in a different user account.  So if it seems
        // advisable, we ask the user to relaunch the program for us.
        if (!IsSafeToRelaunchAutomatically()) {
            AskUserToRelaunch(update_succeeded);
            return;
        }

        // PORTABILITY - needs to be factored to work on platforms other
        // than Windows.
        path executable(argv[3], native);
        path base(argv[1], native);
        if (!exists(executable)) {
            char *paths[] = {"", "engine/win32"};
            char *exe_names[] = {"Halyard", "Tamale"};
            char *suffixes[] = {".exe", "_d.exe"};
            for (int i = 0; i < 2; ++i) {
                for (int j = 0; j < 2; ++j) {
                    for (int k = 0; k < 2; ++k) {
                        std::string filename(std::string(exe_names[j]) + 
                                             suffixes[k]);
                        path exe(base / paths[i] / filename);
                        if (exists(exe))
                            executable = exe;
                    }
                }
            }
        }
        //__debugbreak();
        CommandLine cl(argc-4, const_cast<char**>(argv+4));
        if (!CommandLine::ExecAsync(executable.file_string().c_str(), cl)) {
            printf("Error: Couldn't launch external process: %s\n",
                   cl.WindowsQuotedString().c_str());
            exit(1);
        }
    }   
}

void UpdaterMain(size_t argc, const char **argv) {
    if (argc < 3) {
        printf("Usage: UpdateInstaller srcpath dstpath [command ...]\n");
        exit(1);
    } 

    if (strcmp(argv[1], "--uninstall") == 0) {
        if (argc < 3) {
            printf("Usage: UpdateInstaller --uninstall path\n");
            exit(1);
        } else {
            path root(argv[2], native);
            // Disabled log file--we're supposed to be deleting things, not
            // creating them, and we don't know what directory we should be
            // logging to.
            //LogFile logger(root / "Updates" / "temp" / "log");
            UpdateInstaller::DeleteLockFileForUninstall(root);
            //logger.Log("Uninstall completed.");
            exit(0);
        }
    } 
    
    LogFile::InitLogFile(path(argv[1], native) / "Updates" / "temp" / "log");
    LogFile *logger = LogFile::GetLogFile();
        
    try {
        logger->Log("Checking if install is possible.");
        UpdateInstaller installer = UpdateInstaller(path(argv[1], native),
                                                    path(argv[2], native));

        installer.PrepareForUpdate();
        if (!installer.IsUpdatePossible()) {
            // If we determine, safely, that updating is impossible, we should
            // just relaunch the program.
            // TODO - On Vista, this will show a dialog claiming the update
            // was successful.
            logger->Log("Update is impossible; relaunching.");
            LaunchProgram(false, argc, argv);
            exit(1);
        }

        logger->Log("Install is possible; beginning install.");
        installer.InstallUpdate();
    } catch (std::exception &e) {
        logger->Log(format("Error: %s") % e.what(), LogFile::FATAL);
    } catch (...) {
        logger->Log("Unknown error.", LogFile::FATAL);
    }

    logger->Log("Update installed successfully. Relaunching.");
    LaunchProgram(true, argc, argv);
}

