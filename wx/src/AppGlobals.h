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

#ifndef AppGlobals_H
#define AppGlobals_H

//////////
/// We use this trace mask to debug flicker and other stage drawing problems.
///
#define TRACE_STAGE_DRAWING "STAGE DRAWING"

//////////
/// Enumerations for menu items, toolbar buttons, and other command-
/// generating widgets.
///
enum {
    HALYARD_EXIT = 1,
    HALYARD_NEW_PROGRAM = 100,
    HALYARD_OPEN_PROGRAM,
    HALYARD_SAVE_PROGRAM,
    HALYARD_EDIT_SCRIPTS,
    HALYARD_RELOAD_SCRIPTS,
	HALYARD_EDIT_MODE,
    HALYARD_EDIT_CARD_SCRIPT,
	HALYARD_JUMP_CARD,
	HALYARD_STOP_MOVIES,
    HALYARD_ABOUT = 200,

    HALYARD_SHOW_LOG = 300,
	HALYARD_SHOW_LISTENER,
	HALYARD_SHOW_TIMECODER,
    HALYARD_FULL_SCREEN,
    HALYARD_DISPLAY_XY,
	HALYARD_DISPLAY_GRID,
	HALYARD_DISPLAY_BORDERS,
    HALYARD_ERRORTRACE_COMPILE,
	HALYARD_PROPERTIES,
	HALYARD_INSERT_BACKGROUND,
	HALYARD_RUN_TESTS,

    // From the script editor.
    HALYARD_CLOSE_TAB,
    HALYARD_SAVE_ALL,
    HALYARD_WRAP_LINES,
    HALYARD_SHOW_WHITESPACE,
    HALYARD_SHOW_LINENUMS,
    HALYARD_EXPAND_ALL,
    HALYARD_FIND_AGAIN,
    HALYARD_FIND_SELECTION,
    HALYARD_FIND_IN_NEXT_FILE,
    HALYARD_REPLACE_AND_FIND_AGAIN,
    HALYARD_GOTO_LINE,
    HALYARD_GOTO_DEFINITION,
    HALYARD_TEXT_SIZE_INC,
    HALYARD_TEXT_SIZE_DEC,

	HALYARD_STATUS_BAR = 1000,
	HALYARD_LOCATION_BOX,
	HALYARD_LISTENER_TEXT_ENTRY,
	HALYARD_PROGRAM_TREE,
	HALYARD_PROGRAM_TREE_CTRL,
    HALYARD_STAGE_TIMER
};

//////////
/// The frame number of a movie.
///
typedef long MovieFrame;

enum /* MovieFrame */ {
	//////////
	/// A constant representing the last frame of a movie.
	///
	LAST_FRAME = -1
};

enum {
	//////////
	/// The nominal number of frames/second in a movie.  The real number of
	/// frames/second might be higher or lower, but we try to insulate
	/// scripts from this fact.
	///
	FRAMES_PER_SECOND = 30
};

// Define this symbol to 1 to help debug redraw problems by making each
// suspicious widget a different, alarming color.
#define HALYARD_USE_UGLY_WINDOW_COLORS 0

// Choose an appropriate set of window colors.
#if HALYARD_USE_UGLY_WINDOW_COLORS
#  define STAGE_FRAME_COLOR      (*wxRED)
#  define STAGE_BACKGROUND_COLOR (*wxCYAN)
#  define STAGE_BACKGROUND_COLOR_NEUTRAL (wxColour(128, 128, 128))
#  define STAGE_COLOR            (*wxGREEN)
#  define MOVIE_WINDOW_COLOR     (wxColour(0, 255, 255))
#else // !HALYARD_USE_UGLY_WINDOW_COLORS
#  define STAGE_FRAME_COLOR      (*wxBLACK)
#  define STAGE_BACKGROUND_COLOR (*wxBLACK)
#  define STAGE_BACKGROUND_COLOR_NEUTRAL (wxColour(128, 128, 128))
#  define STAGE_COLOR            (*wxBLACK)
#  define MOVIE_WINDOW_COLOR     (*wxBLACK)
#endif // !HALYARD_USE_UGLY_WINDOW_COLORS

#endif // AppGlobals_H
