// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef AppGlobals_H
#define AppGlobals_H

//////////
// We use this trace mask to debug flicker and other stage drawing problems.
//
#define TRACE_STAGE_DRAWING "STAGE DRAWING"

//////////
// Enumerations for menu items, toolbar buttons, and other command-
// generating widgets.
//
enum {
    FIVEL_EXIT = 1,
    FIVEL_NEW_PROGRAM = 100,
    FIVEL_OPEN_PROGRAM,
    FIVEL_SAVE_PROGRAM,
    FIVEL_RELOAD_SCRIPT,
	FIVEL_EDIT_MODE,
	FIVEL_JUMP_CARD,
	FIVEL_STOP_MOVIES,
    FIVEL_ABOUT = 200,

    FIVEL_SHOW_LOG = 300,
	FIVEL_SHOW_LISTENER,
	FIVEL_SHOW_TIMECODER,
    FIVEL_FULL_SCREEN,
    FIVEL_DISPLAY_XY,
	FIVEL_DISPLAY_GRID,
	FIVEL_DISPLAY_BORDERS,
	FIVEL_PROPERTIES,
	FIVEL_INSERT_BACKGROUND,
	FIVEL_RUN_TESTS,

	FIVEL_STATUS_BAR = 1000,
	FIVEL_TEXT_ENTRY,
	FIVEL_LOCATION_BOX,
	FIVEL_LISTENER_TEXT_ENTRY,
	FIVEL_PROGRAM_TREE,
	FIVEL_PROGRAM_TREE_CTRL
};

//////////
// The frame number of a movie.
//
typedef long MovieFrame;

enum /* MovieFrame */ {
	//////////
	// A constant representing the last frame of a movie.
	//
	LAST_FRAME = -1
};

enum {
	//////////
	// The nominal number of frames/second in a movie.  The real number of
	// frames/second might be higher or lower, but we try to insulate
	// scripts from this fact.
	//
	FRAMES_PER_SECOND = 30
};

// Define this symbol to 1 to help debug redraw problems by making each
// suspicious widget a different, alarming color.
#define FIVEL_USE_UGLY_WINDOW_COLORS 0

// Choose an appropriate set of window colors.
#if FIVEL_USE_UGLY_WINDOW_COLORS
#  define STAGE_FRAME_COLOR      (*wxRED)
#  define STAGE_BACKGROUND_COLOR (*wxCYAN)
#  define STAGE_BACKGROUND_COLOR_NEUTRAL (wxColour(128, 128, 128))
#  define STAGE_COLOR            (*wxGREEN)
#  define MOVIE_WINDOW_COLOR     (wxColour(0, 255, 255))
#else // !FIVEL_USE_UGLY_WINDOW_COLORS
#  define STAGE_FRAME_COLOR      (*wxBLACK)
#  define STAGE_BACKGROUND_COLOR (*wxBLACK)
#  define STAGE_BACKGROUND_COLOR_NEUTRAL (wxColour(128, 128, 128))
#  define STAGE_COLOR            (*wxBLACK)
#  define MOVIE_WINDOW_COLOR     (*wxBLACK)
#endif // !FIVEL_USE_UGLY_WINDOW_COLORS

#endif // AppGlobals_H
