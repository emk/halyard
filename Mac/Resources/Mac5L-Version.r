#include "Types.r"

/* The major version number. */
#define MAJOR           0x3

/* The minor version number and revision.  Example: 0x59 is ".5.9".  No
** component may exceed 9, and there's no way to specify a subrevision.
** Do the best you can. */
#define MINOR_AND_REV   0x59

/* The actual version number, as a string.  This should be followed by
** "(Development)", in parentheses, if this is a development series release. */
#define VERSION_STR    "3.5.11 (Development)"

resource 'vers' (1) {
	MAJOR,
	MINOR_AND_REV,
	release,
	0x0,
	0,
	VERSION_STR,
	VERSION_STR ", Copyright 1999-2002 Trustees of Dartmouth College"
};

resource 'vers' (2) {
	MAJOR,
	MINOR_AND_REV,
	release,
	0x0,
	0,
	VERSION_STR,
	"Dartmouth Interactive Media Laboratory"
};
