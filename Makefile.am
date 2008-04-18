ACLOCAL_AMFLAGS = -I m4
AUTOMAKE_OPTIONS = foreign

SUBDIRS = libs Common wx

check:
	(cd test; ../Common/CommonTest)

# Run Common/TestAll over and over again, and only stop when it dumps core.
# We're using this to stress-test the new PLT v372 support to see if we've
# introduced any intermittent GC bugs.
#
# Before using 'make stress', make sure that you have good ventilation and
# cooling for your computer!
stress: all
	(cd test; ulimit -c unlimited; \
         while test 1; do ../Common/CommonTest || break; done)
