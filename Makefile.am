AUTOMAKE_OPTIONS = foreign

SUBDIRS = libs Common wx

#==========================================================================
#  MzScheme Compilation
#==========================================================================
#  MzScheme has a non-standard build procedure, so we need to do this stuff
#  manually.

all-plt:
	(cd libs/plt/src; $(MAKE) all)
	(cd libs/plt/src; $(MAKE) install)

clean-plt:
	(cd libs/plt/src; $(MAKE) clean)
	(cd libs/plt/bin; rm -f *)
	(cd libs/plt/lib; rm -f *)
