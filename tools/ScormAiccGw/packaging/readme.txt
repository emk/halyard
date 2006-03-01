Steps for packaging the ScormAiccGw ActiveX control
in a signed CAB file.  SSB 03-01-2006

* You need to have the Platform SDK installed, or at 
least have the utilities MAKECERT, CERT2SPC, CABARC 
and SIGNCODE in your path.  As a temporary measure 
I've placed these utilities in '5L/tools/CAB_SIGN'
and '5L/tools/MS Cabinet SDK/BIN'.

* Get a Software Publisher Certificate, or create 
one for testing purposes.  To do the latter, follow 
the instructions under "Making a Test Software 
Publisher Certificate" at
http://msdn2.microsoft.com/en-us/library/aewdt4xt.aspx.
You only need to do that once, and I've done it.  You 
can proceed to the next step and use files Test.cer, 
Test.spc, and TestKey.PVK in 
'5L/tools/packaging/test certificate'.

* Create a signed CAB file that will install the 
control. Refer to documentation at
http://msdn2.microsoft.com/en-us/library/3h8ff753.aspx
and
http://msdn2.microsoft.com/en-us/library/4xy21y81.aspx
To automate the steps, I've provided a batch file 
CreatCAB.bat and information file SAG.inf in 
'5L/tools/packaging' to automate the steps.  It assumes 
a version of ScormAiccGw.dll in
'5L/tools/ScormAiccGw/control/ReleaseMinDependency'.  
If CABARC and SIGNCODE are notin your path, you can 
supply one of two command line arguments to 
CreatCAB.bat, which will identify directories to be 
added to your path environment variable.  Batch file 
SSBCreatCAB.bat invokes CreatCAB with arguments that
cause the utilities temporarily located under '5L/tools'
to be invoked.  The SignCode wizard will open.  I used
all default values to sign the CAB file.


The following documentation was referenced:

"Creating Signed CAB Files for MFC and ATL Controls"
http://msdn2.microsoft.com/en-us/library/4kex18w6.aspx

"Creating a CAB File"
http://msdn2.microsoft.com/en-us/library/3h8ff753.aspx

"Signing a CAB File"
http://msdn2.microsoft.com/en-us/library/4xy21y81.aspx

"Embedding a Signed CAB File on a Web Page"
http://msdn2.microsoft.com/en-us/library/z6h8ccx1.aspx