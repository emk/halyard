Notes on creating a test Software Publisher Certificate
SSB
02-21-2006


You can use the MAKECERT and CERT2SPC utilities provided in the CAB&SIGN directory on the Visual C++ 5.0 CD to make a test Software Publisher Certificate. Note that this test SPC is not valid for software publishing, but can be used to test code signing. 

To make a private key file called TestKey.PVK and a company certificate called Test.CER, run the MAKECERT utility with the following command:

   MAKECERT -u:TestKey -n:CN=IML -k:TestKey.PVK Test.cer

To create a test Software Publisher Certificate (SPC) called TEST.SPC, run the CERT2SPC utility with the following command:

   ROOT.CER test.cer test.spc

This runs a wizard which installs the certificate.  I chose the "untrusted" store.

The batch file CreatSPC.bat invokes the two commands described above.
