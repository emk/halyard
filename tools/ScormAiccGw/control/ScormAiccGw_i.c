/* this file contains the actual definitions of */
/* the IIDs and CLSIDs */

/* link this file in with the server and any clients */


/* File created by MIDL compiler version 5.01.0164 */
/* at Mon Feb 13 08:22:41 2006
 */
/* Compiler settings for C:\Interactive Media Lab\SCORMActiveX\ScormAiccGw\ScormAiccGw.idl:
    Oicf (OptLev=i2), W1, Zp8, env=Win32, ms_ext, c_ext
    error checks: allocation ref bounds_check enum stub_data 
*/
//@@MIDL_FILE_HEADING(  )
#ifdef __cplusplus
extern "C"{
#endif 


#ifndef __IID_DEFINED__
#define __IID_DEFINED__

typedef struct _IID
{
    unsigned long x;
    unsigned short s1;
    unsigned short s2;
    unsigned char  c[8];
} IID;

#endif // __IID_DEFINED__

#ifndef CLSID_DEFINED
#define CLSID_DEFINED
typedef IID CLSID;
#endif // CLSID_DEFINED

const IID IID_IScormAiccGwCtl = {0x508517E7,0xB65D,0x4708,{0x86,0x35,0x6A,0xB3,0x36,0x30,0xFE,0x38}};


const IID LIBID_SCORMAICCGWLib = {0xAC5E8105,0xBB92,0x43D2,{0xA9,0xEA,0x2F,0x13,0x01,0x1C,0x69,0x53}};


const IID DIID__IScormAiccGwCtlEvents = {0xF44C829D,0xA4E8,0x4026,{0xA5,0x2C,0x37,0xFA,0xCA,0x21,0x32,0xCD}};


const CLSID CLSID_ScormAiccGwCtl = {0x8B4E4F9B,0xE7D0,0x4DFF,{0x82,0xAC,0xD9,0x2E,0x06,0xE4,0xB8,0x35}};


#ifdef __cplusplus
}
#endif

