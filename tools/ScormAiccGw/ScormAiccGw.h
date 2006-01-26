/* this ALWAYS GENERATED file contains the definitions for the interfaces */


/* File created by MIDL compiler version 5.01.0164 */
/* at Thu Jan 26 11:52:32 2006
 */
/* Compiler settings for C:\Interactive Media Lab\SCORMActiveX\ScormAiccGw\ScormAiccGw.idl:
    Oicf (OptLev=i2), W1, Zp8, env=Win32, ms_ext, c_ext
    error checks: allocation ref bounds_check enum stub_data 
*/
//@@MIDL_FILE_HEADING(  )


/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 440
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif // __RPCNDR_H_VERSION__

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif /*COM_NO_WINDOWS_H*/

#ifndef __ScormAiccGw_h__
#define __ScormAiccGw_h__

#ifdef __cplusplus
extern "C"{
#endif 

/* Forward Declarations */ 

#ifndef __IScormAiccGwCtl_FWD_DEFINED__
#define __IScormAiccGwCtl_FWD_DEFINED__
typedef interface IScormAiccGwCtl IScormAiccGwCtl;
#endif 	/* __IScormAiccGwCtl_FWD_DEFINED__ */


#ifndef ___IScormAiccGwCtlEvents_FWD_DEFINED__
#define ___IScormAiccGwCtlEvents_FWD_DEFINED__
typedef interface _IScormAiccGwCtlEvents _IScormAiccGwCtlEvents;
#endif 	/* ___IScormAiccGwCtlEvents_FWD_DEFINED__ */


#ifndef __ScormAiccGwCtl_FWD_DEFINED__
#define __ScormAiccGwCtl_FWD_DEFINED__

#ifdef __cplusplus
typedef class ScormAiccGwCtl ScormAiccGwCtl;
#else
typedef struct ScormAiccGwCtl ScormAiccGwCtl;
#endif /* __cplusplus */

#endif 	/* __ScormAiccGwCtl_FWD_DEFINED__ */


/* header files for imported files */
#include "oaidl.h"
#include "ocidl.h"

void __RPC_FAR * __RPC_USER MIDL_user_allocate(size_t);
void __RPC_USER MIDL_user_free( void __RPC_FAR * ); 

#ifndef __IScormAiccGwCtl_INTERFACE_DEFINED__
#define __IScormAiccGwCtl_INTERFACE_DEFINED__

/* interface IScormAiccGwCtl */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IScormAiccGwCtl;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("508517E7-B65D-4708-8635-6AB33630FE38")
    IScormAiccGwCtl : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE isCourseInstalled( 
            /* [in] */ BSTR sCourseGuid,
            /* [retval][out] */ BOOL __RPC_FAR *pbSuccess) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE launchCourse( 
            /* [in] */ BSTR sCourseGUID,
            /* [in] */ BSTR sParams) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE onIdle( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetFileContent( 
            /* [in] */ unsigned int nID,
            /* [retval][out] */ BSTR __RPC_FAR *psContent) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IScormAiccGwCtlVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *QueryInterface )( 
            IScormAiccGwCtl __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void __RPC_FAR *__RPC_FAR *ppvObject);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *AddRef )( 
            IScormAiccGwCtl __RPC_FAR * This);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *Release )( 
            IScormAiccGwCtl __RPC_FAR * This);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfoCount )( 
            IScormAiccGwCtl __RPC_FAR * This,
            /* [out] */ UINT __RPC_FAR *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfo )( 
            IScormAiccGwCtl __RPC_FAR * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetIDsOfNames )( 
            IScormAiccGwCtl __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR __RPC_FAR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID __RPC_FAR *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Invoke )( 
            IScormAiccGwCtl __RPC_FAR * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS __RPC_FAR *pDispParams,
            /* [out] */ VARIANT __RPC_FAR *pVarResult,
            /* [out] */ EXCEPINFO __RPC_FAR *pExcepInfo,
            /* [out] */ UINT __RPC_FAR *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *isCourseInstalled )( 
            IScormAiccGwCtl __RPC_FAR * This,
            /* [in] */ BSTR sCourseGuid,
            /* [retval][out] */ BOOL __RPC_FAR *pbSuccess);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *launchCourse )( 
            IScormAiccGwCtl __RPC_FAR * This,
            /* [in] */ BSTR sCourseGUID,
            /* [in] */ BSTR sParams);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *onIdle )( 
            IScormAiccGwCtl __RPC_FAR * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetFileContent )( 
            IScormAiccGwCtl __RPC_FAR * This,
            /* [in] */ unsigned int nID,
            /* [retval][out] */ BSTR __RPC_FAR *psContent);
        
        END_INTERFACE
    } IScormAiccGwCtlVtbl;

    interface IScormAiccGwCtl
    {
        CONST_VTBL struct IScormAiccGwCtlVtbl __RPC_FAR *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IScormAiccGwCtl_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IScormAiccGwCtl_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IScormAiccGwCtl_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IScormAiccGwCtl_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IScormAiccGwCtl_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IScormAiccGwCtl_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IScormAiccGwCtl_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IScormAiccGwCtl_isCourseInstalled(This,sCourseGuid,pbSuccess)	\
    (This)->lpVtbl -> isCourseInstalled(This,sCourseGuid,pbSuccess)

#define IScormAiccGwCtl_launchCourse(This,sCourseGUID,sParams)	\
    (This)->lpVtbl -> launchCourse(This,sCourseGUID,sParams)

#define IScormAiccGwCtl_onIdle(This)	\
    (This)->lpVtbl -> onIdle(This)

#define IScormAiccGwCtl_GetFileContent(This,nID,psContent)	\
    (This)->lpVtbl -> GetFileContent(This,nID,psContent)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IScormAiccGwCtl_isCourseInstalled_Proxy( 
    IScormAiccGwCtl __RPC_FAR * This,
    /* [in] */ BSTR sCourseGuid,
    /* [retval][out] */ BOOL __RPC_FAR *pbSuccess);


void __RPC_STUB IScormAiccGwCtl_isCourseInstalled_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IScormAiccGwCtl_launchCourse_Proxy( 
    IScormAiccGwCtl __RPC_FAR * This,
    /* [in] */ BSTR sCourseGUID,
    /* [in] */ BSTR sParams);


void __RPC_STUB IScormAiccGwCtl_launchCourse_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IScormAiccGwCtl_onIdle_Proxy( 
    IScormAiccGwCtl __RPC_FAR * This);


void __RPC_STUB IScormAiccGwCtl_onIdle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IScormAiccGwCtl_GetFileContent_Proxy( 
    IScormAiccGwCtl __RPC_FAR * This,
    /* [in] */ unsigned int nID,
    /* [retval][out] */ BSTR __RPC_FAR *psContent);


void __RPC_STUB IScormAiccGwCtl_GetFileContent_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IScormAiccGwCtl_INTERFACE_DEFINED__ */



#ifndef __SCORMAICCGWLib_LIBRARY_DEFINED__
#define __SCORMAICCGWLib_LIBRARY_DEFINED__

/* library SCORMAICCGWLib */
/* [helpstring][version][uuid] */ 


EXTERN_C const IID LIBID_SCORMAICCGWLib;

#ifndef ___IScormAiccGwCtlEvents_DISPINTERFACE_DEFINED__
#define ___IScormAiccGwCtlEvents_DISPINTERFACE_DEFINED__

/* dispinterface _IScormAiccGwCtlEvents */
/* [helpstring][uuid] */ 


EXTERN_C const IID DIID__IScormAiccGwCtlEvents;

#if defined(__cplusplus) && !defined(CINTERFACE)

    MIDL_INTERFACE("F44C829D-A4E8-4026-A52C-37FACA2132CD")
    _IScormAiccGwCtlEvents : public IDispatch
    {
    };
    
#else 	/* C style interface */

    typedef struct _IScormAiccGwCtlEventsVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *QueryInterface )( 
            _IScormAiccGwCtlEvents __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void __RPC_FAR *__RPC_FAR *ppvObject);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *AddRef )( 
            _IScormAiccGwCtlEvents __RPC_FAR * This);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *Release )( 
            _IScormAiccGwCtlEvents __RPC_FAR * This);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfoCount )( 
            _IScormAiccGwCtlEvents __RPC_FAR * This,
            /* [out] */ UINT __RPC_FAR *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfo )( 
            _IScormAiccGwCtlEvents __RPC_FAR * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetIDsOfNames )( 
            _IScormAiccGwCtlEvents __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR __RPC_FAR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID __RPC_FAR *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Invoke )( 
            _IScormAiccGwCtlEvents __RPC_FAR * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS __RPC_FAR *pDispParams,
            /* [out] */ VARIANT __RPC_FAR *pVarResult,
            /* [out] */ EXCEPINFO __RPC_FAR *pExcepInfo,
            /* [out] */ UINT __RPC_FAR *puArgErr);
        
        END_INTERFACE
    } _IScormAiccGwCtlEventsVtbl;

    interface _IScormAiccGwCtlEvents
    {
        CONST_VTBL struct _IScormAiccGwCtlEventsVtbl __RPC_FAR *lpVtbl;
    };

    

#ifdef COBJMACROS


#define _IScormAiccGwCtlEvents_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define _IScormAiccGwCtlEvents_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define _IScormAiccGwCtlEvents_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define _IScormAiccGwCtlEvents_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define _IScormAiccGwCtlEvents_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define _IScormAiccGwCtlEvents_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define _IScormAiccGwCtlEvents_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)

#endif /* COBJMACROS */


#endif 	/* C style interface */


#endif 	/* ___IScormAiccGwCtlEvents_DISPINTERFACE_DEFINED__ */


EXTERN_C const CLSID CLSID_ScormAiccGwCtl;

#ifdef __cplusplus

class DECLSPEC_UUID("8B4E4F9B-E7D0-4DFF-82AC-D92E06E4B835")
ScormAiccGwCtl;
#endif
#endif /* __SCORMAICCGWLib_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

unsigned long             __RPC_USER  BSTR_UserSize(     unsigned long __RPC_FAR *, unsigned long            , BSTR __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserMarshal(  unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserUnmarshal(unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * ); 
void                      __RPC_USER  BSTR_UserFree(     unsigned long __RPC_FAR *, BSTR __RPC_FAR * ); 

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif
