// ScormAiccGwCtl.h : Declaration of the CScormAiccGwCtl

#ifndef __SCORMAICCGWCTL_H_
#define __SCORMAICCGWCTL_H_

#include "resource.h"       // main symbols
#include <atlctl.h>
#include "ScormAiccGwCP.h"

#define SAFE_TO_SCRIPT  1
#define HIDDEN_WINDOW   0

#if HIDDEN_WINDOW          // hidden window
class CScormAiccGwCtl;

class CHiddenWindow : public CWindowImpl<CHiddenWindow>
{

   BEGIN_MSG_MAP(CHiddenWindow)	
      MESSAGE_HANDLER(WM_TIMER, OnTimer)
   END_MSG_MAP()

public:
   CHiddenWindow(CScormAiccGwCtl* pCtl) : m_pCtl(pCtl)	{ ; }

private:
   CScormAiccGwCtl* m_pCtl;
	LRESULT OnTimer(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
};
#endif

/////////////////////////////////////////////////////////////////////////////
// CScormAiccGwCtl
class ATL_NO_VTABLE CScormAiccGwCtl : 
	public CComObjectRootEx<CComSingleThreadModel>,
	public IDispatchImpl<IScormAiccGwCtl, &IID_IScormAiccGwCtl, &LIBID_SCORMAICCGWLib>,
	public CComControl<CScormAiccGwCtl>,
	public IPersistStreamInitImpl<CScormAiccGwCtl>,
	public IOleControlImpl<CScormAiccGwCtl>,
	public IOleObjectImpl<CScormAiccGwCtl>,
	public IOleInPlaceActiveObjectImpl<CScormAiccGwCtl>,
	public IViewObjectExImpl<CScormAiccGwCtl>,
	public IOleInPlaceObjectWindowlessImpl<CScormAiccGwCtl>,
	public ISupportErrorInfo,
	public IConnectionPointContainerImpl<CScormAiccGwCtl>,
	public IPersistStorageImpl<CScormAiccGwCtl>,
	public ISpecifyPropertyPagesImpl<CScormAiccGwCtl>,
	public IQuickActivateImpl<CScormAiccGwCtl>,
	public IDataObjectImpl<CScormAiccGwCtl>,
	public IProvideClassInfo2Impl<&CLSID_ScormAiccGwCtl, &DIID__IScormAiccGwCtlEvents, &LIBID_SCORMAICCGWLib>,
	public IPropertyNotifySinkCP<CScormAiccGwCtl>,
#if SAFE_TO_SCRIPT
   public IObjectSafetyImpl<CScormAiccGwCtl, INTERFACESAFE_FOR_UNTRUSTED_CALLER>,
#endif
	public CComCoClass<CScormAiccGwCtl, &CLSID_ScormAiccGwCtl>,
	public CProxy_IScormAiccGwCtlEvents< CScormAiccGwCtl >
{
public:
#if HIDDEN_WINDOW                      // hidden window
   CScormAiccGwCtl() : m_wndHidden(this), m_hProcess(NULL) { ; }
#else
   CScormAiccGwCtl() : m_hProcess(NULL) { ; }
#endif

DECLARE_REGISTRY_RESOURCEID(IDR_SCORMAICCGWCTL)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CScormAiccGwCtl)
	COM_INTERFACE_ENTRY(IScormAiccGwCtl)
	COM_INTERFACE_ENTRY(IDispatch)
	COM_INTERFACE_ENTRY(IViewObjectEx)
	COM_INTERFACE_ENTRY(IViewObject2)
	COM_INTERFACE_ENTRY(IViewObject)
	COM_INTERFACE_ENTRY(IOleInPlaceObjectWindowless)
	COM_INTERFACE_ENTRY(IOleInPlaceObject)
	COM_INTERFACE_ENTRY2(IOleWindow, IOleInPlaceObjectWindowless)
	COM_INTERFACE_ENTRY(IOleInPlaceActiveObject)
	COM_INTERFACE_ENTRY(IOleControl)
	COM_INTERFACE_ENTRY(IOleObject)
	COM_INTERFACE_ENTRY(IPersistStreamInit)
	COM_INTERFACE_ENTRY2(IPersist, IPersistStreamInit)
	COM_INTERFACE_ENTRY(ISupportErrorInfo)
	COM_INTERFACE_ENTRY(IConnectionPointContainer)
	COM_INTERFACE_ENTRY(ISpecifyPropertyPages)
	COM_INTERFACE_ENTRY(IQuickActivate)
	COM_INTERFACE_ENTRY(IPersistStorage)
	COM_INTERFACE_ENTRY(IDataObject)
	COM_INTERFACE_ENTRY(IProvideClassInfo)
#if SAFE_TO_SCRIPT
   COM_INTERFACE_ENTRY(IObjectSafety)
#endif
	COM_INTERFACE_ENTRY(IProvideClassInfo2)
	COM_INTERFACE_ENTRY_IMPL(IConnectionPointContainer)
END_COM_MAP()

BEGIN_PROP_MAP(CScormAiccGwCtl)
	PROP_DATA_ENTRY("_cx", m_sizeExtent.cx, VT_UI4)
	PROP_DATA_ENTRY("_cy", m_sizeExtent.cy, VT_UI4)
END_PROP_MAP()

BEGIN_CONNECTION_POINT_MAP(CScormAiccGwCtl)
	CONNECTION_POINT_ENTRY(IID_IPropertyNotifySink)
   // Note that ATL v 3.0 "connection Point Wizard" constructs
   // the following line with an error in it.  Needed to manually
   // prepend 'D' to the argument.  SSB 01-25-2006
	CONNECTION_POINT_ENTRY(DIID__IScormAiccGwCtlEvents)
END_CONNECTION_POINT_MAP()

BEGIN_MSG_MAP(CScormAiccGwCtl)
   MESSAGE_HANDLER(WM_TIMER, OnTimer)
	CHAIN_MSG_MAP(CComControl<CScormAiccGwCtl>)
	DEFAULT_REFLECTION_HANDLER()
END_MSG_MAP()


// ISupportsErrorInfo
	STDMETHOD(InterfaceSupportsErrorInfo)(REFIID riid);

// IViewObjectEx
	DECLARE_VIEW_STATUS(VIEWSTATUS_SOLIDBKGND | VIEWSTATUS_OPAQUE)

private:

   // constants to identify course output files
   enum FileID { Finish,
                 Comments,
                 Objectives,
                 Interactions,
                 Paths,
                 Performance,
                 Invalid };

   BOOL OpenKeyForGUID (BSTR sCourseGuid);
   void CloseKeyForGUID() { RegCloseKey(m_hKey); }
   BOOL isMyCourseRunning();

#if HIDDEN_WINDOW                      // hidden window
	//
   // These next two members exist to allow us
   // to handle messages with a "hidden" window"
   //
   CHiddenWindow m_wndHidden;       // contained hidden window object
	HWND m_hHidden;                  // handle of the hidden window

	UINT	m_uintTimer;
#endif

   BSTR m_sCourseGUID;              // GUID for course
   HKEY m_hKey;                     // handle of registry key
   TCHAR m_szAppName[_MAX_PATH];    // application name
   TCHAR m_szCmdLine[_MAX_PATH];    // command line
   TCHAR m_szWrkDir[_MAX_PATH];     // working directory
   HANDLE m_hProcess;               // handle of launched process

// IScormAiccGwCtl
public:
	STDMETHOD(GetFileContent)(/* [in] */ unsigned int nID, 
                             /*[out,retval]*/ BSTR *psContent);
	STDMETHOD(onIdle)();
	STDMETHOD(isCourseInstalled)(/*[in]*/ BSTR sCourseGuid, 
                                /*[out, retval]*/ BOOL *pbSuccess);
	STDMETHOD(launchCourse)(/*[in]*/ BSTR sCourseGUID, /*[in]*/ BSTR sParams);

	HRESULT OnDraw(ATL_DRAWINFO& di);  
   HRESULT FinalConstruct();
   void FinalRelease();
	LRESULT OnTimer(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);


};

#endif //__SCORMAICCGWCTL_H_
