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

// ScormAiccGwCtl.h : Declaration of the CScormAiccGwCtl

#ifndef __SCORMAICCGWCTL_H_
#define __SCORMAICCGWCTL_H_

#include "resource.h"       // main symbols
#include <atlctl.h>
#include "ScormAiccGwCP.h"

#define SAFE_TO_SCRIPT  1

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
	public IPersistPropertyBagImpl<CScormAiccGwCtl>,
	public ISpecifyPropertyPagesImpl<CScormAiccGwCtl>,
	public IQuickActivateImpl<CScormAiccGwCtl>,
	public IDataObjectImpl<CScormAiccGwCtl>,
	public IProvideClassInfo2Impl<&CLSID_ScormAiccGwCtl, &DIID__IScormAiccGwCtlEvents, &LIBID_SCORMAICCGWLib>,
	public IPropertyNotifySinkCP<CScormAiccGwCtl>,
#if SAFE_TO_SCRIPT
   public IObjectSafetyImpl<CScormAiccGwCtl, INTERFACESAFE_FOR_UNTRUSTED_CALLER | INTERFACESAFE_FOR_UNTRUSTED_DATA>,
#endif
	public CComCoClass<CScormAiccGwCtl, &CLSID_ScormAiccGwCtl>,
	public CProxy_IScormAiccGwCtlEvents< CScormAiccGwCtl >
{
public:
	CScormAiccGwCtl();

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
	COM_INTERFACE_ENTRY(IPersistPropertyBag)
	COM_INTERFACE_ENTRY_IMPL(IConnectionPointContainer)
END_COM_MAP()

BEGIN_PROP_MAP(CScormAiccGwCtl)
	PROP_DATA_ENTRY("_cx", m_sizeExtent.cx, VT_UI4)
	PROP_DATA_ENTRY("_cy", m_sizeExtent.cy, VT_UI4)
	PROP_ENTRY("CourseGUID", 1, CLSID_NULL)
	PROP_ENTRY("CourseParams", 2, CLSID_NULL)
END_PROP_MAP()

BEGIN_CONNECTION_POINT_MAP(CScormAiccGwCtl)
	CONNECTION_POINT_ENTRY(IID_IPropertyNotifySink)
   // Note that ATL v 3.0 "connection Point Wizard" constructs
   // the following line with an error in it.  Needed to manually
   // prepend 'D' to the argument.  SSB 01-25-2006
	CONNECTION_POINT_ENTRY(DIID__IScormAiccGwCtlEvents)
END_CONNECTION_POINT_MAP()

BEGIN_MSG_MAP(CScormAiccGwCtl)
	MESSAGE_HANDLER(WM_CREATE, OnCreate)
	MESSAGE_HANDLER(WM_SETFOCUS, OnSetFocus)
	CHAIN_MSG_MAP(CComControl<CScormAiccGwCtl>)
ALT_MSG_MAP(1)                   
   // alternate message map
   // This maps messages from contained window,
   // i.e. the button, to the parent control's
   // maessage handlers
	MESSAGE_HANDLER(WM_LBUTTONDOWN, OnClicked)
	MESSAGE_HANDLER(WM_TIMER, OnTimer)
END_MSG_MAP()


	LRESULT OnSetFocus(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, 
                    BOOL& /*bHandled*/);
	STDMETHOD(SetObjectRects)(LPCRECT prcPos,LPCRECT prcClip);
	LRESULT OnClicked(UINT /*uMsg*/, WPARAM /*wParam*/, 
                     LPARAM /*lParam*/, BOOL& /*bHandled*/);

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

   BOOL OpenKeyForGUID();
   void CloseKeyForGUID() { RegCloseKey(m_hKey); }
   BOOL isMyCourseRunning();
   BOOL CreateTempDir();
   BOOL DestroyTempDir();
   void SizeToLabel();

	CContainedWindow m_ctlButton;
   CComBSTR m_sCourseGUID;          // GUID for course
   CComBSTR m_sCourseParams;        // params for course
   HKEY m_hKey;                     // handle of registry key
   TCHAR m_szAppName[_MAX_PATH];    // application name
   TCHAR m_szCmdLine[_MAX_PATH];    // command line
   TCHAR m_szWrkDir[_MAX_PATH];     // working directory
   TCHAR m_szTmpDir[_MAX_PATH];     // temporary directory
   TCHAR m_szButtonLab[_MAX_PATH];  // button label
   HANDLE m_hProcess;               // handle of launched process
   UINT m_nTimer;                   // timer ID

// IScormAiccGwCtl
public:
	STDMETHOD(get_CourseParams)(/*[out, retval]*/ BSTR *pVal);
	STDMETHOD(put_CourseParams)(/*[in]*/ BSTR newVal);
	STDMETHOD(get_CourseGUID)(/*[out, retval]*/ BSTR *pVal);
	STDMETHOD(put_CourseGUID)(/*[in]*/ BSTR newVal);
	STDMETHOD(isCourseInstalled)(/*[out, retval]*/ BOOL *pbSuccess);
	STDMETHOD(GetFileContent)(/* [in] */ unsigned int nID, 
                             /*[out,retval]*/ BSTR *psContent);

   HRESULT FinalConstruct();
   void FinalRelease();
	LRESULT OnTimer(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);


};

#endif //__SCORMAICCGWCTL_H_
