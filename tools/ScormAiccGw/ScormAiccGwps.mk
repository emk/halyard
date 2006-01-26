
ScormAiccGwps.dll: dlldata.obj ScormAiccGw_p.obj ScormAiccGw_i.obj
	link /dll /out:ScormAiccGwps.dll /def:ScormAiccGwps.def /entry:DllMain dlldata.obj ScormAiccGw_p.obj ScormAiccGw_i.obj \
		kernel32.lib rpcndr.lib rpcns4.lib rpcrt4.lib oleaut32.lib uuid.lib \

.c.obj:
	cl /c /Ox /DWIN32 /D_WIN32_WINNT=0x0400 /DREGISTER_PROXY_DLL \
		$<

clean:
	@del ScormAiccGwps.dll
	@del ScormAiccGwps.lib
	@del ScormAiccGwps.exp
	@del dlldata.obj
	@del ScormAiccGw_p.obj
	@del ScormAiccGw_i.obj
