#pragma once

#include "Configuration.h"

#include "TString.h"

BEGIN_NAMESPACE_FIVEL

class	CConfig 
{
	public:
							CConfig();
							~CConfig();
			void			DumpConfiguration(void);
			bool			CheckConfig(void);
			bool			CheckDevices(void);

			bool			FillGraphicsSpec(FSSpec *theSpec, const char *inName);
			bool			FillCLUTSpec(FSSpec *theSpec, const char *inName);
			bool			FillDataSpec(FSSpec *theSpec, const char *inName);
			bool			FillDebugSpec(FSSpec *theSpec, const char *inName);
			bool			FillScriptSpec(FSSpec *theSpec, const char *inName);
			bool			FillSpec(FSSpec *theSpec, TString &inName);
						
			Rect			GetScreenRect(void);
			
			int16			GetBitDepth(void)
						{ return (bitDepth); }
	private:
			Configuration	theConfiguration;
			FSSpec			defaultSpec;
			GDHandle		mainDevice;
			int32			numDevices;
			Rect			mainRect;
			int32			memAlloc;
			int16			bitDepth;
			
			void			DefineDevices(void);
			void			DefineConfiguration(void);
			
			
			
			Boolean 		MAGestaltAttribute(OSType itsAttr, short itsBit);
			short 			NumToolboxTraps(void);
			Boolean			TrapExists(short theTrap);
			TrapType 		GetTrapType(short theTrap);
};
		
extern CConfig *theConfig;

END_NAMESPACE_FIVEL