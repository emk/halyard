/* For information on editing this file, see the MPW worksheet in this directory. */

#include "Types.r"
#include "SysTypes.r"
#include "Palettes.r"
#include "AEUserTermTypes.r"

/* --- Begin DeRez Output --- */

data 'ALRT' (128, "About Box", purgeable) {
	$"002E 001D 0092 0117 0080 4444"                      /* .....’...€DD */
};

data 'ALRT' (204, "Low Memory Warning", purgeable) {
	$"0068 0082 00CD 01E8 00CC 4444"                      /* .h.‚.Í.è.ÌDD */
};

resource 'ALRT' (2001, "Internal Error") {
	{38, 34, 298, 454},
	129,
	{	/* array: 4 elements */
		/* [1] */
		OK, visible, sound1,
		/* [2] */
		OK, visible, sound1,
		/* [3] */
		OK, visible, sound1,
		/* [4] */
		OK, visible, sound1
	},
	alertPositionParentWindow
};

data 'ALRT' (2002, "Mount CD") {
	$"0028 0028 00F0 0118 07D2 5555"                      /* .(.(.ð...ÒUU */
};

data 'ALRT' (2003, "Redoscript Error") {
	$"0028 0028 00F9 012E 07D3 5555"                      /* .(.(.ù...ÓUU */
};

data 'ALRT' (2050, "Change Memory") {
	$"0028 0028 00F5 0194 0802 5555"                      /* .(.(.õ.”..UU */
};

data 'ALRT' (2060, "Change Bit Depth") {
	$"0029 0028 0106 0195 080C 5555"                      /* .).(...•..UU */
};

resource 'BNDL' (128) {
	'MC5L',
	0,
	{	/* array TypeArray: 2 elements */
		/* [1] */
		'FREF',
		{	/* array IDArray: 1 elements */
			/* [1] */
			0, 131
		},
		/* [2] */
		'ICN#',
		{	/* array IDArray: 1 elements */
			/* [1] */
			0, 128
		}
	}
};

resource 'DITL' (128, purgeable) {
	{	/* array DITLarray: 2 elements */
		/* [1] */
		{67, 96, 87, 154},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{11, 36, 60, 219},
		StaticText {
			disabled,
			"              Mac5L\n   Interactive Media"
			" Lab\nDartmouth Medical School"
		}
	}
};

resource 'DITL' (129) {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{230, 340, 250, 400},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{10, 70, 27, 231},
		StaticText {
			disabled,
			"Internal Mac5L Error:"
		},
		/* [3] */
		{40, 70, 218, 398},
		StaticText {
			disabled,
			"^0"
		}
	}
};

resource 'DITL' (204, purgeable) {
	{	/* array DITLarray: 2 elements */
		/* [1] */
		{71, 288, 91, 348},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{10, 75, 58, 348},
		StaticText {
			disabled,
			"Memory is getting full. Please try to al"
			"leviate the problem by closing some docu"
			"ments. "
		}
	}
};

resource 'DITL' (2002) {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{168, 158, 188, 216},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{168, 83, 188, 141},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{55, 21, 139, 215},
		StaticText {
			disabled,
			"Please insert the CD: \n^0"
		}
	}
};

resource 'DITL' (2003) {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{168, 158, 188, 216},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{168, 83, 188, 141},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{63, 29, 147, 223},
		StaticText {
			disabled,
			"To hang around hit OK. To Quit the progr"
			"am hit Cancel."
		}
	}
};

resource 'DITL' (2050) {
	{	/* array DITLarray: 2 elements */
		/* [1] */
		{172, 281, 192, 341},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{66, 19, 150, 345},
		StaticText {
			disabled,
			"This program needs more memory to run. F"
			"rom the Finder, select the program's ico"
			"n and select \"Get Info...\" from the File"
			" menu. Enter 4096 into the Minimum size:"
			" box."
		}
	}
};

resource 'DITL' (2060) {
	{	/* array DITLarray: 2 elements */
		/* [1] */
		{181, 288, 201, 348},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{74, 23, 161, 350},
		StaticText {
			disabled,
			"You do not have enough memory allocated "
			"to the program for the number of colors "
			"used by your monitor. Either allocate mo"
			"re memory or lower the number of colors."
			" For millions of colors you need to set "
			"Minimum size to 6500. "
		}
	}
};

resource 'FREF' (128) {
	'APPL',
	0,
	""
};

resource 'FREF' (129) {
	'fold',
	1,
	""
};

resource 'FREF' (130) {
	'****',
	2,
	""
};

resource 'FREF' (131) {
	'APPL',
	0,
	""
};

resource 'ICN#' (128) {
	{	/* array: 2 elements */
		/* [1] */
		$"FFFF FFFF FDAD 555D FFF5 AAB9 FFFE F575"
		$"FFEB BEE9 FFFF E3F5 FFFE E1D5 FF07 E7FB"
		$"FE00 047D FC40 08BD FD10 10BF FE08 00FF"
		$"FDC0 01DF F8E0 03CF F1E0 033F F7A0 1EDF"
		$"F3E0 05BF F960 07FF F960 0DFF F160 3FFF"
		$"E7E0 2FFF CFE0 37FF CFF0 1FFF EFF0 FFFF"
		$"FFF8 7FFF FFFE 7FFF FFFC 1FFF FFF0 FFFF"
		$"FFFB FFFF FFFF FFFF FFFF FFFF FFFF FFFF",
		/* [2] */
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
	}
};

resource 'ICON' (128) {
	$"FFFF FFFF FDAD 555D FFF5 AAB9 FFFE F575"
	$"FFEB BEE9 FFFF E3F5 FFFE E1D5 FF07 E7FB"
	$"FE00 047D FC40 08BD FD10 10BF FE08 00FF"
	$"FDC0 01DF F8E0 03CF F1E0 033F F7A0 1EDF"
	$"F3E0 05BF F960 07FF F960 0DFF F160 3FFF"
	$"E7E0 2FFF CFE0 37FF CFF0 1FFF EFF0 FFFF"
	$"FFF8 7FFF FFFE 7FFF FFFC 1FFF FFF0 FFFF"
	$"FFFB FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
};

data 'MC5L' (0, "Owner resource") {
	$"054D 6163 354C"                                     /* .Mac5L */
};

resource 'STR#' (200, "Standards", purgeable) {
	{	/* array StringArray: 2 elements */
		/* [1] */
		"Mac5L",
		/* [2] */
		"Save File As:"
	}
};

resource 'aete' (0, "English") {
	0x1,
	0x0,
	english,
	roman,
	{	/* array Suites: 4 elements */
		/* [1] */
		"Required Suite",
		"Terms that every application should supp"
		"ort",
		'reqd',
		1,
		1,
		{	/* array Events: 0 elements */
		},
		{	/* array Classes: 0 elements */
		},
		{	/* array ComparisonOps: 0 elements */
		},
		{	/* array Enumerations: 0 elements */
		},
		/* [2] */
		"Standard Suite",
		"Common terms for most applications",
		'CoRe',
		1,
		1,
		{	/* array Events: 8 elements */
			/* [1] */
			"close",
			"Close an object",
			'core',
			'clos',
			noReply,
			"",
			replyOptional,
			singleItem,
			notEnumerated,
			notTightBindingFunction,
			enumsAreConstants,
			enumListCanRepeat,
			replyIsValue,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			verbEvent,
			reserved,
			reserved,
			reserved,
			'obj ',
			"the objects to close",
			directParamRequired,
			singleItem,
			notEnumerated,
			changesState,
			enumsAreConstants,
			enumListCanRepeat,
			directParamIsValue,
			directParamIsTarget,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			{	/* array OtherParams: 2 elements */
				/* [1] */
				"saving",
				'savo',
				'savo',
				"specifies whether or not changes should "
				"be saved before closing",
				optional,
				singleItem,
				enumerated,
				reserved,
				enumsAreConstants,
				enumListCanRepeat,
				paramIsValue,
				notParamIsTarget,
				reserved,
				reserved,
				reserved,
				reserved,
				prepositionParam,
				notFeminine,
				notMasculine,
				singular,
				/* [2] */
				"in",
				'kfil',
				'alis',
				"the file in which to save the object",
				optional,
				singleItem,
				notEnumerated,
				reserved,
				enumsAreConstants,
				enumListCanRepeat,
				paramIsValue,
				notParamIsTarget,
				reserved,
				reserved,
				reserved,
				reserved,
				prepositionParam,
				notFeminine,
				notMasculine,
				singular
			},
			/* [2] */
			"data size",
			"Return the size in bytes of an object",
			'core',
			'dsiz',
			'long',
			"the size of the object in bytes",
			replyRequired,
			singleItem,
			notEnumerated,
			notTightBindingFunction,
			enumsAreConstants,
			enumListCanRepeat,
			replyIsValue,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			verbEvent,
			reserved,
			reserved,
			reserved,
			'obj ',
			"the object whose data size is to be retu"
			"rned",
			directParamRequired,
			singleItem,
			notEnumerated,
			doesntChangeState,
			enumsAreConstants,
			enumListCanRepeat,
			directParamIsValue,
			directParamIsTarget,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			{	/* array OtherParams: 0 elements */
			},
			/* [3] */
			"get",
			"Get the data for an object",
			'core',
			'getd',
			'****',
			"The data from the object",
			replyRequired,
			singleItem,
			notEnumerated,
			notTightBindingFunction,
			enumsAreConstants,
			enumListCanRepeat,
			replyIsValue,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			verbEvent,
			reserved,
			reserved,
			reserved,
			'obj ',
			"the object whose data is to be returned",
			directParamRequired,
			singleItem,
			notEnumerated,
			doesntChangeState,
			enumsAreConstants,
			enumListCanRepeat,
			directParamIsValue,
			directParamIsTarget,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			{	/* array OtherParams: 0 elements */
			},
			/* [4] */
			"make",
			"Make a new element",
			'core',
			'crel',
			'obj ',
			"Object specifier for the new element",
			replyRequired,
			singleItem,
			notEnumerated,
			notTightBindingFunction,
			enumsAreConstants,
			enumListCanRepeat,
			replyIsValue,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			verbEvent,
			reserved,
			reserved,
			reserved,
			noParams,
			"",
			directParamOptional,
			singleItem,
			notEnumerated,
			changesState,
			enumsAreConstants,
			enumListCanRepeat,
			directParamIsValue,
			directParamIsTarget,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			{	/* array OtherParams: 4 elements */
				/* [1] */
				"new",
				'kocl',
				'type',
				"the class of the new element",
				required,
				singleItem,
				notEnumerated,
				reserved,
				enumsAreConstants,
				enumListCanRepeat,
				paramIsValue,
				notParamIsTarget,
				reserved,
				reserved,
				reserved,
				reserved,
				prepositionParam,
				notFeminine,
				notMasculine,
				singular,
				/* [2] */
				"at",
				'insh',
				'insl',
				"the location at which to insert the elem"
				"ent",
				optional,
				singleItem,
				notEnumerated,
				reserved,
				enumsAreConstants,
				enumListCanRepeat,
				paramIsValue,
				notParamIsTarget,
				reserved,
				reserved,
				reserved,
				reserved,
				prepositionParam,
				notFeminine,
				notMasculine,
				singular,
				/* [3] */
				"with data",
				'data',
				'****',
				"the initial data for the element",
				optional,
				singleItem,
				notEnumerated,
				reserved,
				enumsAreConstants,
				enumListCanRepeat,
				paramIsValue,
				notParamIsTarget,
				reserved,
				reserved,
				reserved,
				reserved,
				prepositionParam,
				notFeminine,
				notMasculine,
				singular,
				/* [4] */
				"with properties",
				'prdt',
				'reco',
				"the initial values for the properties of"
				" the element",
				optional,
				singleItem,
				notEnumerated,
				reserved,
				enumsAreConstants,
				enumListCanRepeat,
				paramIsValue,
				notParamIsTarget,
				reserved,
				reserved,
				reserved,
				reserved,
				prepositionParam,
				notFeminine,
				notMasculine,
				singular
			},
			/* [5] */
			"open",
			"Open the specified object(s)",
			'aevt',
			'odoc',
			noReply,
			"",
			replyOptional,
			singleItem,
			notEnumerated,
			notTightBindingFunction,
			enumsAreConstants,
			enumListCanRepeat,
			replyIsValue,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			verbEvent,
			reserved,
			reserved,
			reserved,
			'obj ',
			"Objects to open. Can be a list of files "
			"or an object specifier.",
			directParamRequired,
			singleItem,
			notEnumerated,
			changesState,
			enumsAreConstants,
			enumListCanRepeat,
			directParamIsValue,
			directParamIsTarget,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			{	/* array OtherParams: 0 elements */
			},
			/* [6] */
			"print",
			"Print the specified object(s)",
			'aevt',
			'pdoc',
			noReply,
			"",
			replyOptional,
			singleItem,
			notEnumerated,
			notTightBindingFunction,
			enumsAreConstants,
			enumListCanRepeat,
			replyIsValue,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			verbEvent,
			reserved,
			reserved,
			reserved,
			'obj ',
			"Objects to print. Can be a list of files"
			" or an object specifier.",
			directParamRequired,
			singleItem,
			notEnumerated,
			doesntChangeState,
			enumsAreConstants,
			enumListCanRepeat,
			directParamIsValue,
			directParamIsTarget,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			{	/* array OtherParams: 0 elements */
			},
			/* [7] */
			"save",
			"save a set of objects",
			'core',
			'save',
			noReply,
			"",
			replyRequired,
			singleItem,
			notEnumerated,
			notTightBindingFunction,
			enumsAreConstants,
			enumListCanRepeat,
			replyIsValue,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			verbEvent,
			reserved,
			reserved,
			reserved,
			'obj ',
			"Objects to save.",
			directParamRequired,
			singleItem,
			notEnumerated,
			doesntChangeState,
			enumsAreConstants,
			enumListCanRepeat,
			directParamIsValue,
			directParamIsTarget,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			{	/* array OtherParams: 2 elements */
				/* [1] */
				"in",
				'kfil',
				'alis',
				"the file in which to save the object(s)",
				optional,
				singleItem,
				notEnumerated,
				reserved,
				enumsAreConstants,
				enumListCanRepeat,
				paramIsValue,
				notParamIsTarget,
				reserved,
				reserved,
				reserved,
				reserved,
				prepositionParam,
				notFeminine,
				notMasculine,
				singular,
				/* [2] */
				"as",
				'fltp',
				'type',
				"the file type of the document in which t"
				"o save the data",
				optional,
				singleItem,
				notEnumerated,
				reserved,
				enumsAreConstants,
				enumListCanRepeat,
				paramIsValue,
				notParamIsTarget,
				reserved,
				reserved,
				reserved,
				reserved,
				prepositionParam,
				notFeminine,
				notMasculine,
				singular
			},
			/* [8] */
			"set",
			"Set an objectÕs data",
			'core',
			'setd',
			noReply,
			"",
			replyOptional,
			singleItem,
			notEnumerated,
			notTightBindingFunction,
			enumsAreConstants,
			enumListCanRepeat,
			replyIsValue,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			verbEvent,
			reserved,
			reserved,
			reserved,
			'obj ',
			"the object to change",
			directParamRequired,
			singleItem,
			notEnumerated,
			changesState,
			enumsAreConstants,
			enumListCanRepeat,
			directParamIsValue,
			directParamIsTarget,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			{	/* array OtherParams: 1 elements */
				/* [1] */
				"to",
				'data',
				'****',
				"the new value",
				required,
				singleItem,
				notEnumerated,
				reserved,
				enumsAreConstants,
				enumListCanRepeat,
				paramIsValue,
				notParamIsTarget,
				reserved,
				reserved,
				reserved,
				reserved,
				prepositionParam,
				notFeminine,
				notMasculine,
				singular
			}
		},
		{	/* array Classes: 3 elements */
			/* [1] */
			"application",
			'capp',
			"An application program",
			{	/* array Properties: 0 elements */
			},
			{	/* array Elements: 2 elements */
				/* [1] */
				'cwin',
				{	/* array KeyForms: 3 elements */
					/* [1] */
					'indx',
					/* [2] */
					'name',
					/* [3] */
					'rele'
				},
				/* [2] */
				'docu',
				{	/* array KeyForms: 1 elements */
					/* [1] */
					'name'
				}
			},
			/* [2] */
			"window",
			'cwin',
			"A Window",
			{	/* array Properties: 12 elements */
				/* [1] */
				"bounds",
				'pbnd',
				'qdrt',
				"the boundary rectangle for the window",
				reserved,
				singleItem,
				notEnumerated,
				readWrite,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [2] */
				"closeable",
				'hclb',
				'bool',
				"Does the window have a close box?",
				reserved,
				singleItem,
				notEnumerated,
				readOnly,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [3] */
				"titled",
				'ptit',
				'bool',
				"Does the window have a title bar?",
				reserved,
				singleItem,
				notEnumerated,
				readOnly,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [4] */
				"index",
				'pidx',
				'long',
				"the number of the window",
				reserved,
				singleItem,
				notEnumerated,
				readWrite,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [5] */
				"floating",
				'isfl',
				'bool',
				"Does the window float?",
				reserved,
				singleItem,
				notEnumerated,
				readOnly,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [6] */
				"modal",
				'pmod',
				'bool',
				"Is the window modal?",
				reserved,
				singleItem,
				notEnumerated,
				readOnly,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [7] */
				"resizable",
				'prsz',
				'bool',
				"Is the window resizable?",
				reserved,
				singleItem,
				notEnumerated,
				readOnly,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [8] */
				"zoomable",
				'iszm',
				'bool',
				"Is the window zoomable?",
				reserved,
				singleItem,
				notEnumerated,
				readOnly,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [9] */
				"zoomed",
				'pzum',
				'bool',
				"Is the window zoomed?",
				reserved,
				singleItem,
				notEnumerated,
				readWrite,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [10] */
				"name",
				'pnam',
				'itxt',
				"the title of the window",
				reserved,
				singleItem,
				notEnumerated,
				readWrite,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [11] */
				"visible",
				'pvis',
				'bool',
				"is the window visible?",
				reserved,
				singleItem,
				notEnumerated,
				readOnly,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [12] */
				"position",
				'ppos',
				'QDpt',
				"upper left coordinates of window",
				reserved,
				singleItem,
				notEnumerated,
				readOnly,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular
			},
			{	/* array Elements: 0 elements */
			},
			/* [3] */
			"document",
			'docu',
			"A Document",
			{	/* array Properties: 2 elements */
				/* [1] */
				"name",
				'pnam',
				'itxt',
				"the title of the document",
				reserved,
				singleItem,
				notEnumerated,
				readOnly,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular,
				/* [2] */
				"modified",
				'imod',
				'bool',
				"Has the document been modified since the"
				" last save?",
				reserved,
				singleItem,
				notEnumerated,
				readOnly,
				enumsAreConstants,
				enumListCanRepeat,
				propertyIsValue,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				noApostrophe,
				notFeminine,
				notMasculine,
				singular
			},
			{	/* array Elements: 0 elements */
			}
		},
		{	/* array ComparisonOps: 0 elements */
		},
		{	/* array Enumerations: 1 elements */
			/* [1] */
			'savo',
			{	/* array Enumerators: 3 elements */
				/* [1] */
				"yes",
				'yes ',
				"Save objects now",
				/* [2] */
				"no",
				'no  ',
				"Do not save objects",
				/* [3] */
				"ask",
				'ask ',
				"Ask the user whether to save"
			}
		},
		/* [3] */
		"Miscellaneous Standards",
		"Useful events that arenÕt in any other s"
		"uite",
		'misc',
		0,
		0,
		{	/* array Events: 1 elements */
			/* [1] */
			"revert",
			"Revert an object to the most recently sa"
			"ved version",
			'misc',
			'rvrt',
			noReply,
			"",
			replyOptional,
			singleItem,
			notEnumerated,
			notTightBindingFunction,
			enumsAreConstants,
			enumListCanRepeat,
			replyIsValue,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			verbEvent,
			reserved,
			reserved,
			reserved,
			'obj ',
			"object to revert",
			directParamRequired,
			singleItem,
			notEnumerated,
			doesntChangeState,
			enumsAreConstants,
			enumListCanRepeat,
			directParamIsValue,
			directParamIsTarget,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			{	/* array OtherParams: 0 elements */
			}
		},
		{	/* array Classes: 0 elements */
		},
		{	/* array ComparisonOps: 0 elements */
		},
		{	/* array Enumerations: 0 elements */
		},
		/* [4] */
		"odds and ends",
		"Things that should be in some standard s"
		"uite, but arenÕt",
		'Odds',
		1,
		1,
		{	/* array Events: 1 elements */
			/* [1] */
			"select",
			"Select the specified object",
			'misc',
			'slct',
			noReply,
			"",
			replyOptional,
			singleItem,
			notEnumerated,
			notTightBindingFunction,
			enumsAreConstants,
			enumListCanRepeat,
			replyIsValue,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			verbEvent,
			reserved,
			reserved,
			reserved,
			'obj ',
			"the object to select",
			directParamOptional,
			singleItem,
			notEnumerated,
			changesState,
			enumsAreConstants,
			enumListCanRepeat,
			directParamIsValue,
			directParamIsTarget,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			{	/* array OtherParams: 0 elements */
			}
		},
		{	/* array Classes: 0 elements */
		},
		{	/* array ComparisonOps: 0 elements */
		},
		{	/* array Enumerations: 0 elements */
		}
	}
};

resource 'clut' (128) {
	{	/* array ColorSpec: 256 elements */
		/* [1] */
		0, 0, 0,
		/* [2] */
		65535, 65535, 52428,
		/* [3] */
		65535, 65535, 39321,
		/* [4] */
		65535, 65535, 26214,
		/* [5] */
		65535, 65535, 13107,
		/* [6] */
		65535, 65535, 0,
		/* [7] */
		65535, 52428, 65535,
		/* [8] */
		65535, 52428, 52428,
		/* [9] */
		65535, 52428, 39321,
		/* [10] */
		65535, 52428, 26214,
		/* [11] */
		65535, 52428, 13107,
		/* [12] */
		65535, 52428, 0,
		/* [13] */
		65535, 39321, 65535,
		/* [14] */
		65535, 39321, 52428,
		/* [15] */
		65535, 39321, 39321,
		/* [16] */
		65535, 39321, 26214,
		/* [17] */
		65535, 39321, 13107,
		/* [18] */
		65535, 39321, 0,
		/* [19] */
		65535, 26214, 65535,
		/* [20] */
		65535, 26214, 52428,
		/* [21] */
		65535, 26214, 39321,
		/* [22] */
		65535, 26214, 26214,
		/* [23] */
		65535, 26214, 13107,
		/* [24] */
		65535, 26214, 0,
		/* [25] */
		65535, 13107, 65535,
		/* [26] */
		65535, 13107, 52428,
		/* [27] */
		65535, 13107, 39321,
		/* [28] */
		65535, 13107, 26214,
		/* [29] */
		65535, 13107, 13107,
		/* [30] */
		65535, 13107, 0,
		/* [31] */
		65535, 0, 65535,
		/* [32] */
		65535, 0, 52428,
		/* [33] */
		65535, 0, 39321,
		/* [34] */
		65535, 0, 26214,
		/* [35] */
		65535, 0, 13107,
		/* [36] */
		65535, 0, 0,
		/* [37] */
		52428, 65535, 65535,
		/* [38] */
		52428, 65535, 52428,
		/* [39] */
		52428, 65535, 39321,
		/* [40] */
		52428, 65535, 26214,
		/* [41] */
		52428, 65535, 13107,
		/* [42] */
		52428, 65535, 0,
		/* [43] */
		52428, 52428, 65535,
		/* [44] */
		52428, 52428, 52428,
		/* [45] */
		52428, 52428, 39321,
		/* [46] */
		52428, 52428, 26214,
		/* [47] */
		52428, 52428, 13107,
		/* [48] */
		52428, 52428, 0,
		/* [49] */
		52428, 39321, 65535,
		/* [50] */
		52428, 39321, 52428,
		/* [51] */
		52428, 39321, 39321,
		/* [52] */
		52428, 39321, 26214,
		/* [53] */
		52428, 39321, 13107,
		/* [54] */
		52428, 39321, 0,
		/* [55] */
		52428, 26214, 65535,
		/* [56] */
		52428, 26214, 52428,
		/* [57] */
		52428, 26214, 39321,
		/* [58] */
		52428, 26214, 26214,
		/* [59] */
		52428, 26214, 13107,
		/* [60] */
		52428, 26214, 0,
		/* [61] */
		52428, 13107, 65535,
		/* [62] */
		52428, 13107, 52428,
		/* [63] */
		52428, 13107, 39321,
		/* [64] */
		52428, 13107, 26214,
		/* [65] */
		52428, 13107, 13107,
		/* [66] */
		52428, 13107, 0,
		/* [67] */
		52428, 0, 65535,
		/* [68] */
		52428, 0, 52428,
		/* [69] */
		52428, 0, 39321,
		/* [70] */
		52428, 0, 26214,
		/* [71] */
		52428, 0, 13107,
		/* [72] */
		52428, 0, 0,
		/* [73] */
		39321, 65535, 65535,
		/* [74] */
		39321, 65535, 52428,
		/* [75] */
		39321, 65535, 39321,
		/* [76] */
		39321, 65535, 26214,
		/* [77] */
		39321, 65535, 13107,
		/* [78] */
		39321, 65535, 0,
		/* [79] */
		39321, 52428, 65535,
		/* [80] */
		39321, 52428, 52428,
		/* [81] */
		39321, 52428, 39321,
		/* [82] */
		39321, 52428, 26214,
		/* [83] */
		39321, 52428, 13107,
		/* [84] */
		39321, 52428, 0,
		/* [85] */
		39321, 39321, 65535,
		/* [86] */
		39321, 39321, 52428,
		/* [87] */
		39321, 39321, 39321,
		/* [88] */
		39321, 39321, 26214,
		/* [89] */
		39321, 39321, 13107,
		/* [90] */
		39321, 39321, 0,
		/* [91] */
		39321, 26214, 65535,
		/* [92] */
		39321, 26214, 52428,
		/* [93] */
		39321, 26214, 39321,
		/* [94] */
		39321, 26214, 26214,
		/* [95] */
		39321, 26214, 13107,
		/* [96] */
		39321, 26214, 0,
		/* [97] */
		39321, 13107, 65535,
		/* [98] */
		39321, 13107, 52428,
		/* [99] */
		39321, 13107, 39321,
		/* [100] */
		39321, 13107, 26214,
		/* [101] */
		39321, 13107, 13107,
		/* [102] */
		39321, 13107, 0,
		/* [103] */
		39321, 0, 65535,
		/* [104] */
		39321, 0, 52428,
		/* [105] */
		39321, 0, 39321,
		/* [106] */
		39321, 0, 26214,
		/* [107] */
		39321, 0, 13107,
		/* [108] */
		39321, 0, 0,
		/* [109] */
		26214, 65535, 65535,
		/* [110] */
		26214, 65535, 52428,
		/* [111] */
		26214, 65535, 39321,
		/* [112] */
		26214, 65535, 26214,
		/* [113] */
		26214, 65535, 13107,
		/* [114] */
		26214, 65535, 0,
		/* [115] */
		26214, 52428, 65535,
		/* [116] */
		26214, 52428, 52428,
		/* [117] */
		26214, 52428, 39321,
		/* [118] */
		26214, 52428, 26214,
		/* [119] */
		26214, 52428, 13107,
		/* [120] */
		26214, 52428, 0,
		/* [121] */
		26214, 39321, 65535,
		/* [122] */
		26214, 39321, 52428,
		/* [123] */
		26214, 39321, 39321,
		/* [124] */
		26214, 39321, 26214,
		/* [125] */
		26214, 39321, 13107,
		/* [126] */
		26214, 39321, 0,
		/* [127] */
		26214, 26214, 65535,
		/* [128] */
		26214, 26214, 52428,
		/* [129] */
		26214, 26214, 39321,
		/* [130] */
		26214, 26214, 26214,
		/* [131] */
		26214, 26214, 13107,
		/* [132] */
		26214, 26214, 0,
		/* [133] */
		26214, 13107, 65535,
		/* [134] */
		26214, 13107, 52428,
		/* [135] */
		26214, 13107, 39321,
		/* [136] */
		26214, 13107, 26214,
		/* [137] */
		26214, 13107, 13107,
		/* [138] */
		26214, 13107, 0,
		/* [139] */
		26214, 0, 65535,
		/* [140] */
		26214, 0, 52428,
		/* [141] */
		26214, 0, 39321,
		/* [142] */
		26214, 0, 26214,
		/* [143] */
		26214, 0, 13107,
		/* [144] */
		26214, 0, 0,
		/* [145] */
		13107, 65535, 65535,
		/* [146] */
		13107, 65535, 52428,
		/* [147] */
		13107, 65535, 39321,
		/* [148] */
		13107, 65535, 26214,
		/* [149] */
		13107, 65535, 13107,
		/* [150] */
		13107, 65535, 0,
		/* [151] */
		13107, 52428, 65535,
		/* [152] */
		13107, 52428, 52428,
		/* [153] */
		13107, 52428, 39321,
		/* [154] */
		13107, 52428, 26214,
		/* [155] */
		13107, 52428, 13107,
		/* [156] */
		13107, 52428, 0,
		/* [157] */
		13107, 39321, 65535,
		/* [158] */
		13107, 39321, 52428,
		/* [159] */
		13107, 39321, 39321,
		/* [160] */
		13107, 39321, 26214,
		/* [161] */
		13107, 39321, 13107,
		/* [162] */
		13107, 39321, 0,
		/* [163] */
		13107, 26214, 65535,
		/* [164] */
		13107, 26214, 52428,
		/* [165] */
		13107, 26214, 39321,
		/* [166] */
		13107, 26214, 26214,
		/* [167] */
		13107, 26214, 13107,
		/* [168] */
		13107, 26214, 0,
		/* [169] */
		13107, 13107, 65535,
		/* [170] */
		13107, 13107, 52428,
		/* [171] */
		13107, 13107, 39321,
		/* [172] */
		13107, 13107, 26214,
		/* [173] */
		13107, 13107, 13107,
		/* [174] */
		13107, 13107, 0,
		/* [175] */
		13107, 0, 65535,
		/* [176] */
		13107, 0, 52428,
		/* [177] */
		13107, 0, 39321,
		/* [178] */
		13107, 0, 26214,
		/* [179] */
		13107, 0, 13107,
		/* [180] */
		13107, 0, 0,
		/* [181] */
		0, 65535, 65535,
		/* [182] */
		0, 65535, 52428,
		/* [183] */
		0, 65535, 39321,
		/* [184] */
		0, 65535, 26214,
		/* [185] */
		0, 65535, 13107,
		/* [186] */
		0, 65535, 0,
		/* [187] */
		0, 52428, 65535,
		/* [188] */
		0, 52428, 52428,
		/* [189] */
		0, 52428, 39321,
		/* [190] */
		0, 52428, 26214,
		/* [191] */
		0, 52428, 13107,
		/* [192] */
		0, 52428, 0,
		/* [193] */
		0, 39321, 65535,
		/* [194] */
		0, 39321, 52428,
		/* [195] */
		0, 39321, 39321,
		/* [196] */
		0, 39321, 26214,
		/* [197] */
		0, 39321, 13107,
		/* [198] */
		0, 39321, 0,
		/* [199] */
		0, 26214, 65535,
		/* [200] */
		0, 26214, 52428,
		/* [201] */
		0, 26214, 39321,
		/* [202] */
		0, 26214, 26214,
		/* [203] */
		0, 26214, 13107,
		/* [204] */
		0, 26214, 0,
		/* [205] */
		0, 13107, 65535,
		/* [206] */
		0, 13107, 52428,
		/* [207] */
		0, 13107, 39321,
		/* [208] */
		0, 13107, 26214,
		/* [209] */
		0, 13107, 13107,
		/* [210] */
		0, 13107, 0,
		/* [211] */
		0, 0, 65535,
		/* [212] */
		0, 0, 52428,
		/* [213] */
		0, 0, 39321,
		/* [214] */
		0, 0, 26214,
		/* [215] */
		0, 0, 13107,
		/* [216] */
		61166, 0, 0,
		/* [217] */
		56797, 0, 0,
		/* [218] */
		48059, 0, 0,
		/* [219] */
		43690, 0, 0,
		/* [220] */
		34952, 0, 0,
		/* [221] */
		30583, 0, 0,
		/* [222] */
		21845, 0, 0,
		/* [223] */
		17476, 0, 0,
		/* [224] */
		8738, 0, 0,
		/* [225] */
		4369, 0, 0,
		/* [226] */
		0, 61166, 0,
		/* [227] */
		0, 56797, 0,
		/* [228] */
		0, 48059, 0,
		/* [229] */
		0, 43690, 0,
		/* [230] */
		0, 34952, 0,
		/* [231] */
		0, 30583, 0,
		/* [232] */
		0, 21845, 0,
		/* [233] */
		0, 17476, 0,
		/* [234] */
		0, 8738, 0,
		/* [235] */
		0, 4369, 0,
		/* [236] */
		0, 0, 61166,
		/* [237] */
		0, 0, 56797,
		/* [238] */
		0, 0, 48059,
		/* [239] */
		0, 0, 43690,
		/* [240] */
		0, 0, 34952,
		/* [241] */
		0, 0, 30583,
		/* [242] */
		0, 0, 21845,
		/* [243] */
		0, 0, 17476,
		/* [244] */
		0, 0, 8738,
		/* [245] */
		0, 0, 4369,
		/* [246] */
		61166, 61166, 61166,
		/* [247] */
		56797, 56797, 56797,
		/* [248] */
		48059, 48059, 48059,
		/* [249] */
		43690, 43690, 43690,
		/* [250] */
		34952, 34952, 34952,
		/* [251] */
		30583, 30583, 30583,
		/* [252] */
		21845, 21845, 21845,
		/* [253] */
		17476, 17476, 17476,
		/* [254] */
		8738, 8738, 8738,
		/* [255] */
		4369, 4369, 4369,
		/* [256] */
		0, 0, 0
	}
};

resource 'icl4' (128) {
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFA BADB A2A2 ABBA BBBB BA2B 2BD2 2A2F"
	$"FFFF FABA BABA DA2B BBAB BBBB A222 A22F"
	$"FFFF FFFA ABAB ABAD ABBB BBBB 2BBA 2BBF"
	$"FFFF FFFF FABD BDBA BDAA AAB2 33A2 BB2F"
	$"FFFF FFFF FFFF AAAB ABA1 12AA 3ABA BBBF"
	$"FFFF FFFF FFFF FFAD AAA1 122B AABB 2B2F"
	$"FFFF FFFF 0111 1AFF FFA1 233B BBAA BDBF"
	$"FFFF FFF0 1211 1BBB B112 2A22 2ABB AABF"
	$"FFFF FF01 DADD DDDB 1122 A222 A2AA BADF"
	$"FFFF FF0A B2DA 0000 122A 2C12 32AA ABBF"
	$"FFFF FFAD 2DD2 A000 0222 2122 33BA AABF"
	$"FFFF FF05 5E2D 2001 2222 1103 3ADB AFAF"
	$"F765 F0DD 55E7 2001 1222 1033 ABD2 AFFF"
	$"F975 0DD5 555D 2121 2122 223A DDBB BAFF"
	$"F775 0555 5DFD 1112 D223 33AD AADB A9AF"
	$"F795 0055 5FFC 1121 2221 1B2A A2AA BBAF"
	$"F975 50D5 DFF1 1121 1DD2 1BAB ABAA BBAF"
	$"F779 50C5 D5F1 C112 1221 BA2A AAAA AAAF"
	$"F795 00C5 D5F1 2111 11BA ABAB AABA BAFF"
	$"F950 0655 D5F1 D221 2DAD BAAA AAAB AFFF"
	$"F50C 655D 5FFC 2121 12BB DAAB ABBA FFFF"
	$"F50D 555F F9AE 111D DD2A ABAA ABA7 65FF"
	$"FE5D 5DF9 7E9A D122 BAAA AABB A2AE 796F"
	$"F755 5FAD 9999 AD2D DBAB AAAA 2A79 979F"
	$"FD55 5ED9 9979 9AA1 DAAA ABA2 A9D9 E9EF"
	$"FE95 D99E 799E AADC D12B BB2A E797 997F"
	$"FDDD 9979 979A D11D B2AA AAA9 7E99 7E9F"
	$"FED9 E9D9 ED9A A2B2 AA99 7E97 997D 997F"
	$"F9E9 E99D 99E9 9AAA 9797 9799 E979 E79F"
	$"FDD9 DDE9 99E9 E9DE E9E9 E9E9 79E9 7E9F"
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
};

resource 'icl8' (128) {
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFAD 5F83 575F 8311 8311 835F 5983"
	$"595F 595F 5983 115F 115F 571D 1D89 11FF"
	$"FFFF FFFF FFAD 5F83 5F83 5F83 5783 115F"
	$"5F59 8359 5F59 5F59 830F 1D1D 8911 0FFF"
	$"FFFF FFFF FFFF FFAD 835F 835F 835F 8357"
	$"835F 5F5F 595F 595F 0F5F 5F89 0F5F 59FF"
	$"FFFF FFFF FFFF FFFF FFAD 5F57 5F57 5F83"
	$"5F57 83AD ADAD 5F0F 2323 8911 5F59 0FFF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF AD83 835F"
	$"835F AD03 030F ADAD 2389 5F83 595F 59FF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF AD57"
	$"8383 AD05 030F 175F ADAD 595F 115F 0FFF"
	$"FFFF FFFF FFFF FFFF 0005 0505 0583 FFFF"
	$"FFFF AD05 1723 235F 5F5F ADAD 5F57 5FFF"
	$"FFFF FFFF FFFF FF00 050F 0505 0559 5959"
	$"5905 0517 1765 0F0F 0F83 5F5F ADAD 59FF"
	$"FFFF FFFF FFFF 0005 5783 5757 5757 5759"
	$"0303 1717 650F 0F0F 830F 83AD 5F83 57FF"
	$"FFFF FFFF FFFF 0083 590F 5783 0000 0000"
	$"050F 1765 0F0E 0317 2317 83AD 835F 5FFF"
	$"FFFF FFFF FFFF 8357 0F57 570F 8300 0000"
	$"0017 170F 0F03 0F0F 2323 5FAD AD83 5FFF"
	$"FFFF FFFF FFFF 0C66 66FB 0F57 0F00 0005"
	$"0F17 170F 0303 0023 2389 575F ADFF ADFF"
	$"FFC6 A8AA FF00 5A5A 8C8C FB78 0F02 0005"
	$"0317 1717 0300 2323 895D 570F ADFF FFFF"
	$"FFC9 C6B1 005A 5A8C 8C66 667B 0F05 0F03"
	$"0F03 1717 1715 2389 5757 5F5F 5FAD FFFF"
	$"FFC6 C6B1 0C66 668C 8C5A FF57 0303 030F"
	$"570F 0F23 2323 8957 8383 575F 83A7 ADFF"
	$"FFC6 C9B1 0C0C 668C 8CF2 FF0E 0303 0F03"
	$"0F17 1703 035F 0F83 830F 8383 5D5F ADFF"
	$"FFC9 C6B1 B10C 5A8A 5AD5 FE03 0303 0F03"
	$"0357 570F 035D 835F 835F 8383 5F5F ADFF"
	$"FFC6 C6C9 B10C 548C 5A8C F103 0E03 030F"
	$"030F 0F03 5D83 0F83 8383 8383 8383 ADFF"
	$"FFC6 C9B1 0C00 548C 5A8C F103 0F03 0503"
	$"0303 5D83 835F 835F 8383 5F83 5FAD FFFF"
	$"FFC9 B10C 00D2 8C8C 808C F103 570F 0F03"
	$"1757 8357 5F83 8383 8383 835F ADFF FFFF"
	$"FFB1 0C54 D28A 8C80 8CD5 FE0E 1703 0F03"
	$"030F 5D5F 5783 835F 835F 5FAD FFFF FFFF"
	$"FFB1 0C5A 8C8C 8AF1 F1C9 ADFB 0303 0357"
	$"5757 0F83 835F 8383 835F ADC6 A8AA FFFF"
	$"FFFB B15A 8C80 F1C9 C6FB C9AD 5703 170F"
	$"5D83 8383 8383 5D5F 833B ADFB C6E6 A8FF"
	$"FFC6 B18C 66F2 8380 C9C9 C9C9 AD57 0F57"
	$"575D 835F 8383 8383 3BAD C6C9 E6C6 C9FF"
	$"FF80 B166 B1FB 80C9 C9C9 C6C9 C9AD 8303"
	$"5783 8383 835F 833B ADC9 80C9 FBC9 FBFF"
	$"FFFB C9B1 80C9 C9FB C6C9 C9FB ADAD 570E"
	$"5703 0F5D 5F5F 3BAD FBC6 C9C6 C9C9 C6FF"
	$"FF80 8080 C9C9 C6C9 C9C6 C9AD 5703 0357"
	$"5F3B ADAD ADAD ADC9 C6FB C9C9 C6FB C9FF"
	$"FFFB 80C9 FBC9 80C9 FB80 C9AD AD17 5D3B"
	$"ADAD C9C9 C6FB C9C6 C9C9 C680 C9C9 C6FF"
	$"FFC8 FBC8 FBC9 C980 C8C9 FBC9 C9AD ADAD"
	$"C9C6 C9C6 C9C6 C9C9 FBC9 C6C9 FBC6 C9FF"
	$"FF80 80C8 8080 FBC8 C9C9 FBC8 FBC9 80FB"
	$"FBC9 FBC9 FBC9 FBC9 C6C9 FBC9 C6FB C9FF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
};

resource 'ics#' (128) {
	{	/* array: 2 elements */
		/* [1] */
		$"FFFF FDAD FFF5 FFFE FFEB FFFF FFFE FF07"
		$"FE00 FC40 FD10 FE08 FDC0 F8E0 F1E0 F7A0",
		/* [2] */
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
	}
};

resource 'ics4' (128) {
	$"FFFF FFFF FFFF FFFF FFFA BADB A2A2 ABBA"
	$"FFFF FABA BABA DA2B FFFF FFFA ABAB ABAD"
	$"FFFF FFFF FABD BDBA FFFF FFFF FFFF AAAB"
	$"FFFF FFFF FFFF FFAD FFFF FFFF 0111 1AFF"
	$"FFFF FFF0 1211 1BBB FFFF FF01 DADD DDDB"
	$"FFFF FF0A B2DA 0000 FFFF FFAD 2DD2 A000"
	$"FFFF FF05 5E2D 2001 F765 F0DD 55E7 2001"
	$"F975 0DD5 555D 2121 F775 0555 5DFD 1112"
};

resource 'ics8' (128) {
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFAD 5F83 575F 8311 8311 835F 5983"
	$"FFFF FFFF FFAD 5F83 5F83 5F83 5783 115F"
	$"FFFF FFFF FFFF FFAD 835F 835F 835F 8357"
	$"FFFF FFFF FFFF FFFF FFAD 5F57 5F57 5F83"
	$"FFFF FFFF FFFF FFFF FFFF FFFF AD83 835F"
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF AD57"
	$"FFFF FFFF FFFF FFFF 0005 0505 0583 FFFF"
	$"FFFF FFFF FFFF FF00 050F 0505 0559 5959"
	$"FFFF FFFF FFFF 0005 5783 5757 5757 5759"
	$"FFFF FFFF FFFF 0083 590F 5783 0000 0000"
	$"FFFF FFFF FFFF 8357 0F57 570F 8300 0000"
	$"FFFF FFFF FFFF 0C66 66FB 0F57 0F00 0005"
	$"FFC6 A8AA FF00 5A5A 8C8C FB78 0F02 0005"
	$"FFC9 C6B1 005A 5A8C 8C66 667B 0F05 0F03"
	$"FFC6 C6B1 0C66 668C 8C5A FF57 0303 030F"
};

resource 'pltt' (128) {
	{	/* array ColorInfo: 256 elements */
		/* [1] */
		65535, 65535, 65535, pmTolerant, 0,
		/* [2] */
		0, 0, 0, pmTolerant, 0,
		/* [3] */
		30583, 30583, 30583, pmCourteous, 4096,
		/* [4] */
		21845, 21845, 21845, pmCourteous, 4096,
		/* [5] */
		65535, 65535, 0, pmCourteous, 4096,
		/* [6] */
		65535, 26214, 0, pmCourteous, 4096,
		/* [7] */
		56797, 0, 0, pmCourteous, 4096,
		/* [8] */
		65535, 0, 39321, pmCourteous, 4096,
		/* [9] */
		26214, 0, 39321, pmCourteous, 4096,
		/* [10] */
		0, 0, 56797, pmCourteous, 4096,
		/* [11] */
		0, 39321, 65535, pmCourteous, 4096,
		/* [12] */
		0, 61166, 0, pmCourteous, 4096,
		/* [13] */
		0, 26214, 0, pmCourteous, 4096,
		/* [14] */
		26214, 13107, 0, pmCourteous, 4096,
		/* [15] */
		39321, 26214, 13107, pmCourteous, 4096,
		/* [16] */
		48059, 48059, 48059, pmCourteous, 4096,
		/* [17] */
		65535, 65535, 52428, pmCourteous, 4096,
		/* [18] */
		65535, 65535, 39321, pmCourteous, 4096,
		/* [19] */
		65535, 65535, 26214, pmCourteous, 4096,
		/* [20] */
		65535, 65535, 13107, pmCourteous, 4096,
		/* [21] */
		65535, 52428, 65535, pmCourteous, 4096,
		/* [22] */
		65535, 52428, 52428, pmCourteous, 4096,
		/* [23] */
		65535, 52428, 39321, pmCourteous, 4096,
		/* [24] */
		65535, 52428, 26214, pmCourteous, 4096,
		/* [25] */
		65535, 52428, 13107, pmCourteous, 4096,
		/* [26] */
		65535, 52428, 0, pmCourteous, 4096,
		/* [27] */
		65535, 39321, 65535, pmCourteous, 4096,
		/* [28] */
		65535, 39321, 52428, pmCourteous, 4096,
		/* [29] */
		65535, 39321, 39321, pmCourteous, 4096,
		/* [30] */
		65535, 39321, 26214, pmCourteous, 4096,
		/* [31] */
		65535, 39321, 13107, pmCourteous, 4096,
		/* [32] */
		65535, 39321, 0, pmCourteous, 4096,
		/* [33] */
		65535, 26214, 65535, pmCourteous, 4096,
		/* [34] */
		65535, 26214, 52428, pmCourteous, 4096,
		/* [35] */
		65535, 26214, 39321, pmCourteous, 4096,
		/* [36] */
		65535, 26214, 26214, pmCourteous, 4096,
		/* [37] */
		65535, 26214, 13107, pmCourteous, 4096,
		/* [38] */
		65535, 13107, 65535, pmCourteous, 4096,
		/* [39] */
		65535, 13107, 52428, pmCourteous, 4096,
		/* [40] */
		65535, 13107, 39321, pmCourteous, 4096,
		/* [41] */
		65535, 13107, 26214, pmCourteous, 4096,
		/* [42] */
		65535, 13107, 13107, pmCourteous, 4096,
		/* [43] */
		65535, 13107, 0, pmCourteous, 4096,
		/* [44] */
		65535, 0, 65535, pmCourteous, 4096,
		/* [45] */
		65535, 0, 52428, pmCourteous, 4096,
		/* [46] */
		65535, 0, 26214, pmCourteous, 4096,
		/* [47] */
		65535, 0, 13107, pmCourteous, 4096,
		/* [48] */
		65535, 0, 0, pmCourteous, 4096,
		/* [49] */
		52428, 65535, 65535, pmCourteous, 4096,
		/* [50] */
		52428, 65535, 52428, pmCourteous, 4096,
		/* [51] */
		52428, 65535, 39321, pmCourteous, 4096,
		/* [52] */
		52428, 65535, 26214, pmCourteous, 4096,
		/* [53] */
		52428, 65535, 13107, pmCourteous, 4096,
		/* [54] */
		52428, 65535, 0, pmCourteous, 4096,
		/* [55] */
		52428, 52428, 65535, pmCourteous, 4096,
		/* [56] */
		52428, 52428, 52428, pmCourteous, 4096,
		/* [57] */
		52428, 52428, 39321, pmCourteous, 4096,
		/* [58] */
		52428, 52428, 26214, pmCourteous, 4096,
		/* [59] */
		52428, 52428, 13107, pmCourteous, 4096,
		/* [60] */
		52428, 52428, 0, pmCourteous, 4096,
		/* [61] */
		52428, 39321, 65535, pmCourteous, 4096,
		/* [62] */
		52428, 39321, 52428, pmCourteous, 4096,
		/* [63] */
		52428, 39321, 39321, pmCourteous, 4096,
		/* [64] */
		52428, 39321, 26214, pmCourteous, 4096,
		/* [65] */
		52428, 39321, 13107, pmCourteous, 4096,
		/* [66] */
		52428, 39321, 0, pmCourteous, 4096,
		/* [67] */
		52428, 26214, 65535, pmCourteous, 4096,
		/* [68] */
		52428, 26214, 52428, pmCourteous, 4096,
		/* [69] */
		52428, 26214, 39321, pmCourteous, 4096,
		/* [70] */
		52428, 26214, 26214, pmCourteous, 4096,
		/* [71] */
		52428, 26214, 13107, pmCourteous, 4096,
		/* [72] */
		52428, 26214, 0, pmCourteous, 4096,
		/* [73] */
		52428, 13107, 65535, pmCourteous, 4096,
		/* [74] */
		52428, 13107, 52428, pmCourteous, 4096,
		/* [75] */
		52428, 13107, 39321, pmCourteous, 4096,
		/* [76] */
		52428, 13107, 26214, pmCourteous, 4096,
		/* [77] */
		52428, 13107, 13107, pmCourteous, 4096,
		/* [78] */
		52428, 13107, 0, pmCourteous, 4096,
		/* [79] */
		52428, 0, 65535, pmCourteous, 4096,
		/* [80] */
		52428, 0, 52428, pmCourteous, 4096,
		/* [81] */
		52428, 0, 39321, pmCourteous, 4096,
		/* [82] */
		52428, 0, 26214, pmCourteous, 4096,
		/* [83] */
		52428, 0, 13107, pmCourteous, 4096,
		/* [84] */
		52428, 0, 0, pmCourteous, 4096,
		/* [85] */
		39321, 65535, 65535, pmCourteous, 4096,
		/* [86] */
		39321, 65535, 52428, pmCourteous, 4096,
		/* [87] */
		39321, 65535, 39321, pmCourteous, 4096,
		/* [88] */
		39321, 65535, 26214, pmCourteous, 4096,
		/* [89] */
		39321, 65535, 13107, pmCourteous, 4096,
		/* [90] */
		39321, 65535, 0, pmCourteous, 4096,
		/* [91] */
		39321, 52428, 65535, pmCourteous, 4096,
		/* [92] */
		39321, 52428, 52428, pmCourteous, 4096,
		/* [93] */
		39321, 52428, 39321, pmCourteous, 4096,
		/* [94] */
		39321, 52428, 26214, pmCourteous, 4096,
		/* [95] */
		39321, 52428, 13107, pmCourteous, 4096,
		/* [96] */
		39321, 52428, 0, pmCourteous, 4096,
		/* [97] */
		39321, 39321, 65535, pmCourteous, 4096,
		/* [98] */
		39321, 39321, 52428, pmCourteous, 4096,
		/* [99] */
		39321, 39321, 39321, pmCourteous, 4096,
		/* [100] */
		39321, 39321, 26214, pmCourteous, 4096,
		/* [101] */
		39321, 39321, 13107, pmCourteous, 4096,
		/* [102] */
		39321, 39321, 0, pmCourteous, 4096,
		/* [103] */
		39321, 26214, 65535, pmCourteous, 4096,
		/* [104] */
		39321, 26214, 52428, pmCourteous, 4096,
		/* [105] */
		39321, 26214, 39321, pmCourteous, 4096,
		/* [106] */
		39321, 26214, 26214, pmCourteous, 4096,
		/* [107] */
		39321, 26214, 0, pmCourteous, 4096,
		/* [108] */
		39321, 13107, 65535, pmCourteous, 4096,
		/* [109] */
		39321, 13107, 52428, pmCourteous, 4096,
		/* [110] */
		39321, 13107, 39321, pmCourteous, 4096,
		/* [111] */
		39321, 13107, 26214, pmCourteous, 4096,
		/* [112] */
		39321, 13107, 13107, pmCourteous, 4096,
		/* [113] */
		39321, 13107, 0, pmCourteous, 4096,
		/* [114] */
		39321, 0, 65535, pmCourteous, 4096,
		/* [115] */
		39321, 0, 52428, pmCourteous, 4096,
		/* [116] */
		39321, 0, 39321, pmCourteous, 4096,
		/* [117] */
		39321, 0, 26214, pmCourteous, 4096,
		/* [118] */
		39321, 0, 13107, pmCourteous, 4096,
		/* [119] */
		39321, 0, 0, pmCourteous, 4096,
		/* [120] */
		26214, 65535, 65535, pmCourteous, 4096,
		/* [121] */
		26214, 65535, 52428, pmCourteous, 4096,
		/* [122] */
		26214, 65535, 39321, pmCourteous, 4096,
		/* [123] */
		26214, 65535, 26214, pmCourteous, 4096,
		/* [124] */
		26214, 65535, 13107, pmCourteous, 4096,
		/* [125] */
		26214, 65535, 0, pmCourteous, 4096,
		/* [126] */
		26214, 52428, 65535, pmCourteous, 4096,
		/* [127] */
		26214, 52428, 52428, pmCourteous, 4096,
		/* [128] */
		26214, 52428, 39321, pmCourteous, 4096,
		/* [129] */
		26214, 52428, 26214, pmCourteous, 4096,
		/* [130] */
		26214, 52428, 13107, pmCourteous, 4096,
		/* [131] */
		26214, 52428, 0, pmCourteous, 4096,
		/* [132] */
		26214, 39321, 65535, pmCourteous, 4096,
		/* [133] */
		26214, 39321, 52428, pmCourteous, 4096,
		/* [134] */
		26214, 39321, 39321, pmCourteous, 4096,
		/* [135] */
		26214, 39321, 26214, pmCourteous, 4096,
		/* [136] */
		26214, 39321, 13107, pmCourteous, 4096,
		/* [137] */
		26214, 39321, 0, pmCourteous, 4096,
		/* [138] */
		26214, 26214, 65535, pmCourteous, 4096,
		/* [139] */
		26214, 26214, 52428, pmCourteous, 4096,
		/* [140] */
		26214, 26214, 39321, pmCourteous, 4096,
		/* [141] */
		26214, 26214, 26214, pmCourteous, 4096,
		/* [142] */
		26214, 26214, 13107, pmCourteous, 4096,
		/* [143] */
		26214, 26214, 0, pmCourteous, 4096,
		/* [144] */
		26214, 13107, 65535, pmCourteous, 4096,
		/* [145] */
		26214, 13107, 52428, pmCourteous, 4096,
		/* [146] */
		26214, 13107, 39321, pmCourteous, 4096,
		/* [147] */
		26214, 13107, 26214, pmCourteous, 4096,
		/* [148] */
		26214, 13107, 13107, pmCourteous, 4096,
		/* [149] */
		26214, 0, 65535, pmCourteous, 4096,
		/* [150] */
		26214, 0, 52428, pmCourteous, 4096,
		/* [151] */
		26214, 0, 26214, pmCourteous, 4096,
		/* [152] */
		26214, 0, 13107, pmCourteous, 4096,
		/* [153] */
		26214, 0, 0, pmCourteous, 4096,
		/* [154] */
		13107, 65535, 65535, pmCourteous, 4096,
		/* [155] */
		13107, 65535, 52428, pmCourteous, 4096,
		/* [156] */
		13107, 65535, 39321, pmCourteous, 4096,
		/* [157] */
		13107, 65535, 26214, pmCourteous, 4096,
		/* [158] */
		13107, 65535, 13107, pmCourteous, 4096,
		/* [159] */
		13107, 65535, 0, pmCourteous, 4096,
		/* [160] */
		13107, 52428, 65535, pmCourteous, 4096,
		/* [161] */
		13107, 52428, 52428, pmCourteous, 4096,
		/* [162] */
		13107, 52428, 39321, pmCourteous, 4096,
		/* [163] */
		13107, 52428, 26214, pmCourteous, 4096,
		/* [164] */
		13107, 52428, 13107, pmCourteous, 4096,
		/* [165] */
		13107, 52428, 0, pmCourteous, 4096,
		/* [166] */
		13107, 39321, 65535, pmCourteous, 4096,
		/* [167] */
		13107, 39321, 52428, pmCourteous, 4096,
		/* [168] */
		13107, 39321, 39321, pmCourteous, 4096,
		/* [169] */
		13107, 39321, 26214, pmCourteous, 4096,
		/* [170] */
		13107, 39321, 13107, pmCourteous, 4096,
		/* [171] */
		13107, 39321, 0, pmCourteous, 4096,
		/* [172] */
		13107, 26214, 65535, pmCourteous, 4096,
		/* [173] */
		13107, 26214, 52428, pmCourteous, 4096,
		/* [174] */
		13107, 26214, 39321, pmCourteous, 4096,
		/* [175] */
		13107, 26214, 26214, pmCourteous, 4096,
		/* [176] */
		13107, 26214, 13107, pmCourteous, 4096,
		/* [177] */
		13107, 26214, 0, pmCourteous, 4096,
		/* [178] */
		13107, 13107, 65535, pmCourteous, 4096,
		/* [179] */
		13107, 13107, 52428, pmCourteous, 4096,
		/* [180] */
		13107, 13107, 39321, pmCourteous, 4096,
		/* [181] */
		13107, 13107, 26214, pmCourteous, 4096,
		/* [182] */
		13107, 13107, 13107, pmCourteous, 4096,
		/* [183] */
		13107, 13107, 0, pmCourteous, 4096,
		/* [184] */
		13107, 0, 65535, pmCourteous, 4096,
		/* [185] */
		13107, 0, 52428, pmCourteous, 4096,
		/* [186] */
		13107, 0, 39321, pmCourteous, 4096,
		/* [187] */
		13107, 0, 26214, pmCourteous, 4096,
		/* [188] */
		13107, 0, 13107, pmCourteous, 4096,
		/* [189] */
		13107, 0, 0, pmCourteous, 4096,
		/* [190] */
		0, 65535, 65535, pmCourteous, 4096,
		/* [191] */
		0, 65535, 52428, pmCourteous, 4096,
		/* [192] */
		0, 65535, 39321, pmCourteous, 4096,
		/* [193] */
		0, 65535, 26214, pmCourteous, 4096,
		/* [194] */
		0, 65535, 13107, pmCourteous, 4096,
		/* [195] */
		0, 65535, 0, pmCourteous, 4096,
		/* [196] */
		0, 52428, 65535, pmCourteous, 4096,
		/* [197] */
		0, 52428, 52428, pmCourteous, 4096,
		/* [198] */
		0, 52428, 39321, pmCourteous, 4096,
		/* [199] */
		0, 52428, 26214, pmCourteous, 4096,
		/* [200] */
		0, 52428, 13107, pmCourteous, 4096,
		/* [201] */
		0, 52428, 0, pmCourteous, 4096,
		/* [202] */
		0, 39321, 52428, pmCourteous, 4096,
		/* [203] */
		0, 39321, 39321, pmCourteous, 4096,
		/* [204] */
		0, 39321, 26214, pmCourteous, 4096,
		/* [205] */
		0, 39321, 13107, pmCourteous, 4096,
		/* [206] */
		0, 39321, 0, pmCourteous, 4096,
		/* [207] */
		0, 26214, 65535, pmCourteous, 4096,
		/* [208] */
		0, 26214, 52428, pmCourteous, 4096,
		/* [209] */
		0, 26214, 39321, pmCourteous, 4096,
		/* [210] */
		0, 26214, 26214, pmCourteous, 4096,
		/* [211] */
		0, 26214, 13107, pmCourteous, 4096,
		/* [212] */
		0, 13107, 65535, pmCourteous, 4096,
		/* [213] */
		0, 13107, 52428, pmCourteous, 4096,
		/* [214] */
		0, 13107, 39321, pmCourteous, 4096,
		/* [215] */
		0, 13107, 26214, pmCourteous, 4096,
		/* [216] */
		0, 13107, 13107, pmCourteous, 4096,
		/* [217] */
		0, 13107, 0, pmCourteous, 4096,
		/* [218] */
		0, 0, 65535, pmCourteous, 4096,
		/* [219] */
		0, 0, 52428, pmCourteous, 4096,
		/* [220] */
		0, 0, 39321, pmCourteous, 4096,
		/* [221] */
		0, 0, 26214, pmCourteous, 4096,
		/* [222] */
		0, 0, 13107, pmCourteous, 4096,
		/* [223] */
		61166, 0, 0, pmCourteous, 4096,
		/* [224] */
		48059, 0, 0, pmCourteous, 4096,
		/* [225] */
		43690, 0, 0, pmCourteous, 4096,
		/* [226] */
		34952, 0, 0, pmCourteous, 4096,
		/* [227] */
		30583, 0, 0, pmCourteous, 4096,
		/* [228] */
		21845, 0, 0, pmCourteous, 4096,
		/* [229] */
		17476, 0, 0, pmCourteous, 4096,
		/* [230] */
		8738, 0, 0, pmCourteous, 4096,
		/* [231] */
		4369, 0, 0, pmCourteous, 4096,
		/* [232] */
		0, 56797, 0, pmCourteous, 4096,
		/* [233] */
		0, 48059, 0, pmCourteous, 4096,
		/* [234] */
		0, 43690, 0, pmCourteous, 4096,
		/* [235] */
		0, 34952, 0, pmCourteous, 4096,
		/* [236] */
		0, 30583, 0, pmCourteous, 4096,
		/* [237] */
		0, 21845, 0, pmCourteous, 4096,
		/* [238] */
		0, 17476, 0, pmCourteous, 4096,
		/* [239] */
		0, 8738, 0, pmCourteous, 4096,
		/* [240] */
		0, 4369, 0, pmCourteous, 4096,
		/* [241] */
		0, 0, 61166, pmCourteous, 4096,
		/* [242] */
		0, 0, 48059, pmCourteous, 4096,
		/* [243] */
		0, 0, 43690, pmCourteous, 4096,
		/* [244] */
		0, 0, 34952, pmCourteous, 4096,
		/* [245] */
		0, 0, 30583, pmCourteous, 4096,
		/* [246] */
		0, 0, 21845, pmCourteous, 4096,
		/* [247] */
		0, 0, 17476, pmCourteous, 4096,
		/* [248] */
		0, 0, 8738, pmCourteous, 4096,
		/* [249] */
		0, 0, 4369, pmCourteous, 4096,
		/* [250] */
		61166, 61166, 61166, pmCourteous, 4096,
		/* [251] */
		56797, 56797, 56797, pmCourteous, 4096,
		/* [252] */
		43690, 43690, 43690, pmCourteous, 4096,
		/* [253] */
		34952, 34952, 34952, pmCourteous, 4096,
		/* [254] */
		17476, 17476, 17476, pmCourteous, 4096,
		/* [255] */
		8738, 8738, 8738, pmCourteous, 4096,
		/* [256] */
		4369, 4369, 4369, pmCourteous, 4096
	}
};

resource 'snd ' (1000, "Click up", purgeable) {
	FormatOne {
		{	/* array Synthesizers: 1 elements */
			/* [1] */
			sampledSynth, 160
		}
	},
	{	/* array SoundCmnds: 1 elements */
		/* [1] */
		hasData,
		bufferCmd {
			20
		}
	},
	{	/* array DataTables: 1 elements */
		/* [1] */
		268,
		0x2B7745D1,
		266,
		267,
		0x0,
		0x3C,
		$"808F 9874 4062 A9A1 8680 8071 658D 9F6E"
		$"6880 987D 7986 9573 658C 9D6A 6B8F 7A78"
		$"8F99 6A70 8286 7579 8692 8365 6B9C 9D7D"
		$"6D71 7A77 9599 7D62 7192 927D 7381 836E"
		$"798B 947C 787C 7579 8898 866C 717F 8A7F"
		$"8381 837A 7A7A 7D83 8486 7E79 8186 7D7A"
		$"8184 797D 8481 7D84 827A 7681 897F 7A81"
		$"867A 7C83 847D 7D83 7D7A 7D84 8081 8281"
		$"7C7D 8183 8180 7C7A 8086 867E 7E7C 7D80"
		$"827F 7F7F 8281 7F7D 7F7E 7E7F 8282 7F7F"
		$"817F 7C7E 8383 807F 7D7D 8183 807D 7E80"
		$"807F 7E82 7D7F 8083 817D 7D86 837C 7D80"
		$"8380 7D7F 7F7F 8082 807D 7D81 817F 8080"
		$"827E 7E7E 7E82 817F 7D7D 7E81 8080 8280"
		$"7F7E 7F7F 7F7E 8082 807F 7D80 807F 807F"
		$"7E7F 8083 807E 7C80 8282 7D7D 7F7F 8081"
		$"807F 7F7F 7E7E 8080 8181 7F7E"
	}
};

resource 'snd ' (1001, "Click down", purgeable) {
	FormatOne {
		{	/* array Synthesizers: 1 elements */
			/* [1] */
			sampledSynth, 160
		}
	},
	{	/* array SoundCmnds: 1 elements */
		/* [1] */
		hasData,
		bufferCmd {
			20
		}
	},
	{	/* array DataTables: 1 elements */
		/* [1] */
		552,
		0x2B7745D1,
		550,
		551,
		0x0,
		0x3C,
		$"7F7F 7F7F 7F7F 818D 8676 763C 8F81 C370"
		$"8B42 83A2 A56A 558B 799B 8196 658D 9879"
		$"4C4F 86A4 B273 8771 765E 897D A987 556A"
		$"9FB0 7D7D 734D 8F99 9168 7A80 7D73 9686"
		$"6880 8D87 7382 7F77 7492 808A 7386 8086"
		$"7377 7A7D 8D90 8B6D 767D 7E7D 868C 7D7C"
		$"7C7C 7C86 897D 7475 8387 898A 7C73 8083"
		$"8078 848A 7773 7E86 8386 7C7F 7D78 7A84"
		$"8384 867E 797D 827E 8281 7D7A 8183 7E83"
		$"837E 7E7E 7D78 7F86 877A 7A81 847C 867D"
		$"7779 8886 817D 8081 7D7A 7C81 8283 847D"
		$"7A7C 8480 7F7D 7E7E 8386 7F79 7D81 8181"
		$"807D 7C83 8680 7A7D 7E81 8382 7A7E 8482"
		$"7D7A 807D 807F 8383 7E7D 8381 7C7A 8384"
		$"7F82 7E7D 7F83 7D7D 7D81 8082 8381 807C"
		$"817E 7D7D 8286 837C 7C7D 8480 827D 7A7D"
		$"8683 7D7D 817E 8082 7C7A 7F82 837F 817C"
		$"7D82 8181 7D7E 807E 867D 807D 8080 7D7E"
		$"7F7F 8282 807D 7D81 817D 7D81 7F7F 8181"
		$"7C7E 8382 7A7D 8280 807F 7E7D 7E83 817D"
		$"7D81 837E 7E7D 7D81 8381 7D7D 7F82 8080"
		$"7D7E 7F80 8081 7F7D 807E 7E7E 827F 7F82"
		$"7F7E 7F83 817D 7983 8383 7D7E 7D7E 8181"
		$"7D7F 8081 7E7F 817F 7D82 807D 7D82 8780"
		$"7C7D 7F7F 7F82 807D 7D82 837F 7D7D 8081"
		$"807F 7D82 8681 7A7A 8082 8382 7E7D 7D80"
		$"8380 7F7F 7E80 8080 7F7E 7F7D 8281 817F"
		$"7D7E 8082 7E7D 7E7F 8181 827E 7E7E 7F7F"
		$"8280 807F 7E81 7D7F 8082 7F7D 7F82 7F7F"
		$"7F80 7E7E 7F81 8181 807D 7E7F 8182 8080"
		$"817F 7D7E 8181 7E80 7E7E 8081 827D 7C80"
		$"8281 7F7E 8183 7F7E 7D7F 8082 807E 7F80"
		$"7E7E 8081 807D 7F80 8182 8180 7D7D 8082"
		$"807F 7F80 7F7E 7E7F 8081 807D 7E80 817F"
		$"7F7F 7F7F 8082 807F"
	}
};
