#pragma once

BEGIN_NAMESPACE_FIVEL

class CPlayerPane : public LPane {
	
	public:
		void InitPlayerPane(const SPaneInfo	&inPaneInfo);
		void SPaneInfo CreatePaneInfo(LView *superView, Rect inRect);
};

END_NAMESPACE_FIVEL