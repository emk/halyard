#pragma once

class CPlayerPane : public LPane {
	
	public:
		void InitPlayerPane(const SPaneInfo	&inPaneInfo);
		void SPaneInfo CreatePaneInfo(LView *superView, Rect inRect);
};