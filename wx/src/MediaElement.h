// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef MediaElement_H
#define MediaElement_H

//////////
// An abstract interface for elements which play media streams.  This
// is used as a mixin class.
//
class IMediaElement
{
public:
    IMediaElement() {} 
	virtual ~IMediaElement() {}
	
    //////////
    // Pause the media stream.
    //
    // Note: This method may not be happy if the underlying movie layer
    // doesn't like to be paused.  In particular, streaming network
    // QuickTime is chancy.
    //
    virtual void Pause() = 0;

    //////////
    // Resume playing the media stream after a pause.
    //
    virtual void Resume() = 0;
};

#endif // MediaElement_H
