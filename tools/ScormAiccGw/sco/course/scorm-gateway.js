// This library provides a glue layer between SCORM and a protocol which
// closely resembles AICC AGR-006, a popular standard for Windows-based
// training software.  For now, Internet Explorer 6.0 for Windows is
// required, although other platforms could be supported in the future.
var ScormGateway = {
    // The SCORM API object.
    API: null,

    // The GUID of the course to launch.
    guid: null,

    // Initialize the SCORM gateway.  Generally called from
    // <code>onload</code>.
    initialize: function (guid) {
        this.guid = guid;

        // Check browser compatibility.
        if (!this._isSupportedBrowser()) {
            this._setState('controlMissing');
            return;
        }

        // Set up our ActiveX control.
        //var controlGuid = 'CLSID:8B4E4F9B-E7D0-4DFF-82AC-D92E06E4B835';
        //this._gateway = document.createElement('object');
        //this._gateway.id = '_gateway';
        //this._gateway.width = 240;
        //this._gateway.height = 60;
        //this._gateway.classid = controlGuid;
        //$('launchButtonContainer').appendChild(this._gateway);
        //this._gateway.CourseGUID = this._guid
        this._gateway = $('_gateway');

        // Make sure our course is installed.
        try {
            if (!this._gateway.isCourseInstalled()) {
                this._setState('courseMissing');
                return;
            }
        } catch (e) {
            // If an error occurs, we almost certainly don't have the control.
            this._setState('controlMissing');
            return;            
        }
        
        // Connect to the LMS.
        // TODO - Ultimately, how should we handle a missing LMS?
        this.API = this._findAPI(window);
        if (this.API) {
            // TODO - GetLastError
            this.API.LMSInitialize("");
            this.API.LMSSetValue("cmi.core.lesson_status", "incomplete");
        }

        this._setState('courseReady');

        var obj = this;
        setTimeout(function () { obj._setState('courseDone'); }, 20000);
    },

    // Shut down the SCORM gateway.  Generally called from
    // <code>onunload</code>.
    finish: function () {
        // Disconnect from the LMS.
        if (this.API) {
            // TODO - GetLastError
            this.API.LMSFinish("")
        }
    },

    // Launch the course.
    launch: function () {
        // Check the current state before actually doing anything, to
        // prevent multiple launches and other unpleasantness.
        if (this._state == 'courseReady') {
            this._setState('courseRunning');
            this._gateway.launchCourse(this.guid, "Blah, blah, blah.");
            this._sendIdleEventsToGateway();
        }
    },

    // Called from our onAppTerminated event handler.
    onAppTerminated: function () {
        //this._setState('courseDone');
        //var obj = this;
        //setTimeout(function () { obj._setState('courseDone'); }, 10000);
        // TODO - Need to shut down LMS once course is done.
    },

    //=== Private

    // The different states the gateway may be in.  The main course
    // page should have a element for each state.
    _states: ["controlMissing", "courseMissing", "courseReady",
              "courseRunning", "courseDone"],

    // Our current state.
    _state: null,

    // A reference to our ActiveX control.
    _gateway: null,

    // Set our state to the specified value, and show and hide elements
    // as needed on the main page.
    _setState: function (state) {
        if (this._state == state) {
            return;
        } else {
            if (this._state) {
                Element.hide(this._state);
            }
            this._state = state;
            Element.show(state);
        }
    },

    // Given a window _win_, find the associated SCORM API object.
    _findAPI: function (win) {
        // Walk up through our window hierarchy, and look for a SCORM API,
        // starting with the specified window.  This was inspired by
        // <http://www.claroline.net/doc/en/index.php/
        // How_do_I_create_SCORM_content%3F>.
        if (win.API) {
            return win.API;
        } else if (win.parent != null && win.parent != win) {
            return FindScormApi(win.parent);
        } else if (win.opener != null && win.opener != win) {
            return FindScormApi(win.opener);
        } else {
            alert("Cannot connect to the LMS!  Your progress won't be " +
                  "saved, and parts off this course may not work.");
            return null;
        }
    },

    // Pass idle events to our ActiveX control, allowing it to poll for
    // course termination.
    _sendIdleEventsToGateway: function () {
        this._gateway.onIdle();
        if (this._state == "courseRunning") {
            function repeat() { ScormGateway._sendIdleEventsToGateway(); }
            setTimeout(repeat, 2000);
        }
    },

    // Determine whether we support the user's browser.  This code is
    // adapted from <http://msdn.microsoft.com/workshop/author/dhtml/overview/
    // browserdetection.asp>.
    _isSupportedBrowser: function () {
        if (navigator.appName == 'Microsoft Internet Explorer') {
            var re = new RegExp("MSIE ([0-9]{1,}[\.0-9]{0,})");
            if (re.exec(navigator.userAgent) != null
                && parseFloat(RegExp.$1) >= 6.0) {
                return true;
            }
        }
        return false;
    }
}
