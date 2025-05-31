# Elm Timelines

Elm Timelines is a user interface for the creation of timelines, written in [Elm](https://elm-lang.org). You can use it e.g. as an aid for remembering your life. The special trait of Elm Timelines is that it deals with uncertainty. You're not supposed to enter dates but draw timespans within a timescale. If you're not (yet) certain about a timespan's begin or end you can add a range of uncertainty. Elm Timelines visualizes this as "fade-in" resp. "fade-out".

![Elm Timelines UI](doc/elm-timelines.png)

Despite Elm Timelines models and visualizes time it is not a calendar application. Instead Elm Timelines provides you a canvas for your mind e.g. for your personal memory process. By visualizing different aspects of your life e.g. living places, partners, projects, the screen may inspire you with new questions and jog your memory. However Elm Timelines does not force you into any topic and you can go to the future as well.

## Setup

To use Elm Timelines just click the link below. You don't need an account. You can just start with entering data. When you revisit that link later on, your data is still there. The data you enter is stored locally *in your browser* (using `localStorage`). Your data never leaves your computer. No one else can access it.

[elm-timelines.html](https://jri.github.io/elm-timelines/elm-timelines.html)

> Hint: If you use Elm Timelines in a **Private Browser Window** data you enter will *not* be available later on. If you want store your data permanently (in your browser) don't use Elm Timelines in a private browser window.

### Working offline

Elm Timelines does not need a web server at all. You can download (by right-clicking) the HTML file above -- it's just 41 KB -- and open it locally in your browser. No internet required from then on. The same as above applies: the data you enter is stored locally in your browser. If you reopen that HTML file (on your hard drive) later on your data is still there.

> Hints: You can start working online, entering data, and later on decide to use the offline approach. In this case a manual step is required: you need to transfer your data from one local-storage to another (within your browser). You can do this by the means of the browser console's "Storage" tab and copying a single JSON string. Open a GitHub ticket if you need help.

> Also if you move the downloaded HTML file to another location on your hard drive you need to transfer your data like mentioned above.

The situations described here in the hints are a consequence of how [localStorage](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage) works.

## How to use

The Elm Timelines UI consists of a global *time scale* and several thematic *timelines*. A timeline contains several *timespans*. Each timespan has a begin and an end plus respective *uncertainty ranges*. Timespans can overlap. There is also a *page title*.

To create a timespan point the mouse to a timeline and draw a rectangle.

![Creating a timespan](doc/create-timespan.png)

To move a timespan within a timeline drag it with the mouse. To adjust its begin and end drag its middle handlers. To adjust the begin's or end's uncertainty range drag the respective upper handlers.

![The timespan handles](doc/timespan-handles.png)

To globally zoom in and out the time scale use the `+` and `-` buttons.

To edit a timeline's or a timespan's title click on it. The same applies to the page title.

![Editing a title](doc/edit-title.png)

The default time scale is set to 1960-2040. You can change the time scale any time by using the `Settings` button.

![The settings dialog](doc/settings-dialog.png)

To delete a timeline or a timespan select it and press the `Delete` button. To select a timeline click on its "header" area.

![Selecting a timeline](doc/select-timeline.png)



## Managing your data

## Development
