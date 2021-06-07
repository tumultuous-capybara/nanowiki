#lang nanowiki

@title{Nanowiki Architecture Notes}

@section{Connections}

@bold{Design Goals:} The connection API needs to allow for bi-directional linking between pages, at the data level, adding a reference to each. Additionally, each reference needs to be described in the document, typically in the format of a paragraph of text, as well as having queryable semantic data. Lastly, auxiliary functionality, like adding a link fragment, changing the link title, and hiding the link content.

@bold{Constraints & Considerations:} One possible split is between raw data provided to the pages and functions to handle them, where the data is both the arguments and static data, interpreted into document content by functions, as a very restricted message-passing system. This brings up other questions about the shape of the data, particularly, if there should be a single data node, or two shared pieces of data, one for each direction of the connection. Additionally, it's unintuitive that adding a half connection from a page requires no details from the page itself, instead using mostly the description data pulled from the other page. The reverse isn't any more descriptive, though.

A big pattern is splitting up connections into types with names and mostly the same behavior, 'overview' for a wide-ranging category with lots of links, 'summary' for a shorter brief on the topic, 'textual-link', 'super-category', etc.

An alternative is giving them access to the page modules in order to pull defined values right out of them, as well as calling defined functions. This adds some likely namespace conflicts, but makes organization very intuitive. Additionally, pages can just directly import what they want to re-export from helpful racket code, with the `reprovide` macro.

Page-modules are closer to the message-passing object ideal, but there's still the split between connection messages and functions, when the often have a one-to-one mapping. Functions that generate connection data is a fine approach, but makes connections more complex than they need to be, as opposed to simply functions with a few extra keyword arguments. The biggest problem with the page-module approach is the fact that new, unusual syntax has to be learned for calling the function in a different page, even for people that know racket.

Assuming the module-based solution, that throws another problem into bright relief which is present in all connection schemes: temporality and the API avaliable to page authors, as well as what data is avaliable to be queried. If I wanted to maximize the amount of fully dynamic data avaliable, I could map out the dependencies of each query into a graph, then topological sort to find the correct order. The macros made avalaible to the user will have to be very carefully constructed in order to provide expressive power and enable the user to collect and transform it post-connection-pass, and pull in information on it while building connection data. A more robust solution is to seperate connection data into a static and dyanmic data, with the former typically a single symbol, but capable of taking any object.

@bold{Edge Updating:}If the functions that determine connection data are pure, it's also possible to update them be evaluating them again, though the arbitrary nature of the computation would mean the whole thing would need to be updated in multiple async scheduled passes, until everything was static. This also likely results in endless update loops, with functions fighting and deadlocking over describing things. Error reporting from this situation is also nearly impossible, not to mention the user's surprise that state updates to the page don't work as expected. (particularly heading and hidden status changes)

@bold{Dependency Graphs & Scheduling: }

@bold{Phases of Evaluation:}

@bold{Macro Design:}

@section{Connection Document Formatting & Headers}

The default formatting of connections has a significant impact on how they're utilized, and how easy it is to scan them for relevant information. Specifically, the two broad options are prose-style, with a bold link to the page in question inside the paragraph itself, or something more like a heading, with the full, capitalized name, followed by the paragraph. The optional header system is also highly related to this, which forms section-like headings and groups connections under them.


@section{Page/File Mapping}

When the styles and tools of nanowiki work better with small pages, it's necessary for files that start growing out of control to
