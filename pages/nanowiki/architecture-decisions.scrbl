#lang nanowiki

@title{Nanowiki Archetecture Decisions}

@section{Connections}

@bold{Design Constraints, Considerations, and Goals:} The connection API needs to allow for bi-directional linking between pages, at the data level, adding a reference to each. Additionally, each reference needs to be described in the document, typically in the format of a paragraph of text, as well as having queryable semantic data.

One possible split is between raw data provided to the pages and functions to handle them, where the data is both the arguments and static data, interpreted into document content by functions, but this is essentially just a cut-rate message passing system.

A big pattern is splitting up connections into types with names and mostly the same behavior, 'overview' for a wide-ranging category with lots of links, 'summary' for a shorter brief on the topic, 'textual-link', 'super-category', etc.

An alternative is giving them access to the page modules in order to pull defined values right out of them, as well as calling defined functions. This adds some likely namespace conflicts, but makes organization very intuitive. Additionally, pages can just directly import what they want to re-export from helpful racket code, with the `reprovide` macro.

Assuming the module-based solution, that throws another problem into bright relief: temporality and the API avaliable to page authors, as well as what data is avaliable to be queried Connections
