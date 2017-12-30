# Revision history for pancake

## 0.1.9 -- 2017-12-30

* New features:
  * Position retention on reload, redisplay, and history navigation.
  * Use of Haskeline with history-based URI completion.
  * Partial (and optional) line wrapping delegation to Emacs.
  * `pancake-uri-at-point' command.
* A few uncluttering XSLTs are included into the distribution.
* Other minor improvements.


## 0.1.8 -- 2017-12-19

* XSLT-based web page transformation (uncluttering) support is
  introduced.
* Breaking pancake.el change: `pancake-browse-url' is removed.
* Bugfix: empty tables handling.
* A few minor improvements.


## 0.1.7 -- 2017-12-09

* 'b' and 'f' commands are replaced with '[' and ']'.
* The man page is elaborated.
* Other minor adjustments, improvements, and bugfixes.


## 0.1.6 -- 2017-11-28

* New features:
  * User-defined digits and radices for reference numbering.
  * File saving.
  * Image support in the Emacs interface.
  * Handling of Emacs "mode" file variables.
  * Optional div indentation.
* Minor improvements and adjustments.


## 0.1.5 -- 2017-11-20

* A man page is added.
* Slightly improved SIGINT handling.
* Gopher directory parsing is relaxed.
* Handling of --help and --version options.
* Minor bugfixes and Emacs interface improvements.
* Makefile is introduced to generate binary releases.


## 0.1.4 -- 2017-11-12

* Handling of footnotes and table column alignments.
* Emacs interface improvements:
  * Completing reading based on URI history.
  * Navigation through section headings.
* Other minor adjustments and bugfixes.


## 0.1.3 -- 2017-11-06

* New features:
  * User can explicitly tell which reader to use for a document.
  * Metadata (content type, effective uri) reading from downloader
    command output.
  * Denotations for embedding, buttonized hyperlinks in the Emacs
    interface.
* Minor improvements and bugfixes.


## 0.1.2 -- 2017-11-03

* New features:
  * Document reloading.
  * Table rendering.
  * Fragment identifiers handling.
* Minor improvements and bugfixes.


## 0.1.1 -- 2017-10-31

* An Emacs interface is introduced.
* Minor adjustments and bugfixes.


## 0.1.0.0 -- 2017-10-26

* First version.
