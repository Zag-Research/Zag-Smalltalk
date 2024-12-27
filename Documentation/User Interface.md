Supporting all the I/O components that provide the UI in Pharo, Cuis, or Squeak seems like a lot of work for limited value - or more accurately, it isn't where we see our contribution to Smalltalk.

Instead, we are looking for interfaces that will run remotely, so all we need to provide isthe network support, and the API for the external interface to be able to talk to Zag.

#### [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) [spec 3.18](https://github.com/microsoft/language-server-protocol/blob/gh-pages/_specifications/lsp/3.18/specification.md) [Wikipedia](https://en.wikipedia.org/wiki/Language_Server_Protocol)

The Language Server Protocol is one way to get a GUI for Zag. It is supported by tools such as VSCode and Emacs: [Pharo-LanguageServer](https://github.com/badetitou/Pharo-LanguageServer?tab=readme-ov-file) and [LSP discussion on forum.world.st](http://forum.world.st/Language-Server-Protocol-td4933444.html) (see particularly Stephan Marr’s comment)
#### [Webside](https://github.com/guillermoamaral/Webside)

Another possibility for Zag GUI.

#### Home-grown UI
Mohammed had been working on an interface based on PharoJS and talking directly to the standard Pharo UI code. Unfortunately this code has been lost.