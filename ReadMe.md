<h1>Note</h1>
<b>This component is very buggy and should not be used. Please use "SynEdit"
instead of it.</b> If there is somebody who wants to overtake this project,
please write an issue and ask about it.

<h3>Description</h3>

A syntax highlighting edit control for the RAD Studio (Delphi and C++ Builder VCL):
* Predefined highlighter definitions for 70 programming languages
* User defined highlighter definions
* Multi highglighter support (for example: HTTP and PHP)
* Minimap
* Matching pair highlighting (for example "<" and ">" in HTML)
* Search with regular expressions, forwards / backwards, whole words, case sensitive, selection only
* Search result highlighting
* Bookmarks
* User defined marks
* Word wrap
* Code folding
* Synchron edit
* Completion proposal popup window
* Unlimited undo and redo
* Macro recorder
* Fonts with variable character width
* Windows DPI Aware

<h3>Build requirements</h3>

* Delphi XE4 or higher
* C++ Builder XE6 or higher

<h3>Usage example</h3>

```objectpascal
  with BCEditor1 do 
  begin
    Highlighter.LoadFromFile('JSON.json');
    Highlighter.Colors.LoadFromFile('Default.json'); 
    ...
    Text := Highlighter.Sample; 
  end;
```
<b>Note!</b> LoadFromStream / LoadFromResource does not support multi-highlighters (for example HTML with Scripts.json). Override TBCBaseEditor.CreateFileStream function, if you want to load multi-highlighters from a stream.

<h3>Demo</h3>

  * <a href="http://www.mysqlfront.de/bonecode/BCEditor_Demo.zip">BCEditor Demo</a>

<h3>Screenshot</h3>

![bceditor0](https://cloud.githubusercontent.com/assets/11475177/20067778/2e403442-a51f-11e6-8c3e-532ae48b7d72.png)
