<h3>Description</h3>

A syntax highlighting edit control for the RAD Studio (Delphi and C++ Builder VCL):
* Predefined highlighter definitions for 70 programming languages
* User defined highlighter definions
* Multi highglighter support (for example: HTTP and PHP)
* Matching pair highlighting (for example "<" and ">" in HTML)
* Search with regular expressions, forwards / backwards, whole words, case sensitive, selection only
* Search result highlighting
* Bookmarks
* User defined marks
* Word wrap
* Code folding
* Synchron edit
* Completion proposal
* Undo, redo
* Fonts with variable character width
* Macro recorder

<h3>Build requirements</h3>

* Delphi XE4 or higher
* C++ Builder XE4 or higher

<h3>Usage example</h3>

```objectpascal
  with BCEditor1 do 
  begin
    Highlighter.LoadFromFile('JSON.json');
    Highlighter.Colors.LoadFromFile('Default.json'); 
    LoadFromFile(GetHighlighterFileName('JSON.json')); 
    ...
    Lines.Text := Highlighter.Info.General.Sample; 
  end;
```
<b>Note!</b> LoadFromStream / LoadFromResource does not support multi-highlighters (for example HTML with Scripts.json). Override TBCBaseEditor.CreateFileStream function, if you want to load multi-highlighters from a stream.

<h3>Demo</h3>

  * <a href="http://www.mysqlfront.de/bonecode/BCEditor_Demo.zip">BCEditor Demo</a>

<h3>Screenshot</h3>

![bceditor0](https://cloud.githubusercontent.com/assets/11475177/20067778/2e403442-a51f-11e6-8c3e-532ae48b7d72.png)
