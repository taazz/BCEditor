unit BCEditor.Delphi.Designtime;

interface {********************************************************************}

uses
  Classes,
  BCEditor, BCEditor.Commands, BCEditor.Properties;

procedure Register();

implementation {***************************************************************}

uses
  Windows,
  Graphics, Dialogs, Forms,
  DesignEditors, DesignIntf, StrEdit, VCLEditors;

{******************************************************************************}

procedure Register();
begin
  RegisterComponents('BCEditor', [TBCEditor, TBCEditorMacroRecorder]);

  UnlistPublishedProperty(TBCEditor, 'Cursor');
  UnlistPublishedProperty(TBCEditor, 'CustomHint');
  UnlistPublishedProperty(TBCEditor, 'Hint');
  UnlistPublishedProperty(TBCEditor, 'ParentCustomHint');
  UnlistPublishedProperty(TBCEditor, 'ParentColor');

  RegisterPropertyEditor(TypeInfo(Char), nil, '', TCharProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), nil, '', TStringListProperty);
end;

end.
