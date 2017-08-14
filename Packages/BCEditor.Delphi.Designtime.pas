unit BCEditor.Delphi.Designtime;

interface {********************************************************************}

uses
  Classes,
  BCEditor, BCEditor.Commands, BCEditor.Properties;

procedure Register();

implementation {***************************************************************}

uses
  DesignEditors, DesignIntf, StrEdit, VCLEditors;

procedure Register;
begin
  RegisterComponents('BCEditor', [TBCEditor, TBCEditorMacroRecorder]);

  UnlistPublishedProperty(TBCEditor, 'CustomHint');
  UnlistPublishedProperty(TBCEditor, 'ParentCustomHint');
  UnlistPublishedProperty(TBCEditor, 'ParentColor');
  UnlistPublishedProperty(TBCEditor, 'Hint');

  RegisterPropertyEditor(TypeInfo(Char), nil, '', TCharProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), nil, '', TStringListProperty);
end;

end.
