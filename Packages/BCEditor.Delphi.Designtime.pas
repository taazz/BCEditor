unit BCEditor.Delphi.Designtime;

interface {********************************************************************}

uses
  Classes,
  BCEditor, BCEditor.Print, BCEditor.Print.Preview, BCEditor.MacroRecorder,
  BCEditor.Properties;

procedure Register();

implementation {***************************************************************}

uses
  DesignEditors, DesignIntf, StrEdit, VCLEditors;

procedure Register;
begin
  RegisterComponents('BCEditor', [TBCEditor, TBCEditorPrint, TBCEditorPrintPreview, TBCEditorMacroRecorder]);

  UnlistPublishedProperty(TBCEditor, 'CustomHint');
  UnlistPublishedProperty(TBCEditor, 'Hint');
  RegisterPropertyEditor(TypeInfo(Char), nil, '', TCharProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), nil, '', TStringListProperty);
end;

end.
