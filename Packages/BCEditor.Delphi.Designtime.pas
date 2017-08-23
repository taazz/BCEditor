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

type
  TBCEditorFontProperty = class(TClassProperty)
  public
    function GetAttributes(): TPropertyAttributes; override;
    procedure Edit(); override;
  end;

{******************************************************************************}

procedure Register();
begin
  RegisterComponents('BCEditor', [TBCEditor, TBCEditorMacroRecorder]);

  UnlistPublishedProperty(TBCEditor, 'CustomHint');
  UnlistPublishedProperty(TBCEditor, 'DoubleBuffered');
  UnlistPublishedProperty(TBCEditor, 'ParentCustomHint');
  UnlistPublishedProperty(TBCEditor, 'ParentColor');
  UnlistPublishedProperty(TBCEditor, 'Hint');

  RegisterPropertyEditor(TypeInfo(Char), nil, '', TCharProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), nil, '', TStringListProperty);
  RegisterPropertyEditor(TypeInfo(TFont), nil, '', TBCEditorFontProperty);
end;

{ TBCEditorFontProperty *******************************************************}

procedure TBCEditorFontProperty.Edit();
var
  LFontDialog: TFontDialog;
begin
  LFontDialog := TFontDialog.Create(Application);
  LFontDialog.Options := LFontDialog.Options
    - [fdEffects, fdFixedPitchOnly];
  LFontDialog.Font := TFont(GetOrdValue());
  if (LFontDialog.Execute(0)) then
    SetOrdValue(Longint(LFontDialog.Font));
end;

function TBCEditorFontProperty.GetAttributes(): TPropertyAttributes;
begin
  Result := inherited GetAttributes() + [paDialog] - [paSubProperties];
end;

end.
