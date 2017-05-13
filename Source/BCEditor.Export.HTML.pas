unit BCEditor.Export.HTML;

interface

uses
  Classes, SysUtils,
  Graphics,
  BCEditor.Lines, BCEditor.Highlighter;

type
  TBCEditorExportHTML = class(TObject)
  strict private type
    TBCEditorLines = class(BCEditor.Lines.TBCEditorLines);
  private
    FCharSet: string;
    FFont: TFont;
    FHighlighter: TBCEditorHighlighter;
    FLines: TBCEditorLines;
    FStringList: TStrings;
    FTabWidth: Integer;
    procedure CreateFooter;
    procedure CreateHeader;
    procedure CreateHTMLDocument;
    procedure CreateInternalCSS;
    procedure CreateLines;
  public
    constructor Create(ALines: BCEditor.Lines.TBCEditorLines;
      AHighlighter: TBCEditorHighlighter; AFont: TFont; const ATabWidth: Integer;
      const ACharSet: string); overload;
    destructor Destroy; override;
    procedure SaveToStream(AStream: TStream; AEncoding: System.SysUtils.TEncoding);
  end;

implementation

uses
  Windows,
  UITypes, StrUtils,
  BCEditor.Consts, BCEditor.Utils;

constructor TBCEditorExportHTML.Create(ALines: BCEditor.Lines.TBCEditorLines;
  AHighlighter: TBCEditorHighlighter; AFont: TFont; const ATabWidth: Integer;
  const ACharSet: string);
begin
  inherited Create;

  FStringList := TStringList.Create;

  FCharSet := ACharSet;
  if FCharSet = '' then
    FCharSet := 'utf-8';
  FLines := TBCEditorLines(ALines);
  FHighlighter := AHighlighter;
  FFont := AFont;
  FTabWidth := ATabWidth;
end;

destructor TBCEditorExportHTML.Destroy;
begin
  FStringList.Free;

  inherited Destroy;
end;

procedure TBCEditorExportHTML.CreateFooter;
begin
  FStringList.Add('</body>');
  FStringList.Add('</html>');
end;

procedure TBCEditorExportHTML.CreateHeader;
begin
  FStringList.Add('<!DOCTYPE HTML>');
  FStringList.Add('');
  FStringList.Add('<html>');
  FStringList.Add('<head>');
  FStringList.Add('  <meta charset="' + FCharSet + '">');

  CreateInternalCSS;

  FStringList.Add('</head>');
  FStringList.Add('');
  FStringList.Add('<body class="Editor">');
end;

procedure TBCEditorExportHTML.CreateHTMLDocument;
begin
  if not Assigned(FHighlighter) then
    Exit;
  if FLines.Count = 0 then
    Exit;

  CreateHeader;
  CreateLines;
  CreateFooter;
end;

procedure TBCEditorExportHTML.CreateInternalCSS;

  function ColorToHex(const AColor: TColor): string;
  begin
    Result := IntToHex(GetRValue(AColor), 2) + IntToHex(GetGValue(AColor), 2) + IntToHex(GetBValue(AColor), 2);
  end;

var
  LElement: TBCEditorHighlighter.PElement;
  LIndex: Integer;
  LStyles: TList;
begin
  FStringList.Add('  <style>');

  FStringList.Add('    body {');
  FStringList.Add('      font-family: ' + FFont.Name + ';');
  FStringList.Add('      font-size: ' + IntToStr(FFont.Size) + 'px;');
  FStringList.Add('    }');

  LStyles := FHighlighter.Colors.Styles;
  for LIndex := 0 to LStyles.Count - 1 do
  begin
    LElement := LStyles.Items[LIndex];

    FStringList.Add('    .' + LElement^.Name + ' { ');
    FStringList.Add('      color: #' + ColorToHex(LElement^.Foreground) + ';');
    FStringList.Add('      background-color: #' + ColorToHex(LElement^.Background) + ';');

    if TFontStyle.fsBold in LElement^.FontStyles then
      FStringList.Add('      font-weight: bold;');

    if TFontStyle.fsItalic in LElement^.FontStyles then
      FStringList.Add('      font-style: italic;');

    if TFontStyle.fsUnderline in LElement^.FontStyles then
      FStringList.Add('      text-decoration: underline;');

    if TFontStyle.fsStrikeOut in LElement^.FontStyles then
      FStringList.Add('      text-decoration: line-through;');

    FStringList.Add('    }');
    FStringList.Add('');
  end;
  FStringList.Add('  </style>');
end;

procedure TBCEditorExportHTML.CreateLines;
var
  LColumn: Integer;
  LHighlighterAttribute: TBCEditorHighlighter.TAttribute;
  LIndex: Integer;
  LPreviousElement: string;
  LTextLine: string;
  LToken: string;
begin
  LPreviousElement := '';
  for LIndex := 0 to FLines.Count - 1 do
  begin
    if LIndex = 0 then
      FHighlighter.ResetCurrentRange
    else
      FHighlighter.SetCurrentRange(FLines.Items[LIndex - 1].Range);
    FHighlighter.SetCurrentLine(FLines[LIndex]);
    LTextLine := '';
    LColumn := 0;
    while not FHighlighter.GetEndOfLine do
    begin
      LHighlighterAttribute := FHighlighter.GetTokenAttribute;
      FHighlighter.GetTokenText(LToken);
      if LToken = BCEDITOR_TAB_CHAR then
      begin
        LTextLine := LTextLine + ReplaceStr(StringOfChar(BCEDITOR_SPACE_CHAR, FTabWidth - LColumn mod FTabWidth), BCEDITOR_SPACE_CHAR, '&nbsp;');
        Inc(LColumn, FTabWidth - LColumn mod FTabWidth);
      end
      else
        Inc(LColumn, FHighlighter.GetTokenLength());
      if LToken = BCEDITOR_TAB_CHAR then
        // Added before
      else
      if LToken = BCEDITOR_SPACE_CHAR then
        LTextLine := LTextLine + '&nbsp;'
      else
      if LToken = '&' then
        LTextLine := LTextLine + '&amp;'
      else
      if LToken = '<' then
        LTextLine := LTextLine + '&lt;'
      else
      if LToken = '>' then
        LTextLine := LTextLine + '&gt;'
      else
      if LToken = '"' then
        LTextLine := LTextLine + '&quot;'
      else
      if Assigned(LHighlighterAttribute) then
      begin
        if (LPreviousElement <> '') and (LPreviousElement <> LHighlighterAttribute.Element) then
          LTextLine := LTextLine + '</span>';
        if LPreviousElement <> LHighlighterAttribute.Element then
          LTextLine := LTextLine + '<span class="' + LHighlighterAttribute.Element + '">';
        LTextLine := LTextLine + LToken;
        LPreviousElement := LHighlighterAttribute.Element;
      end
      else
        LTextLine := LTextLine + LToken;
      FHighlighter.Next;
    end;
    FStringList.Add(LTextLine + '<br>');
  end;
  if LPreviousElement <> '' then
    FStringList.Add('</span>');
end;

procedure TBCEditorExportHTML.SaveToStream(AStream: TStream; AEncoding: System.SysUtils.TEncoding);
begin
  CreateHTMLDocument;
  if not Assigned(AEncoding) then
    AEncoding := TEncoding.UTF8;
  FStringList.SaveToStream(AStream, AEncoding);
end;

end.

