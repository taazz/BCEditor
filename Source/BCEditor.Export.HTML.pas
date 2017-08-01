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
  LLine: Integer;
  LPreviousElement: string;
  LTextLine: string;
  LTokenText: string;
  LToken: TBCEditorHighlighter.TTokenFind;
begin
  LPreviousElement := '';
  for LLine := 0 to FLines.Count - 1 do
  begin
    LTextLine := '';
    LColumn := 0;
    if (FHighlighter.FindFirstToken(FLines.Items[LLine].BeginRange,
      PChar(FLines.Items[LLine].Text), Length(FLines.Items[LLine].Text), 0,
      LToken)) then
      repeat
        SetString(LTokenText, LToken.Text, LToken.Length);
        if LTokenText = BCEDITOR_TAB_CHAR then
        begin
          LTextLine := LTextLine + ReplaceStr(StringOfChar(BCEDITOR_SPACE_CHAR, FTabWidth - LColumn mod FTabWidth), BCEDITOR_SPACE_CHAR, '&nbsp;');
          Inc(LColumn, FTabWidth - LColumn mod FTabWidth);
        end
        else
          Inc(LColumn, LToken.Length);
        if LTokenText = BCEDITOR_TAB_CHAR then
          // Added before
        else
        if LTokenText = BCEDITOR_SPACE_CHAR then
          LTextLine := LTextLine + '&nbsp;'
        else
        if LTokenText = '&' then
          LTextLine := LTextLine + '&amp;'
        else
        if LTokenText = '<' then
          LTextLine := LTextLine + '&lt;'
        else
        if LTokenText = '>' then
          LTextLine := LTextLine + '&gt;'
        else
        if LTokenText = '"' then
          LTextLine := LTextLine + '&quot;'
        else
        if Assigned(LToken.Attribute) then
        begin
          if (LPreviousElement <> '') and (LPreviousElement <> LToken.Attribute.Element) then
            LTextLine := LTextLine + '</span>';
          if LPreviousElement <> LToken.Attribute.Element then
            LTextLine := LTextLine + '<span class="' + LToken.Attribute.Element + '">';
          LTextLine := LTextLine + LTokenText;
          LPreviousElement := LToken.Attribute.Element;
        end
        else
          LTextLine := LTextLine + LTokenText;
      until (not FHighlighter.FindNextToken(LToken));
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

