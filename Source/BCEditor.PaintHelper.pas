unit BCEditor.PaintHelper;

interface {********************************************************************}

uses
  Windows,
  SysUtils, UITypes, Generics.Collections,
  Graphics,
  BCEditor.Types;

type
  TBCEditorPaintHelper = class(TObject)
  type

    TObjectFont = packed record
      Handle: HFont;
      Style: TFontStyles;
    end;

    TObjectFonts = class(TList<TObjectFont>)
    strict private
      FFont: TFont;
      procedure SetFont(const AValue: TFont);
    public
      function Add(const AStyle: TFontStyles): Integer;
      procedure Clear();
      constructor Create();
      destructor Destroy(); override;
      property Font: TFont read FFont write SetFont;
    end;

  strict private
    FBackgroundColor: TColor;
    FBrush: TBrush;
    FForegroundColor: TColor;
    FHandle: HDC;
    FHandles: TStack<HDC>;
    FObjectFonts: TObjectFonts;
    FSavedDCs: TStack<Integer>;
    FStyle: TFontStyles;
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetFont(const AValue: TFont);
    procedure SetForegroundColor(const AValue: TColor);
    procedure SetStyle(const AValue: TFontStyles);
  public
    procedure BeginDraw(const AHandle: HDC);
    constructor Create(const AFont: TFont);
    destructor Destroy(); override;
    procedure EndDraw();
    function ExtTextOut(X, Y: Integer; Options: Longint;
      Rect: TRect; Str: LPCWSTR; Count: Longint; Dx: PInteger): BOOL; {$IFNDEF Debug} inline; {$ENDIF}
    function FillRect(const ARect: TRect): BOOL; {$IFNDEF Debug} inline; {$ENDIF}
    function FrameRect(const ARect: TRect; AColor: TColor): Integer; {$IFNDEF Debug} inline; {$ENDIF}
    function TextHeight(const AText: PChar; const ALength: Integer): Integer;
    function TextWidth(const AText: PChar; const ALength: Integer): Integer;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property ForegroundColor: TColor read FForegroundColor write SetForegroundColor;
    property Font: TFont write SetFont;
    property Style: TFontStyles read FStyle write SetStyle;
  end;

implementation {***************************************************************}

uses
  Types;

{ TBCEditorPaintHelper.TObjectFonts *******************************************}

function TBCEditorPaintHelper.TObjectFonts.Add(const AStyle: TFontStyles): Integer;
const
  CBolds: array [Boolean] of Integer = (400, 700);
var
  LFont: TObjectFont;
  LIndex: Integer;
  LLogFont: TLogFont;
begin
  Result := -1;

  for LIndex := 0 to Count - 1 do
    if (Items[LIndex].Style = AStyle) then
      Result := LIndex;

  if (Result < 0) then
  begin
    GetObject(FFont.Handle, SizeOf(LLogFont), @LLogFont);
    LLogFont.lfWeight := CBolds[fsBold in AStyle];
    LLogFont.lfItalic := Ord(BOOL(fsItalic in AStyle));
    LLogFont.lfUnderline := Ord(BOOL(fsUnderline in AStyle));
    LLogFont.lfStrikeOut := Ord(BOOL(fsStrikeOut in AStyle));
    StrCopy(@LLogFont.lfFaceName[0], PChar(FFont.Name));
    LFont.Handle := CreateFontIndirect(LLogFont);
    LFont.Style := AStyle;
    Result := inherited Add(LFont);
  end;
end;

procedure TBCEditorPaintHelper.TObjectFonts.Clear();
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
    DeleteObject(Items[LIndex].Handle);

  inherited;
end;

constructor TBCEditorPaintHelper.TObjectFonts.Create();
begin
  inherited;

  FFont := nil;
end;

destructor TBCEditorPaintHelper.TObjectFonts.Destroy();
begin
  Clear();

  inherited;
end;

procedure TBCEditorPaintHelper.TObjectFonts.SetFont(const AValue: TFont);
begin
  if (not Assigned(FFont)
    or (AValue.Name <> FFont.Name)
    or (AValue.Size <> FFont.Size)) then
  begin
    FFont := AValue;
    Clear();
  end;
end;

{ TBCEditorPaintHelper ********************************************************}

procedure TBCEditorPaintHelper.BeginDraw(const AHandle: HDC);
begin
  Assert(AHandle <> 0);

  FHandles.Push(FHandle);
  FSavedDCs.Push(SaveDC(FHandle));

  FHandle := AHandle;

  SelectObject(FHandle, FObjectFonts.Items[FObjectFonts.Add(FStyle)].Handle);
  SetTextColor(FHandle, ColorToRGB(FForegroundColor));
  SetBkColor(FHandle, ColorToRGB(FBackgroundColor));
  SetBkMode(FHandle, TRANSPARENT);
end;

constructor TBCEditorPaintHelper.Create(const AFont: TFont);
begin
  inherited Create();

  FBackgroundColor := clWindow;
  FBrush := TBrush.Create();
  FForegroundColor := clWindowText;
  FHandle := 0;
  FHandles := TStack<HDC>.Create();
  FObjectFonts := TObjectFonts.Create();
  FSavedDCs := TStack<Integer>.Create();
  FStyle := [];
  SetFont(AFont);
end;

destructor TBCEditorPaintHelper.Destroy();
begin
  FBrush.Free();
  FHandles.Free();
  FObjectFonts.Free();
  FSavedDCs.Free();

  inherited;
end;

procedure TBCEditorPaintHelper.EndDraw();
begin
  Assert(FHandles.Count > 0);

  FHandle := FHandles.Pop();
  RestoreDC(FHandle, FSavedDCs.Pop());
end;

function TBCEditorPaintHelper.ExtTextOut(X, Y: Integer; Options: Longint;
  Rect: TRect; Str: LPCWSTR; Count: Longint; Dx: PInteger): BOOL;
begin
  Assert(FHandle <> 0);

  Result := Windows.ExtTextOut(FHandle, X, Y, Options, @Rect, Str, Count, Dx);
end;

function TBCEditorPaintHelper.FillRect(const ARect: TRect): BOOL;
begin
  Assert(FHandle <> 0);

  Result := Windows.ExtTextOut(FHandle, 0, 0, ETO_OPAQUE, ARect, '', 0, nil);
end;

function TBCEditorPaintHelper.FrameRect(const ARect: TRect; AColor: TColor): Integer;
begin
  FBrush.Color := AColor;
  Result := Windows.FrameRect(FHandle, ARect, FBrush.Handle);
end;

procedure TBCEditorPaintHelper.SetBackgroundColor(const AValue: TColor);
begin
  if (AValue <> FBackgroundColor) then
  begin
    FBackgroundColor := AValue;
    if (FHandle <> 0) then
      SetBkColor(FHandle, ColorToRGB(FBackgroundColor));
  end;
end;

procedure TBCEditorPaintHelper.SetFont(const AValue: TFont);
begin
  Assert(Assigned(AValue));

  FObjectFonts.Font := AValue;
  ForegroundColor := AValue.Color;
  Style := AValue.Style;
end;

procedure TBCEditorPaintHelper.SetForegroundColor(const AValue: TColor);
begin
  if (AValue <> FForegroundColor) then
  begin
    FForegroundColor := AValue;
    if (FHandle <> 0) then
      SetTextColor(FHandle, ColorToRGB(FForegroundColor));
  end;
end;

procedure TBCEditorPaintHelper.SetStyle(const AValue: TFontStyles);
begin
  if (AValue <> FStyle) then
  begin
    FStyle := AValue;
    if (FHandle <> 0) then
      SelectObject(FHandle, FObjectFonts.Items[FObjectFonts.Add(FStyle)].Handle);
  end;
end;

function TBCEditorPaintHelper.TextHeight(const AText: PChar; const ALength: Integer): Integer;
var
  LSize: TSize;
begin
  Assert(FHandle <> 0);

  if (not GetTextExtentPoint32(FHandle, AText, ALength, LSize)) then
    RaiseLastOSError();
  Result := LSize.cy;
end;

function TBCEditorPaintHelper.TextWidth(const AText: PChar; const ALength: Integer): Integer;
var
  LSize: TSize;
begin
  Assert(FHandle <> 0);

  if (not GetTextExtentPoint32(FHandle, AText, ALength, LSize)) then
    RaiseLastOSError();
  Result := LSize.cx;
end;

end.
