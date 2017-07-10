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
    FCanvas: TCanvas;
    FDrawingCount: Integer;
    FForegroundColor: TColor;
    FObjectFonts: TObjectFonts;
    FSavedHandles: TStack<Integer>;
    FStyle: TFontStyles;
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetFont(const AValue: TFont);
    procedure SetForegroundColor(const AValue: TColor);
    procedure SetStyle(const AValue: TFontStyles);
  public
    procedure BeginDrawing(const ACanvas: TCanvas);
    constructor Create(const AFont: TFont);
    destructor Destroy(); override;
    procedure EndDrawing();
    function ExtTextOut(X, Y: Integer; Options: Longint;
      Rect: TRect; Str: LPCWSTR; Count: Longint; Dx: PInteger): BOOL; {$IFNDEF Debug} inline; {$ENDIF}
    function FillRect(const ARect: TRect): BOOL; {$IFNDEF Debug} inline; {$ENDIF}
    function FrameRect(const ARect: TRect; AColor: TColor): Integer; {$IFNDEF Debug} inline; {$ENDIF}
    function TextHeight(const AText: PChar; const ALength: Integer): Integer;
    function TextWidth(const AText: PChar; const ALength: Integer): Integer;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property Canvas: TCanvas read FCanvas;
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

procedure TBCEditorPaintHelper.BeginDrawing(const ACanvas: TCanvas);
begin
  Assert((FDrawingCount = 0) or (ACanvas = FCanvas));

  if (FDrawingCount = 0) then
    FCanvas := ACanvas;
  Inc(FDrawingCount);

  FSavedHandles.Push(SaveDC(FCanvas.Handle));

  SelectObject(FCanvas.Handle, FObjectFonts.Items[FObjectFonts.Add(FStyle)].Handle);
  SetTextColor(FCanvas.Handle, ColorToRGB(FForegroundColor));
  SetBkColor(FCanvas.Handle, ColorToRGB(FBackgroundColor));
  SetBkMode(FCanvas.Handle, TRANSPARENT);
end;

constructor TBCEditorPaintHelper.Create(const AFont: TFont);
begin
  inherited Create();

  FBrush := TBrush.Create();
  FObjectFonts := TObjectFonts.Create();
  FSavedHandles := TStack<Integer>.Create();
  FForegroundColor := clWindowText;
  FBackgroundColor := clWindow;
  FStyle := [];
  Font := AFont;
end;

destructor TBCEditorPaintHelper.Destroy();
begin
  FBrush.Free();
  FObjectFonts.Free();
  FSavedHandles.Free();

  inherited;
end;

procedure TBCEditorPaintHelper.EndDrawing();
begin
  if (FDrawingCount > 0) then
  begin
    Dec(FDrawingCount);
    RestoreDC(FCanvas.Handle, FSavedHandles.Pop());
    if (FDrawingCount = 0) then
      FCanvas := nil;
  end;
end;

function TBCEditorPaintHelper.ExtTextOut(X, Y: Integer; Options: Longint;
  Rect: TRect; Str: LPCWSTR; Count: Longint; Dx: PInteger): BOOL;
begin
  Result := Windows.ExtTextOut(Canvas.Handle, X, Y, Options, @Rect, Str, Count, Dx);
end;

function TBCEditorPaintHelper.FillRect(const ARect: TRect): BOOL;
begin
  Result := Windows.ExtTextOut(Canvas.Handle, 0, 0, ETO_OPAQUE, ARect, '', 0, nil);
end;

function TBCEditorPaintHelper.FrameRect(const ARect: TRect; AColor: TColor): Integer;
begin
  FBrush.Color := AColor;
  Result := Windows.FrameRect(Canvas.Handle, ARect, FBrush.Handle);
end;

procedure TBCEditorPaintHelper.SetBackgroundColor(const AValue: TColor);
begin
  if (AValue <> FBackgroundColor) then
  begin
    FBackgroundColor := AValue;
    if (Assigned(FCanvas)) then
      SetBkColor(FCanvas.Handle, ColorToRGB(FBackgroundColor));
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
    if (Assigned(FCanvas)) then
      SetTextColor(FCanvas.Handle, ColorToRGB(FForegroundColor));
  end;
end;

procedure TBCEditorPaintHelper.SetStyle(const AValue: TFontStyles);
begin
  if (AValue <> FStyle) then
  begin
    FStyle := AValue;
    if (Assigned(FCanvas)) then
      SelectObject(FCanvas.Handle, FObjectFonts.Items[FObjectFonts.Add(FStyle)].Handle);
  end;
end;

function TBCEditorPaintHelper.TextHeight(const AText: PChar; const ALength: Integer): Integer;
var
  LSize: TSize;
begin
  Assert(Assigned(FCanvas));

  if (not GetTextExtentPoint32(FCanvas.Handle, AText, ALength, LSize)) then
    RaiseLastOSError();
  Result := LSize.cy;
end;

function TBCEditorPaintHelper.TextWidth(const AText: PChar; const ALength: Integer): Integer;
var
  LSize: TSize;
begin
  Assert(Assigned(FCanvas));

  if (not GetTextExtentPoint32(FCanvas.Handle, AText, ALength, LSize)) then
    RaiseLastOSError();
  Result := LSize.cx;
end;

end.
