unit BCEditor.Editor.Search;

interface {********************************************************************}

uses
  Classes, Generics.Collections, RegularExpressions,
  Controls, Graphics,
  BCEditor.Types, BCEditor.Consts, BCEditor.Lines;

type
  TBCEditorSearch = class(TPersistent)
  public type
    TChangeEvent = procedure(Event: TBCEditorSearchEvent) of object;

    THighlighter = class(TPersistent)
    type

      TColors = class(TPersistent)
      strict private
        FBackground: TColor;
        FBorder: TColor;
        FForeground: TColor;
        FOnChange: TChangeEvent;
        procedure DoChange;
        procedure SetBackground(const AValue: TColor);
        procedure SetBorder(const AValue: TColor);
        procedure SetForeground(const AValue: TColor);
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property Background: TColor read FBackground write SetBackground default clSearchHighlighter;
        property Border: TColor read FBorder write SetBorder default clNone;
        property Foreground: TColor read FForeground write SetForeground default clWindowText;
        property OnChange: TChangeEvent read FOnChange write FOnChange;
      end;

    strict private
      FColors: TColors;
      FOnChange: TChangeEvent;
      procedure DoChange;
      procedure SetColors(const AValue: TColors);
      procedure SetOnChange(AValue: TChangeEvent);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
    published
      property Colors: TColors read FColors write SetColors;
      property OnChange: TChangeEvent read FOnChange write SetOnChange;
    end;

    TMap = class(TPersistent)
    type
      TColors = class(TPersistent)
      strict private
        FActiveLine: TColor;
        FBackground: TColor;
        FForeground: TColor;
        FOnChange: TChangeEvent;
        procedure SetActiveLine(AValue: TColor);
        procedure SetBackground(AValue: TColor);
        procedure SetForeground(AValue: TColor);
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property ActiveLine: TColor read FActiveLine write SetActiveLine default clSearchMapActiveLine;
        property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
        property Foreground: TColor read FForeground write SetForeground default clSearchHighlighter;
        property OnChange: TChangeEvent read FOnChange write FOnChange;
      end;

    strict private const
      DefaultOptions = [moShowActiveLine];
    strict private
      FAlign: TBCEditorSearchMapAlign;
      FColors: TColors;
      FCursor: TCursor;
      FOnChange: TChangeEvent;
      FOptions: TBCEditorSearchMapOptions;
      FVisible: Boolean;
      FWidth: Integer;
      procedure DoChange;
      procedure SetAlign(const AValue: TBCEditorSearchMapAlign);
      procedure SetColors(const AValue: TColors);
      procedure SetOnChange(AValue: TChangeEvent);
      procedure SetOptions(const AValue: TBCEditorSearchMapOptions);
      procedure SetVisible(AValue: Boolean);
      procedure SetWidth(AValue: Integer);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
      function GetWidth: Integer;
    published
      property Align: TBCEditorSearchMapAlign read FAlign write SetAlign default saRight;
      property Colors: TColors read FColors write SetColors;
      property Cursor: TCursor read FCursor write FCursor default crArrow;
      property Options: TBCEditorSearchMapOptions read FOptions write SetOptions default DefaultOptions;
      property Visible: Boolean read FVisible write SetVisible default False;
      property Width: Integer read FWidth write SetWidth default 5;
      property OnChange: TChangeEvent read FOnChange write SetOnChange;
    end;

    TInSelection = class(TPersistent)
    strict private
      FActive: Boolean;
      FBackground: TColor;
      FOnChange: TChangeEvent;
      FSelectionBeginPosition: TBCEditorTextPosition;
      FSelectionEndPosition: TBCEditorTextPosition;
      procedure SetActive(AValue: Boolean);
      procedure SetBackground(const AValue: TColor);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
      property SelectionBeginPosition: TBCEditorTextPosition read FSelectionBeginPosition write FSelectionBeginPosition;
      property SelectionEndPosition: TBCEditorTextPosition read FSelectionEndPosition write FSelectionEndPosition;
    published
      property Active: Boolean read FActive write SetActive default False;
      property Background: TColor read FBackground write SetBackground default clSearchInSelectionBackground;
      property OnChange: TChangeEvent read FOnChange write FOnChange;
    end;

  strict private const
    DefaultOptions = [soHighlightResults, soSearchOnTyping];
  strict private
    FCaseSensitive: Boolean;
    FEnabled: Boolean;
    FEngineType: TBCEditorSearchEngine;
    FHighlighter: THighlighter;
    FInSelection: TInSelection;
    FMap: TMap;
    FOnChange: TChangeEvent;
    FOptions: TBCEditorSearchOptions;
    FPattern: string;
    FVisible: Boolean;
    FWholeWordsOnly: Boolean;
    procedure SetCaseSensitive(const AValue: Boolean);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetEngineType(const AValue: TBCEditorSearchEngine);
    procedure SetHighlighter(const AValue: THighlighter);
    procedure SetInSelection(const AValue: TInSelection);
    procedure SetMap(const AValue: TMap);
    procedure SetOnChange(const AValue: TChangeEvent);
    procedure SetPattern(const AValue: string);
    procedure SetWholeWordsOnly(const AValue: Boolean);
  protected
    property OnChange: TChangeEvent read FOnChange write SetOnChange;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Assign(ASource: TPersistent); override;
    property Pattern: string read FPattern write SetPattern;
    property Visible: Boolean read FVisible write FVisible;
  published
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive default False;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Engine: TBCEditorSearchEngine read FEngineType write SetEngineType default seNormal;
    property Highlighter: THighlighter read FHighlighter write SetHighlighter;
    property InSelection: TInSelection read FInSelection write SetInSelection;
    property Map: TMap read FMap write SetMap;
    property Options: TBCEditorSearchOptions read FOptions write FOptions default DefaultOptions;
    property WholeWordsOnly: Boolean read FWholeWordsOnly write SetWholeWordsOnly default False;
  end;

implementation {***************************************************************}

uses
  Windows,
  Math, SysUtils, Character,
  BCEditor.Language;

{ TBCEditorSearch.THighlighter.TColors ****************************************}

constructor TBCEditorSearch.THighlighter.TColors.Create;
begin
  inherited;

  FBackground := clSearchHighlighter;
  FBorder := clNone;
  FForeground := clWindowText;
end;

procedure TBCEditorSearch.THighlighter.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TColors then
  with ASource as TColors do
  begin
    Self.FBackground := FBackground;
    Self.FBorder := FBorder;
    Self.FForeground := FForeground;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.THighlighter.TColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(seInvalidate);
end;

procedure TBCEditorSearch.THighlighter.TColors.SetBackground(const AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearch.THighlighter.TColors.SetBorder(const AValue: TColor);
begin
  if FBorder <> AValue then
  begin
    FBorder := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearch.THighlighter.TColors.SetForeground(const AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    DoChange;
  end;
end;

{ TBCEditorSearch.THighlighter **************************************************}

constructor TBCEditorSearch.THighlighter.Create;
begin
  inherited;

  FColors := TColors.Create;
end;

destructor TBCEditorSearch.THighlighter.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorSearch.THighlighter.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is THighlighter) then
  with ASource as THighlighter do
  begin
    Self.FColors.Assign(Colors);
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.THighlighter.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(seInvalidate);
end;

procedure TBCEditorSearch.THighlighter.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorSearch.THighlighter.SetOnChange(AValue: TChangeEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := FOnChange;
end;

{ TBCEditorSearch.TMap.TColors ************************************************}

constructor TBCEditorSearch.TMap.TColors.Create;
begin
  inherited;

  FActiveLine := clSearchMapActiveLine;
  FBackground := clLeftMarginBackground;
  FForeground := clSearchHighlighter;
end;

procedure TBCEditorSearch.TMap.TColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TColors) then
  with ASource as TColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    Self.FActiveLine := FActiveLine;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(seInvalidate);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.TMap.TColors.SetActiveLine(AValue: TColor);
begin
  if FActiveLine <> AValue then
  begin
    FActiveLine := AValue;
    if Assigned(FOnChange) then
      FOnChange(seInvalidate);
  end;
end;

procedure TBCEditorSearch.TMap.TColors.SetBackground(AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    if Assigned(FOnChange) then
      FOnChange(seInvalidate);
  end;
end;

procedure TBCEditorSearch.TMap.TColors.SetForeground(AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    if Assigned(FOnChange) then
      FOnChange(seInvalidate);
  end;
end;

{ TBCEditorSearch.TMap ********************************************************}

constructor TBCEditorSearch.TMap.Create;
begin
  inherited;

  FAlign := saRight;
  FColors := TColors.Create;
  FOptions := [moShowActiveLine];
  FVisible := False;
  FWidth := 5;
  FCursor := crArrow;
end;

destructor TBCEditorSearch.TMap.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorSearch.TMap.Assign(ASource: TPersistent);
begin
  if ASource is TMap then
  with ASource as TMap do
  begin
    Self.FAlign := FAlign;
    Self.FVisible := FVisible;
    Self.FOptions := Options;
    Self.FWidth := FWidth;
    Self.FColors.Assign(FColors);
    Self.FCursor := FCursor;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.TMap.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(seInvalidate);
end;

function TBCEditorSearch.TMap.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

procedure TBCEditorSearch.TMap.SetAlign(const AValue: TBCEditorSearchMapAlign);
begin
  if FAlign <> AValue then
  begin
    FAlign := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearch.TMap.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorSearch.TMap.SetOnChange(AValue: TChangeEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := FOnChange;
end;

procedure TBCEditorSearch.TMap.SetOptions(const AValue: TBCEditorSearchMapOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearch.TMap.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearch.TMap.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
    FWidth := AValue;
  DoChange;
end;

{ TBCEditorSearch.TInSelection ************************************************}

constructor TBCEditorSearch.TInSelection.Create;
begin
  inherited;

  FActive := False;
  FBackground := clSearchInSelectionBackground;
end;

procedure TBCEditorSearch.TInSelection.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TInSelection) then
  with ASource as TInSelection do
  begin
    Self.FActive := FActive;
    Self.FBackground := FBackground;
    Self.FSelectionBeginPosition := FSelectionBeginPosition;
    Self.FSelectionEndPosition := FSelectionEndPosition;
    if (Assigned(FOnChange)) then
      FOnChange(seChange);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.TInSelection.SetActive(AValue: Boolean);
begin
  if FActive <> AValue then
  begin
    FActive := AValue;
    if Assigned(FOnChange) then
      FOnChange(seChange);
  end;
end;

procedure TBCEditorSearch.TInSelection.SetBackground(const AValue: TColor);
begin
  if (AValue <> FBackground) then
  begin
    FBackground := AValue;
    if Assigned(FOnChange) then
      FOnChange(seInvalidate);
  end;
end;
{ TBCEditorSearch *************************************************************}

constructor TBCEditorSearch.Create();
begin
  inherited;

  FCaseSensitive := False;
  FEnabled := True;
  FEngineType := seNormal;
  FHighlighter := THighlighter.Create;
  FInSelection := TInSelection.Create;
  FMap := TBCEditorSearch.TMap.Create;
  FOptions := DefaultOptions;
  FWholeWordsOnly := False;
end;

destructor TBCEditorSearch.Destroy();
begin
  FHighlighter.Free();
  FInSelection.Free();
  FMap.Free();

  inherited;
end;

procedure TBCEditorSearch.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSearch) then
  with ASource as TBCEditorSearch do
  begin
    Self.FCaseSensitive := FCaseSensitive;
    Self.FEnabled := FEnabled;
    Self.FEngineType := FEngineType;
    Self.FHighlighter.Assign(FHighlighter);
    Self.FInSelection.Assign(FInSelection);
    Self.FMap.Assign(FMap);
    Self.FOptions := FOptions;
    Self.FWholeWordsOnly := FWholeWordsOnly;
    if (Assigned(FOnChange)) then
      FOnChange(seChange);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.SetCaseSensitive(const AValue: Boolean);
begin
  if (AValue <> FCaseSensitive) then
  begin
    FCaseSensitive := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(seChange);
  end;
end;

procedure TBCEditorSearch.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(seChange);
  end;
end;

procedure TBCEditorSearch.SetEngineType(const AValue: TBCEditorSearchEngine);
begin
  if (FEngineType <> AValue) then
  begin
    FEngineType := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(seChange);
  end;
end;

procedure TBCEditorSearch.SetHighlighter(const AValue: THighlighter);
begin
  FHighlighter.Assign(AValue);
end;

procedure TBCEditorSearch.SetInSelection(const AValue: TInSelection);
begin
  FInSelection.Assign(AValue);
end;

procedure TBCEditorSearch.SetMap(const AValue: TMap);
begin
  FMap.Assign(AValue);
end;

procedure TBCEditorSearch.SetOnChange(const AValue: TChangeEvent);
begin
  FOnChange := AValue;
  FMap.OnChange := FOnChange;
  FHighlighter.OnChange := FOnChange;
  FInSelection.OnChange := FOnChange;
end;

procedure TBCEditorSearch.SetPattern(const AValue: string);
begin
  if (AValue <> FPattern) then
  begin
    FPattern := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(seChange);
  end;
end;

procedure TBCEditorSearch.SetWholeWordsOnly(const AValue: Boolean);
begin
  if (AValue <> FWholeWordsOnly) then
  begin
    FWholeWordsOnly := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(seChange);
  end;
end;

end.
