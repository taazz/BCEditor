unit BCEditor.Editor.Search;

interface {********************************************************************}

uses
  Classes, Generics.Collections, RegularExpressions,
  Controls, Graphics,
  BCEditor.Types, BCEditor.Consts, BCEditor.Lines;

type
  TBCEditorSearch = class(TPersistent)
  public type
    TExecutedEvent = procedure(ASender: TObject; const AErrorMessage: string) of object;
    TWrapAroundEvent = function(ASender: TObject; const APattern: string; const ABackwards: Boolean): Boolean of object;

    THighlighter = class(TPersistent)
    type

      TColors = class(TPersistent)
      strict private
        FBackground: TColor;
        FBorder: TColor;
        FForeground: TColor;
        FOnChange: TNotifyEvent;
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
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
      end;

    strict private
      FColors: TColors;
      FOnChange: TNotifyEvent;
      procedure DoChange;
      procedure SetColors(const AValue: TColors);
      procedure SetOnChange(AValue: TNotifyEvent);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
    published
      property Colors: TColors read FColors write SetColors;
      property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    end;

  strict private const
    DefaultOptions = [];
  strict private
    FCaseSensitive: Boolean;
    FEnabled: Boolean;
    FEngineType: TBCEditorSearchEngine;
    FHighlighter: THighlighter;
    FOnChange: TNotifyEvent;
    FOnFind: TNotifyEvent;
    FOnExecuted: TExecutedEvent;
    FOnWrapAround: TWrapAroundEvent;
    FOptions: TBCEditorSearchOptions;
    FPattern: string;
    FVisible: Boolean;
    FWholeWordsOnly: Boolean;
    procedure SetCaseSensitive(const AValue: Boolean);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetEngineType(const AValue: TBCEditorSearchEngine);
    procedure SetHighlighter(const AValue: THighlighter);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetPattern(const AValue: string);
    procedure SetWholeWordsOnly(const AValue: Boolean);
  protected
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create();
    destructor Destroy(); override;
    property Pattern: string read FPattern write SetPattern;
    property Visible: Boolean read FVisible write FVisible;
  published
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive default False;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Engine: TBCEditorSearchEngine read FEngineType write SetEngineType default seNormal;
    property Highlighter: THighlighter read FHighlighter write SetHighlighter;
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
    property OnExecuted: TExecutedEvent read FOnExecuted write FOnExecuted;
    property OnWrapAround: TWrapAroundEvent read FOnWrapAround write FOnWrapAround;
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
    FOnChange(Self);
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
    FOnChange(Self);
end;

procedure TBCEditorSearch.THighlighter.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorSearch.THighlighter.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := FOnChange;
end;

{ TBCEditorSearch *************************************************************}

procedure TBCEditorSearch.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSearch) then
  with ASource as TBCEditorSearch do
  begin
    Self.FCaseSensitive := FCaseSensitive;
    Self.FEnabled := FEnabled;
    Self.FEngineType := FEngineType;
    Self.FHighlighter.Assign(FHighlighter);
    Self.FOptions := FOptions;
    Self.FWholeWordsOnly := FWholeWordsOnly;
    if (Assigned(FOnChange)) then
      FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorSearch.Create();
begin
  inherited;

  FCaseSensitive := False;
  FEnabled := True;
  FEngineType := seNormal;
  FHighlighter := THighlighter.Create;
  FOnExecuted := nil;
  FOptions := DefaultOptions;
  FWholeWordsOnly := False;
end;

destructor TBCEditorSearch.Destroy();
begin
  FHighlighter.Free();

  inherited;
end;

procedure TBCEditorSearch.SetCaseSensitive(const AValue: Boolean);
begin
  if (AValue <> FCaseSensitive) then
  begin
    FCaseSensitive := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorSearch.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorSearch.SetEngineType(const AValue: TBCEditorSearchEngine);
begin
  if (FEngineType <> AValue) then
  begin
    FEngineType := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorSearch.SetHighlighter(const AValue: THighlighter);
begin
  FHighlighter.Assign(AValue);
end;

procedure TBCEditorSearch.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FHighlighter.OnChange := FOnChange;
end;

procedure TBCEditorSearch.SetPattern(const AValue: string);
begin
  if (AValue <> FPattern) then
  begin
    FPattern := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorSearch.SetWholeWordsOnly(const AValue: Boolean);
begin
  if (AValue <> FWholeWordsOnly) then
  begin
    FWholeWordsOnly := AValue;
    if (Assigned(FOnChange)) then
      FOnChange(Self);
  end;
end;

end.
