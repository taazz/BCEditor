unit BCEditor.Editor.CodeFolding;

interface {********************************************************************}

uses
  Classes, SysUtils, Types, Generics.Collections,
  Graphics, Controls,
  BCEditor.Types, BCEditor.Consts;

type
  TBCEditorCodeFoldingChangeEvent = procedure(Event: TBCEditorCodeFoldingChanges) of object;

  TBCEditorCodeFoldingRegionItem = class(TCollectionItem)
    strict private
      FBeginWithBreakChar: Boolean;
      FBreakCharFollows: Boolean;
      FBreakIfNotFoundBeforeNextRegion: string;
      FCloseAtNextToken: Boolean;
      FCloseToken: string;
      FCloseTokenBeginningOfLine: Boolean;
      FCloseTokenLength: Integer;
      FNoSubs: Boolean;
      FOpenIsClose: Boolean;
      FOpenToken: string;
      FOpenTokenBeginningOfLine: Boolean;
      FOpenTokenBreaksLine: Boolean;
      FOpenTokenCanBeFollowedBy: string;
      FOpenTokenEnd: string;
      FOpenTokenLength: Integer;
      FParentRegionItem: TBCEditorCodeFoldingRegionItem;
      FSharedClose: Boolean;
      FShowGuideLine: Boolean;
      FSkipIfFoundAfterOpenTokenArray: TBCEditorArrayOfString;
      FSkipIfFoundAfterOpenTokenArrayCount: Integer;
      FTokenEndIsPreviousLine: Boolean;
      procedure SetSkipIfFoundAfterOpenTokenArrayCount(const AValue: Integer);
    public
      constructor Create(ACollection: TCollection); override;
      property BeginWithBreakChar: Boolean read FBeginWithBreakChar write FBeginWithBreakChar;
      property BreakCharFollows: Boolean read FBreakCharFollows write FBreakCharFollows default True;
      property BreakIfNotFoundBeforeNextRegion: string read FBreakIfNotFoundBeforeNextRegion write FBreakIfNotFoundBeforeNextRegion;
      property CloseAtNextToken: Boolean read FCloseAtNextToken write FCloseAtNextToken;
      property CloseToken: string read FCloseToken write FCloseToken;
      property CloseTokenBeginningOfLine: Boolean read FCloseTokenBeginningOfLine write FCloseTokenBeginningOfLine default False;
      property CloseTokenLength: Integer read FCloseTokenLength write FCloseTokenLength;
      property NoSubs: Boolean read FNoSubs write FNoSubs default False;
      property OpenIsClose: Boolean read FOpenIsClose write FOpenIsClose default False;
      property OpenToken: string read FOpenToken write FOpenToken;
      property OpenTokenBeginningOfLine: Boolean read FOpenTokenBeginningOfLine write FOpenTokenBeginningOfLine default False;
      property OpenTokenBreaksLine: Boolean read FOpenTokenBreaksLine write FOpenTokenBreaksLine default False;
      property OpenTokenCanBeFollowedBy: string read FOpenTokenCanBeFollowedBy write FOpenTokenCanBeFollowedBy;
      property OpenTokenEnd: string read FOpenTokenEnd write FOpenTokenEnd;
      property OpenTokenLength: Integer read FOpenTokenLength write FOpenTokenLength;
      property ParentRegionItem: TBCEditorCodeFoldingRegionItem read FParentRegionItem write FParentRegionItem;
      property SharedClose: Boolean read FSharedClose write FSharedClose default False;
      property ShowGuideLine: Boolean read FShowGuideLine write FShowGuideLine default True;
      property SkipIfFoundAfterOpenTokenArray: TBCEditorArrayOfString read FSkipIfFoundAfterOpenTokenArray write FSkipIfFoundAfterOpenTokenArray;
      property SkipIfFoundAfterOpenTokenArrayCount: Integer read FSkipIfFoundAfterOpenTokenArrayCount write SetSkipIfFoundAfterOpenTokenArrayCount;
      property TokenEndIsPreviousLine: Boolean read FTokenEndIsPreviousLine write FTokenEndIsPreviousLine default False;
    end;

  TBCEditorCodeFoldingRegions = array of Pointer;

  TBCEditorCodeFolding = class(TPersistent)
  type
    TAllRanges = class;
    TSkipRegions = class;

    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FForeground: TColor;
      FIndent: TColor;
      FIndentHighlight: TColor;
      FOnChange: TBCEditorCodeFoldingChangeEvent;
      procedure DoChange;
      procedure SetBackground(const AValue: TColor);
      procedure SetForeground(const AValue: TColor);
      procedure SetIndent(const AValue: TColor);
      procedure SetIndentHighlight(const AValue: TColor);
    protected
      property OnChange: TBCEditorCodeFoldingChangeEvent read FOnChange write FOnChange;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
      property Foreground: TColor read FForeground write SetForeground default clLeftMarginForeground;
      property Indent: TColor read FIndent write SetIndent default clIndent;
      property IndentHighlight: TColor read FIndentHighlight write SetIndentHighlight default clIndentHighlight;
    end;

    TRegion = class(TCollection)
    strict private
      FCloseToken: string;
      FEscapeChar: Char;
      FFoldTags: Boolean;
      FOpenToken: string;
      FSkipRegions: TSkipRegions;
      FStringEscapeChar: Char;
      function GetItem(AIndex: Integer): TBCEditorCodeFoldingRegionItem;
    public
      constructor Create(AItemClass: TCollectionItemClass);
      destructor Destroy; override;
      function Add(const AOpenToken: string; const ACloseToken: string): TBCEditorCodeFoldingRegionItem;
      function Contains(const AOpenToken: string; const ACloseToken: string): Boolean;
      property CloseToken: string read FCloseToken write FCloseToken;
      property EscapeChar: Char read FEscapeChar write FEscapeChar default BCEDITOR_NONE_CHAR;
      property FoldTags: Boolean read FFoldTags write FFoldTags default False;
      property Items[AIndex: Integer]: TBCEditorCodeFoldingRegionItem read GetItem; default;
      property OpenToken: string read FOpenToken write FOpenToken;
      property SkipRegions: TBCEditorCodeFolding.TSkipRegions read FSkipRegions;
      property StringEscapeChar: Char read FStringEscapeChar write FStringEscapeChar default BCEDITOR_NONE_CHAR;
    end;

    TSkipRegions = class(TCollection)
    type

      TItem = class(TCollectionItem)
      strict private
        FCloseToken: string;
        FOpenToken: string;
        FRegionType: TBCEditorRangeItemType;
        FSkipEmptyChars: Boolean;
        FSkipIfNextCharIsNot: Char;
      public
        property OpenToken: string read FOpenToken write FOpenToken;
        property CloseToken: string read FCloseToken write FCloseToken;
        property RegionType: TBCEditorRangeItemType read FRegionType write FRegionType;
        property SkipEmptyChars: Boolean read FSkipEmptyChars write FSkipEmptyChars;
        property SkipIfNextCharIsNot: Char read FSkipIfNextCharIsNot write FSkipIfNextCharIsNot default BCEDITOR_NONE_CHAR;
      end;

    strict private
      function GetSkipRegionItem(AIndex: Integer): TItem;
    public
      function Add(const AOpenToken, ACloseToken: string): TItem;
      function Contains(const AOpenToken, ACloseToken: string): Boolean;
      property SkipRegionItems[AIndex: Integer]: TItem read GetSkipRegionItem; default;
    end;

    TRanges = class(TPersistent)
    type

      TRange = class
      strict private
        FAllCodeFoldingRanges: TAllRanges;
        FBeginLine: Integer;
        FCollapsed: Boolean;
        FCollapsedBy: Integer;
        FEndLine: Integer;
        FFoldRangeLevel: Integer;
        FIndentLevel: Integer;
        FIsExtraTokenFound: Boolean;
        FParentCollapsed: Boolean;
        FRegionItem: TBCEditorCodeFoldingRegionItem;
        FSubCodeFoldingRanges: TRanges;
        FUndoListed: Boolean;
      public
        constructor Create;
        destructor Destroy; override;
        function Collapsable: Boolean;
        procedure MoveBy(LineCount: Integer);
        procedure MoveChildren(By: Integer);
        procedure SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
        procedure Widen(LineCount: Integer);
        property AllCodeFoldingRanges: TAllRanges read FAllCodeFoldingRanges write FAllCodeFoldingRanges;
        property BeginLine: Integer read FBeginLine write FBeginLine;
        property Collapsed: Boolean read FCollapsed write FCollapsed default False;
        property CollapsedBy: Integer read FCollapsedBy write FCollapsedBy;
        property EndLine: Integer read FEndLine write FEndLine;
        property FoldRangeLevel: Integer read FFoldRangeLevel write FFoldRangeLevel;
        property IndentLevel: Integer read FIndentLevel write FIndentLevel;
        property IsExtraTokenFound: Boolean read FIsExtraTokenFound write FIsExtraTokenFound default False;
        property ParentCollapsed: Boolean read FParentCollapsed write FParentCollapsed;
        property RegionItem: TBCEditorCodeFoldingRegionItem read FRegionItem write FRegionItem;
        property SubCodeFoldingRanges: TRanges read FSubCodeFoldingRanges;
        property UndoListed: Boolean read FUndoListed write FUndoListed default False;
      end;

    strict private
      FList: TList;
      function GetCount: Integer;
      function GetItem(AIndex: Integer): TRange;
    public
      constructor Create;
      destructor Destroy; override;
      function Add(AAllCodeFoldingRanges: TAllRanges; ABeginLine, AIndentLevel, AFoldRangeLevel: Integer;
        ARegionItem: TBCEditorCodeFoldingRegionItem; AEndLine: Integer = 0): TRange;
      procedure Clear;
      property Count: Integer read GetCount;
      property Items[AIndex: Integer]: TRange read GetItem; default;
    end;

    TAllRanges = class(TRanges)
    strict private
      FList: TObjectList<TRanges.TRange>;
      function GetAllCount: Integer;
      function GetItem(AIndex: Integer): TRanges.TRange;
      procedure SetItem(AIndex: Integer; Value: TRanges.TRange);
    public
      constructor Create;
      destructor Destroy; override;
      procedure ClearAll;
      procedure Delete(AIndex: Integer); overload;
      procedure Delete(FoldRange: TRanges.TRange); overload;
      procedure SetParentCollapsedOfSubCodeFoldingRanges(AFoldRange: TRanges.TRange);
      procedure UpdateFoldRanges;
      property AllCount: Integer read GetAllCount;
      property Items[AIndex: Integer]: TRanges.TRange read GetItem write SetItem; default;
      property List: TObjectList<TRanges.TRange> read FList;
    end;

  strict private const
    DefaultOptions = [cfoHighlightIndentGuides, cfoShowTreeLine];
  strict private
    FColors: TColors;
    FDelayInterval: Cardinal;
    FMouseOverHint: Boolean;
    FOnChange: TBCEditorCodeFoldingChangeEvent;
    FOptions: TBCEditorCodeFoldingOptions;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColors(const AValue: TColors);
    procedure SetOnChange(AValue: TBCEditorCodeFoldingChangeEvent);
    procedure SetOptions(AValue: TBCEditorCodeFoldingOptions);
    procedure SetVisible(const AValue: Boolean);
  protected
    property OnChange: TBCEditorCodeFoldingChangeEvent read FOnChange write SetOnChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorCodeFoldingOption; const AEnabled: Boolean);
  published
    property Colors: TColors read FColors write SetColors;
    property DelayInterval: Cardinal read FDelayInterval write FDelayInterval default 300;
    property Options: TBCEditorCodeFoldingOptions read FOptions write SetOptions default DefaultOptions;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation {***************************************************************}

uses
  Math;

{ TBCEditorCodeFolding.TColors *************************************************}

constructor TBCEditorCodeFolding.TColors.Create;
begin
  inherited;

  FBackground := clLeftMarginBackground;
  FForeground := clLeftMarginForeground;
  FIndent := clIndent;
  FIndentHighlight := clIndentHighlight;
end;

procedure TBCEditorCodeFolding.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TColors then
  with ASource as TColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    Self.FIndent := FIndent;
    Self.FIndentHighlight := FIndentHighlight;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCodeFolding.TColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(fcRefresh);
end;

procedure TBCEditorCodeFolding.TColors.SetBackground(const AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.TColors.SetForeground(const AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.TColors.SetIndent(const AValue: TColor);
begin
  if FIndent <> AValue then
  begin
    FIndent := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.TColors.SetIndentHighlight(const AValue: TColor);
begin
  if FIndentHighlight <> AValue then
  begin
    FIndentHighlight := AValue;
    DoChange;
  end;
end;

{ TBCEditorCodeFolding.TRegion.TItem ******************************************}

constructor TBCEditorCodeFoldingRegionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FSkipIfFoundAfterOpenTokenArrayCount := 0;
  FBreakIfNotFoundBeforeNextRegion := '';
  FCloseTokenBeginningOfLine := False;
  FNoSubs := False;
  FOpenIsClose := False;
  FOpenTokenBeginningOfLine := False;
  FOpenTokenBreaksLine := False;
  FSharedClose := False;
  FBreakCharFollows := True;
end;

procedure TBCEditorCodeFoldingRegionItem.SetSkipIfFoundAfterOpenTokenArrayCount(const AValue: Integer);
begin
  FSkipIfFoundAfterOpenTokenArrayCount := AValue;
  SetLength(FSkipIfFoundAfterOpenTokenArray, AValue);
end;

{ TBCEditorCodeFolding.TRegion ************************************************}

constructor TBCEditorCodeFolding.TRegion.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);

  FSkipRegions := TBCEditorCodeFolding.TSkipRegions.Create(TSkipRegions.TItem);
  FEscapeChar := BCEDITOR_NONE_CHAR;
  FStringEscapeChar := BCEDITOR_NONE_CHAR;
  FFoldTags := False;
end;

destructor TBCEditorCodeFolding.TRegion.Destroy;
begin
  FSkipRegions.Free;

  inherited;
end;

function TBCEditorCodeFolding.TRegion.Add(const AOpenToken: string; const ACloseToken: string): TBCEditorCodeFoldingRegionItem;
begin
  Result := TBCEditorCodeFoldingRegionItem(inherited Add);
  with Result do
  begin
    OpenToken := AOpenToken;
    OpenTokenLength := Length(AOpenToken);
    CloseToken := ACloseToken;
    CloseTokenLength := Length(ACloseToken);
  end;
end;

function TBCEditorCodeFolding.TRegion.Contains(const AOpenToken: string; const ACloseToken: string): Boolean;
var
  LIndex: Integer;
  LItem: TBCEditorCodeFoldingRegionItem;
begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin
    LItem := Items[LIndex];
    if (LItem.OpenToken = AOpenToken) and (LItem.CloseToken = ACloseToken) then
      Exit(True);
  end;
end;

function TBCEditorCodeFolding.TRegion.GetItem(AIndex: Integer): TBCEditorCodeFoldingRegionItem;
begin
  Result := TBCEditorCodeFoldingRegionItem(inherited Items[AIndex]);
end;

{ TBCEditorCodeFolding.TSkipRegions *******************************************}

function TBCEditorCodeFolding.TSkipRegions.Add(const AOpenToken, ACloseToken: string): TItem;
begin
  Result := TItem(inherited Add);
  with Result do
  begin
    OpenToken := AOpenToken;
    CloseToken := ACloseToken;
  end;
end;

function TBCEditorCodeFolding.TSkipRegions.Contains(const AOpenToken, ACloseToken: string): Boolean;
var
  LIndex: Integer;
  LSkipRegion: TItem;
begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin
    LSkipRegion := SkipRegionItems[LIndex];
    if (LSkipRegion.OpenToken = AOpenToken) and (LSkipRegion.CloseToken = ACloseToken) then
      Exit(True);
  end;
end;

function TBCEditorCodeFolding.TSkipRegions.GetSkipRegionItem(AIndex: Integer): TItem;
begin
  Result := TItem(inherited Items[AIndex]);
end;

{ TBCEditorCodeFolding.TRanges.TRange *****************************************}

function TBCEditorCodeFolding.TRanges.TRange.Collapsable: Boolean;
begin
  Result := (FBeginLine < FEndLine) or RegionItem.TokenEndIsPreviousLine and (FBeginLine = FEndLine);
end;

constructor TBCEditorCodeFolding.TRanges.TRange.Create;
begin
  inherited;

  FSubCodeFoldingRanges := TRanges.Create;
  FCollapsed := False;
  FCollapsedBy := -1;
  FIsExtraTokenFound := False;
  FUndoListed := False;
end;

destructor TBCEditorCodeFolding.TRanges.TRange.Destroy;
begin;
  FSubCodeFoldingRanges.Clear;
  FSubCodeFoldingRanges.Free;
  FSubCodeFoldingRanges := nil;

  inherited;
end;

procedure TBCEditorCodeFolding.TRanges.TRange.MoveBy(LineCount: Integer);
begin
  Inc(FBeginLine, LineCount);
  Inc(FEndLine, LineCount);
end;

procedure TBCEditorCodeFolding.TRanges.TRange.MoveChildren(By: Integer);
var
  LCodeFoldingRange: TRange;
  LIndex: Integer;
begin
  for LIndex := 0 to FSubCodeFoldingRanges.Count - 1 do
  begin
    LCodeFoldingRange := FSubCodeFoldingRanges[LIndex];
    if Assigned(LCodeFoldingRange) then
    begin
      LCodeFoldingRange.MoveChildren(By);

      with FAllCodeFoldingRanges.List do
      if LCodeFoldingRange.FParentCollapsed then
        Move(IndexOf(LCodeFoldingRange), IndexOf(LCodeFoldingRange) + By);
    end;
  end;
end;

procedure TBCEditorCodeFolding.TRanges.TRange.SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
var
  LCodeFoldingRange: TRange;
  LIndex: Integer;
begin
  if Assigned(FSubCodeFoldingRanges) then
  for LIndex := 0 to FSubCodeFoldingRanges.Count - 1 do
  begin
    LCodeFoldingRange := FSubCodeFoldingRanges[LIndex];
    LCodeFoldingRange.SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed, ACollapsedBy);

    if (LCodeFoldingRange.FCollapsedBy = -1) or (LCodeFoldingRange.FCollapsedBy = ACollapsedBy) then
    begin
      LCodeFoldingRange.FParentCollapsed := AParentCollapsed;

      if not AParentCollapsed then
        LCodeFoldingRange.FCollapsedBy := -1
      else
        LCodeFoldingRange.FCollapsedBy := ACollapsedBy;
    end;
  end;
end;

procedure TBCEditorCodeFolding.TRanges.TRange.Widen(LineCount: Integer);
begin
  Inc(FEndLine, LineCount);
end;

{ TBCEditorCodeFolding.TRanges ************************************************}

constructor TBCEditorCodeFolding.TRanges.Create;
begin
  inherited;

  FList := TList.Create;
end;

destructor TBCEditorCodeFolding.TRanges.Destroy;
begin
  FList.Clear;
  FList.Free;
  FList := nil;

  inherited;
end;

function TBCEditorCodeFolding.TRanges.Add(AAllCodeFoldingRanges: TAllRanges; ABeginLine, AIndentLevel, AFoldRangeLevel: Integer;
  ARegionItem: TBCEditorCodeFoldingRegionItem; AEndLine: Integer): TRange;
begin
  Result := TRange.Create;
  with Result do
  begin
    BeginLine := ABeginLine;
    EndLine := AEndLine;
    IndentLevel := AIndentLevel;
    FoldRangeLevel := AFoldRangeLevel;
    AllCodeFoldingRanges := AAllCodeFoldingRanges;
    RegionItem := ARegionItem;
  end;
  FList.Add(Result);
  AAllCodeFoldingRanges.List.Add(Result);
end;

procedure TBCEditorCodeFolding.TRanges.Clear;
begin
  FList.Clear;
end;

function TBCEditorCodeFolding.TRanges.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBCEditorCodeFolding.TRanges.GetItem(AIndex: Integer): TRange;
begin
  Result := FList[AIndex];
end;

{ TBCEditorCodeFolding.TAllRanges *********************************************}

constructor TBCEditorCodeFolding.TAllRanges.Create;
begin
  inherited;

  FList := TObjectList<TRanges.TRange>.Create;
end;

destructor TBCEditorCodeFolding.TAllRanges.Destroy;
begin
  FList.Free();

  inherited;
end;

procedure TBCEditorCodeFolding.TAllRanges.ClearAll;
begin
  Clear;
  FList.Clear();
end;

procedure TBCEditorCodeFolding.TAllRanges.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

procedure TBCEditorCodeFolding.TAllRanges.Delete(FoldRange: TRanges.TRange);
var
  LIndex: Integer;
begin
  for LIndex := 0 to FList.Count - 1 do
  if FList[LIndex] = FoldRange then
  begin
    TRanges.TRange(FList[LIndex]).Free;
    FList[LIndex] := nil;
    FList.Delete(LIndex);
    Break;
  end;
end;

function TBCEditorCodeFolding.TAllRanges.GetAllCount: Integer;
begin
  Result := FList.Count;
end;

function TBCEditorCodeFolding.TAllRanges.GetItem(AIndex: Integer): TRanges.TRange;
begin
  if Cardinal(AIndex) < Cardinal(FList.Count) then
    Result := FList.List[AIndex]
  else
    Result := nil;
end;

procedure TBCEditorCodeFolding.TAllRanges.SetItem(AIndex: Integer; Value: TRanges.TRange);
begin
  FList[AIndex] := Value;
end;

procedure TBCEditorCodeFolding.TAllRanges.SetParentCollapsedOfSubCodeFoldingRanges(AFoldRange: TRanges.TRange);
var
  LFoldRange: TRanges.TRange;
  LIndex: Integer;
begin
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if LFoldRange = AFoldRange then
      Continue;
    if LFoldRange.BeginLine > AFoldRange.EndLine then
      Break;
    if (LFoldRange.EndLine > AFoldRange.EndLine) and (LFoldRange.EndLine <> AFoldRange.EndLine) then
      LFoldRange.ParentCollapsed := True;
  end;
end;

procedure TBCEditorCodeFolding.TAllRanges.UpdateFoldRanges;
var
  LFoldRange: TRanges.TRange;
  LIndex: Integer;
begin
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if Assigned(LFoldRange) then
      LFoldRange.ParentCollapsed := False;
  end;
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if Assigned(LFoldRange) and not LFoldRange.ParentCollapsed then
      SetParentCollapsedOfSubCodeFoldingRanges(LFoldRange);
  end;
end;

{ TBCEditorCodeFolding ********************************************************}

constructor TBCEditorCodeFolding.Create;
begin
  inherited;

  FVisible := False;
  FOptions := DefaultOptions;
  FColors := TColors.Create;
  FDelayInterval := 300;

  FMouseOverHint := False;
end;

destructor TBCEditorCodeFolding.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TBCEditorCodeFolding.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCodeFolding then
  with ASource as TBCEditorCodeFolding do
  begin
    Self.FVisible := FVisible;
    Self.FOptions := FOptions;
    Self.FColors.Assign(FColors);
    if Assigned(Self.OnChange) then
      Self.OnChange(fcRescan);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCodeFolding.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(fcRefresh);
end;

procedure TBCEditorCodeFolding.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorCodeFolding.SetOnChange(AValue: TBCEditorCodeFoldingChangeEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := AValue;
end;

procedure TBCEditorCodeFolding.SetOption(const AOption: TBCEditorCodeFoldingOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorCodeFolding.SetOptions(AValue: TBCEditorCodeFoldingOptions);
var
  LRescan: Boolean;
begin
  LRescan := not (cfoFoldMultilineComments in FOptions) and (cfoFoldMultilineComments in AValue) or
    (cfoFoldMultilineComments in FOptions) and not (cfoFoldMultilineComments in AValue);

  FOptions := AValue;
  if LRescan then
    FOnChange(fcRescan)
  else
    DoChange;
end;

procedure TBCEditorCodeFolding.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    if Assigned(FOnChange) then
      FOnChange(fcEnabled);
  end;
end;

end.
