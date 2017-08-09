unit BCEditor.Lines;

interface {********************************************************************}

uses
  SysUtils, Classes, Generics.Collections, RegularExpressions, SyncObjs,
  Graphics, Controls, StdCtrls, Types,
  BCEditor.Consts, BCEditor.Types, BCEditor.Highlighter, BCEditor.Language;

type
  TBCEditorLines = class(TStrings)
  private type
    TJobThread = class(TThread)
    protected type
      TJob = (tjActivateSyncEdit, tjSearch);
      TJobs = set of TJob;
      TExecutedProc = procedure(const AData: Pointer) of object;
    private
      FEvent: TEvent;
      FJob: TJob;
      FLines: TBCEditorLines;
      FExecuted: TExecutedProc;
      function Terminated(): Boolean;
    protected
      procedure Execute(); override;
      function IsRunning(): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
      function IsRunning(const AJob: TJob): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
      function IsRunning(const AJobs: TJobs): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
      procedure Run(const AJob: TJob; const AExecuted: TExecutedProc);
      property Job: TJob read FJob;
    public
      constructor Create(const ALines: TBCEditorLines);
      destructor Destroy(); override;
      procedure Terminate();
    end;

    TWordSearch = class
    private
      FArea: TBCEditorLinesArea;
      FErrorMessage: string;
      FFoundLength: Integer;
      FFoundPosition: TBCEditorLinesPosition;
      FIgnoreNumbers: Boolean;
      FLines: TBCEditorLines;
      function FindNormal(const APosition: TBCEditorLinesPosition;
        const AFoundLength: Integer): Boolean;
    public
      constructor Create(const ALines: TBCEditorLines;
        const AArea: TBCEditorLinesArea; const AIgnoreNumbers: Boolean = False);
      function Find(var APosition: TBCEditorLinesPosition;
        out AFoundLength: Integer): Boolean;
      property Area: TBCEditorLinesArea read FArea;
      property ErrorMessage: string read FErrorMessage;
    end;

  protected type
    TCompare = function(Lines: TBCEditorLines; Line1, Line2: Integer): Integer;
    TChangeEvent = procedure(Sender: TObject; const Line: Integer) of object;

    PSearchResult = ^TSearchResult;
    TSearchResult = record
      Area: TBCEditorLinesArea;
      Backwards: Boolean;
      Count: Integer;
      ErrorMessage: string;
    end;

    TOption = (loTrimTrailingSpaces, loTrimTrailingLines, loUndoGrouped,
      loUndoAfterLoad, loUndoAfterSave, loSyncEditCaseSensitive, loReadOnly,
      loBeyondEndOfFile, loBeyondEndOfLine);
    TOptions = set of TOption;

    TState = set of (lsLoading, lsSaving, lsInserting, lsSearching,
      lsScanningSyncEdit,
      lsDontTrim, lsUndo, lsRedo, lsBlockModify,
      lsCaretChanged, lsSelChanged,
      lsSearch, lsSyncEdit);

    TTask = (ltActivateSyncEdit);

    TLine = packed record
    type
      TFlags = set of (lfContainsTabs);
      TState = (lsLoaded, lsModified, lsSaved);
    public
      Background: TColor;
      BeginRange: Pointer;
      CodeFolding: packed record
        BeginRange: Pointer;
        EndRange: Pointer;
        TreeLine: Boolean;
      end;
      FirstRow: Integer;
      Flags: TFlags;
      Foreground: TColor;
      RowCount: Integer;
      State: TLine.TState;
      Text: string;
    end;
    TLines = TList<TLine>;

    TSearch = class
    public type
      TJob = (sjFind, sjFindAndReplace);
    strict private
      FArea: TBCEditorLinesArea;
      FBackwards: Boolean;
      FCaseSensitive: Boolean;
      FEngine: (eNormal, eLinesRegExpr, eTextRegExpr);
      FErrorMessage: string;
      FFoundLength: Integer;
      FFoundPosition: TBCEditorLinesPosition;
      FJob: TJob;
      FLines: TBCEditorLines;
      FPattern: string;
      FPrompt: Boolean;
      FRegEx: TRegEx;
      FRegExOptions: TRegexOptions;
      FReplaceText: string;
      FWholeWords: Boolean;
      function FindLinesRegEx(const APosition: TBCEditorLinesPosition; const AFoundLength: Integer): Boolean;
      function FindNormal(const APosition: TBCEditorLinesPosition; const AFoundLength: Integer): Boolean;
      function FindTextRegEx(const APosition: TBCEditorLinesPosition; const AFoundLength: Integer): Boolean;
      function GetRegExpr(): Boolean;
    protected
      function Find(var APosition: TBCEditorLinesPosition; out AFoundLength: Integer): Boolean;
      procedure Replace();
      property Lines: TBCEditorLines read FLines;
    public
      constructor Create(const ALines: TBCEditorLines;
        const AArea: TBCEditorLinesArea;
        const ACaseSensitive, AWholeWords, ARegExpr, ABackwards: Boolean;
        const APattern: string;
        const AJob: TJob = sjFind; const AReplaceText: string = '';
        const APrompt: Boolean = False);
      property Area: TBCEditorLinesArea read FArea;
      property Backwards: Boolean read FBackwards;
      property CaseSensitive: Boolean read FCaseSensitive;
      property ErrorMessage: string read FErrorMessage;
      property Job: TJob read FJob;
      property Pattern: string read FPattern;
      property Prompt: Boolean read FPrompt;
      property RegExpr: Boolean read GetRegExpr;
      property ReplaceText: string read FReplaceText;
      property WholeWords: Boolean read FWholeWords;
    end;

    TSyncEditItem = record
      Area: TBCEditorLinesArea;
      Id: Integer;
    end;

    TUndoItem = packed record
    type
      TType = (utSelection, utInsert, utReplace, utBackspace, utDelete,
        utClear, utInsertIndent, utDeleteIndent);
    public
      BlockNumber: Integer;
      UndoType: TType;
      CaretPosition: TBCEditorLinesPosition;
      SelArea: TBCEditorLinesArea;
      Area: TBCEditorLinesArea;
      Text: string;
      function ToString(): string;
    end;

    TUndoList = class(TList<TUndoItem>)
    strict private
      FBlockNumber: Integer;
      FChanges: Integer;
      FCurrentBlockNumber: Integer;
      FGroupBreak: Boolean;
      FLines: TBCEditorLines;
      FUpdateCount: Integer;
      function GetUpdated(): Boolean;
    public
      procedure BeginUpdate();
      procedure Clear();
      constructor Create(const ALines: TBCEditorLines);
      procedure EndUpdate();
      procedure GroupBreak();
      function Peek(): TUndoItem;
      function Pop(): TUndoItem;
      procedure Push(const AUndoType: TUndoItem.TType;
        const ACaretPosition: TBCEditorLinesPosition; const ASelArea: TBCEditorLinesArea;
        const AArea: TBCEditorLinesArea; const AText: string = '';
        const ABlockNumber: Integer = 0); overload;
      property Changes: Integer read FChanges;
      property Lines: TBCEditorLines read FLines;
      property Updated: Boolean read GetUpdated;
      property UpdateCount: Integer read FUpdateCount;
    end;

  public type
    TMark = class;
    TMarkList = class;

    TMark = class
    protected
      FData: Pointer;
      FMarkList: TMarkList;
      FImageIndex: Integer;
      FIndex: Integer;
      FPos: TPoint;
      FVisible: Boolean;
    public
      constructor Create(const AMarkList: TMarkList);
      property Data: Pointer read FData write FData;
      property ImageIndex: Integer read FImageIndex write FImageIndex;
      property Index: Integer read FIndex write FIndex;
      property MarkList: TMarkList read FMarkList;
      property Pos: TPoint read FPos write FPos;
      property Visible: Boolean read FVisible write FVisible;
    end;

    TMarkList = class(TList<TMark>)
    private
      FLines: TBCEditorLines;
      FOnChange: TNotifyEvent;
    protected
      procedure Notify(const Item: TMark; Action: TCollectionNotification); override;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    public
      procedure Clear();
      procedure ClearLine(const ALine: Integer);
      constructor Create(const ALines: TBCEditorLines);
      procedure Delete(AIndex: Integer);
      destructor Destroy(); override;
      function IndexOfIndex(const AIndex: Integer): Integer;
      function Remove(AValue: TMark): Integer;
    end;

  private const
    DefaultOptions = [loUndoGrouped];
  protected const
    BOFPosition: TBCEditorLinesPosition = ( Char: 0; Line: 0; );

  private type
    TReplaceTextParams = record
      Area: TBCEditorLinesArea;
      ExecuteAllways: Boolean;
      Text: string;
      UndoType: TUndoItem.TType;
    end;

  private
    FBookmarks: TMarkList;
    FCaretPosition: TBCEditorLinesPosition;
    FCaseSensitive: Boolean;
    FFoundAreas: TList<TBCEditorLinesArea>;
    FHighlighter: TBCEditorHighlighter;
    FItems: TLines;
    FJobThread: TJobThread;
    FMarks: TMarkList;
    FModified: Boolean;
    FOldCaretPosition: TBCEditorLinesPosition;
    FOldSelArea: TBCEditorLinesArea;
    FOldUndoListCount: Integer;
    FOnAfterUpdate: TNotifyEvent;
    FOnBeforeUpdate: TNotifyEvent;
    FOnBookmarksChange: TNotifyEvent;
    FOnCaretChanged: TNotifyEvent;
    FOnCleared: TNotifyEvent;
    FOnDeleting: TChangeEvent;
    FOnInserted: TChangeEvent;
    FOnLoaded: TNotifyEvent;
    FOnMarksChange: TNotifyEvent;
    FOnReplacePrompt: TBCEditorReplacePromptEvent;
    FOnSelChange: TNotifyEvent;
    FOnSyncEditChange: TNotifyEvent;
    FOnUpdated: TChangeEvent;
    FOptions: TOptions;
    FRedoList: TUndoList;
    FReplaceText: TReplaceTextParams;
    FSearch: TSearch;
    FSearchAll: Boolean;
    FSearchPosition: TBCEditorLinesPosition;
    FSearchResult: TSearchResult;
    FSelArea: TBCEditorLinesArea;
    FSortOrder: TBCEditorSortOrder;
    FState: TState;
    FSyncEditArea: TBCEditorLinesArea;
    FSyncEditItems: TList<TSyncEditItem>;
    FUndoList: TUndoList;
    procedure BookmarksChanged(ASender: TObject); {$IFNDEF Debug} inline; {$ENDIF}
    function ContainsWordBreakChar(const AText: string): Boolean;
    procedure DoDelete(ALine: Integer);
    procedure DoDeleteIndent(ABeginPosition, AEndPosition: TBCEditorLinesPosition;
      const AIndentText: string);
    procedure DoDeleteText(const AArea: TBCEditorLinesArea);
    procedure DoInsertIndent(const AArea: TBCEditorLinesArea;
      const AIndentText: string);
    procedure DoInsert(ALine: Integer; const AText: string);
    function DoInsertText(APosition: TBCEditorLinesPosition;
      const AText: string): TBCEditorLinesPosition;
    procedure DoPut(ALine: Integer; const AText: string);
    procedure ExchangeItems(ALine1, ALine2: Integer);
    procedure ExecuteUndoRedo(const List: TUndoList);
    function Search(const ATerminated: TBCEditorTerminatedFunc;
      const AJobThread: TJobThread): Boolean;
    function GetArea(): TBCEditorLinesArea; {$IFNDEF Debug} inline; {$ENDIF}
    function GetBOLPosition(ALine: Integer): TBCEditorLinesPosition; {$IFNDEF Debug} inline; {$ENDIF}
    function GetCanRedo(): Boolean;
    function GetCanUndo(): Boolean;
    function GetChar(APosition: TBCEditorLinesPosition): Char;
    function GetEOFPosition(): TBCEditorLinesPosition;
    function GetEOLPosition(ALine: Integer): TBCEditorLinesPosition; {$IFNDEF Debug} inline; {$ENDIF}
    function GetLineArea(ALine: Integer): TBCEditorLinesArea; {$IFNDEF Debug} inline; {$ENDIF}
    function GetSelArea(): TBCEditorLinesArea; {$IFNDEF Debug} inline; {$ENDIF}
    function GetSyncEdit(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function GetTextIn(const AArea: TBCEditorLinesArea): string;
    function GetTextLength(): Integer;
    procedure InternalClear(const AClearUndo: Boolean); overload;
    procedure JobExecuted();
    procedure MarksChanged(ASender: TObject); {$IFNDEF Debug} inline; {$ENDIF}
    procedure ReplaceText(); overload;
    procedure ReplaceText(const AArea: TBCEditorLinesArea; const AText: string;
      const AExecuteAllways: Boolean); overload;
    procedure ReplaceText(const AArea: TBCEditorLinesArea; const AText: string;
      const AUndoType: TUndoItem.TType; const AExecuteAllways: Boolean = False); overload;
    function ScanSyncEdit(const ATerminated: TBCEditorTerminatedFunc;
      const ACheckAvailable: Boolean): Boolean;
    procedure SetCaretPosition(const AValue: TBCEditorLinesPosition);
    procedure SetModified(const AValue: Boolean);
    procedure SetSelArea(AValue: TBCEditorLinesArea);
    procedure StartJob(const AJob: TJobThread.TJob; const ABlockModify: Boolean;
      const AExecuted: TJobThread.TExecutedProc);
    function SyncEditItemIn(const AArea: TBCEditorLinesArea): Boolean;
    procedure QuickSort(ALeft, ARight: Integer; ACompare: TCompare);
  protected
    procedure ActivateSyncEdit(const AHighlighter: TBCEditorHighlighter;
      const AExecuted: TJobThread.TExecutedProc);
    function CharIndexOf(const APosition: TBCEditorLinesPosition): Integer; overload; inline;
    function CharIndexOf(const APosition: TBCEditorLinesPosition;
      const ARelativePosition: TBCEditorLinesPosition): Integer; overload;
    procedure ClearUndo();
    function CompareStrings(const S1, S2: string): Integer; override;
    procedure DeactivateSyncEdit(); {$IFNDEF Debug} inline; {$ENDIF}
    procedure DeleteIndent(ABeginPosition, AEndPosition: TBCEditorLinesPosition;
      const AIndentText: string);
    procedure DeleteText(const AArea: TBCEditorLinesArea;
      const ABackspace: Boolean = False);
    function Get(ALine: Integer): string; override;
    function GetCanModify(): Boolean; virtual;
    function GetCount(): Integer; override;
    function GetTextStr(): string; override;
    procedure InsertIndent(ABeginPosition, AEndPosition: TBCEditorLinesPosition;
      const AIndentText: string);
    procedure InsertFile(const APosition: TBCEditorLinesPosition;
      const AFilename: string);
    procedure InsertText(const APosition: TBCEditorLinesPosition;
      const AText: string); overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsWordBreakChar(const AChar: Char): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function PositionOf(const ACharIndex: Integer): TBCEditorLinesPosition; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function PositionOf(const ACharIndex: Integer;
      const ARelativePosition: TBCEditorLinesPosition): TBCEditorLinesPosition; overload;
    procedure Put(ALine: Integer; const AText: string); override;
    procedure Redo(); {$IFNDEF Debug} inline; {$ENDIF}
    procedure ReplaceText(const AArea: TBCEditorLinesArea;
      const AText: string); overload; {$IFNDEF Debug} inline; {$ENDIF}
    function ScanMatchingPair(const AHighlighter: TBCEditorHighlighter;
      const ATerminated: TBCEditorTerminatedFunc;
      const APosition: TBCEditorLinesPosition;
      out AOpenArea, ACloseArea: TBCEditorLinesArea): Boolean;
    procedure SetBackground(const ALine: Integer; const AValue: TColor); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetBeginRange(const ALine: Integer; const AValue: Pointer); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetCodeFoldingBeginRange(const ALine: Integer; const AValue: Pointer);
    procedure SetCodeFoldingEndRange(const ALine: Integer; const AValue: Pointer);
    procedure SetCodeFoldingTreeLine(const ALine: Integer; const AValue: Boolean);
    procedure SetForeground(const ALine: Integer; const AValue: TColor); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetRow(const ALine: Integer; const AFirstRow, ARowCount: Integer); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetTextStr(const AValue: string); override;
    procedure SetUpdateState(AUpdating: Boolean); override;
    procedure Sort(const ABeginLine, AEndLine: Integer; ACompare: TCompare = nil);
    procedure StartSearch(const ASearch: TSearch; const APosition: TBCEditorLinesPosition;
      const ASearchAll, ABlockModify: Boolean; const AExecuted: TJobThread.TExecutedProc);
    function SyncEditAvailable(const AHighlighter: TBCEditorHighlighter;
      const ATerminated: TBCEditorTerminatedFunc): Boolean;
    function SyncEditItemIndexOf(const APosition: TBCEditorLinesPosition): Integer;
    procedure TerminateJob();
    procedure Undo(); {$IFNDEF Debug} inline; {$ENDIF}
    procedure UndoGroupBreak();
    function ValidPosition(const APosition: TBCEditorLinesPosition): Boolean;
    property Area: TBCEditorLinesArea read GetArea;
    property BOLPosition[Line: Integer]: TBCEditorLinesPosition read GetBOLPosition;
    property Bookmarks: TMarkList read FBookmarks;
    property CanModify: Boolean read GetCanModify;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretPosition: TBCEditorLinesPosition read FCaretPosition write SetCaretPosition;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property Char[Position: TBCEditorLinesPosition]: Char read GetChar;
    property EOFPosition: TBCEditorLinesPosition read GetEOFPosition;
    property EOLPosition[ALine: Integer]: TBCEditorLinesPosition read GetEOLPosition;
    property FoundAreas: TList<TBCEditorLinesArea> read FFoundAreas;
    property Items: TLines read FItems;
    property Marks: TMarkList read FMarks;
    property Modified: Boolean read FModified write SetModified;
    property LineArea[Line: Integer]: TBCEditorLinesArea read GetLineArea;
    property OnAfterUpdate: TNotifyEvent read FOnAfterUpdate write FOnAfterUpdate;
    property OnBeforeUpdate: TNotifyEvent read FOnBeforeUpdate write FOnBeforeUpdate;
    property OnBookmarksChange: TNotifyEvent read FOnBookmarksChange write FOnBookmarksChange;
    property OnCaretChanged: TNotifyEvent read FOnCaretChanged write FOnCaretChanged;
    property OnCleared: TNotifyEvent read FOnCleared write FOnCleared;
    property OnDeleting: TChangeEvent read FOnDeleting write FOnDeleting;
    property OnInserted: TChangeEvent read FOnInserted write FOnInserted;
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
    property OnMarksChange: TNotifyEvent read FOnMarksChange write FOnMarksChange;
    property OnReplacePrompt: TBCEditorReplacePromptEvent read FOnReplacePrompt write FOnReplacePrompt;
    property OnSelChange: TNotifyEvent read FOnSelChange write FOnSelChange;
    property OnSyncEditChange: TNotifyEvent read FOnSyncEditChange write FOnSyncEditChange;
    property OnUpdated: TChangeEvent read FOnUpdated write FOnUpdated;
    property Options: TOptions read FOptions write FOptions;
    property RedoList: TUndoList read FRedoList;
    property SelArea: TBCEditorLinesArea read GetSelArea write SetSelArea;
    property SelBeginPosition: TBCEditorLinesPosition read FSelArea.BeginPosition;
    property SelEndPosition: TBCEditorLinesPosition read FSelArea.BeginPosition;
    property SortOrder: TBCEditorSortOrder read FSortOrder write FSortOrder;
    property State: TState read FState;
    property SyncEdit: Boolean read GetSyncEdit;
    property SyncEditArea: TBCEditorLinesArea read FSyncEditArea;
    property SyncEditItems: TList<TSyncEditItem> read FSyncEditItems;
    property TextIn[const Area: TBCEditorLinesArea]: string read GetTextIn;
    property TextLength: Integer read GetTextLength;
    property UndoList: TUndoList read FUndoList;
  public
    function Add(const AText: string): Integer; override;
    procedure BeginUpdate();
    procedure Clear(); overload; override;
    constructor Create();
    procedure Delete(ALine: Integer); overload; override;
    destructor Destroy; override;
    procedure EndUpdate();
    procedure Insert(ALine: Integer; const AText: string); override;
    procedure SaveToStream(AStream: TStream; AEncoding: TEncoding = nil); override;
  end;

function IsCombiningDiacriticalMark(const AChar: Char): Boolean;

implementation {***************************************************************}

uses
  Windows,
  Math, StrUtils, SysConst;

function HasLineBreak(const AText: string): Boolean;
var
  LEndPos: PChar;
  LPos: PChar;
begin
  LPos := PChar(AText); LEndPos := PChar(@AText[Length(AText)]);
  while (LPos <= LEndPos) do
    if (CharInSet(LPos^, [BCEDITOR_LINEFEED, BCEDITOR_CARRIAGE_RETURN])) then
      Exit(True)
    else
      Inc(LPos);
  Result := False;
end;

function IsCombiningDiacriticalMark(const AChar: Char): Boolean;
begin
  case Word(AChar) of
    $0300..$036F, $1DC0..$1DFF, $20D0..$20FF:
      Result := True
  else
    Result := False;
  end;
end;

{ TBCEditorLines.TJobThread ***************************************************}

constructor TBCEditorLines.TJobThread.Create(const ALines: TBCEditorLines);
begin
  inherited Create(False);

  FLines := ALines;

  FEvent := TEvent.Create(nil, True, False, '');
end;

destructor TBCEditorLines.TJobThread.Destroy();
begin
  FEvent.Free();

  inherited;
end;

procedure TBCEditorLines.TJobThread.Execute();
begin
  while ((FEvent.WaitFor(INFINITE) = wrSignaled) and not Terminated) do
    if (not Terminated) then
    begin
      case (FJob) of
        tjActivateSyncEdit:
          FLines.ScanSyncEdit(Terminated, False);
        tjSearch:
          FLines.Search(Terminated, Self);
      end;
      if (not Terminated) then
      begin
        FEvent.ResetEvent();
        if (Assigned(FExecuted)) then
          Synchronize(FLines.JobExecuted);
      end;
    end;
end;

function TBCEditorLines.TJobThread.IsRunning(): Boolean;
begin
  Result := FEvent.WaitFor(IGNORE) = wrSignaled;
end;

function TBCEditorLines.TJobThread.IsRunning(const AJob: TJob): Boolean;
begin
  Result := IsRunning([AJob]);
end;

function TBCEditorLines.TJobThread.IsRunning(const AJobs: TJobs): Boolean;
begin
  Assert(AJobs <> []);

  Result := IsRunning() and (FJob in AJobs);
end;

procedure TBCEditorLines.TJobThread.Run(const AJob: TJob;
  const AExecuted: TExecutedProc);
begin
  FJob := AJob;
  FExecuted := AExecuted;

  FEvent.SetEvent();
end;

procedure TBCEditorLines.TJobThread.Terminate();
begin
  FEvent.SetEvent();

  inherited;
end;

function TBCEditorLines.TJobThread.Terminated(): Boolean;
begin
  Result := inherited Terminated;
end;

{ TBCEditorLines.TMark ********************************************************}

constructor TBCEditorLines.TMark.Create(const AMarkList: TMarkList);
begin
  inherited Create;

  FMarkList := AMarkList;

  FData := nil;
  FImageIndex := -1;
  FIndex := -1;
  FPos := InvalidLinesPosition;
  FVisible := True;
end;

{ TBCEditorLines.TMarkList ****************************************************}

procedure TBCEditorLines.TMarkList.Clear();
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
    Items[LIndex].Free();

  inherited;
end;

procedure TBCEditorLines.TMarkList.ClearLine(const ALine: Integer);
var
  LIndex: Integer;
begin
  for LIndex := Count - 1 downto 0 do
    if (Items[LIndex].Pos.Y = ALine) then
      Delete(LIndex);
end;

constructor TBCEditorLines.TMarkList.Create(const ALines: TBCEditorLines);
begin
  inherited Create();

  FLines := ALines;
end;

procedure TBCEditorLines.TMarkList.Delete(AIndex: Integer);
begin
  Items[AIndex].Free();

  inherited;
end;

destructor TBCEditorLines.TMarkList.Destroy();
begin
  Clear();

  inherited;
end;

function TBCEditorLines.TMarkList.IndexOfIndex(const AIndex: Integer): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to Count - 1 do
    if (Items[LIndex].Index = AIndex) then
      Exit(LIndex);
end;

procedure TBCEditorLines.TMarkList.Notify(const Item: TMark; Action: TCollectionNotification);
begin
  inherited;

  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

function TBCEditorLines.TMarkList.Remove(AValue: TMark): Integer;
begin
  Result := IndexOf(AValue);
  if Result >= 0 then
    Delete(Result);
end;

{ TBCEditorLines.TSearch ******************************************************}

constructor TBCEditorLines.TSearch.Create(const ALines: TBCEditorLines;
  const AArea: TBCEditorLinesArea;
  const ACaseSensitive, AWholeWords, ARegExpr, ABackwards: Boolean;
  const APattern: string;
  const AJob: TJob = sjFind; const AReplaceText: string = '';
  const APrompt: Boolean = False);
var
  LIndex: Integer;
begin
  Assert((BOFPosition <= AArea.BeginPosition) and (AArea.BeginPosition <= AArea.EndPosition) and (AArea.EndPosition <= ALines.EOFPosition));
  Assert(APattern <> '');

  inherited Create();

  FLines := ALines;

  FArea := AArea;
  FBackwards := ABackwards;
  FCaseSensitive := ACaseSensitive;
  if (not ARegExpr) then
    FEngine := eNormal
  else if ((Length(APattern) >= 1) and ((APattern[1] = '^') or (APattern[Length(APattern)] = '$'))) then
    FEngine := eLinesRegExpr
  else
    FEngine := eTextRegExpr;
  FJob := AJob;
  if (FCaseSensitive or (FEngine <> eNormal)) then
    FPattern := APattern
  else
  begin
    // Since we modify FPattern with CharLowerBuff, we need a copy of the
    // string - not only a copy of the pointer to the string...
    FPattern := Copy(APattern, 1, Length(APattern));
    CharLowerBuff(PChar(FPattern), Length(FPattern));
  end;
  FPrompt := APrompt;
  FReplaceText := AReplaceText;
  FWholeWords := AWholeWords;

  if (FPattern = '') then
    FErrorMessage := SBCEditorPatternIsEmpty;

  if (FEngine = eNormal) then
  begin
    if (FWholeWords) then
      for LIndex := 1 to Length(FPattern) do
        if (FLines.IsWordBreakChar(FPattern[LIndex])) then
        begin
          FErrorMessage := SBCEditorPatternContainsWordBreakChar;
          FPattern := '';
          break;
        end;
  end
  else
  begin
    FRegExOptions := [roSingleLine, roCompiled];
    {$if CompilerVersion > 26}
    Include(FRegExOptions, roNotEmpty);
    {$endif}
    if (FCaseSensitive) then
      Exclude(FRegExOptions, roIgnoreCase)
    else
      Include(FRegExOptions, roIgnoreCase);
    FRegEx := TRegEx.Create(FPattern, FRegExOptions);
  end;
end;

function TBCEditorLines.TSearch.Find(var APosition: TBCEditorLinesPosition;
  out AFoundLength: Integer): Boolean;
begin
  Assert((FArea.BeginPosition <= APosition) and (APosition <= FArea.EndPosition));

  if (FErrorMessage <> '') then
    Result := False
  else
  begin
    case (FEngine) of
      eNormal: Result := FindNormal(APosition, AFoundLength);
      eLinesRegExpr: Result := FindLinesRegEx(APosition, AFoundLength);
      eTextRegExpr: Result := FindTextRegEx(APosition, AFoundLength);
      else raise ERangeError.Create('FEngine: ' + IntToStr(Ord(FEngine)));
    end;

    if (Result) then
      // if this fails, there is a bug in the FindXxx routine
      if (FBackwards) then
        Assert(FFoundPosition >= FArea.BeginPosition)
      else
        Assert(FFoundPosition <= FArea.EndPosition);

    if (Result) then
    begin
      APosition := FFoundPosition;
      AFoundLength := FFoundLength;
    end;
  end;
end;

function TBCEditorLines.TSearch.FindLinesRegEx(const APosition: TBCEditorLinesPosition;
  const AFoundLength: Integer): Boolean;
var
  LMatch: TMatch;
begin
  FFoundPosition := APosition;

  Result := False;
  if (FBackwards) then
    while (not Result and (FFoundPosition.Line >= 0)) do
    begin
      try
        LMatch := FRegEx.Match(FLines[FFoundPosition.Line]);
      except
        on E: Exception do
          FErrorMessage := E.Message;
      end;

      Result := (FErrorMessage = '') and LMatch.Success and (LMatch.Index - 1 < FFoundPosition.Char);
      while (Result and LMatch.Success) do
      begin
        FFoundPosition.Char := LMatch.Index - 1;
        LMatch := LMatch.NextMatch();
      end;

      if (not Result) then
        if (FFoundPosition.Line = 0) then
          FFoundPosition := LinesPosition(0, -1)
        else
          FFoundPosition := FLines.EOLPosition[FFoundPosition.Line - 1];
    end
  else
    while (not Result and (FFoundPosition.Line < FLines.Count)) do
    begin
      try
        LMatch := FRegEx.Match(FLines[FFoundPosition.Line]);
        while (LMatch.Success and (LMatch.Index - 1 < FFoundPosition.Char)) do
          LMatch := LMatch.NextMatch();
      except
        on E: Exception do
          FErrorMessage := E.Message;
      end;

      Result := (FErrorMessage = '') and LMatch.Success;
      if (Result) then
        FFoundPosition.Char := LMatch.Index - 1
      else
        FFoundPosition := FLines.BOLPosition[FFoundPosition.Line + 1];
    end;

  if (Result) then
    FFoundLength := LMatch.Length;
end;

function TBCEditorLines.TSearch.FindNormal(const APosition: TBCEditorLinesPosition;
  const AFoundLength: Integer): Boolean;
var
  LLineBeginPos: PChar;
  LLineCompPos: PChar;
  LLineEndPos: PChar;
  LLineLength: Integer;
  LLinePos: PChar;
  LLineText: string;
  LPatternBeginPos: PChar;
  LPatternEndPos: PChar;
  LPatternLength: Integer;
  LPatternPos: PChar;
begin
  LPatternLength := Length(FPattern);
  LPatternBeginPos := @FPattern[1];
  LPatternEndPos := @FPattern[LPatternLength];

  Result := False;
  if (LPatternLength > 0) then
  begin
    FFoundPosition := APosition;

    while (not Result
      and (FBackwards and (FFoundPosition > Max(FArea.BeginPosition, FLines.BOFPosition))
        or not FBackwards and (FFoundPosition < Min(FArea.EndPosition, FLines.EOFPosition)))) do
    begin
      LLineLength := Length(FLines.Items[FFoundPosition.Line].Text);
      if (FFoundPosition.Line = FArea.EndPosition.Line) then
        LLineLength := Min(LLineLength, FArea.EndPosition.Char + 1);

      if ((LLineLength >= LPatternLength)
        and (0 <= FFoundPosition.Char) and (FFoundPosition.Char < LLineLength)) then
      begin
        if (FCaseSensitive) then
          LLineText := FLines.Items[FFoundPosition.Line].Text
        else
        begin
          // Since we modify LLineText with CharLowerBuff, we need a copy of the
          // string - not only a copy of the pointer to the string...
          LLineText := Copy(FLines.Items[FFoundPosition.Line].Text, 1, LLineLength);
          CharLowerBuff(PChar(LLineText), LLineLength);
        end;

        if (FFoundPosition.Line = FArea.BeginPosition.Line) then
          LLineBeginPos := @LLineText[1 + FArea.BeginPosition.Char]
        else
          LLineBeginPos := @LLineText[1 + FFoundPosition.Char];
        LLineEndPos := @LLineText[1 + LLineLength - LPatternLength];

        if (FBackwards) then
          LLinePos := LLineEndPos
        else
          LLinePos := LLineBeginPos;

        while (not Result
          and (FBackwards and (LLinePos >= LLineBeginPos)
            or not FBackwards and (LLinePos <= LLineEndPos))) do
        begin
          if ((LLinePos^ = LPatternBeginPos^)
            and (not FWholeWords or not FLines.IsWordBreakChar(LLinePos^))) then
          begin
            LPatternPos := LPatternBeginPos;
            LLineCompPos := LLinePos;
            while ((LPatternPos <= LPatternEndPos)
              and (LPatternPos^ = LLineCompPos^)) do
            begin
              Inc(LPatternPos);
              Inc(LLineCompPos);
            end;
            Result := LPatternPos > LPatternEndPos;
          end;

          if (not Result) then
            if (FBackwards) then
              Dec(LLinePos)
            else
              Inc(LLinePos);
        end;

        FFoundPosition.Char := LLinePos - @LLineText[1];
      end;

      if (not Result) then
        if (FBackwards) then
        begin
          if (FFoundPosition.Line = 0) then
            FFoundPosition := LinesPosition(0, -1)
          else
            FFoundPosition := FLines.EOLPosition[FFoundPosition.Line - 1];
        end
        else
          FFoundPosition := FLines.BOLPosition[FFoundPosition.Line + 1];
    end;

    if (Result) then
      FFoundLength := LPatternLength;
  end;
end;

function TBCEditorLines.TSearch.FindTextRegEx(const APosition: TBCEditorLinesPosition;
  const AFoundLength: Integer): Boolean;
var
  LFoundPosition: Integer;
  LInput: string;
  LMatch: TMatch;
begin
  FFoundPosition := APosition;

  LInput := FLines.Text;
  LFoundPosition := FLines.CharIndexOf(FFoundPosition);

  if (FBackwards) then
  begin
    try
      LMatch := FRegEx.Match(LInput);
    except
      on E: Exception do
        FErrorMessage := E.Message;
    end;

    Result := (FErrorMessage = '') and LMatch.Success and (LMatch.Index - 1 < LFoundPosition);
    while (Result and LMatch.Success) do
    begin
      LFoundPosition := LMatch.Index - 1;
      LMatch := LMatch.NextMatch();
    end;
  end
  else
  begin
    try
      LMatch := FRegEx.Match(LInput);
      while (LMatch.Success and (LMatch.Index - 1 < LFoundPosition)) do
        LMatch := LMatch.NextMatch();
    except
      on E: Exception do
        FErrorMessage := E.Message;
    end;

    Result := (FErrorMessage = '') and LMatch.Success;
    if (Result) then
      LFoundPosition := LMatch.Index - 1
    else
      LFoundPosition := Length(LInput);
  end;

  if (Result) then
  begin
    FFoundPosition := FLines.PositionOf(LFoundPosition);
    FFoundLength := LMatch.Length;
  end;
end;

function TBCEditorLines.TSearch.GetRegExpr(): Boolean;
begin
  Result := FEngine in [eLinesRegExpr, eTextRegExpr];
end;

procedure TBCEditorLines.TSearch.Replace();
var
  LEndPosition: TBCEditorLinesPosition;
begin
  Assert((BOFPosition <= FFoundPosition) and (FFoundPosition <= FLines.EOFPosition));

  LEndPosition := FLines.PositionOf(FFoundLength, FFoundPosition);

  if (FEngine = eNormal) then
    FLines.ReplaceText(LinesArea(FFoundPosition, LEndPosition), FReplaceText, True)
  else
    FLines.ReplaceText(LinesArea(FFoundPosition, LEndPosition),
      FRegEx.Replace(FLines.TextIn[LinesArea(FFoundPosition, LEndPosition)], FPattern, FReplaceText, FRegExOptions), True);
end;

{ TBCEditorLines.TWordSearch **************************************************}

constructor TBCEditorLines.TWordSearch.Create(const ALines: TBCEditorLines;
  const AArea: TBCEditorLinesArea; const AIgnoreNumbers: Boolean = False);
begin
  Assert((BOFPosition <= AArea.BeginPosition) and (AArea.BeginPosition <= AArea.EndPosition) and (AArea.EndPosition <= ALines.EOFPosition));

  inherited Create();

  FLines := ALines;

  FArea := AArea;
  FIgnoreNumbers := AIgnoreNumbers;
end;

function TBCEditorLines.TWordSearch.Find(var APosition: TBCEditorLinesPosition;
  out AFoundLength: Integer): Boolean;
begin
  Assert((FArea.BeginPosition <= APosition) and (APosition <= FArea.EndPosition));

  Result := FindNormal(APosition, AFoundLength);

  if (Result) then
  begin
    APosition := FFoundPosition;
    AFoundLength := FFoundLength;
  end;
end;

function TBCEditorLines.TWordSearch.FindNormal(const APosition: TBCEditorLinesPosition;
  const AFoundLength: Integer): Boolean;
var
  LLineLength: Integer;
  LLinePos: PChar;
  LLineEndPos: PChar;
  LLineText: string;
begin
  Result := False;

  LLinePos := nil; // Compiler warning only
  LLineEndPos := nil; // Compiler warning only
  LLineText := ''; // Compiler warning only

  FFoundPosition := APosition;

  while (not Result
    and (FFoundPosition < FArea.EndPosition)) do
  begin
    if (FFoundPosition.Line < FArea.EndPosition.Line) then
      LLineLength := Length(FLines.Items[FFoundPosition.Line].Text)
    else
      LLineLength := FArea.EndPosition.Char;

    if (LLineLength > FFoundPosition.Char) then
    begin
      LLineText := FLines.Items[FFoundPosition.Line].Text;

      LLinePos := @LLineText[1 + FFoundPosition.Char];
      LLineEndPos := @LLineText[LLineLength];
      while (not Result
        and (LLinePos <= LLineEndPos)) do
      begin
        Result := not FLines.IsWordBreakChar(LLinePos^);

        if (not Result) then
          Inc(LLinePos);
      end;

      if (FIgnoreNumbers) then
        Result := Result and not CharInSet(LLinePos^, ['0' .. '9']);
    end;

    if (not Result) then
      FFoundPosition := FLines.BOLPosition[FFoundPosition.Line + 1]
    else
      FFoundPosition.Char := LLinePos - @LLineText[1];
  end;

  if (Result) then
  begin
    while ((@LLinePos[1] <= LLineEndPos) and not FLines.IsWordBreakChar(LLinePos[1])) do
      Inc(LLinePos);

    FFoundLength := LLinePos + 1 - @LLineText[1] - FFoundPosition.Char;
  end;
end;

{ TBCEditorLines.TUndoList ****************************************************}

function TBCEditorLines.TUndoItem.ToString(): string;
begin
  Result :=
    'BlockNumber: ' + IntToStr(BlockNumber) + #13#10
    + 'UndoType: ' + IntToStr(Ord(UndoType)) + #13#10
    + 'CaretPosition: ' + CaretPosition.ToString() + #13#10
    + 'SelArea: ' + SelArea.ToString() + #13#10
    + 'Area: ' + Area.ToString() + #13#10
    + 'Text: ' + Text;
end;

{ TBCEditorLines.TUndoList ****************************************************}

procedure TBCEditorLines.TUndoList.BeginUpdate();
begin
  if (FUpdateCount = 0) then
  begin
    Inc(FBlockNumber);
    FChanges := 0;
    FCurrentBlockNumber := FBlockNumber;
  end;

  Inc(FUpdateCount);
end;

procedure TBCEditorLines.TUndoList.Clear();
begin
  inherited;

  FBlockNumber := 0;
  FGroupBreak := False;
end;

constructor TBCEditorLines.TUndoList.Create(const ALines: TBCEditorLines);
begin
  inherited Create();

  FLines := ALines;

  FBlockNumber := 0;
  FUpdateCount := 0;
end;

procedure TBCEditorLines.TUndoList.EndUpdate();
begin
  if (FUpdateCount > 0) then
  begin
    Dec(FUpdateCount);

    if (FUpdateCount = 0) then
    begin
      FChanges := 0;
      FCurrentBlockNumber := 0;
    end;
  end;
end;

function TBCEditorLines.TUndoList.GetUpdated(): Boolean;
begin
  Result := (FUpdateCount > 0) and (FChanges > 0);
end;

procedure TBCEditorLines.TUndoList.GroupBreak();
begin
  FGroupBreak := True;
end;

function TBCEditorLines.TUndoList.Peek(): TUndoItem;
begin
  Assert(Count > 0);

  Result := List[Count - 1];
end;

function TBCEditorLines.TUndoList.Pop(): TUndoItem;
begin
  Result := Peek();
  Delete(Count - 1);
end;

procedure TBCEditorLines.TUndoList.Push(const AUndoType: TUndoItem.TType;
  const ACaretPosition: TBCEditorLinesPosition; const ASelArea: TBCEditorLinesArea;
  const AArea: TBCEditorLinesArea; const AText: string = '';
  const ABlockNumber: Integer = 0);
var
  LHandled: Boolean;
  LItem: TUndoItem;
begin
  if (not (lsLoading in FLines.State)) then
  begin
    LHandled := False;
    if ((FLines.State * [lsUndo, lsRedo] = [])
      and (loUndoGrouped in FLines.Options)
      and not FGroupBreak
      and (Count > 0) and (List[Count - 1].UndoType = AUndoType)) then
      case (AUndoType) of
        utSelection: LHandled := True; // Ignore
        utInsert:
          if (List[Count - 1].Area.EndPosition = AArea.BeginPosition) then
          begin
            List[Count - 1].Area.EndPosition := AArea.EndPosition;
            LHandled := True;
          end;
        utReplace:
          if (List[Count - 1].Area.EndPosition = AArea.BeginPosition) then
          begin
            List[Count - 1].Area.EndPosition := AArea.EndPosition;
            List[Count - 1].Text := List[Count - 1].Text + AText;
            LHandled := True;
          end;
        utBackspace:
          if (List[Count - 1].Area.BeginPosition = AArea.EndPosition) then
          begin
            List[Count - 1].Area.BeginPosition := AArea.BeginPosition;
            List[Count - 1].Text := AText + List[Count - 1].Text;
            LHandled := True;
          end;
        utDelete:
          if (List[Count - 1].Area.EndPosition = AArea.BeginPosition) then
          begin
            List[Count - 1].Area.EndPosition := AArea.EndPosition;
            List[Count - 1].Text := List[Count - 1].Text + AText;
            LHandled := True;
          end;
      end;

    if (not LHandled) then
    begin
      if (ABlockNumber > 0) then
        LItem.BlockNumber := ABlockNumber
      else if (FCurrentBlockNumber > 0) then
        LItem.BlockNumber := FCurrentBlockNumber
      else
      begin
        Inc(FBlockNumber);
        LItem.BlockNumber := FBlockNumber;
      end;
      LItem.Area := AArea;
      LItem.CaretPosition := ACaretPosition;
      LItem.SelArea := ASelArea;
      LItem.Text := AText;
      LItem.UndoType := AUndoType;
      Add(LItem);
    end;

    if (FUpdateCount > 0) then
      Inc(FChanges);
    FGroupBreak := False;
  end;
end;

{ TBCEditorLines **************************************************************}

function CompareLines(ALines: TBCEditorLines; AIndex1, AIndex2: Integer): Integer;
begin
  Result := ALines.CompareStrings(ALines.Items[AIndex1].Text, ALines.Items[AIndex2].Text);
  if (ALines.SortOrder = soDesc) then
    Result := - Result;
end;

procedure TBCEditorLines.ActivateSyncEdit(const AHighlighter: TBCEditorHighlighter;
  const AExecuted: TJobThread.TExecutedProc);
begin
  if (not SelArea.IsEmpty()) then
  begin
    FSyncEditArea := FSelArea;
    SetSelArea(LinesArea(CaretPosition, CaretPosition));

    Include(FState, lsSyncEdit);

    FHighlighter := AHighlighter;

    StartJob(tjActivateSyncEdit, True, AExecuted);

    if (Assigned(FOnSyncEditChange)) then
      FOnSyncEditChange(Self);
  end;
end;

function TBCEditorLines.Add(const AText: string): Integer;
begin
  if (not CanModify) then
    Result := -1
  else
  begin
    TerminateJob();
    DeactivateSyncEdit();

    Result := Count;
    Insert(Count, AText);
  end;
end;

procedure TBCEditorLines.BeginUpdate();
begin
  if (TThread.CurrentThread.ThreadID <> MainThreadID) then
    TThread.Synchronize(nil, BeginUpdate)
  else
    inherited;
end;

procedure TBCEditorLines.BookmarksChanged(ASender: TObject);
begin
  if (Assigned(FOnBookmarksChange)) then
    FOnBookmarksChange(ASender);
end;

function TBCEditorLines.CharIndexOf(const APosition: TBCEditorLinesPosition): Integer;
begin
  Result := CharIndexOf(APosition, BOFPosition);
end;

function TBCEditorLines.CharIndexOf(const APosition: TBCEditorLinesPosition;
  const ARelativePosition: TBCEditorLinesPosition): Integer;
var
  LLine: Integer;
  LLineBreakLength: Integer;
begin
  LLineBreakLength := Length(LineBreak);

  if (APosition.Line = ARelativePosition.Line) then
    Result := ARelativePosition.Char - APosition.Char
  else if (APosition < ARelativePosition) then
  begin
    Result := - ARelativePosition.Char - LLineBreakLength;
    for LLine := APosition.Line - 1 downto ARelativePosition.Line + 1 do
    begin
      Dec(Result, Length(Items[LLine].Text));
      Dec(Result, LLineBreakLength);
    end;
    Dec(Result, Length(Items[APosition.Line].Text) - APosition.Char);
  end
  else
  begin
    Result := Length(Items[ARelativePosition.Line].Text) - APosition.Char + LLineBreakLength;
    for LLine := ARelativePosition.Line + 1 to APosition.Line - 1 do
    begin
      Inc(Result, Length(Items[LLine].Text));
      Inc(Result, LLineBreakLength);
    end;
    Inc(Result, APosition.Char);
  end;
end;

procedure TBCEditorLines.Clear();
begin
  if (CanModify) then
  begin
    TerminateJob();
    FBookmarks.Clear();
    FMarks.Clear();
    FFoundAreas.Clear();
    DeactivateSyncEdit();

    InternalClear(True);
  end;
end;

procedure TBCEditorLines.ClearUndo();
begin
  UndoList.Clear();
  RedoList.Clear();
end;

function TBCEditorLines.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then
    Result := CompareStr(S1, S2)
  else
    Result := CompareText(S1, S2);

  if SortOrder = soDesc then
    Result := -1 * Result;
end;

function TBCEditorLines.ContainsWordBreakChar(const AText: string): Boolean;
var
  LEndPos: PChar;
  LPos: PChar;
begin
  if (AText = '') then
    Result := False
  else
  begin
    LPos := @AText[1];
    LEndPos := @AText[Length(AText)];
    while ((LPos <= LEndPos) and not IsWordBreakChar(LPos^)) do
      Inc(LPos);
    Result := LPos <= LEndPos;
  end;
end;

constructor TBCEditorLines.Create();
begin
  inherited Create();

  FBookmarks := TMarkList.Create(Self);
  FBookmarks.OnChange := BookmarksChanged;
  FCaretPosition := BOFPosition;
  FCaseSensitive := False;
  FItems := TLines.Create();
  FJobThread := nil;
  FMarks := TMarkList.Create(Self);
  FMarks.OnChange := MarksChanged;
  FModified := False;
  FOnAfterUpdate := nil;
  FOnBeforeUpdate := nil;
  FOnCaretChanged := nil;
  FOnCleared := nil;
  FOnDeleting := nil;
  FOnInserted := nil;
  FOnLoaded := nil;
  FOnSelChange := nil;
  FOnUpdated := nil;
  FOptions := DefaultOptions;
  FRedoList := TUndoList.Create(Self);
  FFoundAreas := TList<TBCEditorLinesArea>.Create();
  FSelArea := LinesArea(BOFPosition, BOFPosition);
  FSyncEditArea := InvalidLinesArea;
  FSyncEditItems := TList<TSyncEditItem>.Create();
  FState := [];
  FUndoList := TUndoList.Create(Self);
end;

procedure TBCEditorLines.DeactivateSyncEdit();
begin
  if (lsSyncEdit in FState) then
  begin
    TerminateJob();

    Exclude(FState, lsSyncEdit);

    if (Assigned(FOnSyncEditChange)) then
      FOnSyncEditChange(Self);

    FSyncEditArea := InvalidLinesArea;
    FSyncEditItems.Clear();
  end;
end;

procedure TBCEditorLines.Delete(ALine: Integer);
var
  LBeginPosition: TBCEditorLinesPosition;
  LCaretPosition: TBCEditorLinesPosition;
  LIndex: Integer;
  LSelArea: TBCEditorLinesArea;
  LText: string;
  LUndoType: TUndoItem.TType;
begin
  if (CanModify) then
  begin
    TerminateJob();
    DeactivateSyncEdit();

    Assert((0 <= ALine) and (ALine < Count));

    LCaretPosition := CaretPosition;
    LSelArea := LinesArea(SelBeginPosition, SelEndPosition);
    if (Count = 1) then
    begin
      LBeginPosition := BOLPosition[ALine];
      LText := Items[ALine].Text;
      LUndoType := utClear;
    end
    else if (ALine < Count - 1) then
    begin
      LBeginPosition := BOLPosition[ALine];
      LText := Items[ALine].Text + LineBreak;
      LUndoType := utDelete;
    end
    else
    begin
      LBeginPosition := EOLPosition[ALine - 1];
      LText := LineBreak + Items[ALine].Text;
      LUndoType := utDelete;
    end;

    BeginUpdate();
    try
      DoDelete(ALine);

      for LIndex := FBookmarks.Count - 1 downto 0 do
        if (FBookmarks[LIndex].Pos.Y = ALine) then
          FBookmarks.Delete(LIndex)
        else if (FBookmarks[LIndex].Pos.Y > ALine) then
          FBookmarks[LIndex].Pos := Point(FBookmarks[LIndex].Pos.X, FBookmarks[LIndex].Pos.Y - 1);
      for LIndex := FBookmarks.Count - 1 downto 0 do
        if (FMarks[LIndex].Pos.Y = ALine) then
          FMarks.Delete(LIndex)
        else if (FMarks[LIndex].Pos.Y > ALine) then
          FMarks[LIndex].Pos := Point(FMarks[LIndex].Pos.X, FMarks[LIndex].Pos.Y - 1);

      UndoList.Push(LUndoType, LCaretPosition, LSelArea,
        LinesArea(LBeginPosition, InvalidLinesPosition), LText);
    finally
      EndUpdate();
    end;
  end;
end;

procedure TBCEditorLines.DeleteIndent(ABeginPosition, AEndPosition: TBCEditorLinesPosition;
  const AIndentText: string);
var
  LArea: TBCEditorLinesArea;
  LCaretPosition: TBCEditorLinesPosition;
  LLine: Integer;
  LIndentFound: Boolean;
  LIndentTextLength: Integer;
  LSelArea: TBCEditorLinesArea;
begin
  if (CanModify) then
  begin
    TerminateJob();
    DeactivateSyncEdit();

    LArea := GetSelArea();

    Assert((BOFPosition <= LArea.BeginPosition) and (LArea.EndPosition <= EOFPosition));

    LIndentTextLength := Length(AIndentText);
    LIndentFound := LArea.BeginPosition.Line <> LArea.EndPosition.Line;
    for LLine := LArea.BeginPosition.Line to LArea.EndPosition.Line do
      if (Copy(Items[LLine].Text, 1 + LArea.BeginPosition.Char, LIndentTextLength) <> AIndentText) then
      begin
        LIndentFound := False;
        break;
      end;

    if (LIndentFound) then
    begin
      LCaretPosition := CaretPosition;

      DoDeleteIndent(LArea.BeginPosition, LArea.EndPosition, AIndentText);

      UndoList.Push(utDeleteIndent, LCaretPosition, LSelArea,
        LArea, AIndentText);

      RedoList.Clear();
    end
    else
    begin
      BeginUpdate();
      try
        for LLine := LArea.BeginPosition.Line to LArea.EndPosition.Line do
          if (LeftStr(Items[LLine].Text, LIndentTextLength) = AIndentText) then
            DeleteText(LinesArea(BOLPosition[LLine], LinesPosition(Length(AIndentText), LLine)));
      finally
        EndUpdate();
      end;
    end;

    if ((ABeginPosition <= CaretPosition) and (CaretPosition <= AEndPosition)
      and (CaretPosition.Char > Length(Items[CaretPosition.Line].Text))) then
      FCaretPosition.Char := Length(Items[CaretPosition.Line].Text);
    if (FSelArea.Contains(ABeginPosition) and FSelArea.Contains(AEndPosition))
      and (CaretPosition.Char > Length(Items[FSelArea.BeginPosition.Line].Text)) then
      FSelArea.BeginPosition.Char := Length(Items[GetSelArea().BeginPosition.Line].Text);
    if ((ABeginPosition <= FSelArea.EndPosition) and (FSelArea.EndPosition <= AEndPosition)
      and (FSelArea.EndPosition.Char > Length(Items[FSelArea.EndPosition.Line].Text))) then
      FSelArea.EndPosition.Char := Length(Items[GetSelArea().EndPosition.Line].Text);
  end;
end;

procedure TBCEditorLines.DeleteText(const AArea: TBCEditorLinesArea;
  const ABackspace: Boolean = False);
begin
  if (ABackspace) then
    ReplaceText(AArea, '', utBackspace)
  else
    ReplaceText(AArea, '', utDelete);
end;

destructor TBCEditorLines.Destroy();
begin
  if (Assigned(FJobThread)) then
  begin
    FJobThread.Terminate();
    FJobThread.WaitFor();
    FJobThread.Free();
  end;

  FBookmarks.Free();
  FItems.Free();
  FMarks.Free();
  FRedoList.Free();
  FFoundAreas.Free();
  FSyncEditItems.Free();
  FUndoList.Free();

  inherited;
end;

procedure TBCEditorLines.DoDelete(ALine: Integer);
begin
  Assert((0 <= ALine) and (ALine < Count));

  if (Assigned(FOnDeleting)) then
    FOnDeleting(Self, ALine);

  Items.Delete(ALine);

  FModified := True;
  FFoundAreas.Clear();

  if (Count = 0) then
    CaretPosition := BOFPosition
  else if (ALine < Count) then
    CaretPosition := BOLPosition[ALine]
  else
    CaretPosition := EOLPosition[ALine - 1];

  if ((Count = 0) and Assigned(FOnCleared)) then
    FOnCleared(Self);
end;

procedure TBCEditorLines.DoDeleteIndent(ABeginPosition, AEndPosition: TBCEditorLinesPosition;
  const AIndentText: string);
var
  LLine: Integer;
  LLinesBeginPosition: TBCEditorLinesPosition;
  LLinesEndPosition: TBCEditorLinesPosition;
begin
  Assert((BOFPosition <= ABeginPosition) and (AEndPosition <= EOFPosition));
  Assert(ABeginPosition <= AEndPosition);

  if (Count > 0) then
  begin
    LLinesBeginPosition := BOLPosition[ABeginPosition.Line];
    if (ABeginPosition = AEndPosition) then
      LLinesEndPosition := EOLPosition[AEndPosition.Line]
    else if ((AEndPosition.Char = 0) and (AEndPosition.Line > ABeginPosition.Line)) then
      LLinesEndPosition := EOLPosition[AEndPosition.Line - 1]
    else
      LLinesEndPosition := AEndPosition;

    BeginUpdate();
    try
      for LLine := LLinesBeginPosition.Line to LLinesEndPosition.Line do
        if (LeftStr(Items[LLine].Text, Length(AIndentText)) = AIndentText) then
          DoPut(LLine, Copy(Items[LLine].Text, 1 + Length(AIndentText), MaxInt));
    finally
      EndUpdate();
    end;
  end;
end;

procedure TBCEditorLines.DoDeleteText(const AArea: TBCEditorLinesArea);
var
  Line: Integer;
begin
  Assert((BOFPosition <= AArea.BeginPosition) and (AArea.EndPosition <= EOFPosition));
  Assert(AArea.BeginPosition <= AArea.EndPosition);

  if (AArea.IsEmpty()) then
    // Nothing to do...
  else if (AArea.BeginPosition.Line = AArea.EndPosition.Line) then
    DoPut(AArea.BeginPosition.Line, LeftStr(Items[AArea.BeginPosition.Line].Text, AArea.BeginPosition.Char)
      + Copy(Items[AArea.EndPosition.Line].Text, 1 + AArea.EndPosition.Char, MaxInt))
  else
  begin
    BeginUpdate();
    try
      DoPut(AArea.BeginPosition.Line, LeftStr(Items[AArea.BeginPosition.Line].Text, AArea.BeginPosition.Char)
        + Copy(Items[AArea.EndPosition.Line].Text, 1 + AArea.EndPosition.Char, MaxInt));

      for Line := AArea.EndPosition.Line downto AArea.BeginPosition.Line + 1 do
        DoDelete(Line);
    finally
      EndUpdate();
    end;
  end;
end;

procedure TBCEditorLines.DoInsertIndent(const AArea: TBCEditorLinesArea;
  const AIndentText: string);
var
  LEndLine: Integer;
  LLine: Integer;
begin
  Assert((BOFPosition <= AArea.BeginPosition) and (AArea.EndPosition <= EOFPosition));
  Assert(AArea.BeginPosition <= AArea.EndPosition);

  if (Count > 0) then
  begin
    if ((AArea.EndPosition.Char = 0) and (AArea.EndPosition.Line > AArea.BeginPosition.Line)) then
      LEndLine := AArea.EndPosition.Line - 1
    else
      LEndLine := AArea.EndPosition.Line;

    BeginUpdate();
    try
      for LLine := AArea.BeginPosition.Line to LEndLine do
        DoPut(LLine, AIndentText + Items[LLine].Text);
    finally
      EndUpdate();
    end;
  end;
end;

procedure TBCEditorLines.DoInsert(ALine: Integer; const AText: string);
var
  LLine: TLine;
begin
  Assert((0 <= ALine) and (ALine <= Count));

  BeginUpdate();
  try
    LLine.Background := clNone;
    LLine.BeginRange := nil;
    LLine.CodeFolding.BeginRange := nil;
    LLine.CodeFolding.EndRange := nil;
    LLine.CodeFolding.TreeLine := False;
    LLine.Flags := [];
    LLine.FirstRow := -1;
    LLine.Foreground := clNone;
    LLine.State := lsModified;
    LLine.Text := '';
    Items.Insert(ALine, LLine);

    Include(FState, lsInserting);
    try
      DoPut(ALine, AText);
    finally
      Exclude(FState, lsInserting);
    end;

    if (ALine < Count - 1) then
      CaretPosition := BOLPosition[ALine + 1]
    else
      CaretPosition := EOLPosition[ALine];
    SetSelArea(LinesArea(CaretPosition, CaretPosition));

    if (Assigned(FOnInserted)) then
      FOnInserted(Self, ALine);
  finally
    EndUpdate();
  end;
end;

function TBCEditorLines.DoInsertText(APosition: TBCEditorLinesPosition;
  const AText: string): TBCEditorLinesPosition;
var
  LEndPos: PChar;
  LEOL: Boolean;
  LLine: Integer;
  LLineBeginPos: PChar;
  LLineBreak: array [0..2] of System.Char;
  LLineEnd: string;
  LPos: PChar;
begin
  Assert(BOFPosition <= APosition);
  Assert((APosition.Line = 0) and (Count = 0) or (APosition.Line < Count) and (APosition.Char <= Length(Items[APosition.Line].Text)));

  if (AText = '') then
    Result := APosition
  else if (not HasLineBreak(AText)) then
  begin
    if (Count = 0) then
    begin
      DoInsert(0, AText);
      Result := EOLPosition[0];
    end
    else
    begin
      DoPut(APosition.Line, LeftStr(Items[APosition.Line].Text, APosition.Char)
        + AText
        + Copy(Items[APosition.Line].Text, 1 + APosition.Char, MaxInt));
      Result := LinesPosition(APosition.Char + Length(AText), APosition.Line);
    end;
  end
  else
  begin
    LLineBreak[0] := #0; LLineBreak[1] := #0; LLineBreak[2] := #0;


    BeginUpdate();
    try
      LLine := APosition.Line;

      LPos := @AText[1];
      LEndPos := @AText[Length(AText)];

      LLineBeginPos := LPos;
      while ((LPos <= LEndPos) and not CharInSet(LPos^, [BCEDITOR_LINEFEED, BCEDITOR_CARRIAGE_RETURN])) do
        Inc(LPos);

      if (Count = 0) then
      begin
        DoInsert(0, LeftStr(AText, LPos - LLineBeginPos));
        LLine := 1;
      end
      else if (LLine < Count) then
      begin
        if (APosition.Char = 0) then
        begin
          LLineEnd := Items[LLine].Text;
          if (LLineBeginPos < LPos) then
            DoPut(LLine, LeftStr(AText, LPos - LLineBeginPos))
          else if (Items[LLine].Text <> '') then
            DoPut(LLine, '');
        end
        else
        begin
          LLineEnd := Copy(Items[LLine].Text, 1 + APosition.Char, MaxInt);
          if (LLineBeginPos < LPos) then
            DoPut(LLine, LeftStr(Items[LLine].Text, APosition.Char) + LeftStr(AText, LPos - LLineBeginPos))
          else if (Length(Items[LLine].Text) > APosition.Char) then
            DoPut(LLine, LeftStr(Items[LLine].Text, APosition.Char));
        end;
        Inc(LLine);
      end
      else
      begin
        DoInsert(LLine, LeftStr(AText, LPos - LLineBeginPos));
        Inc(LLine);
      end;

      if (LPos <= LEndPos) then
      begin
        LLineBreak[0] := LPos^;
        if ((LLineBreak[0] = BCEDITOR_CARRIAGE_RETURN) and (LPos < LEndPos) and (LPos[1] = BCEDITOR_LINEFEED)) then
          LLineBreak[1] := LPos[1];
      end;

      LEOL := (LPos <= LEndPos) and (LPos[0] = LLineBreak[0]) and ((LLineBreak[1] = #0) or (LPos < LEndPos) and (LPos[1] = LLineBreak[1]));
      while (LEOL) do
      begin
        if (LLineBreak[1] = #0) then
          Inc(LPos)
        else
          Inc(LPos, 2);
        LLineBeginPos := LPos;
        repeat
          LEOL := (LPos <= LEndPos) and (LPos[0] = LLineBreak[0]) and ((LLineBreak[1] = #0) or (LPos < LEndPos) and (LPos[1] = LLineBreak[1]));
          if (not LEOL) then
            Inc(LPos);
        until ((LPos > LEndPos) or LEOL);
        if (LEOL) then
        begin
          DoInsert(LLine, Copy(AText, 1 + LLineBeginPos - @AText[1], LPos - LLineBeginPos));
          Inc(LLine);
        end;
      end;

      if (LPos <= LEndPos) then
      begin
        DoInsert(LLine, Copy(AText, LPos - @AText[1], LEndPos + 1 - LPos) + LLineEnd);
        Result := LinesPosition(LEndPos + 1 - (LLineBeginPos + 1), LLine);
      end
      else
      begin
        DoInsert(LLine, RightStr(AText, LEndPos + 1 - LLineBeginPos) + LLineEnd);
        Result := LinesPosition(1 + LEndPos + 1 - (LLineBeginPos + 1), LLine);
      end;

    finally
      EndUpdate();
    end;

    if ((lsLoading in State) and (LLineBreak[0] <> #0)) then
      LineBreak := StrPas(PChar(@LLineBreak[0]));
  end;
end;

procedure TBCEditorLines.DoPut(ALine: Integer; const AText: string);
var
  LModified: Boolean;
  LPos: PChar;
  LEndPos: PChar;
begin
  Assert((0 <= ALine) and (ALine < Count));

  LModified := AText <> Items[ALine].Text;
  if (LModified) then
  begin
    Items.List[ALine].Flags := [];
    Items.List[ALine].State := lsModified;
    Items.List[ALine].Text := AText;

    if (AText <> '') then
    begin
      LPos := @AText[1];
      LEndPos := @AText[Length(AText)];
      while (LPos <= LEndPos) do
      begin
        if (LPos^ = BCEDITOR_TAB_CHAR) then
        begin
          Include(Items.List[ALine].Flags, lfContainsTabs);
          break;
        end;
        Inc(LPos);
      end;
    end;
  end;

  CaretPosition := EOLPosition[ALine];

  if (LModified and not (lsInserting in State)) then
  begin
    FModified := True;
    FFoundAreas.Clear();

    if (Assigned(FOnUpdated)) then
      FOnUpdated(Self, ALine);
  end;
end;

procedure TBCEditorLines.EndUpdate();
begin
  if (TThread.CurrentThread.ThreadID <> MainThreadID) then
    TThread.Synchronize(nil, EndUpdate)
  else
    inherited;
end;

procedure TBCEditorLines.ExchangeItems(ALine1, ALine2: Integer);
var
  LLine: TLine;
begin
  LLine := Items[ALine1];
  Items[ALine1] := Items[ALine2];
  Items[ALine2] := LLine;
end;

var
  Progress: string;

procedure TBCEditorLines.ExecuteUndoRedo(const List: TUndoList);
var
  LPreviousBlockNumber: Integer;
  LCaretPosition: TBCEditorLinesPosition;
  LDestinationList: TUndoList;
  LEndPosition: TBCEditorLinesPosition;
  LSelArea: TBCEditorLinesArea;
  LText: string;
  LUndoItem: TUndoItem;
begin
  if (CanModify and (List.Count > 0)) then
  begin
    TerminateJob();
    DeactivateSyncEdit();

    if (List = UndoList) then
    begin
      Include(FState, lsUndo);
      LDestinationList := RedoList;

      LUndoItem := List.Peek();
      Progress := RightStr(Progress + '-U' + LUndoItem.BlockNumber.ToString(), 50);
    end
    else
    begin
      Include(FState, lsRedo);
      LDestinationList := UndoList;
      Progress := RightStr(Progress + '-R' + LUndoItem.BlockNumber.ToString(), 50);
    end;

    BeginUpdate();
    try
      LCaretPosition := CaretPosition;
      LSelArea := FSelArea;

      repeat
        LUndoItem := List.Pop();

        case (LUndoItem.UndoType) of
          utSelection:
            begin
              LDestinationList.Push(LUndoItem.UndoType, LCaretPosition, LSelArea,
                LUndoItem.Area, LUndoItem.Text, LUndoItem.BlockNumber);
            end;
          utInsert,
          utReplace,
          utBackspace,
          utDelete:
            begin
              if (not LUndoItem.Area.IsEmpty()
               and ((LUndoItem.UndoType in [utReplace])
                 or ((LUndoItem.UndoType in [utBackspace, utDelete]) xor (List = UndoList)))) then
              begin
                LText := TextIn[LUndoItem.Area];
                DoDeleteText(LUndoItem.Area);
                if (not (LUndoItem.UndoType in [utReplace])) then
                  LDestinationList.Push(LUndoItem.UndoType, LCaretPosition, LSelArea,
                    LUndoItem.Area, LText, LUndoItem.BlockNumber);
              end
              else
                LText := '';
              if ((LUndoItem.UndoType in [utReplace])
                  or ((LUndoItem.UndoType in [utBackspace, utDelete]) xor (List <> UndoList))) then
              begin
                if (LUndoItem.Text = '') then
                  LEndPosition := LUndoItem.Area.BeginPosition
                else
                try
                  LEndPosition := DoInsertText(LUndoItem.Area.BeginPosition, LUndoItem.Text);
                except
                  on E: Exception do
                    E.RaiseOuterException(EAssertionFailed.Create(LUndoItem.ToString() + #13#10
                      + 'Progress: ' + Progress + #13#10#13#10
                      + E.ClassName + ':' + #13#10
                      + E.Message));
                end;
                LDestinationList.Push(LUndoItem.UndoType, LCaretPosition, LSelArea,
                  LinesArea(LUndoItem.Area.BeginPosition, LEndPosition), LText, LUndoItem.BlockNumber);
              end;
            end;
          utClear:
            if (List = RedoList) then
            begin
              LText := Text;
              InternalClear(False);
              LDestinationList.Push(LUndoItem.UndoType, LCaretPosition, LSelArea,
                LinesArea(BOFPosition, InvalidLinesPosition), LText, LUndoItem.BlockNumber);
            end
            else
            begin
              LEndPosition := DoInsertText(LUndoItem.Area.BeginPosition, LUndoItem.Text);
              LDestinationList.Push(LUndoItem.UndoType, LCaretPosition, LSelArea,
                LinesArea(LUndoItem.Area.BeginPosition, LEndPosition), '', LUndoItem.BlockNumber);
            end;
          utInsertIndent,
          utDeleteIndent:
            begin
              if ((LUndoItem.UndoType <> utInsertIndent) xor (List = UndoList)) then
                DoDeleteIndent(LUndoItem.Area.BeginPosition, LUndoItem.Area.EndPosition,
                  LUndoItem.Text)
              else
                DoInsertIndent(LUndoItem.Area, LUndoItem.Text);
              LDestinationList.Push(LUndoItem.UndoType, LCaretPosition, LSelArea,
                LUndoItem.Area, LUndoItem.Text, LUndoItem.BlockNumber);
            end;
          else raise ERangeError.Create('UndoType: ' + IntToStr(Ord(LUndoItem.UndoType)));
        end;

        LCaretPosition := LUndoItem.CaretPosition;
        LSelArea := LUndoItem.SelArea;

        LPreviousBlockNumber := LUndoItem.BlockNumber;
        if (List.Count > 0) then
          LUndoItem := List.Peek();
      until ((List.Count = 0)
        or (LUndoItem.BlockNumber <> LPreviousBlockNumber));

      CaretPosition := LCaretPosition;
      SetSelArea(LSelArea);
    finally
      EndUpdate();
    end;

    if (List = UndoList) then
      Exclude(FState, lsUndo)
    else
      Exclude(FState, lsRedo);
  end;
end;

function TBCEditorLines.Get(ALine: Integer): string;
begin
  Assert((0 <= ALine) and (ALine < Count));

  Result := Items[ALine].Text;
end;

function TBCEditorLines.GetArea(): TBCEditorLinesArea;
begin
  Result := LinesArea(BOFPosition, EOFPosition);
end;

function TBCEditorLines.GetBOLPosition(ALine: Integer): TBCEditorLinesPosition;
begin
  Result := LinesPosition(0, ALine);
end;

function TBCEditorLines.GetCanModify(): Boolean;
begin
  Result := not (loReadOnly in FOptions)
    and not (lsBlockModify in FState);
end;

function TBCEditorLines.GetCanRedo(): Boolean;
begin
  Result := RedoList.Count > 0;
end;

function TBCEditorLines.GetCanUndo(): Boolean;
begin
  Result := UndoList.Count > 0;
end;

function TBCEditorLines.GetChar(APosition: TBCEditorLinesPosition): Char;
begin
  Assert((0 <= APosition.Line) and (APosition.Line < Items.Count));
  Assert((0 <= APosition.Char) and (APosition.Char < Length(Items.List[APosition.Line].Text)));

  Result := Items[APosition.Line].Text[1 + APosition.Char];
end;

function TBCEditorLines.GetCount(): Integer;
begin
  Result := Items.Count;
end;

function TBCEditorLines.GetEOFPosition(): TBCEditorLinesPosition;
begin
  if (Count = 0) then
    Result := BOFPosition
  else
    Result := EOLPosition[Count - 1];
end;

function TBCEditorLines.GetEOLPosition(ALine: Integer): TBCEditorLinesPosition;
begin
  Assert((0 <= ALine) and (ALine < Count));

  Result := LinesPosition(Length(Items[ALine].Text), ALine)
end;

function TBCEditorLines.GetLineArea(ALine: Integer): TBCEditorLinesArea;
begin
  Result := LinesArea(BOLPosition[ALine], EOLPosition[ALine]);
end;

function TBCEditorLines.GetSelArea(): TBCEditorLinesArea;
begin
  Result := LinesArea(
    Min(FSelArea.BeginPosition, FSelArea.EndPosition),
    Max(FSelArea.BeginPosition, FSelArea.EndPosition));
end;

function TBCEditorLines.GetSyncEdit(): Boolean;
begin
  Result := lsSyncEdit in FState;
end;

function TBCEditorLines.GetTextIn(const AArea: TBCEditorLinesArea): string;
var
  LEndChar: Integer;
  LEndLine: Integer;
  LLine: Integer;
  LStringBuilder: TStringBuilder;
begin
  Assert((BOFPosition <= AArea.BeginPosition) and (AArea.EndPosition <= EOFPosition));
  Assert(AArea.BeginPosition <= AArea.EndPosition);

  if (Count = 0) then
  begin
    Assert((AArea.BeginPosition = BOFPosition) and AArea.IsEmpty());
    Result := '';
  end
  else
  begin
    Assert(AArea.BeginPosition.Char <= Length(Items[AArea.BeginPosition.Line].Text));
    Assert(AArea.EndPosition.Char <= Length(Items[AArea.EndPosition.Line].Text));

    LEndLine := AArea.EndPosition.Line;
    if ((loTrimTrailingLines in Options) and (lsSaving in State)) then
      while ((LEndLine > 0)
        and (Trim(Items[LEndLine].Text) = '')
        and (Trim(Items[LEndLine - 1].Text) = '')) do
        Dec(LEndLine);

    if (AArea.IsEmpty()) then
      Result := ''
    else if (AArea.BeginPosition.Line = LEndLine) then
    begin
      if (LEndLine = AArea.EndPosition.Line) then
        LEndChar := AArea.EndPosition.Char
      else
        LEndChar := Length(Items[LEndLine].Text);
      if ((loTrimTrailingSpaces in Options) and (lsSaving in State)) then
        while ((LEndChar > 0) and (Items[LEndLine].Text[1 + LEndChar - 1] = BCEDITOR_SPACE_CHAR)) do
          Dec(LEndChar);
      Result := Copy(Items[AArea.BeginPosition.Line].Text, 1 + AArea.BeginPosition.Char, LEndChar - AArea.BeginPosition.Char)
    end
    else
    begin
      LStringBuilder := TStringBuilder.Create();

      LEndChar := Length(Items[AArea.BeginPosition.Line].Text);
      if ((loTrimTrailingSpaces in Options) and (lsSaving in State)) then
        while ((LEndChar > AArea.BeginPosition.Char) and (Items[AArea.BeginPosition.Line].Text[1 + LEndChar - 1] = BCEDITOR_SPACE_CHAR)) do
          Dec(LEndChar);

      LStringBuilder.Append(Items[AArea.BeginPosition.Line].Text, AArea.BeginPosition.Char, LEndChar - AArea.BeginPosition.Char);
      for LLine := AArea.BeginPosition.Line + 1 to LEndLine - 1 do
      begin
        LStringBuilder.Append(LineBreak);
        LEndChar := Length(Items[LLine].Text);
        if ((loTrimTrailingSpaces in Options) and (lsSaving in State)) then
          while ((LEndChar > 0) and (Items[LLine].Text[1 + LEndChar - 1] = BCEDITOR_SPACE_CHAR)) do
            Dec(LEndChar);

        LStringBuilder.Append(Items[LLine].Text, 0, LEndChar);
      end;
      if (LEndLine = AArea.EndPosition.Line) then
        LEndChar := AArea.EndPosition.Char
      else
        LEndChar := Length(Items[LEndLine].Text);
      if ((loTrimTrailingSpaces in Options) and (lsSaving in State) and (LEndChar = Length(Items[LEndLine].Text))) then
        while ((LEndChar > 0) and (Items[LEndLine].Text[1 + LEndChar - 1] = BCEDITOR_SPACE_CHAR)) do
          Dec(LEndChar);
      if ((LEndChar > 0)
        or not (loTrimTrailingSpaces in Options) or not (lsSaving in State) or (LEndChar <> Length(Items[LEndLine].Text))) then
      begin
        LStringBuilder.Append(LineBreak);
        LStringBuilder.Append(Items[LEndLine].Text, 0, LEndChar);
      end;

      Result := LStringBuilder.ToString();

      LStringBuilder.Free();
    end;
  end;
end;

function TBCEditorLines.GetTextLength(): Integer;
var
  LLine: Integer;
  LLineBreakLength: Integer;
begin
  Result := 0;
  LLineBreakLength := Length(LineBreak);
  for LLine := 0 to Count - 2 do
  begin
    Inc(Result, Length(Items[LLine].Text));
    Inc(Result, LLineBreakLength);
  end;
  if (Count > 0) then
    Inc(Result, Length(Items[Count - 1].Text))
end;

function TBCEditorLines.GetTextStr(): string;
begin
  Include(FState, lsSaving);
  try
    Result := TextIn[LinesArea(BOFPosition, EOFPosition)];
  finally
    Exclude(FState, lsSaving);
  end;
end;

procedure TBCEditorLines.Insert(ALine: Integer; const AText: string);
var
  LCaretPosition: TBCEditorLinesPosition;
  LIndex: Integer;
  LSelArea: TBCEditorLinesArea;
begin
  if (CanModify) then
  begin
    TerminateJob();
    DeactivateSyncEdit();

    LCaretPosition := CaretPosition;
    LSelArea := FSelArea;

    DoInsert(ALine, AText);

    for LIndex := FBookmarks.Count - 1 downto 0 do
      if (FBookmarks[LIndex].Pos.Y > ALine) then
        FBookmarks[LIndex].Pos := Point(FBookmarks[LIndex].Pos.X, FBookmarks[LIndex].Pos.Y + 1);
    for LIndex := FBookmarks.Count - 1 downto 0 do
      if (FMarks[LIndex].Pos.Y > ALine) then
        FMarks[LIndex].Pos := Point(FMarks[LIndex].Pos.X, FMarks[LIndex].Pos.Y + 1);

    if (not (lsLoading in State)) then
    begin
      UndoList.Push(utInsert, LCaretPosition, LSelArea,
        LinesArea(BOLPosition[ALine], LinesPosition(Length(AText), ALine)));

      RedoList.Clear();
    end;
  end;
end;

procedure TBCEditorLines.InsertIndent(ABeginPosition, AEndPosition: TBCEditorLinesPosition;
  const AIndentText: string);
var
  LArea: TBCEditorLinesArea;
  LCaretPosition: TBCEditorLinesPosition;
  LSelArea: TBCEditorLinesArea;
begin
  if (CanModify) then
  begin
    TerminateJob();
    DeactivateSyncEdit();

    LArea.BeginPosition := Min(ABeginPosition, AEndPosition);
    LArea.EndPosition := Max(ABeginPosition, AEndPosition);

    LCaretPosition := CaretPosition;
    LSelArea := FSelArea;

    DoInsertIndent(LArea, AIndentText);

    UndoList.Push(utInsertIndent, LCaretPosition, LSelArea,
      LArea, AIndentText);

    RedoList.Clear();
  end;
end;

procedure TBCEditorLines.InsertFile(const APosition: TBCEditorLinesPosition;
  const AFilename: string);
var
  LBuffer: TBytes;
  LEncoding: TEncoding;
  LSize: Integer;
  LStream: TFileStream;
begin
  if (CanModify and not SyncEdit) then
  begin
    TerminateJob();
    DeactivateSyncEdit();

    BeginUpdate();
    try
      LStream := TFileStream.Create(AFilename, fmShareDenyWrite);
      LSize := LStream.Size - LStream.Position;
      SetLength(LBuffer, LSize);
      LStream.Read(LBuffer, 0, LSize);
      LEncoding := nil;
      LSize := TEncoding.GetBufferEncoding(LBuffer, LEncoding, Encoding);
      if (not Assigned(LEncoding)) then
      begin
        LStream.Read(LBuffer, LSize);
        InsertText(APosition, StringOf(LBuffer));
      end
      else
      begin
        InsertText(APosition, LEncoding.GetString(LBuffer, LSize, Length(LBuffer) - LSize));
        LEncoding.Free();
      end;
      LStream.Free();
    finally
      EndUpdate();
    end;
  end;
end;

procedure TBCEditorLines.InsertText(const APosition: TBCEditorLinesPosition;
  const AText: string);
begin
  ReplaceText(LinesArea(APosition, InvalidLinesPosition), AText, utInsert);
end;

procedure TBCEditorLines.InternalClear(const AClearUndo: Boolean);
begin
  if (AClearUndo) then
    ClearUndo();

  Items.Clear();
  LineBreak := BCEDITOR_CARRIAGE_RETURN + BCEDITOR_LINEFEED;
  FCaretPosition := BOFPosition;
  FSelArea := LinesArea(BOFPosition, BOFPosition);
  if (Assigned(FOnCleared)) then
    FOnCleared(Self);
end;

function TBCEditorLines.IsWordBreakChar(const AChar: Char): Boolean;
begin
  Result := CharInSet(AChar,
    [BCEDITOR_NONE_CHAR .. BCEDITOR_SPACE_CHAR]
    + BCEDITOR_WORD_BREAK_CHARACTERS
    + BCEDITOR_EXTRA_WORD_BREAK_CHARACTERS);
end;

procedure TBCEditorLines.JobExecuted();
begin
  Assert(Assigned(FJobThread) and not FJobThread.Terminated);

  FState := FState - [lsSearching, lsScanningSyncEdit, lsBlockModify];

  BeginUpdate();
  try
    case (FJobThread.FJob) of
      tjActivateSyncEdit:
        if (FSyncEditItems.Count > 0) then
        begin
          SetCaretPosition(FSyncEditItems[0].Area.BeginPosition);
          SetSelArea(FSyncEditItems[0].Area);
          if (Assigned(FJobThread.FExecuted)) then
            FJobThread.FExecuted(nil);
        end;
      tjSearch:
        begin
          if (Assigned(FJobThread.FExecuted)) then
            FJobThread.FExecuted(@FSearchResult);
        end;
    end;
  finally
    EndUpdate();
  end;
end;

procedure TBCEditorLines.MarksChanged(ASender: TObject);
begin
  if (Assigned(FOnMarksChange)) then
    FOnMarksChange(ASender);
end;

function TBCEditorLines.PositionOf(const ACharIndex: Integer): TBCEditorLinesPosition;
begin
  Result := PositionOf(ACharIndex, BOFPosition);
end;

function TBCEditorLines.PositionOf(const ACharIndex: Integer;
  const ARelativePosition: TBCEditorLinesPosition): TBCEditorLinesPosition;
var
  LLength: Integer;
  LLineBreakLength: Integer;
begin
  Assert((BOFPosition <= ARelativePosition) and (ARelativePosition <= EOFPosition));

  if (Count = 0) then
  begin
    if (ACharIndex <> 0) then
      raise ERangeError.CreateFmt(SCharIndexOutOfBounds, [ACharIndex]);
    Result := BOFPosition;
  end
  else
  begin
    LLength := ACharIndex;

    Result := ARelativePosition;

    if ((0 <= Result.Char + LLength) and (Result.Char + LLength <= Length(Items[Result.Line].Text))) then
      Inc(Result.Char, LLength)
    else if (LLength < 0) then
    begin
      LLineBreakLength := Length(LineBreak);

      Inc(LLength, Result.Char + LLineBreakLength);
      Dec(Result.Line);

      if ((0 <= LLength) and (LLength < LLineBreakLength)) then
        LLength := 0
      else
        while ((Result.Line >= 0) and (LLength < LLineBreakLength)) do
        begin
          Inc(LLength, Length(Items[Result.Line].Text) + LLineBreakLength);
          Dec(Result.Line);
        end;

      if (Result.Line < 0) then
        Result := BOFPosition
      else
      begin
        if (- LLength > Length(Items[Result.Line].Text)) then
          raise ERangeError.CreateFmt(SCharIndexOutOfBounds, [ACharIndex]);

        Result.Char := LLength + Length(Items[Result.Line].Text);
      end;
    end
    else
    begin
      LLineBreakLength := Length(LineBreak);

      Dec(LLength, (Length(Items[Result.Line].Text) - Result.Char) + LLineBreakLength);
      Inc(Result.Line);

      if (LLength < 0) then
        LLength := 0;

      while ((Result.Line < Count) and (LLength >= Length(Items[Result.Line].Text) + LLineBreakLength)) do
      begin
        Dec(LLength, Length(Items[Result.Line].Text) + LLineBreakLength);
        Inc(Result.Line);
      end;

      Result.Char := LLength;

      Result := Min(Result, EOFPosition);
    end;
  end;
end;

procedure TBCEditorLines.Put(ALine: Integer; const AText: string);
begin
  Assert((0 <= ALine) and (ALine < Count));

  if (CanModify) then
  begin
    TerminateJob();
    DeactivateSyncEdit();

    ReplaceText(LinesArea(BOLPosition[ALine], EOLPosition[ALine]), AText);
  end;
end;

procedure TBCEditorLines.QuickSort(ALeft, ARight: Integer; ACompare: TCompare);
var
  LLeft: Integer;
  LMiddle: Integer;
  LRight: Integer;
begin
  repeat
    LLeft := ALeft;
    LRight := ARight;
    LMiddle := (ALeft + ARight) shr 1;
    repeat
      while ACompare(Self, LLeft, LMiddle) < 0 do
        Inc(LLeft);
      while ACompare(Self, LRight, LMiddle) > 0 do
        Dec(LRight);
      if LLeft <= LRight then
      begin
        if LLeft <> LRight then
          ExchangeItems(LLeft, LRight);
        if LMiddle = LLeft then
          LMiddle := LRight
        else
        if LMiddle = LRight then
          LMiddle := LLeft;
        Inc(LLeft);
        Dec(LRight);
      end;
    until LLeft > LRight;
    if ALeft < LRight then
      QuickSort(ALeft, LRight, ACompare);
    ALeft := LLeft;
  until LLeft >= ARight;
end;

procedure TBCEditorLines.Redo();
begin
  ExecuteUndoRedo(RedoList);
end;

procedure TBCEditorLines.ReplaceText();
begin
  ReplaceText(FReplaceText.Area, FReplaceText.Text, FReplaceText.UndoType, FReplaceText.ExecuteAllways);
end;

procedure TBCEditorLines.ReplaceText(const AArea: TBCEditorLinesArea;
  const AText: string);
begin
  ReplaceText(AArea, AText, False);
end;

procedure TBCEditorLines.ReplaceText(const AArea: TBCEditorLinesArea;
  const AText: string; const AExecuteAllways: Boolean);
begin
  if (not AArea.IsEmpty() and (AText <> '')) then
    ReplaceText(AArea, AText, utReplace, AExecuteAllways)
  else if (AText <> '') then
    ReplaceText(AArea, AText, utInsert, AExecuteAllways)
  else if (not AArea.IsEmpty()) then
    ReplaceText(AArea, AText, utDelete, AExecuteAllways);
end;

procedure TBCEditorLines.ReplaceText(const AArea: TBCEditorLinesArea; const AText: string;
  const AUndoType: TUndoItem.TType; const AExecuteAllways: Boolean = False);

  function Execute(const AArea: TBCEditorLinesArea; const AText: string;
    const AUndoType: TUndoItem.TType): TBCEditorLinesPosition;

    procedure UpdateMarkDelete(const AMark: TMark);
    begin
      if (AMark.Pos.Y = AArea.EndPosition.Line) then
        if (AArea.BeginPosition.Line = AArea.EndPosition.Line) then
          AMark.Pos :=
            Point(
              AMark.Pos.X - (AArea.EndPosition.Char - AArea.BeginPosition.Char),
              AMark.Pos.Y)
        else
          AMark.Pos :=
            Point(
              AMark.Pos.X - AArea.EndPosition.Char,
              AMark.Pos.Y - (AArea.EndPosition.Line - AArea.BeginPosition.Line))
      else if (AArea.EndPosition.Line > AArea.BeginPosition.Line) then
        AMark.Pos :=
          Point(
            AMark.Pos.X,
            AMark.Pos.Y - (AArea.EndPosition.Line - AArea.BeginPosition.Line));
    end;

    procedure UpdateMarkInsert(const AMark: TMark);
    begin
      if (AMark.Pos.Y = AArea.BeginPosition.Line) then
        if (AArea.BeginPosition.Line = Result.Line) then
          AMark.Pos :=
            LinesPosition(
              AMark.Pos.X + (Result.Char - AArea.BeginPosition.Char),
              AMark.Pos.Y)
        else
          FSyncEditArea.EndPosition :=
            LinesPosition(
              AMark.Pos.X + Result.Char,
              AMark.Pos.Y + Result.Line - AArea.BeginPosition.Line)
      else if (Result.Line > AArea.BeginPosition.Line) then
        AMark.Pos :=
          Point(
            AMark.Pos.X,
            AMark.Pos.Y + Result.Line - AArea.BeginPosition.Line);
    end;

  var
    LArea: TBCEditorLinesArea;
    LCaretPosition: TBCEditorLinesPosition;
    LSelArea: TBCEditorLinesArea;
    LText: string;
    LIndex: Integer;
  begin
    LCaretPosition := CaretPosition;
    LSelArea := FSelArea;

    if ((AUndoType = utInsert) or AArea.IsEmpty()) then
      LText := ''
    else
    begin
      LText := TextIn[AArea];
      DoDeleteText(AArea);
      for LIndex := 0 to FBookmarks.Count - 1 do
        UpdateMarkDelete(FBookmarks[LIndex]);
      for LIndex := 0 to FBookmarks.Count - 1 do
        UpdateMarkDelete(FMarks[LIndex]);
    end;

    if ((AUndoType in [utBackspace, utDelete]) or (AText = '')) then
      Result := AArea.BeginPosition
    else
    begin
      Result := DoInsertText(AArea.BeginPosition, AText);
      for LIndex := 0 to FBookmarks.Count - 1 do
        UpdateMarkInsert(FBookmarks[LIndex]);
      for LIndex := 0 to FBookmarks.Count - 1 do
        UpdateMarkInsert(FMarks[LIndex]);
    end;

    case (AUndoType) of
      utInsert: LArea := LinesArea(AArea.BeginPosition, InvalidLinesPosition);
      utReplace: LArea := LinesArea(AArea.BeginPosition, Result);
      utBackspace,
      utDelete: LArea := AArea;
      else raise ERangeError.Create('UndoType: ' + IntToStr(Ord(AUndoType)));
    end;

    UndoList.Push(utReplace, LCaretPosition, LSelArea,
      LArea, LText);
  end;

var
  LChar: Integer;
  LCurrentIndex: Integer;
  LEndPosition: TBCEditorLinesPosition;
  LIndex: Integer;
  LIndex2: Integer;
  LLength: Integer;
  LNewCaretPosition: TBCEditorLinesPosition;
begin
  if (TThread.CurrentThread.ThreadID <> MainThreadID) then
  begin
    FReplaceText.Area := AArea;
    FReplaceText.Text := AText;
    FReplaceText.UndoType := AUndoType;
    FReplaceText.ExecuteAllways := AExecuteAllways;
    TThread.Synchronize(nil, ReplaceText);
  end
  else
  begin
    Assert((BOFPosition <= AArea.BeginPosition) and (AArea.EndPosition <= EOFPosition));
    Assert((AArea.EndPosition = InvalidLinesPosition) or (AArea.BeginPosition <= AArea.EndPosition));

    if ((CanModify or AExecuteAllways)
      and (not AArea.IsEmpty() or (AText <> ''))) then
    begin
      if (not AExecuteAllways) then
        TerminateJob();

      BeginUpdate();
      try
        if (not SyncEdit) then
          LNewCaretPosition := Execute(AArea, AText, AUndoType)
        else
        begin
          if ((AUndoType = utBackspace) or (AArea.EndPosition = InvalidLinesPosition)) then
            LCurrentIndex := SyncEditItemIndexOf(AArea.BeginPosition)
          else
            LCurrentIndex := SyncEditItemIndexOf(AArea.EndPosition);
          if (not SyncEditItemIn(AArea)
            or (AUndoType = utBackspace) and (LCurrentIndex >= 0) and (AArea.EndPosition = FSyncEditItems[LCurrentIndex].Area.BeginPosition)
            or (AUndoType = utDelete) and (LCurrentIndex >= 0) and (AArea.BeginPosition = FSyncEditItems[LCurrentIndex].Area.EndPosition)) then
          begin
            LNewCaretPosition := Execute(AArea, AText, AUndoType);

            // Apply Delete in SyncEditItems
            for LIndex := 0 to FSyncEditItems.Count - 1 do
              if (FSyncEditItems[LIndex].Area.BeginPosition >= AArea.EndPosition) then
                if (FSyncEditItems[LIndex].Area.BeginPosition.Line = AArea.EndPosition.Line) then
                  if (AArea.BeginPosition.Line = AArea.EndPosition.Line) then
                  begin
                    FSyncEditItems.List[LIndex].Area.BeginPosition :=
                      LinesPosition(
                        FSyncEditItems[LIndex].Area.BeginPosition.Char - (AArea.EndPosition.Char - AArea.BeginPosition.Char),
                        SyncEditItems.List[LIndex].Area.BeginPosition.Line);
                    FSyncEditItems.List[LIndex].Area.EndPosition :=
                      LinesPosition(
                        FSyncEditItems[LIndex].Area.EndPosition.Char - (AArea.EndPosition.Char - AArea.BeginPosition.Char),
                        SyncEditItems[LIndex].Area.EndPosition.Line);
                  end
                  else
                  begin
                    FSyncEditItems.List[LIndex].Area.BeginPosition :=
                      LinesPosition(
                        FSyncEditItems[LIndex].Area.BeginPosition.Char - AArea.EndPosition.Char,
                        FSyncEditItems.List[LIndex].Area.BeginPosition.Line - (AArea.EndPosition.Line - AArea.BeginPosition.Line));
                    FSyncEditItems.List[LIndex].Area.EndPosition :=
                      LinesPosition(
                        FSyncEditItems[LIndex].Area.EndPosition.Char - AArea.EndPosition.Char,
                        FSyncEditItems[LIndex].Area.EndPosition.Line - (AArea.EndPosition.Line - AArea.BeginPosition.Line));
                  end
                else if (AArea.EndPosition.Line > AArea.BeginPosition.Line) then
                begin
                  FSyncEditItems.List[LIndex].Area.BeginPosition.Line :=
                    FSyncEditItems[LIndex].Area.BeginPosition.Line - (AArea.EndPosition.Line - AArea.BeginPosition.Line);
                  FSyncEditItems.List[LIndex].Area.EndPosition.Line :=
                    FSyncEditItems[LIndex].Area.EndPosition.Line - (AArea.EndPosition.Line - AArea.BeginPosition.Line);
                end;

            // Apply Delete in SyncEditArea.EndPosition
            if (FSyncEditArea.EndPosition.Line = AArea.EndPosition.Line) then
              if (AArea.BeginPosition.Line = AArea.EndPosition.Line) then
                FSyncEditArea.EndPosition :=
                  LinesPosition(
                    FSyncEditArea.EndPosition.Char - (AArea.EndPosition.Char - AArea.BeginPosition.Char),
                    FSyncEditArea.EndPosition.Line)
              else
                FSyncEditArea.EndPosition :=
                  LinesPosition(
                    FSyncEditArea.EndPosition.Char - AArea.EndPosition.Char,
                    FSyncEditArea.EndPosition.Line - (AArea.EndPosition.Line - AArea.BeginPosition.Line))
            else if (AArea.EndPosition.Line > AArea.BeginPosition.Line) then
              FSyncEditArea.EndPosition.Line :=
                FSyncEditArea.EndPosition.Line - (AArea.EndPosition.Line - AArea.BeginPosition.Line);

            // Apply Insert in SyncEditItems
            for LIndex := 0 to FSyncEditItems.Count - 1 do
              if (FSyncEditItems[LIndex].Area.BeginPosition > AArea.BeginPosition) then
                if (FSyncEditItems[LIndex].Area.BeginPosition.Line = AArea.BeginPosition.Line) then
                  if (AArea.BeginPosition.Line = LNewCaretPosition.Line) then
                  begin
                    FSyncEditItems.List[LIndex].Area.BeginPosition :=
                      LinesPosition(
                        FSyncEditItems[LIndex].Area.BeginPosition.Char + (LNewCaretPosition.Char - AArea.BeginPosition.Char),
                        FSyncEditItems.List[LIndex].Area.BeginPosition.Line);
                    FSyncEditItems.List[LIndex].Area.EndPosition :=
                      LinesPosition(
                        FSyncEditItems[LIndex].Area.EndPosition.Char + (LNewCaretPosition.Char - AArea.BeginPosition.Char),
                        FSyncEditItems[LIndex].Area.EndPosition.Line);
                  end
                  else
                  begin
                    FSyncEditItems.List[LIndex].Area.BeginPosition :=
                      LinesPosition(
                        FSyncEditItems[LIndex].Area.BeginPosition.Char + LNewCaretPosition.Char,
                        FSyncEditItems.List[LIndex].Area.BeginPosition.Line + LNewCaretPosition.Line - AArea.BeginPosition.Line);
                    FSyncEditItems.List[LIndex].Area.EndPosition :=
                      LinesPosition(
                        FSyncEditItems[LIndex].Area.EndPosition.Char + LNewCaretPosition.Char,
                        FSyncEditItems[LIndex].Area.EndPosition.Line + LNewCaretPosition.Line - AArea.BeginPosition.Line);
                  end
                else if (LNewCaretPosition.Line > AArea.BeginPosition.Line) then
                begin
                  FSyncEditItems.List[LIndex].Area.BeginPosition.Line :=
                    FSyncEditItems[LIndex].Area.BeginPosition.Line + AArea.EndPosition.Line - AArea.BeginPosition.Line;
                  FSyncEditItems.List[LIndex].Area.EndPosition.Line :=
                    FSyncEditItems[LIndex].Area.EndPosition.Line + AArea.EndPosition.Line - AArea.BeginPosition.Line;
                end;

            // Apply Insert in SyncEditArea.EndPosition
            if (FSyncEditArea.EndPosition.Line = AArea.EndPosition.Line) then
              if (AArea.BeginPosition.Line = LNewCaretPosition.Line) then
                FSyncEditArea.EndPosition :=
                  LinesPosition(
                    FSyncEditArea.EndPosition.Char + (LNewCaretPosition.Char - AArea.BeginPosition.Char),
                    FSyncEditArea.EndPosition.Line)
              else
                FSyncEditArea.EndPosition :=
                  LinesPosition(
                    FSyncEditArea.EndPosition.Char + LNewCaretPosition.Char,
                    FSyncEditArea.EndPosition.Line + LNewCaretPosition.Line - AArea.BeginPosition.Line)
            else if (LNewCaretPosition.Line > AArea.BeginPosition.Line) then
              FSyncEditArea.EndPosition.Line :=
                FSyncEditArea.EndPosition.Line + LNewCaretPosition.Line - AArea.BeginPosition.Line;
          end
          else
          begin
            if ((LCurrentIndex >= 0)
              and not ContainsWordBreakChar(AText)) then
            begin
              LChar := AArea.BeginPosition.Char - FSyncEditItems[LCurrentIndex].Area.BeginPosition.Char;
              LLength := Length(AText);
              if (not AArea.IsEmpty()) then
                Dec(LLength, AArea.EndPosition.Char - AArea.BeginPosition.Char);

              // Apply Delete + Insert in SyncEditItems
              for LIndex := 0 to FSyncEditItems.Count - 1 do
                if (FSyncEditItems[LIndex].Id = FSyncEditItems[LCurrentIndex].Id) then
                begin
                  LEndPosition := Execute(LinesArea(
                    LinesPosition(FSyncEditItems[LIndex].Area.BeginPosition.Char + LChar, FSyncEditItems[LIndex].Area.BeginPosition.Line),
                    LinesPosition(FSyncEditItems[LIndex].Area.BeginPosition.Char + LChar + AArea.EndPosition.Char - AArea.BeginPosition.Char, FSyncEditItems[LIndex].Area.BeginPosition.Line)),
                    AText, AUndoType);
                  if (LIndex = LCurrentIndex) then
                    LNewCaretPosition := LEndPosition;
                  Inc(FSyncEditItems.List[LIndex].Area.EndPosition.Char, LLength);
                  LIndex2 := LIndex + 1;
                  while ((LIndex2 < FSyncEditItems.Count)
                    and (FSyncEditItems[LIndex2].Area.BeginPosition.Line = FSyncEditItems[LIndex].Area.BeginPosition.Line)) do
                  begin
                    Inc(FSyncEditItems.List[LIndex2].Area.BeginPosition.Char, LLength);
                    Inc(FSyncEditItems.List[LIndex2].Area.EndPosition.Char, LLength);
                    Inc(LIndex2);
                  end;
                end;

              // Apply Delete + Insert in SyncEditArea.EndPosition
              if (AArea.EndPosition.Line = FSyncEditArea.EndPosition.Line) then
                Inc(FSyncEditArea.EndPosition.Char, LLength);
            end
            else if (AUndoType = utBackspace) then
              LNewCaretPosition := AArea.EndPosition
            else
              LNewCaretPosition := AArea.BeginPosition;
          end;
        end;

        CaretPosition := LNewCaretPosition;
      finally
        EndUpdate();
      end;
    end;
  end;
end;

procedure TBCEditorLines.SaveToStream(AStream: TStream; AEncoding: TEncoding);
begin
  inherited;

  if (not (loUndoAfterSave in Options)) then
  begin
    UndoList.Clear();
    RedoList.Clear();
  end;
end;

function TBCEditorLines.ScanMatchingPair(const AHighlighter: TBCEditorHighlighter;
  const ATerminated: TBCEditorTerminatedFunc;
  const APosition: TBCEditorLinesPosition;
  out AOpenArea, ACloseArea: TBCEditorLinesArea): Boolean;
var
  LBeginPosition: TBCEditorLinesPosition;
  LCloseArea: TBCEditorLinesArea;
  LDeep: Integer;
  LFoundLengthCloseToken: Integer;
  LFoundLengthOpenToken: Integer;
  LMatchingPair: Integer;
  LOpenArea: TBCEditorLinesArea;
  LPosition: TBCEditorLinesPosition;
  LSearchCloseToken: TBCEditorLines.TSearch;
  LSearchOpenToken: TBCEditorLines.TSearch;
begin
  Result := False;

  for LMatchingPair := 0 to AHighlighter.MatchingPairs.Count - 1 do
    if (not Result and not ATerminated()) then
    begin
      LSearchCloseToken := TBCEditorLines.TSearch.Create(Self,
        LinesArea(LinesPosition(Max(0, APosition.Char + 1 - Length(AHighlighter.MatchingPairs[LMatchingPair].CloseToken)), APosition.Line),
          LinesPosition(Min(Length(Items[APosition.Line].Text), APosition.Char + Length(AHighlighter.MatchingPairs[LMatchingPair].CloseToken) - 1), APosition.Line)),
        False, False, False, False, AHighlighter.MatchingPairs[LMatchingPair].CloseToken);
      LBeginPosition := LSearchCloseToken.Area.BeginPosition;
      if (LSearchCloseToken.Find(LBeginPosition, LFoundLengthCloseToken)) then
      begin
        LCloseArea.BeginPosition := LBeginPosition;
        LCloseArea.EndPosition := PositionOf(LFoundLengthCloseToken, LCloseArea.BeginPosition);

        LDeep := 0;

        LSearchOpenToken := TBCEditorLines.TSearch.Create(Self,
          LinesArea(BOFPosition,
            PositionOf(- 1, LCloseArea.BeginPosition)),
          False, False, False, True, AHighlighter.MatchingPairs[LMatchingPair].OpenToken);
        LOpenArea.BeginPosition := LSearchOpenToken.Area.EndPosition;

        LPosition := LSearchOpenToken.Area.EndPosition;
        while (Result and not ATerminated()
          and LSearchOpenToken.Find(LOpenArea.BeginPosition, LFoundLengthOpenToken)) do
        begin
          LSearchCloseToken.Free();
          LSearchCloseToken := TBCEditorLines.TSearch.Create(Self,
            LinesArea(LSearchOpenToken.Area.BeginPosition,
              LCloseArea.BeginPosition),
            False, False, False, True, AHighlighter.MatchingPairs[LMatchingPair].CloseToken);

          if (LSearchCloseToken.Find(LPosition, LFoundLengthCloseToken)
            and (LPosition > LOpenArea.BeginPosition)) then
          begin
            Inc(LDeep);
            LPosition := PositionOf(-1, LPosition);
            LOpenArea.BeginPosition := LPosition;
          end
          else if (LDeep > 0) then
          begin
            Dec(LDeep);
            LPosition := PositionOf(-1, LOpenArea.BeginPosition);
            LOpenArea.BeginPosition := LPosition;
          end
          else
          begin
            LOpenArea.EndPosition := PositionOf(LFoundLengthOpenToken, LOpenArea.BeginPosition);
            Result := True;
          end;
        end;
        LSearchOpenToken.Free();
      end;
      LSearchCloseToken.Free();
    end;

  for LMatchingPair := 0 to AHighlighter.MatchingPairs.Count - 1 do
    if (not Result and not ATerminated()) then
    begin
      LSearchOpenToken := TBCEditorLines.TSearch.Create(Self,
        LinesArea(LinesPosition(Max(0, APosition.Char + 1 - Length(AHighlighter.MatchingPairs[LMatchingPair].CloseToken)), APosition.Line),
          LinesPosition(Min(Length(Items[APosition.Line].Text), Min(Length(Items[APosition.Line].Text), APosition.Char + Length(AHighlighter.MatchingPairs[LMatchingPair].OpenToken) - 1)), APosition.Line)),
        False, False, False, False, AHighlighter.MatchingPairs[LMatchingPair].OpenToken);
      LBeginPosition := LSearchOpenToken.Area.BeginPosition;
      if (LSearchOpenToken.Find(LBeginPosition, LFoundLengthOpenToken)) then
      begin
        LOpenArea.BeginPosition := LBeginPosition;
        LOpenArea.EndPosition := PositionOf(1, LOpenArea.BeginPosition);

        LDeep := 0;

        LSearchCloseToken := TBCEditorLines.TSearch.Create(Self,
          LinesArea(LOpenArea.EndPosition,
            EOFPosition),
          False, False, False, False, AHighlighter.MatchingPairs[LMatchingPair].CloseToken);
        LCloseArea.BeginPosition := LSearchCloseToken.Area.BeginPosition;

        LPosition := LSearchCloseToken.Area.BeginPosition;
        while (not Result and not ATerminated()
          and LSearchCloseToken.Find(LCloseArea.BeginPosition, LFoundLengthCloseToken)) do
        begin
          LSearchOpenToken.Free();
          LSearchOpenToken := TBCEditorLines.TSearch.Create(Self,
            LinesArea(LSearchCloseToken.Area.BeginPosition,
              LCloseArea.BeginPosition),
            False, False, False, False, AHighlighter.MatchingPairs[LMatchingPair].OpenToken);

          if (LSearchOpenToken.Find(LPosition, LFoundLengthOpenToken)
            and (LPosition < LCloseArea.BeginPosition)) then
          begin
            Inc(LDeep);
            LPosition := PositionOf(1, LPosition);
            LCloseArea.BeginPosition := LPosition;
          end
          else if (LDeep > 0) then
          begin
            Dec(LDeep);
            LPosition := PositionOf(1, LCloseArea.BeginPosition);
            LCloseArea.BeginPosition := LPosition;
          end
          else
          begin
            LCloseArea.EndPosition := PositionOf(LFoundLengthCloseToken, LCloseArea.BeginPosition);
            Result := True;
          end;
        end;
        LSearchCloseToken.Free();
      end;
      LSearchOpenToken.Free()
    end;

  if (Result) then
  begin
    AOpenArea := LOpenArea;
    ACloseArea := LCloseArea;
  end;
end;

function TBCEditorLines.ScanSyncEdit(const ATerminated: TBCEditorTerminatedFunc;
  const ACheckAvailable: Boolean): Boolean;
type
  TWord = record
    Count: Integer;
    Id: Integer;
    Word: string;
  end;
var
  LWords: TList<TWord>;

  function InsertIndex(const AWord: string; out AIndex: Integer): Boolean;
  type
    Tstrcmp = function(lpString1, lpString2: PWideChar): Integer; stdcall;
  var
    Left: Integer;
    Mid: Integer;
    Right: Integer;
    strcmp: Tstrcmp;
  begin
    Assert(AWord <> '');

    Result := True;

    if (loSyncEditCaseSensitive in Options) then
      strcmp := lstrcmp
    else
      strcmp := lstrcmpi;

    if ((LWords.Count = 0) or (strcmp(PChar(LWords.Items[LWords.Count - 1].Word), PChar(AWord)) < 0)) then
      AIndex := LWords.Count
    else
    begin
      Left := 0;
      Right := LWords.Count - 1;
      while (Left <= Right) do
      begin
        Mid := (Right + Left) div 2;
        case (strcmp(PChar(LWords.Items[Mid].Word), PChar(AWord))) of
          -1: begin Left := Mid + 1;  AIndex := Mid + 1; end;
          0: begin Result := False; AIndex := Mid; break; end;
          1: begin Right := Mid - 1; AIndex := Mid; end;
        end;
      end;
    end;
  end;

var
  LArea: TBCEditorLinesArea;
  LIgnoreWord: Boolean;
  LIndex: Integer;
  LItem: TSyncEditItem;
  LKeyListIndex: Integer;
  LSearch: TWordSearch;
  LLength: Integer;
  LPosition: TBCEditorLinesPosition;
  LWord: TWord;
  LWordsIndex: Integer;
begin
  Result := False;

  if (not ACheckAvailable) then
    FSyncEditItems.Clear();

  LWords := TList<TWord>.Create();

  if (ACheckAvailable) then
    LArea := LinesArea(Min(FSelArea.BeginPosition, FSelArea.EndPosition),
      Max(FSelArea.BeginPosition, FSelArea.EndPosition))
  else
    LArea := LinesArea(Min(FSyncEditArea.BeginPosition, FSyncEditArea.EndPosition),
      Max(FSyncEditArea.BeginPosition, FSyncEditArea.EndPosition));
  LSearch := TWordSearch.Create(Self, LArea, True);
  LPosition := LSearch.Area.BeginPosition;
  while ((not Assigned(ATerminated) or not ATerminated())
    and (not ACheckAvailable or not Result)
    and LSearch.Area.Contains(LPosition)
    and LSearch.Find(LPosition, LLength)) do
  begin
    LItem.Area := LinesArea(LPosition, PositionOf(LLength, LPosition));
    LWord.Word := TextIn[LItem.Area];

    LIgnoreWord := False;
    for LKeyListIndex := 0 to FHighlighter.MainRules.KeyListCount - 1 do
      if (not Assigned(ATerminated) or not ATerminated()) then
        LIgnoreWord := LIgnoreWord
          or not FHighlighter.MainRules.KeyList[LKeyListIndex].SyncEdit
            and (FHighlighter.MainRules.KeyList[LKeyListIndex].KeyList.IndexOf(LWord.Word) >= 0);

    if (not LIgnoreWord) then
    begin
      if (InsertIndex(LWord.Word, LWordsIndex)) then
      begin
        LWord.Id := LWords.Count;
        LWord.Count := 1;
        LWords.Insert(LWordsIndex, LWord);
      end
      else
      begin
        Inc(LWords.List[LWordsIndex].Count);
        Result := True;
      end;
      LItem.Id := LWords[LWordsIndex].Id;
      FSyncEditItems.Add(LItem);
    end;

    LPosition := LItem.Area.EndPosition;
  end;
  LSearch.Free();

  if (Result and not ACheckAvailable) then
    for LWordsIndex := 0 to LWords.Count - 1 do
      if (LWords.Items[LWordsIndex].Count = 1) then
        for LIndex := 0 to FSyncEditItems.Count - 1 do
          if (FSyncEditItems.Items[LIndex].Id = LWords.Items[LWordsIndex].Id) then
          begin
            FSyncEditItems.Delete(LIndex);
            break;
          end;

  LWords.Free();

  if (ACheckAvailable) then
    FSyncEditItems.Clear();
end;

function TBCEditorLines.Search(const ATerminated: TBCEditorTerminatedFunc;
  const AJobThread: TJobThread): Boolean;
var
  LFoundArea: TBCEditorLinesArea;
  LFoundAreas: TList<TBCEditorLinesArea>;
  LFoundLength: Integer;
  LReplaceAction: TBCEditorReplaceAction;
begin
  if (FSearch.Job = sjFindAndReplace) then
    BeginUpdate();

  try
    FSearchResult.Area := InvalidLinesArea;
    FSearchResult.Backwards := FSearch.Backwards;
    FSearchResult.Count := 0;

    if (not FSearchAll) then
      LFoundAreas := nil
    else
      LFoundAreas := TList<TBCEditorLinesArea>.Create();

    if (not FSearchAll) then
      LReplaceAction := raReplace
    else
      LReplaceAction := raReplaceAll;

    repeat
      if (FSearch.Backwards) then
        FSearchPosition := PositionOf(-1, FSearchPosition);

      Result := FSearch.Find(FSearchPosition, LFoundLength);

      if (Result) then
      begin
        LFoundArea.BeginPosition := FSearchPosition;
        LFoundArea.EndPosition := PositionOf(LFoundLength, LFoundArea.BeginPosition);

        if (Assigned(LFoundAreas)) then
          if (FSearch.Backwards) then
            LFoundAreas.Insert(0, LFoundArea)
          else
            LFoundAreas.Insert(LFoundAreas.Count, LFoundArea);

        if (FSearch.Job = sjFind) then
        begin
          if (FSearchResult.Area = InvalidLinesArea) then
            FSearchResult.Area := LFoundArea;
          Inc(FSearchResult.Count);
        end
        else
        begin
          if (FSearch.Prompt
            and Assigned(FOnReplacePrompt) and (LReplaceAction <> raReplaceAll)) then
            FOnReplacePrompt(FSearch, LFoundArea, FSearch.ReplaceText, LReplaceAction);
          if (LReplaceAction in [raReplace, raReplaceAll]) then
          begin
            FSearch.Replace();
            if (FSearchResult.Area = InvalidLinesArea) then
              FSearchResult.Area := LFoundArea;
            Inc(FSearchResult.Count);
          end
        end;
      end;

      if (Result and not FSearch.Backwards) then
        FSearchPosition := PositionOf(1, FSearchPosition);
    until (ATerminated() or not Result or not FSearchAll or (LReplaceAction = raCancel));

    if (ATerminated()) then
      FSearchResult.ErrorMessage := SBCEditorTerminatedByUser
    else
      FSearchResult.ErrorMessage := FSearch.ErrorMessage;
    if (FSearchResult.ErrorMessage <> '') then
      FSearchResult.Area := InvalidLinesArea;

    if (Assigned(LFoundAreas)) then
    begin
      FFoundAreas.AddRange(LFoundAreas.ToArray);
      LFoundAreas.Free();
    end;
  finally
    if (FSearch.Job = sjFindAndReplace) then
      EndUpdate();
  end;

  FreeAndNil(FSearch);
end;

procedure TBCEditorLines.SetBackground(const ALine: Integer; const AValue: TColor);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Items.List[ALine].Background := AValue;
end;

procedure TBCEditorLines.SetBeginRange(const ALine: Integer; const AValue: Pointer);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Items.List[ALine].BeginRange := AValue;
end;

procedure TBCEditorLines.SetCaretPosition(const AValue: TBCEditorLinesPosition);
var
  LValue: TBCEditorLinesPosition;
begin
  Assert(BOFPosition <= AValue);

  LValue := AValue;
  if (not (loBeyondEndOfLine in FOptions)) then
    if (LValue.Line < Count) then
      LValue.Char := Min(LValue.Char, Length(Items[LValue.Line].Text))
    else
      LValue.Char := 0;
  if (not (loBeyondEndOfLine in FOptions)) then
    LValue.Line := Max(0, Min(LValue.Line, Count - 1));

  if (LValue <> FCaretPosition) then
  begin
    BeginUpdate();
    try
      FCaretPosition := LValue;

      SetSelArea(LinesArea(Min(AValue, EOFPosition), Min(AValue, EOFPosition)));

      Include(FState, lsCaretChanged);
    finally
      EndUpdate();
    end;
  end
  else
    SetSelArea(LinesArea(AValue, AValue));
end;

procedure TBCEditorLines.SetCodeFoldingBeginRange(const ALine: Integer; const AValue: Pointer);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Items.List[ALine].CodeFolding.BeginRange := AValue;
end;

procedure TBCEditorLines.SetCodeFoldingEndRange(const ALine: Integer; const AValue: Pointer);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Items.List[ALine].CodeFolding.EndRange := AValue;
end;

procedure TBCEditorLines.SetCodeFoldingTreeLine(const ALine: Integer; const AValue: Boolean);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Items.List[ALine].CodeFolding.TreeLine := AValue;
end;

procedure TBCEditorLines.SetForeground(const ALine: Integer; const AValue: TColor);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Items.List[ALine].Foreground := AValue;
end;

procedure TBCEditorLines.SetModified(const AValue: Boolean);
var
  LLine: Integer;
begin
  if ((AValue <> FModified) and (not AValue or CanModify)) then
  begin
    FModified := AValue;

    if (not FModified) then
    begin
      UndoList.GroupBreak();

      BeginUpdate();
      try
        for LLine := 0 to Count - 1 do
          if (Items[LLine].State = lsModified) then
            Items.List[LLine].State := lsSaved;
      finally
        EndUpdate();
      end;
    end;
  end;
end;

procedure TBCEditorLines.SetRow(const ALine: Integer; const AFirstRow, ARowCount: Integer);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Items.List[ALine].FirstRow := AFirstRow;
  Items.List[ALine].RowCount := ARowCount;
end;

procedure TBCEditorLines.SetSelArea(AValue: TBCEditorLinesArea);
var
  LValue: TBCEditorLinesArea;
begin
  Assert(BOFPosition <= AValue.BeginPosition);

  LValue := AValue;
  if (LValue.BeginPosition.Line < Count) then
    LValue.BeginPosition.Char := Min(LValue.BeginPosition.Char, Length(Items[LValue.BeginPosition.Line].Text))
  else
    LValue.BeginPosition := EOFPosition;
  if (LValue.EndPosition.Line < Count) then
    LValue.EndPosition.Char := Min(LValue.EndPosition.Char, Length(Items[LValue.EndPosition.Line].Text))
  else
    LValue.EndPosition := EOFPosition;

  if (LValue <> FSelArea) then
  begin
    BeginUpdate();
    try
      FSelArea := LValue;

      Include(FState, lsSelChanged);
    finally
      EndUpdate();
    end;
  end;
end;

procedure TBCEditorLines.SetTextStr(const AValue: string);
var
  LLine: Integer;
  LOldCaretPosition: TBCEditorLinesPosition;
  LOldSelArea: TBCEditorLinesArea;
begin
  if (CanModify) then
  begin
    TerminateJob();
    DeactivateSyncEdit();

    LOldCaretPosition := CaretPosition;
    LOldSelArea := FSelArea;

    Include(FState, lsLoading);

    BeginUpdate();
    try
      if (loUndoAfterLoad in Options) then
        DeleteText(LinesArea(BOFPosition, EOFPosition));

      InternalClear(not (loUndoAfterLoad in Options));

      InsertText(BOFPosition, AValue);

      if (loUndoAfterLoad in Options) then
        UndoList.Push(utInsert, BOFPosition, InvalidLinesArea,
          LinesArea(BOFPosition, FCaretPosition));

      RedoList.Clear();

      for LLine := 0 to Count - 1 do
        Items.List[LLine].State := lsLoaded;

      CaretPosition := BOFPosition;
    finally
      EndUpdate();
    end;

    Exclude(FState, lsLoading);
    FModified := False;
  end;
end;

procedure TBCEditorLines.SetUpdateState(AUpdating: Boolean);
begin
  if (AUpdating) then
  begin
    if (Assigned(FOnBeforeUpdate)) then
      FOnBeforeUpdate(Self);

    UndoList.BeginUpdate();

    FState := FState - [lsCaretChanged, lsSelChanged];
    FOldUndoListCount := UndoList.Count;
    FOldCaretPosition := CaretPosition;
    FOldSelArea := FSelArea;
  end
  else
  begin
    if (not (lsRedo in State) and ((lsCaretChanged in State) or (lsSelChanged in State)) and not UndoList.Updated) then
    begin
      if (not (lsUndo in State)) then
      begin
        if ((UndoList.Count = FOldUndoListCount)
          and (CaretPosition <> FOldCaretPosition)
            or (FSelArea <> FOldSelArea)) then
          UndoList.Push(utSelection, FOldCaretPosition, FOldSelArea,
            InvalidLinesArea);
        RedoList.Clear();
      end;
    end;

    UndoList.EndUpdate();

    if (Assigned(FOnCaretChanged) and (lsCaretChanged in FState)) then
      FOnCaretChanged(Self);
    if (Assigned(FOnSelChange) and (lsSelChanged in FState)) then
      FOnSelChange(Self);
    if (Assigned(FOnAfterUpdate)) then
      FOnAfterUpdate(Self);
    if (Assigned(FOnLoaded) and (lsLoading in FState)) then
      FOnLoaded(Self);

    FState := FState - [lsCaretChanged, lsSelChanged];
  end;
end;

procedure TBCEditorLines.Sort(const ABeginLine, AEndLine: Integer; ACompare: TCompare = nil);
var
  LArea: TBCEditorLinesArea;
  LCompare: TCompare;
  LText: string;
begin
  if (CanModify) then
  begin
    TerminateJob();
    DeactivateSyncEdit();

    BeginUpdate();
    try
      if (AEndLine < Count - 1) then
        LArea := LinesArea(BOLPosition[ABeginLine], BOLPosition[ABeginLine + 1])
      else
        LArea := LinesArea(BOLPosition[ABeginLine], LinesPosition(Length(Items[AEndLine].Text), AEndLine));

      LText := TextIn[LArea];
      UndoList.Push(utDelete, CaretPosition, FSelArea,
        LinesArea(LArea.BeginPosition, InvalidLinesPosition), LText);

      if (not Assigned(ACompare)) then
        LCompare := CompareLines
      else
        LCompare := ACompare;
      QuickSort(ABeginLine, AEndLine, LCompare);

      UndoList.Push(utInsert, InvalidLinesPosition, InvalidLinesArea,
        LArea);
    finally
      EndUpdate();
    end;

    RedoList.Clear();
  end;
end;

procedure TBCEditorLines.StartJob(const AJob: TJobThread.TJob;
  const ABlockModify: Boolean;
  const AExecuted: TJobThread.TExecutedProc);
begin
  case (AJob) of
    tjActivateSyncEdit: Include(FState, lsScanningSyncEdit);
    tjSearch: Include(FState, lsSearching);
    else raise ERangeError.Create('AJob: ' + IntToStr(Ord(AJob)));
  end;

  TerminateJob();

  if (not Assigned(FJobThread)) then
    FJobThread := TJobThread.Create(Self);

  if (ABlockModify) then
    Include(FState, lsBlockModify);

  FJobThread.Run(AJob, AExecuted);
end;

procedure TBCEditorLines.StartSearch(const ASearch: TSearch;
  const APosition: TBCEditorLinesPosition; const ASearchAll, ABlockModify: Boolean;
  const AExecuted: TJobThread.TExecutedProc);
begin
  TerminateJob();
  FFoundAreas.Clear();

  FSearch := ASearch;
  FSearchAll := ASearchAll;
  FSearchPosition := APosition;

  StartJob(tjSearch, ABlockModify, AExecuted);
end;

function TBCEditorLines.SyncEditItemIn(const AArea: TBCEditorLinesArea): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  LIndex := 0;
  while (not Result
    and (LIndex < FSyncEditItems.Count)
    and ((LIndex = 0) or (FSyncEditItems[LIndex - 1].Area.BeginPosition < AArea.BeginPosition))) do
  begin
    if (AArea.EndPosition = InvalidLinesPosition) then
      Result := FSyncEditItems[LIndex].Area.Contains(AArea.BeginPosition)
        or (FSyncEditItems[LIndex].Area.EndPosition = AArea.BeginPosition)
    else
      Result := FSyncEditItems[LIndex].Area.IntersectWith(AArea);
    Inc(LIndex);
  end;
end;

function TBCEditorLines.SyncEditAvailable(const AHighlighter: TBCEditorHighlighter;
  const ATerminated: TBCEditorTerminatedFunc): Boolean;
begin
  Assert(not (lsSyncEdit in FState));

  FHighlighter := AHighlighter;
  Result := ScanSyncEdit(ATerminated, True);
end;

function TBCEditorLines.SyncEditItemIndexOf(const APosition: TBCEditorLinesPosition): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  LIndex := 0;
  while ((Result < 0) and (LIndex < FSyncEditItems.Count)) do
  begin
    if (FSyncEditItems[LIndex].Area.Contains(APosition)
      or (FSyncEditItems[LIndex].Area.EndPosition = APosition)) then
      Result := LIndex;
    Inc(LIndex);
  end;
end;

procedure TBCEditorLines.TerminateJob();
begin
  if (Assigned(FJobThread) and FJobThread.IsRunning()) then
  begin
    FJobThread.Terminate();
    FJobThread.WaitFor();
    FJobThread.Free();
    FJobThread := nil;

    FState := FState - [lsSearching, lsScanningSyncEdit, lsBlockModify];
  end;
end;

procedure TBCEditorLines.Undo();
begin
  ExecuteUndoRedo(UndoList);
end;

procedure TBCEditorLines.UndoGroupBreak();
begin
  if ((loUndoGrouped in Options) and CanUndo) then
    UndoList.GroupBreak();
end;

function TBCEditorLines.ValidPosition(const APosition: TBCEditorLinesPosition): Boolean;
begin
  Result := (0 <= APosition.Line) and (APosition.Line < Count)
    and (0 <= APosition.Char) and (APosition.Char < Length(Items[APosition.Line].Text));
end;

end.
