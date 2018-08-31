unit BCEditor.Commands;

interface {********************************************************************}

uses
  ActiveX,
  Classes, SysUtils, Types, Generics.Collections,
  Controls, Menus,
  BCEditor.Types;

type
  TBCEditorCommandCategory = Integer;
  TBCEditorCommand = Integer;

type
  { Command Data }

  PBCEditorCommandData = ^TBCEditorCommandData;
  TBCEditorCommandData = TBytes;

  PBCEditorCommandDataChar = ^TBCEditorCommandDataChar;
  TBCEditorCommandDataChar = packed record
    Char: Char;
    class function Create(const AChar: Char): TBCEditorCommandData; static;
  end;

  PBCEditorCommandDataDropOLE = ^TBCEditorCommandDataDropOLE;
  TBCEditorCommandDataDropOLE = packed record
    Pos: TPoint;
    dataObj: IDataObject;
    class function Create(const APos: TPoint; const AdataObj: IDataObject): TBCEditorCommandData; static;
  end;

  PBCEditorCommandDataFind = ^TBCEditorCommandDataFind;
  TBCEditorCommandDataFind = packed record
    Options: TBCEditorFindOptions;
  private
    FPatternLength: Int64;
    function GetPattern(): string; {$IFNDEF Debug} inline; {$ENDIF}
  public
    class function Create(const APattern: string;
      const AOptions: TBCEditorFindOptions): TBCEditorCommandData; static;
    class operator Equal(const a, b: TBCEditorCommandDataFind): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    class operator Implicit(const a: TBCEditorCommandDataFind): TBCEditorCommandData; {$IFNDEF Debug} inline; {$ENDIF}
    property Pattern: string read GetPattern;
  end;

  PBCEditorCommandDataMoveCaret = ^TBCEditorCommandDataMoveCaret;
  TBCEditorCommandDataMoveCaret = packed record
    X: Integer;
    Y: Integer;
    Selection: Boolean;
    class function Create(const AX, AY: Integer; const ASelection: Boolean = False): TBCEditorCommandData; static;
  end;

  PBCEditorCommandDataPosition = ^TBCEditorCommandDataPosition;
  TBCEditorCommandDataPosition = packed record
    Pos: TPoint;
    Selection: Boolean;
    class function Create(const APosition: TBCEditorLinesPosition;
      const ASelection: Boolean = False): TBCEditorCommandData; static;
  end;

  PBCEditorCommandDataReplace = ^TBCEditorCommandDataReplace;
  TBCEditorCommandDataReplace = packed record
  private
    FPatternLength: Int64;
    FReplaceTextLength: Int64;
    function GetPattern(): string; {$IFNDEF Debug} inline; {$ENDIF}
    function GetReplaceText(): string; {$IFNDEF Debug} inline; {$ENDIF}
  public
    Options: TBCEditorReplaceOptions;
    class function Create(const APattern, AReplaceText: string;
      const AOptions: TBCEditorReplaceOptions): TBCEditorCommandData; static;
    class operator Implicit(const a: TBCEditorCommandDataReplace): TBCEditorCommandData; {$IFNDEF Debug} inline; {$ENDIF}
    property Pattern: string read GetPattern;
    property ReplaceText: string read GetReplaceText;
  end;

  PBCEditorCommandDataScrollTo = ^TBCEditorCommandDataScrollTo;
  TBCEditorCommandDataScrollTo = packed record
    Pos: TPoint;
    class function Create(const APos: TPoint): TBCEditorCommandData; static;
  end;

  PBCEditorCommandDataSelection = ^TBCEditorCommandDataSelection;
  TBCEditorCommandDataSelection = packed record
    BeginPos: TPoint;
    EndPos: TPoint;
    CaretToBeginPosition: Boolean;
    class function Create(const ASelArea: TBCEditorLinesArea;
      const ACaretToBeginPosition: Boolean = False): TBCEditorCommandData; static;
  end;

  PBCEditorCommandDataText = ^TBCEditorCommandDataText;
  TBCEditorCommandDataText = packed record
    Delete: Boolean;
    Selection: Boolean;
  private
    FTextLength: Int64;
    function GetText(): string; {$IFNDEF Debug} inline; {$ENDIF}
  public
    class function Create(const AText: string; const ADelete: Boolean = False;
      const ASelection: Boolean = False): TBCEditorCommandData; static;
    property Text: string read GetText;
  end;

  TBCEditorHookedCommandProc = procedure(const AEditor: Pointer; const ABefore: LongBool;
    const ACommand: Integer; const AData: Pointer; const ADataSize: Int64;
    var AHandled: LongBool; const AHandlerData: Pointer); stdcall;
  TBCEditorHookedCommandObjectProc = procedure(ASender: TObject; const ABefore: Boolean;
    const ACommand: TBCEditorCommand; const AData: TBCEditorCommandData; var AHandled: Boolean) of object;
  TBCEditorAfterProcessCommandEvent = procedure(ASender: TObject;
    const ACommand: TBCEditorCommand; const AData: TBCEditorCommandData; const AHandled: Boolean) of object;
  TBCEditorBeforeProcessCommandEvent = procedure(ASender: TObject;
    const ACommand: TBCEditorCommand; const AData: TBCEditorCommandData; var AHandled: Boolean) of object;

  TBCEditorHookedCommandHandler = record
    HandlerData: Pointer;
    ObjectProc: TBCEditorHookedCommandObjectProc;
    Proc: TBCEditorHookedCommandProc;
    class operator Equal(a, b: TBCEditorHookedCommandHandler): Boolean; inline;
  end;

  TBCEditorHookedCommandHandlers = class(TList<TBCEditorHookedCommandHandler>)
  private
    FEditor: TCustomControl;
  public
    procedure Broadcast(const ABefore: Boolean; const ACommand: TBCEditorCommand;
      const AData: TBCEditorCommandData; var AHandled: Boolean);
    constructor Create(const AEditor: TCustomControl);
  end;

  TBCEditorCommandManager = class
  type

    TItem = class(TPersistent)
    private
      FShortCuts: TList<TShortCut>;
    public
      EnabledWhileRecording: Boolean;
      Command: TBCEditorCommand;
      CommandCategory: TBCEditorCommandCategory;
      Ident: string;
      Recordable: Boolean;
      procedure Assign(ASource: TPersistent); override;
      constructor Create();
      destructor Destroy(); override;
      property ShortCuts: TList<TShortCut> read FShortCuts;
    end;

  private
    FItems: TObjectList<TItem>;
    function EnabledWhileRecording(const ACommand: TBCEditorCommand): Boolean;
    function GetCount(): Integer;
    function GetItem(AIndex: Integer): TItem;
    function IndexOf(const ACommand: TBCEditorCommand): Integer; overload;
    function IndexOf(const AShortCut: TShortCut): Integer; overload;
    function InsertIndex(const ACommand: TBCEditorCommand; out AIndex: Integer): Boolean;
    function Recordable(const ACommand: TBCEditorCommand): Boolean;
  public
    procedure AddShortCut(const ACommand: TBCEditorCommand; const AShortCut: TShortCut);
    procedure Clear();
    function CommandCategoryOf(const ACommand: TBCEditorCommand): TBCEditorCommandCategory;
    function CommandToIdent(const ACommand: TBCEditorCommand): string;
    constructor Create();
    destructor Destroy(); override;
    procedure RegisterCommand(const ACommand: TBCEditorCommand;
      const ACommandCategory: TBCEditorCommandCategory; const AIdent: string;
      const AShortCut: TShortCut = 0;
      const AEnabledWhileRecording: Boolean = True; const ARecordable: Boolean = True);
    procedure RemoveShortCut(const AShortCut: TShortCut);
    procedure Reset();
    function TryCommandToShortCut(const ACommand: TBCEditorCommand; out AShortCut: TShortCut): Boolean;
    function TryShortCutToCommand(const AShortCut: TShortCut; out ACommand: TBCEditorCommand): Boolean;
    procedure UnregisterCommand(const ACommand: TBCEditorCommand);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TItem read GetItem; default;
  end;

  TCustomBCEditorMacroRecorder = class(TComponent)
  private type
    TAfterPlaybackStepEvent = procedure(ASender: TObject; const ACommand: TBCEditorCommand; const AData: TBCEditorCommandData) of object;
    TBeforePlaybackStepEvent = procedure(ASender: TObject; const ACommand: TBCEditorCommand; const AData: TBCEditorCommandData; var AAllow: Boolean) of object;

    TItem = record
      Command: TBCEditorCommand;
      DataPosition: Int64;
      DataSize: Int64;
    end;

  type
    TCommand = record
      Command: TBCEditorCommand;
      Data: TBCEditorCommandData;
    end;
    TState = (msStopped, msStepped, msRecording, msPlaying);

  strict private
    FAfterPlayback: TNotifyEvent;
    FAfterRecord: TNotifyEvent;
    FAfterPlaybackStep: TAfterPlaybackStepEvent;
    FBeforePlayback: TNotifyEvent;
    FBeforeRecord: TNotifyEvent;
    FBeforePlaybackStep: TBeforePlaybackStepEvent;
    FCurrentCommand: Integer;
    FData: TBytesStream;
    FEditor: TCustomControl;
    FItems: TList<TItem>;
    FOnStateChange: TNotifyEvent;
    FState: TState;
    procedure EditorCommand(ASender: TObject; const ABefore: Boolean;
      const ACommand: TBCEditorCommand; const AData: TBCEditorCommandData; var AHandled: Boolean);
    function GetCommand(AIndex: Integer): TCommand;
    function GetCommandCount: Integer;
    function GetData(const ACommand: Integer): TBCEditorCommandData;
    function GetIsEmpty(): Boolean;
    procedure SetEditor(AValue: TCustomControl);
    procedure SetState(AValue: TState);
    function PlaybackStep(const APlay: Boolean): Boolean; overload;
  protected
    procedure Notification(AComponent: TComponent; aOperation: TOperation); override;
    property AfterPlayback: TNotifyEvent read FAfterPlayback write FAfterPlayback;
    property AfterPlaybackStep: TAfterPlaybackStepEvent read FAfterPlaybackStep write FAfterPlaybackStep;
    property AfterRecording: TNotifyEvent read FAfterRecord write FAfterRecord;
    property BeforePlayback: TNotifyEvent read FBeforePlayback write FBeforePlayback;
    property BeforePlaybackStep: TBeforePlaybackStepEvent read FBeforePlaybackStep write FBeforePlaybackStep;
    property BeforeRecording: TNotifyEvent read FBeforeRecord write FBeforeRecord;
    property Editor: TCustomControl read FEditor write SetEditor;
    property CommandCount: Integer read GetCommandCount;
    property Commands[AIndex: Integer]: TCommand read GetCommand;
    property IsEmpty: Boolean read GetIsEmpty;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property State: TState read FState;
  public
    procedure AddCommand(const ACommand: TBCEditorCommand; const AData: TBCEditorCommandData);
    procedure Clear();
    constructor Create(AOwner: TComponent); override;
    procedure DeleteCommand(const AIndex: Integer);
    destructor Destroy(); override;
    procedure InsertCommand(const AIndex: Integer; const ACommand: TBCEditorCommand;
      const AData: TBCEditorCommandData);
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromStream(AStream: TStream; AClear: Boolean = True);
    procedure SaveToFile(const AFilename: string);
    procedure SaveToStream(AStream: TStream);
    procedure Playback();
    procedure StartRecord();
    function PlaybackStep(): Boolean; overload;
    procedure Stop();
  end;

  TBCEditorMacroRecorder = class(TCustomBCEditorMacroRecorder)
  public
    property CommandCount;
    property Commands;
    property IsEmpty;
    property State;
  published
    property AfterPlayback;
    property AfterPlaybackStep;
    property AfterRecording;
    property BeforePlayback;
    property BeforePlaybackStep;
    property BeforeRecording;
    property Editor;
    property OnStateChange;
  end;

var
  BCEditorCommandManager: TBCEditorCommandManager;

implementation {***************************************************************}

uses
  Windows,
  Math,
  BCEditor, BCEditor.Consts;

const
  SCannotRecord = 'Cannot record macro: Already recording or playing';
  SCannotPlay = 'Cannot play macro: Already recording or playing';
  SNotTCustomBCEditor = 'Value must be a TCustomBCEditor class object';

{ TBCEditorCommandDataChar ****************************************************}

class function TBCEditorCommandDataChar.Create(const AChar: Char): TBCEditorCommandData;
var
  LData: TBCEditorCommandDataChar;
begin
  LData.Char := AChar;
  Result := BytesOf(@LData, SizeOf(LData));
end;

{ TBCEditorCommandDataDropOLE *************************************************}

class function TBCEditorCommandDataDropOLE.Create(const APos: TPoint; const AdataObj: IDataObject): TBCEditorCommandData;
var
  LData: TBCEditorCommandDataDropOLE;
begin
  LData.Pos := APos;
  LData.dataObj := AdataObj;
  Result := BytesOf(@LData, SizeOf(LData));
end;

{ TBCEditorCommandDataFind ****************************************************}

class function TBCEditorCommandDataFind.Create(const APattern: string;
  const AOptions: TBCEditorFindOptions): TBCEditorCommandData;
var
  LData: ^TBCEditorCommandDataFind;
  LSize: Int64;
begin
  LSize := SizeOf(TBCEditorCommandDataFind) + Length(APattern) * SizeOf(Char);
  SetLength(Result, LSize);
  LData := @Result[0];
  LData^.Options := AOptions;
  LData^.FPatternLength := Length(APattern);
  MoveMemory(@PAnsiChar(Result)[SizeOf(TBCEditorCommandDataFind)], PChar(APattern), Length(APattern) * SizeOf(Char));
end;

function TBCEditorCommandDataFind.GetPattern(): string;
begin
  SetString(Result, PChar(@PAnsiChar(Pointer(@Self))[SizeOf(TBCEditorCommandDataFind)]), FPatternLength);
end;

class operator TBCEditorCommandDataFind.Equal(const a, b: TBCEditorCommandDataFind): Boolean;
begin
  Result := (a.Options = b.Options) and (a.FPatternLength = b.FPatternLength) and (a.Pattern = b.Pattern);
end;

class operator TBCEditorCommandDataFind.Implicit(const a: TBCEditorCommandDataFind): TBCEditorCommandData;
begin
  Result := BytesOf(@a, SizeOf(TBCEditorCommandDataFind) + a.FPatternLength * SizeOf(Char));
end;

{ TBCEditorCommandDataMoveCaret ***********************************************}

class function TBCEditorCommandDataMoveCaret.Create(const AX, AY: Integer;
  const ASelection: Boolean = False): TBCEditorCommandData;
var
  LData: TBCEditorCommandDataMoveCaret;
begin
  LData.X := AX;
  LData.Y := AY;
  LData.Selection := ASelection;
  Result := BytesOf(@LData, SizeOf(LData));
end;

{ TBCEditorCommandDataPosition ************************************************}

class function TBCEditorCommandDataPosition.Create(const APosition: TBCEditorLinesPosition;
  const ASelection: Boolean = False): TBCEditorCommandData;
var
  LData: TBCEditorCommandDataPosition;
begin
  LData.Pos := APosition;
  LData.Selection := ASelection;
  Result := BytesOf(@LData, SizeOf(LData));
end;

{ TBCEditorCommandDataReplace *************************************************}

class function TBCEditorCommandDataReplace.Create(const APattern, AReplaceText: string;
  const AOptions: TBCEditorReplaceOptions): TBCEditorCommandData;
var
  LData: ^TBCEditorCommandDataReplace;
  LSize: Int64;
begin
  LSize := SizeOf(TBCEditorCommandDataReplace) + Length(APattern) * SizeOf(Char) + Length(AReplaceText) * SizeOf(Char);
  SetLength(Result, LSize);
  LData := @Result[0];
  LData^.Options := AOptions;
  LData^.FPatternLength := Length(APattern);
  LData^.FReplaceTextLength := Length(AReplaceText);
  MoveMemory(@PAnsiChar(LData)[SizeOf(TBCEditorCommandDataReplace)], PChar(APattern), Length(APattern) * SizeOf(Char));
  MoveMemory(@PAnsiChar(LData)[SizeOf(TBCEditorCommandDataReplace) + Length(APattern) * SizeOf(Char)], PChar(AReplaceText), Length(AReplaceText) * SizeOf(Char));
end;

function TBCEditorCommandDataReplace.GetPattern(): string;
begin
  SetString(Result, PChar(@PAnsiChar(Pointer(@Self))[SizeOf(TBCEditorCommandDataReplace)]), FPatternLength);
end;

function TBCEditorCommandDataReplace.GetReplaceText(): string;
begin
  SetString(Result, PChar(@PAnsiChar(Pointer(@Self))[SizeOf(TBCEditorCommandDataReplace) + FPatternLength * SizeOf(Char)]), FReplaceTextLength);
end;

class operator TBCEditorCommandDataReplace.Implicit(const a: TBCEditorCommandDataReplace): TBCEditorCommandData;
begin
  Result := BytesOf(@a, SizeOf(TBCEditorCommandDataReplace) + a.FPatternLength * SizeOf(Char) + a.FReplaceTextLength * SizeOf(Char));
end;

{ TBCEditorCommandDataScrollTo ************************************************}

class function TBCEditorCommandDataScrollTo.Create(const APos: TPoint): TBCEditorCommandData;
var
  LData: TBCEditorCommandDataScrollTo;
begin
  LData.Pos := APos;
  Result := BytesOf(@LData, SizeOf(LData));
end;

{ TBCEditorCommandDataSelection ***********************************************}

class function TBCEditorCommandDataSelection.Create(const ASelArea: TBCEditorLinesArea;
  const ACaretToBeginPosition: Boolean = False): TBCEditorCommandData;
var
  LData: TBCEditorCommandDataSelection;
begin
  LData.BeginPos := ASelArea.BeginPosition;
  LData.EndPos := ASelArea.EndPosition;
  LData.CaretToBeginPosition := ACaretToBeginPosition;
  Result := BytesOf(@LData, SizeOf(LData));
end;

{ TBCEditorCommandDataText ****************************************************}

class function TBCEditorCommandDataText.Create(const AText: string; const ADelete: Boolean = False;
  const ASelection: Boolean = False): TBCEditorCommandData;
var
  LData: ^TBCEditorCommandDataText;
  LSize: Int64;
begin
  LSize := SizeOf(TBCEditorCommandDataText) + Length(AText) * SizeOf(Char);
  SetLength(Result, LSize);
  LData := @Result[0];
  LData^.Delete := ADelete;
  LData^.Selection := ASelection;
  LData^.FTextLength := Length(AText);
  MoveMemory(@PAnsiChar(LData)[SizeOf(TBCEditorCommandDataText)], PChar(AText), Length(AText) * SizeOf(Char));
end;

function TBCEditorCommandDataText.GetText(): string;
begin
  SetString(Result, PChar(@PAnsiChar(Pointer(@Self))[SizeOf(TBCEditorCommandDataText)]), FTextLength);
end;

{ TBCEditorHookedCommandHandler ***********************************************}

class operator TBCEditorHookedCommandHandler.Equal(a, b: TBCEditorHookedCommandHandler): Boolean;
begin
  Result := (a.HandlerData = b.HandlerData)
    and (TMethod(a.ObjectProc) = TMethod(b.ObjectProc))
    and (Pointer((@a.Proc)^) = Pointer((@b.Proc)^));
end;

{ TBCEditorHookedCommandHandlers **********************************************}

procedure TBCEditorHookedCommandHandlers.Broadcast(const ABefore: Boolean;
  const ACommand: TBCEditorCommand; const AData: TBCEditorCommandData; var AHandled: Boolean);
var
  LHandledLong: LongBool;
  LIndex: Integer;
begin
  if (ABefore) then
    for LIndex := Count - 1 downto 0 do
      if (Assigned(Items[LIndex].ObjectProc)) then
        Items[LIndex].ObjectProc(FEditor, ABefore, ACommand, AData, AHandled)
      else
      begin
        LHandledLong := AHandled;
        Items[LIndex].Proc(FEditor, ABefore, Ord(ACommand), @AData[0], Length(AData),
          LHandledLong, Items[LIndex].HandlerData);
        AHandled := LHandledLong;
      end
  else
    for LIndex := 0 to Count - 1 do
      if (Assigned(Items[LIndex].ObjectProc)) then
        Items[LIndex].ObjectProc(FEditor, ABefore, ACommand, AData, AHandled)
      else
      begin
        LHandledLong := AHandled;
        Items[LIndex].Proc(FEditor, ABefore, Ord(ACommand), @AData[0], Length(AData),
          LHandledLong, Items[LIndex].HandlerData);
        AHandled := LHandledLong;
      end;
end;

constructor TBCEditorHookedCommandHandlers.Create(const AEditor: TCustomControl);
begin
  Assert(AEditor is TCustomBCEditor);

  inherited Create();

  FEditor := AEditor;
end;

{ TBCEditorCommandManager.TItem ***********************************************}

procedure TBCEditorCommandManager.TItem.Assign(ASource: TPersistent);
var
  LIndex: Integer;
begin
  Assert(ASource is TItem);

  EnabledWhileRecording := TItem(ASource).EnabledWhileRecording;
  Command := TItem(ASource).Command;
  CommandCategory := TItem(ASource).CommandCategory;
  Ident := TItem(ASource).Ident;
  Recordable := TItem(ASource).Recordable;
  ShortCuts.Clear();
  for LIndex := 0 to TItem(ASource).ShortCuts.Count - 1 do
    ShortCuts.Add(TItem(ASource).ShortCuts[LIndex]);
end;

constructor TBCEditorCommandManager.TItem.Create();
begin
  inherited Create();

  FShortCuts := TList<TShortCut>.Create();
end;

destructor TBCEditorCommandManager.TItem.Destroy();
begin
  FShortCuts.Free();

  inherited;
end;

{ TBCEditorCommandManager *****************************************************}

procedure TBCEditorCommandManager.AddShortCut(const ACommand: TBCEditorCommand; const AShortCut: TShortCut);
var
  LIndex: Integer;
begin
  if (AShortCut > 0) then
  begin
    RemoveShortCut(AShortCut);

    LIndex := IndexOf(ACommand);
    if (LIndex >= 0) then
      FItems[LIndex].ShortCuts.Add(AShortCut);
  end;
end;

procedure TBCEditorCommandManager.Clear();
begin
  FItems.Clear();
end;

function TBCEditorCommandManager.CommandCategoryOf(const ACommand: TBCEditorCommand): TBCEditorCommandCategory;
var
  LIndex: Integer;
begin
  Result := eccText;
  LIndex := IndexOf(ACommand);
  if (LIndex >= 0) then
    Result := FItems[LIndex].CommandCategory;
end;

function TBCEditorCommandManager.CommandToIdent(const ACommand: TBCEditorCommand): string;
var
  LIndex: Integer;
begin
  Result := IntToStr(Ord(ACommand));

  LIndex := IndexOf(ACommand);
  if ((LIndex >= 0) and (FItems[LIndex].ShortCuts.Count > 0)) then
  begin
    Result := FItems[LIndex].Ident;
    if (Result = '') then
      Result := IntToStr(Ord(ACommand));
  end;
end;

constructor TBCEditorCommandManager.Create();
begin
  inherited;

  FItems := TObjectList<TItem>.Create();
  Reset();
end;

destructor TBCEditorCommandManager.Destroy();
begin
  FItems.Free();

  inherited;
end;

function TBCEditorCommandManager.EnabledWhileRecording(const ACommand: TBCEditorCommand): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ACommand);
  if (LIndex < 0) then
    Result := False
  else
    Result := FItems[LIndex].EnabledWhileRecording;
end;

function TBCEditorCommandManager.GetCount(): Integer;
begin
  Result := FItems.Count;
end;

function TBCEditorCommandManager.GetItem(AIndex: Integer): TItem;
begin
  Result := FItems[AIndex];
end;

function TBCEditorCommandManager.IndexOf(const ACommand: TBCEditorCommand): Integer;
begin
  if (InsertIndex(ACommand, Result)) then
    Result := -1;
end;

function TBCEditorCommandManager.IndexOf(const AShortCut: TShortCut): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to FItems.Count - 1 do
    if (FItems[LIndex].ShortCuts.IndexOf(AShortCut) >= 0) then
      Result := LIndex;
end;

function TBCEditorCommandManager.InsertIndex(const ACommand: TBCEditorCommand; out AIndex: Integer): Boolean;
var
  LLeft: Integer;
  LMid: Integer;
  LRight: Integer;
begin
  Result := True;

  if ((FItems.Count = 0) or (ACommand > FItems[FItems.Count - 1].Command)) then
    AIndex := FItems.Count
  else
  begin
    LLeft := 0;
    LRight := FItems.Count - 1;
    while (LLeft <= LRight) do
    begin
      LMid := (LRight + LLeft) div 2;
      case (Sign(Ord(FItems[LMid].Command) - Ord(ACommand))) of
        -1: begin LLeft := LMid + 1;  AIndex := LMid + 1; end;
        0: begin Result := False; AIndex := LMid; break; end;
        1: begin LRight := LMid - 1; AIndex := LMid; end;
      end;
    end;
  end;
end;

function TBCEditorCommandManager.Recordable(const ACommand: TBCEditorCommand): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ACommand);
  if (LIndex < 0) then
    Result := False
  else
    Result := FItems[LIndex].Recordable;
end;

procedure TBCEditorCommandManager.RegisterCommand(const ACommand: TBCEditorCommand;
  const ACommandCategory: TBCEditorCommandCategory; const AIdent: string;
  const AShortCut: TShortCut = 0; const AEnabledWhileRecording: Boolean = True;
  const ARecordable: Boolean = True);
var
  LIndex: Integer;
  LItem: TItem;
begin
  Assert((ACommand <= ecLast) or (ACommand >= ecUser));
  Assert((ACommandCategory <= eccLast) or (ACommandCategory >= eccUser));

  LItem := TItem.Create();
  LItem.Command := ACommand;
  LItem.CommandCategory := ACommandCategory;
  LItem.Ident := AIdent;
  LItem.EnabledWhileRecording := AEnabledWhileRecording;
  LItem.Recordable := ARecordable;

  if (InsertIndex(ACommand, LIndex)) then
    FItems.Insert(LIndex, LItem)
  else
  begin
    FItems[LIndex].Assign(LItem);
    LItem.Free();
  end;

  AddShortCut(ACommand, AShortCut);
end;

procedure TBCEditorCommandManager.RemoveShortCut(const AShortCut: TShortCut);
var
  LIndex: Integer;
  LShortCutIndex: Integer;
begin
  if (AShortCut > 0) then
  begin
    LIndex := IndexOf(AShortCut);
    if (LIndex >= 0) then
      for LShortCutIndex := FItems.List[LIndex].ShortCuts.Count - 1 downto 0 do
        if (FItems[LIndex].ShortCuts[LShortCutIndex] = AShortCut) then
          FItems[LIndex].ShortCuts.Delete(LShortCutIndex);
  end;
end;

procedure TBCEditorCommandManager.Reset();
begin
  RegisterCommand(ecCancel, eccState, 'ecCancel', ShortCut(VK_ESCAPE, []));
  RegisterCommand(ecInsertTextMode, eccState, 'ecInsertTextMode');
  RegisterCommand(ecSyncEdit, eccState, 'ecSyncEdit', ShortCut(Ord('J'), [ssCtrl, ssShift]));
  RegisterCommand(ecOverwriteTextMode, eccState, 'ecOverwriteTextMode');
  RegisterCommand(ecToggleTextMode, eccState, 'ecToggleTextMode', ShortCut(VK_INSERT, []));

  RegisterCommand(ecGotoBookmark1, eccBookmark, 'ecGotoBookmark1', ShortCut(Ord('1'), [ssCtrl]));
  RegisterCommand(ecGotoBookmark2, eccBookmark, 'ecGotoBookmark2', ShortCut(Ord('2'), [ssCtrl]));
  RegisterCommand(ecGotoBookmark3, eccBookmark, 'ecGotoBookmark3', ShortCut(Ord('3'), [ssCtrl]));
  RegisterCommand(ecGotoBookmark4, eccBookmark, 'ecGotoBookmark4', ShortCut(Ord('4'), [ssCtrl]));
  RegisterCommand(ecGotoBookmark5, eccBookmark, 'ecGotoBookmark5', ShortCut(Ord('5'), [ssCtrl]));
  RegisterCommand(ecGotoBookmark6, eccBookmark, 'ecGotoBookmark6', ShortCut(Ord('6'), [ssCtrl]));
  RegisterCommand(ecGotoBookmark7, eccBookmark, 'ecGotoBookmark7', ShortCut(Ord('7'), [ssCtrl]));
  RegisterCommand(ecGotoBookmark8, eccBookmark, 'ecGotoBookmark8', ShortCut(Ord('8'), [ssCtrl]));
  RegisterCommand(ecGotoBookmark9, eccBookmark, 'ecGotoBookmark9', ShortCut(Ord('9'), [ssCtrl]));
  RegisterCommand(ecGotoBookmark0, eccBookmark, 'ecGotoBookmark0', ShortCut(Ord('0'), [ssCtrl]));
  RegisterCommand(ecGotoNextBookmark, eccBookmark, 'ecGotoNextBookmark', ShortCut(VK_F2, []));
  RegisterCommand(ecGotoPreviousBookmark, eccBookmark, 'ecGotoPreviousBookmark', ShortCut(VK_F2, [ssShift]));
  RegisterCommand(ecSetBookmark1, eccBookmark, 'ecSetBookmark1', ShortCut(Ord('1'), [ssCtrl, ssShift]));
  RegisterCommand(ecSetBookmark2, eccBookmark, 'ecSetBookmark2', ShortCut(Ord('2'), [ssCtrl, ssShift]));
  RegisterCommand(ecSetBookmark3, eccBookmark, 'ecSetBookmark3', ShortCut(Ord('3'), [ssCtrl, ssShift]));
  RegisterCommand(ecSetBookmark4, eccBookmark, 'ecSetBookmark4', ShortCut(Ord('4'), [ssCtrl, ssShift]));
  RegisterCommand(ecSetBookmark5, eccBookmark, 'ecSetBookmark5', ShortCut(Ord('5'), [ssCtrl, ssShift]));
  RegisterCommand(ecSetBookmark6, eccBookmark, 'ecSetBookmark6', ShortCut(Ord('6'), [ssCtrl, ssShift]));
  RegisterCommand(ecSetBookmark7, eccBookmark, 'ecSetBookmark7', ShortCut(Ord('7'), [ssCtrl, ssShift]));
  RegisterCommand(ecSetBookmark8, eccBookmark, 'ecSetBookmark8', ShortCut(Ord('8'), [ssCtrl, ssShift]));
  RegisterCommand(ecSetBookmark9, eccBookmark, 'ecSetBookmark9', ShortCut(Ord('9'), [ssCtrl, ssShift]));
  RegisterCommand(ecSetBookmark0, eccBookmark, 'ecSetBookmark0', ShortCut(Ord('0'), [ssCtrl, ssShift]));

  RegisterCommand(ecScrollTo, eccScroll, 'ecScroll', 0, True, False);
  RegisterCommand(ecScrollDown, eccScroll, 'ecScrollDown', ShortCut(VK_DOWN, [ssCtrl]), True, False);
  RegisterCommand(ecScrollLeft, eccScroll, 'ecScrollLeft', 0, True, False);
  RegisterCommand(ecScrollRight, eccScroll, 'ecScrollRight', 0, True, False);
  RegisterCommand(ecScrollUp, eccScroll, 'ecScrollUp', ShortCut(VK_UP, [ssCtrl]), True, False);

  RegisterCommand(ecBOL, eccMoveCaret, 'ecBOL', ShortCut(VK_HOME, []));
  RegisterCommand(ecBOF, eccMoveCaret, 'ecBOF', ShortCut(VK_HOME, [ssCtrl]));
  RegisterCommand(ecPageTop, eccMoveCaret, 'ecBOP', ShortCut(VK_PRIOR, [ssCtrl]), False);
  RegisterCommand(ecDown, eccMoveCaret, 'ecDown', ShortCut(VK_DOWN, []));
  RegisterCommand(ecEOF, eccMoveCaret, 'ecEOF', ShortCut(VK_END, [ssCtrl]));
  RegisterCommand(ecEOL, eccMoveCaret, 'ecEOL', ShortCut(VK_END, []));
  RegisterCommand(ecPageBottom, eccMoveCaret, 'ecEOP', ShortCut(VK_NEXT, [ssCtrl]), False);
  RegisterCommand(ecFindBackward, eccMoveCaret, 'ecFindBackward');
  RegisterCommand(ecFindFirst, eccMoveCaret, 'ecFindFirst');
  RegisterCommand(ecFindForeward, eccMoveCaret, 'ecFindForeward');
  RegisterCommand(ecFindNext, eccMoveCaret, 'ecFindNext', ShortCut(VK_F3, []));
  RegisterCommand(ecFindPrevious, eccMoveCaret, 'ecFindPrevious', ShortCut(VK_F3, [ssShift]));
  RegisterCommand(ecGotoMatchingPair, eccMoveCaret, 'ecGotoMatchingPair', ShortCut(Ord('D'), [ssCtrl]));
  RegisterCommand(ecLeft, eccMoveCaret, 'ecLeft', ShortCut(VK_LEFT, []));
  RegisterCommand(ecPageDown, eccMoveCaret, 'ecPageDown', ShortCut(VK_NEXT, []), False);
  RegisterCommand(ecPageUp, eccMoveCaret, 'ecPageUp', ShortCut(VK_PRIOR, []), False);
  RegisterCommand(ecPosition, eccMoveCaret, 'ecPosition', 0, False);
  RegisterCommand(ecRight, eccMoveCaret, 'ecRight', ShortCut(VK_RIGHT, []));
  RegisterCommand(ecSel, eccMoveCaret, 'ecSel', 0, False);
  RegisterCommand(ecSelDown, eccMoveCaret, 'ecSelDown', ShortCut(VK_DOWN, [ssShift]));
  RegisterCommand(ecSelectAll, eccMoveCaret, 'ecSelectAll', ShortCut(Ord('A'), [ssCtrl]));
  RegisterCommand(ecSelEOF, eccMoveCaret, 'ecSelEOF', ShortCut(VK_END, [ssShift, ssCtrl]));
  RegisterCommand(ecSelBOF, eccMoveCaret, 'ecSelBOF', ShortCut(VK_HOME, [ssShift, ssCtrl]));
  RegisterCommand(ecSelLeft, eccMoveCaret, 'ecSelLeft', ShortCut(VK_LEFT, [ssShift]));
  RegisterCommand(ecSelBOL, eccMoveCaret, 'ecSelBOL', ShortCut(VK_HOME, [ssShift]));
  RegisterCommand(ecSelEOL, eccMoveCaret, 'ecSelEOL', ShortCut(VK_END, [ssShift]));
  RegisterCommand(ecSelEOP, eccMoveCaret, 'ecSelEOP', ShortCut(VK_NEXT, [ssShift, ssCtrl]));
  RegisterCommand(ecSelPageDown, eccMoveCaret, 'ecSelPageDown', ShortCut(VK_NEXT, [ssShift]));
  RegisterCommand(ecSelBOP, eccMoveCaret, 'ecSelBOP', ShortCut(VK_PRIOR, [ssShift, ssCtrl]));
  RegisterCommand(ecSelPageUp, eccMoveCaret, 'ecSelPageUp', ShortCut(VK_PRIOR, [ssShift]));
  RegisterCommand(ecSelRight, eccMoveCaret, 'ecSelRight', ShortCut(VK_RIGHT, [ssShift]));
  RegisterCommand(ecSelUp, eccMoveCaret, 'ecSelUp', ShortCut(VK_UP, [ssShift]));
  RegisterCommand(ecSelWord, eccMoveCaret, 'ecSelWord');
  RegisterCommand(ecSelWordLeft, eccMoveCaret, 'ecSelWordLeft', ShortCut(VK_LEFT, [ssShift, ssCtrl]));
  RegisterCommand(ecSelWordRight, eccMoveCaret, 'ecSelWordRight', ShortCut(VK_RIGHT, [ssShift, ssCtrl]));
  RegisterCommand(ecUp, eccMoveCaret, 'ecUp', ShortCut(VK_UP, []));
  RegisterCommand(ecWordLeft, eccMoveCaret, 'ecWordLeft', ShortCut(VK_LEFT, [ssCtrl]));
  RegisterCommand(ecWordRight, eccMoveCaret, 'ecWordRight', ShortCut(VK_RIGHT, [ssCtrl]));

  RegisterCommand(ecAcceptDrop, eccText, 'ecAcceptDrop', 0, False);
  RegisterCommand(ecBackspace, eccText, 'ecBackspace', ShortCut(VK_BACK, []));
  AddShortCut(ecBackspace, ShortCut(VK_BACK, [ssShift]));
  AddShortCut(ecBackspace, ShortCut(VK_BACK, [ssCtrl]));
  RegisterCommand(ecBlockComment, eccText, 'ecBlockComment', ShortCut(VK_OEM_2, [ssCtrl, ssShift]));
  RegisterCommand(ecBlockIndent, eccText, 'ecBlockIndent', ShortCut(Ord('I'), [ssCtrl, ssShift]));
  RegisterCommand(ecBlockUnindent, eccText, 'ecBlockUnindent', ShortCut(Ord('U'), [ssCtrl, ssShift]));
  RegisterCommand(ecChar, eccText, 'ecChar');
  RegisterCommand(ecClear, eccText, 'ecClear');
  RegisterCommand(ecDeleteToBOL, eccText, 'ecDeleteToBOL');
  RegisterCommand(ecDelete, eccText, 'ecDeleteChar', ShortCut(VK_DELETE, []));
  RegisterCommand(ecDeleteToEOL, eccText, 'ecDeleteToEOL', ShortCut(Ord('Y'), [ssCtrl, ssShift]));
  RegisterCommand(ecDeleteLastWord, eccText, 'ecDeleteLastWord', ShortCut(VK_BACK, [ssCtrl]));
  RegisterCommand(ecDeleteLine, eccText, 'ecDeleteLine', ShortCut(Ord('Y'), [ssCtrl]));
  RegisterCommand(ecDeleteWord, eccText, 'ecDeleteWord', ShortCut(Ord('T'), [ssCtrl]));
  AddShortCut(ecDeleteWord, ShortCut(VK_DELETE, [ssCtrl]));
  RegisterCommand(ecDropOLE, eccText, 'ecDropOLE', 0, False);
  RegisterCommand(ecInsertLine, eccText, 'ecInsertLine', ShortCut(Ord('M'), [ssCtrl]));
  RegisterCommand(ecLineComment, eccText, 'ecLineComment', ShortCut(VK_OEM_2, [ssCtrl]));
  RegisterCommand(ecLowerCase, eccText, 'ecLowerCase');
  RegisterCommand(ecReturn, eccText, 'ecReturn', ShortCut(VK_RETURN, []));
  AddShortCut(ecReturn, ShortCut(VK_RETURN, [ssShift]));
  RegisterCommand(ecReplace, eccText, 'ecReplace');
  RegisterCommand(ecShiftTab, eccText, 'ecShiftTab', ShortCut(VK_TAB, [ssShift]));
  RegisterCommand(ecTab, eccText, 'ecTab', ShortCut(VK_TAB, []));
  RegisterCommand(ecText, eccText, 'ecText');
  RegisterCommand(ecUpperCase, eccText, 'ecUpperCase');

  RegisterCommand(ecRedo, eccUndo, 'ecRedo', ShortCut(VK_BACK, [ssAlt, ssShift]));
  AddShortCut(ecRedo, ShortCut(Ord('Z'), [ssCtrl, ssShift]));
  RegisterCommand(ecUndo, eccUndo, 'ecUndo', ShortCut(VK_BACK, [ssAlt]));
  AddShortCut(ecUndo, ShortCut(Ord('Z'), [ssCtrl]));

  RegisterCommand(ecCopyToClipboard, eccClipboard, 'ecCopyToClipboard', ShortCut(VK_INSERT, [ssCtrl]));
  AddShortCut(ecCopyToClipboard, ShortCut(Ord('C'), [ssCtrl]));
  RegisterCommand(ecCutToClipboard, eccClipboard, 'ecCutToClipboard', ShortCut(VK_DELETE, [ssShift]));
  AddShortCut(ecCutToClipboard, ShortCut(Ord('X'), [ssCtrl]));
  RegisterCommand(ecPasteFromClipboard, eccClipboard, 'ecPasteFromClipboard', ShortCut(VK_INSERT, [ssShift]));
  AddShortCut(ecPasteFromClipboard, ShortCut(Ord('V'), [ssCtrl]));

  RegisterCommand(ecShowCompletionProposal, eccShowDialog, 'ecShowCompletionProposal', ShortCut(VK_SPACE, [ssCtrl]), True, False);
  RegisterCommand(ecShowFind, eccShowDialog, 'ecShowFind', ShortCut(Ord('F'), [ssCtrl]), True, False);
  RegisterCommand(ecShowGotoLine, eccShowDialog, 'ecShowGotoLine', ShortCut(Ord('G'), [ssAlt]), True, False);
  RegisterCommand(ecShowReplace, eccShowDialog, 'ecShowReplace', ShortCut(Ord('R'), [ssCtrl]), True, False);

  RegisterCommand(ecPlaybackMacro, eccMacroRecorder, 'ecPlaybackMacro', ShortCut(Ord('P'), [ssCtrl, ssShift]));
  RegisterCommand(ecRecordMacro, eccMacroRecorder, 'ecRecordMacro', ShortCut(Ord('R'), [ssCtrl, ssShift]));
  RegisterCommand(ecStepMacro, eccMacroRecorder, 'ecStepMacro');
  RegisterCommand(ecStopMacro, eccMacroRecorder, 'ecStopMacro');
end;

function TBCEditorCommandManager.TryCommandToShortCut(const ACommand: TBCEditorCommand;
  out AShortCut: TShortCut): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ACommand);
  Result := (LIndex >= 0) and (FItems[LIndex].ShortCuts.Count > 0);
  if (Result) then
    AShortCut := FItems[LIndex].ShortCuts[0];
end;

function TBCEditorCommandManager.TryShortCutToCommand(const AShortCut: TShortCut;
  out ACommand: TBCEditorCommand): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AShortCut);
  Result := LIndex >= 0;
  if (Result) then
    ACommand := FItems[LIndex].Command;
end;

procedure TBCEditorCommandManager.UnregisterCommand(const ACommand: TBCEditorCommand);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ACommand);
  if (LIndex >= 0) then
    FItems.Delete(LIndex);
end;

{ TCustomBCEditorMacroRecorder ************************************************}

procedure TCustomBCEditorMacroRecorder.AddCommand(const ACommand: TBCEditorCommand; const AData: TBCEditorCommandData);
begin
  InsertCommand(FItems.Count, ACommand, AData);
end;

procedure TCustomBCEditorMacroRecorder.Clear();
begin
  FItems.Clear();
  FData.Clear();
end;

constructor TCustomBCEditorMacroRecorder.Create(AOwner: TComponent);
begin
  inherited;

  FItems := TList<TItem>.Create();
  FData := TBytesStream.Create();
end;

procedure TCustomBCEditorMacroRecorder.DeleteCommand(const AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

destructor TCustomBCEditorMacroRecorder.Destroy();
begin
  FItems.Free();
  FData.Free();

  inherited;
end;

procedure TCustomBCEditorMacroRecorder.EditorCommand(ASender: TObject; const ABefore: Boolean;
  const ACommand: TBCEditorCommand; const AData: TBCEditorCommandData; var AHandled: Boolean);
begin
  if (ABefore) then
  begin
    case (ACommand) of
      ecCancel:
        if (FState = msRecording) then
        begin
          Stop();
          AHandled := True;
        end;
      ecRecordMacro:
        begin
          if (FState = msRecording) then
            Stop()
          else
            StartRecord();
          AHandled := True;
        end;
      ecPlaybackMacro:
        begin
          Playback();
          AHandled := True;
        end;
      ecStepMacro:
        begin
          PlaybackStep();
          AHandled := True;
        end;
      ecSyncEdit:
        AHandled := True;
      ecTerminate:
        FEditor := nil;
      else
        if ((FState = msRecording)
          and Assigned(BCEditorCommandManager)
          and not BCEditorCommandManager.EnabledWhileRecording(ACommand)) then
        begin
          MessageBeep(UINT(-1));
          AHandled := True;
        end;
    end;
  end
  else
  begin
    if ((ASender = FEditor) and not AHandled) then
      case (ACommand) of
        ecRecordMacro,
        ecPlaybackMacro,
        ecStepMacro: ;
        else
          if ((State = msRecording)
            and Assigned(BCEditorCommandManager)
            and BCEditorCommandManager.Recordable(ACommand)) then
            AddCommand(ACommand, AData);
      end;
  end;
end;

function TCustomBCEditorMacroRecorder.GetCommand(AIndex: Integer): TCommand;
begin
  Result.Command := FItems[AIndex].Command;
  Result.Data := BytesOf(@PAnsiChar(FData.Memory)[FItems[AIndex].DataPosition], FItems[AIndex].DataSize);
end;

function TCustomBCEditorMacroRecorder.GetCommandCount(): Integer;
begin
  Result := FItems.Count;
end;

function TCustomBCEditorMacroRecorder.GetData(const ACommand: Integer): TBCEditorCommandData;
begin
  if (FItems[FCurrentCommand].DataSize = 0) then
    Result := nil
  else
    Result := BytesOf(@PAnsiChar(FData.Memory)[FItems[ACommand].DataPosition], FItems[ACommand].DataSize);
end;

function TCustomBCEditorMacroRecorder.GetIsEmpty(): Boolean;
begin
  Result := FItems.Count = 0;
end;

procedure TCustomBCEditorMacroRecorder.InsertCommand(const AIndex: Integer;
  const ACommand: TBCEditorCommand; const AData: TBCEditorCommandData);
var
  LCommand: TItem;
begin
  LCommand.Command := ACommand;
  if (not Assigned(AData)) then
  begin
    LCommand.DataSize := 0;
    LCommand.DataPosition := 0;
  end
  else
  begin
    LCommand.DataSize := Length(AData);
    LCommand.DataPosition := FData.Position;
    FData.Write(AData, Length(AData));
  end;
  FItems.Insert(AIndex, LCommand);
end;

procedure TCustomBCEditorMacroRecorder.LoadFromFile(const AFilename: string);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFilename, fmOpenRead);
  try
    LoadFromStream(LStream);
  finally
    LStream.Free();
  end;
end;

procedure TCustomBCEditorMacroRecorder.LoadFromStream(AStream: TStream; AClear: Boolean = True);
var
  LCommand: TItem;
begin
  Stop();
  if (AClear) then
    Clear();

  while ((AStream.Read(LCommand.Command, SizeOf(LCommand.Command)) = SizeOf(LCommand.Command))
    and (AStream.Read(LCommand.DataSize, SizeOf(LCommand.DataSize)) = SizeOf(LCommand.DataSize))
    and (FData.CopyFrom(AStream, LCommand.DataSize) = LCommand.DataSize)) do
    FItems.Add(LCommand);
end;

procedure TCustomBCEditorMacroRecorder.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if AOperation = opRemove then
    if (AComponent = Editor) or (AComponent is TCustomBCEditor) then
      FEditor := nil;
end;

procedure TCustomBCEditorMacroRecorder.Playback();
begin
  if (FState in [msRecording, msPlaying]) then
    raise ERangeError.Create(SCannotPlay);

  SetState(msPlaying);
  try
    while (FState = msPlaying) do
      PlaybackStep();
  finally
    SetState(msStopped);
  end;
end;

procedure TCustomBCEditorMacroRecorder.SaveToFile(const AFilename: string);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFilename, fmCreate);
  try
    SaveToStream(LStream);
  finally
    LStream.Free();
  end;
end;

procedure TCustomBCEditorMacroRecorder.SaveToStream(AStream: TStream);
var
  LIndex: Integer;
begin
  for LIndex := 0 to FItems.Count - 1 do
  begin
    AStream.Write(FItems.List[LIndex].Command, SizeOf(FItems.List[LIndex].Command));
    AStream.Write(FItems.List[LIndex].DataSize, SizeOf(FItems.List[LIndex].DataSize));
    AStream.Write(PAnsiChar(FData.Memory)[FItems[LIndex].DataPosition], FItems[LIndex].DataSize);
  end;
end;

procedure TCustomBCEditorMacroRecorder.SetEditor(AValue: TCustomControl);
begin
  if (Assigned(AValue) and not (AValue is TCustomBCEditor)) then
    raise ERangeError.Create(SNotTCustomBCEditor);

  if (AValue <> FEditor) then
  begin
    if (Assigned(FEditor)) then
      TCustomBCEditor(FEditor).UnregisterCommandHandler(EditorCommand);
    FEditor := AValue;
    if (Assigned(FEditor)) then
      TCustomBCEditor(FEditor).RegisterCommandHandler(EditorCommand);
  end;
end;

procedure TCustomBCEditorMacroRecorder.SetState(AValue: TState);
begin
  if (AValue <> FState) then
  begin
    case (FState) of
      msRecording:
        if (Assigned(FAfterRecord)) then
          FAfterRecord(Self);
      msPlaying:
        if (Assigned(FAfterPlayback)) then
          FAfterPlayback(Self);
    end;

    FState := AValue;

    if (Assigned(OnStateChange)) then
      OnStateChange(Self);

    case (FState) of
      msRecording:
        if (Assigned(FBeforeRecord)) then
          FBeforeRecord(Self);
      msPlaying:
        if (Assigned(FBeforePlayback)) then
          FBeforePlayback(Self);
    end;
  end;
end;

procedure TCustomBCEditorMacroRecorder.StartRecord();
begin
  if (FState in [msRecording, msPlaying]) then
    raise ERangeError.Create(SCannotRecord);

  Clear();
  SetState(msRecording);
end;

function TCustomBCEditorMacroRecorder.PlaybackStep(): Boolean;
begin
  Result := True;

  if (Assigned(FBeforePlaybackStep)) then
    FBeforePlaybackStep(Self, FItems[FCurrentCommand].Command, GetData(FCurrentCommand), Result);

  Result := Result and PlaybackStep(False);

  if (Assigned(FAfterPlaybackStep)) then
    FAfterPlaybackStep(Self, FItems[FCurrentCommand].Command, GetData(FCurrentCommand));
end;

function TCustomBCEditorMacroRecorder.PlaybackStep(const APlay: Boolean): Boolean;
begin
  if ((FState = msRecording) or not APlay and (FState = msPlaying)) then
    raise ERangeError.Create(SCannotPlay);

  Result := FCurrentCommand < FItems.Count;
  if (Result) then
  begin
    TCustomBCEditor(FEditor).ProcessCommand(FItems[FCurrentCommand].Command, GetData(FCurrentCommand));
    Inc(FCurrentCommand);
  end;

  if (FCurrentCommand = FItems.Count) then
    SetState(msStopped)
  else if (not APlay) then
    SetState(msStepped);
end;

procedure TCustomBCEditorMacroRecorder.Stop();
begin
  if (FState <> msStopped) then
    SetState(msStopped);

  FCurrentCommand := 0;
end;

initialization
  BCEditorCommandManager := TBCEditorCommandManager.Create();
finalization
  BCEditorCommandManager.Free();
end.
