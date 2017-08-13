unit BCEditor.Commands;

interface {********************************************************************}

uses
  Classes, SysUtils, Types, Generics.Collections,
  Controls, Menus,
  BCEditor.Types;

type
  TBCEditorCommandCategory = (
    eccUnknown = 0,
    eccState = 1,
    eccBookmark = 2,
    eccScroll = 3,
    eccMoveCaret = 4,
    eccText = 5,
    eccUndo = 6,
    eccClipboard = 7,
    eccShowDialog = 8,
    eccMacroRecorder = 9

    // Last defined category: 9
  );

  TBCEditorCommand = (
    ecNone = 0,

    ecCancel = 104,
    ecInsertTextMode = 1,
    ecOverwriteTextMode = 2,
    ecSyncEdit = 3,
    ecToggleTextMode = 4,

    ecGotoBookmark1 = 5,
    ecGotoBookmark2 = 6,
    ecGotoBookmark3 = 7,
    ecGotoBookmark4 = 8,
    ecGotoBookmark5 = 9,
    ecGotoBookmark6 = 10,
    ecGotoBookmark7 = 11,
    ecGotoBookmark8 = 12,
    ecGotoBookmark9 = 13,
    ecGotoBookmark0 = 14,
    ecGotoNextBookmark = 15,
    ecGotoPreviousBookmark = 16,
    ecSetBookmark1 = 17,
    ecSetBookmark2 = 18,
    ecSetBookmark3 = 19,
    ecSetBookmark4 = 20,
    ecSetBookmark5 = 21,
    ecSetBookmark6 = 22,
    ecSetBookmark7 = 23,
    ecSetBookmark8 = 24,
    ecSetBookmark9 = 25,
    ecSetBookmark0 = 26,

    ecScrollDown = 27,
    ecScrollLeft = 28,
    ecScrollRight = 29,
    ecScrollTo = 30,
    ecScrollUp = 31,

    ecBOF = 32,
    ecBOL = 33,
    ecBOP = 34,
    ecDown = 35,
    ecEOF = 36,
    ecEOL = 37,
    ecEOP = 38,
    ecFindBackward = 39,
    ecFindFirst = 40,
    ecFindForeward = 41,
    ecFindNext = 42,
    ecFindPrevious = 43,
    ecLeft = 44,
    ecPageDown = 45,
    ecPageUp = 46,
    ecPosition = 47,
    ecRight = 48,
    ecSel = 49,
    ecSelBOF = 50,
    ecSelBOL = 51,
    ecSelBOP = 52,
    ecSelDown = 53,
    ecSelectAll = 54,
    ecSelEOF = 55,
    ecSelEOL = 56,
    ecSelEOP = 57,
    ecSelLeft = 58,
    ecSelPageDown = 59,
    ecSelPageUp = 60,
    ecSelRight = 61,
    ecSelUp = 62,
    ecSelWord = 63,
    ecSelWordLeft = 64,
    ecSelWordRight = 65,
    ecUnselect = 66,
    ecUp = 67,
    ecWordLeft = 68,
    ecWordRight = 69,

    ecBackspace = 70,
    ecBlockComment = 71,
    ecBlockIndent = 72,
    ecBlockUnindent = 73,
    ecChar = 74,
    ecClear = 75,
    ecDeleteToBOL = 76,
    ecDeleteChar = 77,
    ecDeleteToEOL = 78,
    ecDeleteLastWord = 79,
    ecDeleteLine = 80,
    ecDeleteWord = 81,
    ecInsertLine = 82,
    ecLineComment = 83,
    ecLowerCase = 84,
    ecReturn = 85,
    ecReplace = 86,
    ecShiftTab = 87,
    ecTab = 88,
    ecText = 89,
    ecUpperCase = 90,

    ecRedo = 91,
    ecUndo = 92,

    ecCopyToClipboard = 93,
    ecCutToClipboard = 94,
    ecPasteFromClipboard = 95,

    ecShowCompletionProposal = 96,
    ecShowFind = 97,
    ecShowGotoLine = 98,
    ecShowReplace = 99,

    ecPlaybackMacro = 100,
    ecRecordMacro = 101,
    ecStepMacro = 102,
    ecStopMacro = 103

    // Last defined command: 104
  );

const
  // User defined command classes must be eccUser + x
  eccUser = 100;
  // User defined commands must be ecUser + x
  ecUser = 10000;

type
  { Command Data }

  PBCEditorCDChar = ^TBCEditorCDChar;
  TBCEditorCDChar = packed record
    Char: Char;
    class function Create(const AChar: Char): TBytes; static;
  end;

  PBCEditorCDFind = ^TBCEditorCDFind;
  TBCEditorCDFind = packed record
    Options: TBCEditorFindOptions;
  private
    FPattern: PChar;
    FPatternLength: Int64;
    function GetPattern(): string;
  public
    class function Create(const APattern: string;
      const AOptions: TBCEditorFindOptions): TBytes; static;
    property Pattern: string read GetPattern;
  end;

  PBCEditorCDMoveCaret = ^TBCEditorCDMoveCaret;
  TBCEditorCDMoveCaret = packed record
    X: Integer;
    Y: Integer;
    Selection: Boolean;
    class function Create(const AX, AY: Integer; const ASelection: Boolean = False): TBytes; static;
  end;

  PBCEditorCDPosition = ^TBCEditorCDPosition;
  TBCEditorCDPosition = packed record
    Pos: TPoint;
    Selection: Boolean;
    class function Create(const APosition: TBCEditorLinesPosition;
      const ASelection: Boolean = False): TBytes; static;
  end;

  PBCEditorCDReplace = ^TBCEditorCDReplace;
  TBCEditorCDReplace = packed record
  private
    FPattern: PChar;
    FPatternLength: Int64;
    FReplaceText: PChar;
    FReplaceTextLength: Int64;
    function GetPattern(): string;
    function GetReplaceText(): string;
  public
    Options: TBCEditorReplaceOptions;
    class function Create(const APattern, AReplaceText: string;
      const AOptions: TBCEditorReplaceOptions): TBytes; static;
    property Pattern: string read GetPattern;
    property ReplaceText: string read GetReplaceText;
  end;

  PBCEditorCDScrollTo = ^TBCEditorCDScrollTo;
  TBCEditorCDScrollTo = packed record
    Pos: TPoint;
    class function Create(const APos: TPoint): TBytes; static;
  end;

  PBCEditorCDSelection = ^TBCEditorCDSelection;
  TBCEditorCDSelection = packed record
    CaretPos: TPoint;
    BeginPos: TPoint;
    EndPos: TPoint;
    class function Create(const ACaretPosition: TBCEditorLinesPosition;
      const ASelArea: TBCEditorLinesArea): TBytes; static;
  end;

  PBCEditorCDText = ^TBCEditorCDText;
  TBCEditorCDText = packed record
    Delete: Boolean;
    Selection: Boolean;
  private
    FText: PChar;
    FTextLength: Int64;
    function GetText(): string;
  public
    class function Create(const AText: string; const ADelete: Boolean = False;
      const ASelection: Boolean = False): TBytes; static;
    property Text: string read GetText;
  end;

  TBCEditorHookedCommandProc = procedure(const AEditor: Pointer; const ABefore: LongBool;
    const ACommand: Integer; const AData: TBytes; const ADataSize: Int64;
    var AHandled: LongBool; const AHandlerData: Pointer); stdcall;
  TBCEditorHookedCommandObjectProc = procedure(ASender: TObject; const ABefore: Boolean;
    const ACommand: TBCEditorCommand; const AData: TBytes; var AHandled: Boolean) of object;
  TBCEditorProcessCommandEvent = procedure(ASender: TObject;
    const ACommand: TBCEditorCommand; const AData: TBytes; var AAllowed: Boolean) of object;

  TBCEditorHookedCommandHandler = record
    HandlerData: Pointer;
    ObjectProc: TBCEditorHookedCommandObjectProc;
    Proc: TBCEditorHookedCommandProc;
    class operator Equal(a, b: TBCEditorHookedCommandHandler): Boolean; inline;
  end;

  TBCEditorCommands = class
  private type
    TCommand = record
      EnabledWhileRecording: Boolean;
      Command: TBCEditorCommand;
      CommandCategory: TBCEditorCommandCategory;
      Ident: string;
      Recordable: Boolean;
      ShortCuts: array of TShortCut;
    end;
  private
    FCommands: TList<TCommand>;
    function IndexOf(const ACommand: TBCEditorCommand): Integer; overload;
    function IndexOf(const AShortCut: TShortCut): Integer; overload;
    function InsertIndex(const ACommand: TBCEditorCommand; out AIndex: Integer): Boolean;
  protected
    function EnabledWhileRecording(const ACommand: TBCEditorCommand): Boolean;
    function Recordable(const ACommand: TBCEditorCommand): Boolean;
  public
    procedure AddShortCut(const ACommand: TBCEditorCommand; const AShortCut: TShortCut);
    function CommandToCommandClass(const ACommand: TBCEditorCommand;
      out ACommandCategory: TBCEditorCommandCategory): Boolean;
    function CommandToIdent(const ACommand: TBCEditorCommand; out AIdent: string): Boolean;
    function CommandToShortCut(const ACommand: TBCEditorCommand; out AShortCut: TShortCut): Boolean;
    constructor Create();
    destructor Destroy(); override;
    procedure RegisterCommand(const ACommand: TBCEditorCommand;
      const ACommandCategory: TBCEditorCommandCategory; const AIdent: string;
      const AShortCut: TShortCut = 0;
      const AEnabledWhileRecording: Boolean = True; const ARecordable: Boolean = True);
    procedure ResetShortCuts();
    function ShortCutToCommand(const AShortCut: TShortCut; out ACommand: TBCEditorCommand): Boolean;
  end;

  TCustomBCEditorMacroRecorder = class(TComponent)
  private type
    TItem = record
      Command: TBCEditorCommand;
      DataPosition: Int64;
      DataSize: Int64;
    end;

  type
    TCommand = record
      Command: TBCEditorCommand;
      Data: TBytes;
    end;
    TState = (msStopped, msStepped, msRecording, msPlaying);

  strict private
    FCurrentCommand: Integer;
    FData: TBytesStream;
    FEditor: TCustomControl;
    FItems: TList<TItem>;
    FOnStateChange: TNotifyEvent;
    FState: TState;
    procedure EditorCommand(ASender: TObject; const ABefore: Boolean;
      const ACommand: TBCEditorCommand; const AData: TBytes; var AHandled: Boolean);
    function GetCommand(AIndex: Integer): TCommand;
    function GetCommandCount: Integer;
    function GetIsEmpty(): Boolean;
    procedure SetEditor(AValue: TCustomControl);
    procedure SetState(AValue: TState);
    function Step(const APlay: Boolean): Boolean; overload;
  protected
    procedure Notification(AComponent: TComponent; aOperation: TOperation); override;
    property Editor: TCustomControl read FEditor write SetEditor;
    property CommandCount: Integer read GetCommandCount;
    property Commands[AIndex: Integer]: TCommand read GetCommand;
    property IsEmpty: Boolean read GetIsEmpty;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property State: TState read FState;
  public
    procedure AddCommand(const ACommand: TBCEditorCommand; const AData: TBytes);
    procedure Clear();
    constructor Create(AOwner: TComponent); override;
    procedure DeleteCommand(const AIndex: Integer);
    destructor Destroy(); override;
    procedure InsertCommand(const AIndex: Integer; const ACommand: TBCEditorCommand;
      const AData: TBytes);
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromStream(AStream: TStream; AClear: Boolean = True);
    procedure SaveToFile(const AFilename: string);
    procedure SaveToStream(AStream: TStream);
    procedure Playback();
    procedure StartRecord();
    function Step(): Boolean; overload;
    procedure Stop();
  end;

  TBCEditorMacroRecorder = class(TCustomBCEditorMacroRecorder)
  public
    property CommandCount;
    property Commands;
    property IsEmpty;
    property State;
  published
    property Editor;
    property OnStateChange;
  end;

var
  GBCEditorCommands: TBCEditorCommands;

implementation {***************************************************************}

uses
  Windows,
  BCEditor,
  Math;

resourcestring
  SBCEditorCannotRecord = 'Cannot record macro: Already recording or playing';
  SBCEditorCannotPlay = 'Cannot play macro: Already recording or playing';
  SBCEditorNotTCustomBCEditor = 'Value must be a TCustomBCEditor class object';

{ TBCEditorCDChar *************************************************************}

class function TBCEditorCDChar.Create(const AChar: Char): TBytes;
var
  LData: TBCEditorCDChar;
begin
  LData.Char := AChar;
  Result := BytesOf(@LData, SizeOf(LData));
end;

{ TBCEditorCDFind *************************************************************}

class function TBCEditorCDFind.Create(const APattern: string;
  const AOptions: TBCEditorFindOptions): TBytes;
var
  LData: PBCEditorCDFind;
  LSize: Int64;
begin
  LSize := SizeOf(TBCEditorCDFind) + Length(APattern) * SizeOf(Char);
  SetLength(Result, LSize);
  LData := @Result[0];
  LData^.Options := AOptions;
  LData^.FPattern := @PAnsiChar(Result)[SizeOf(TBCEditorCDFind)];
  LData^.FPatternLength := Length(APattern);
  MoveMemory(LData^.FPattern, PChar(APattern), Length(APattern) * SizeOf(Char));
end;

function TBCEditorCDFind.GetPattern(): string;
begin
  SetString(Result, FPattern, FPatternLength);
end;

{ TBCEditorCDMoveCaret ********************************************************}

class function TBCEditorCDMoveCaret.Create(const AX, AY: Integer;
  const ASelection: Boolean = False): TBytes;
var
  LData: TBCEditorCDMoveCaret;
begin
  LData.X := AX;
  LData.Y := AY;
  LData.Selection := ASelection;
  Result := BytesOf(@LData, SizeOf(LData));
end;

{ TBCEditorCDPosition *********************************************************}

class function TBCEditorCDPosition.Create(const APosition: TBCEditorLinesPosition;
  const ASelection: Boolean = False): TBytes;
var
  LData: TBCEditorCDPosition;
begin
  LData.Pos := APosition;
  LData.Selection := ASelection;
  Result := BytesOf(@LData, SizeOf(LData));
end;

{ TBCEditorCDReplace **********************************************************}

class function TBCEditorCDReplace.Create(const APattern, AReplaceText: string;
  const AOptions: TBCEditorReplaceOptions): TBytes;
var
  LData: PBCEditorCDReplace;
  LSize: Int64;
begin
  LSize := SizeOf(TBCEditorCDReplace) + Length(APattern) * SizeOf(Char) + Length(AReplaceText) * SizeOf(Char);
  SetLength(Result, LSize);
  LData := @Result[0];
  LData^.Options := AOptions;
  LData^.FPattern := @PAnsiChar(LData)[SizeOf(TBCEditorCDReplace)];
  LData^.FPatternLength := Length(APattern);
  LData^.FReplaceText := @PAnsiChar(LData)[SizeOf(TBCEditorCDReplace) + Length(APattern) * SizeOf(Char)];
  LData^.FReplaceTextLength := Length(AReplaceText);
  MoveMemory(LData^.FPattern, PChar(APattern), Length(APattern) * SizeOf(Char));
  MoveMemory(LData^.FReplaceText, PChar(AReplaceText), Length(AReplaceText) * SizeOf(Char));
end;

function TBCEditorCDReplace.GetPattern(): string;
begin
  SetString(Result, FPattern, FPatternLength);
end;

function TBCEditorCDReplace.GetReplaceText(): string;
begin
  SetString(Result, FReplaceText, FReplaceTextLength);
end;

{ TBCEditorCDScrollTo ********************************************************}

class function TBCEditorCDScrollTo.Create(const APos: TPoint): TBytes;
var
  LData: TBCEditorCDScrollTo;
begin
  LData.Pos := APos;
  Result := BytesOf(@LData, SizeOf(LData));
end;

{ TBCEditorCDSelection ********************************************************}

class function TBCEditorCDSelection.Create(const ACaretPosition: TBCEditorLinesPosition;
  const ASelArea: TBCEditorLinesArea): TBytes;
var
  LData: TBCEditorCDSelection;
begin
  LData.CaretPos := ACaretPosition;
  LData.BeginPos := ASelArea.BeginPosition;
  LData.EndPos := ASelArea.EndPosition;
  Result := BytesOf(@LData, SizeOf(LData));
end;

{ TBCEditorCDText *************************************************************}

class function TBCEditorCDText.Create(const AText: string; const ADelete: Boolean = False;
  const ASelection: Boolean = False): TBytes;
var
  LData: PBCEditorCDText;
  LSize: Int64;
begin
  LSize := SizeOf(TBCEditorCDText) + Length(AText) * SizeOf(Char);
  SetLength(Result, LSize);
  LData := @Result[0];
  LData^.Delete := ADelete;
  LData^.Selection := ASelection;
  LData^.FText := @PAnsiChar(LData)[SizeOf(TBCEditorCDText)];
  LData^.FTextLength := Length(AText);
  MoveMemory(LData^.FText, PChar(AText), Length(AText) * SizeOf(Char));
end;

function TBCEditorCDText.GetText(): string;
begin
  SetString(Result, FText, FTextLength);
end;

{ TBCEditorHookedCommandHandler ***********************************************}

class operator TBCEditorHookedCommandHandler.Equal(a, b: TBCEditorHookedCommandHandler): Boolean;
begin
  Result := (a.HandlerData = b.HandlerData)
    and (TMethod(a.ObjectProc) = TMethod(b.ObjectProc))
    and (Pointer((@a.Proc)^) = Pointer((@b.Proc)^));
end;

{ TBCEditorCommands ***********************************************************}

procedure TBCEditorCommands.AddShortCut(const ACommand: TBCEditorCommand; const AShortCut: TShortCut);
begin

end;

function TBCEditorCommands.CommandToCommandClass(const ACommand: TBCEditorCommand;
  out ACommandCategory: TBCEditorCommandCategory): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ACommand);
  Result := LIndex >= 0;
  if (Result) then
    ACommandCategory := FCommands[LIndex].CommandCategory;
end;

function TBCEditorCommands.CommandToIdent(const ACommand: TBCEditorCommand; out AIdent: string): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ACommand);
  Result := (LIndex >= 0) and (Length(FCommands[LIndex].ShortCuts) > 0);
  if (Result) then
  begin
    AIdent := FCommands[LIndex].Ident;
    if (AIdent = '') then
      AIdent := IntToStr(Ord(ACommand));
  end;
end;

function TBCEditorCommands.CommandToShortCut(const ACommand: TBCEditorCommand;
  out AShortCut: TShortCut): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ACommand);
  Result := (LIndex >= 0) and (Length(FCommands[LIndex].ShortCuts) > 0);
  if (Result) then
    AShortCut := FCommands[LIndex].ShortCuts[0];
end;

constructor TBCEditorCommands.Create();
begin
  inherited;

  FCommands := TList<TCommand>.Create();
  ResetShortCuts();
end;

destructor TBCEditorCommands.Destroy();
begin
  FCommands.Free();

  inherited;
end;

function TBCEditorCommands.EnabledWhileRecording(const ACommand: TBCEditorCommand): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ACommand);
  if (LIndex < 0) then
    Result := False
  else
    Result := FCommands[LIndex].EnabledWhileRecording;
end;

function TBCEditorCommands.IndexOf(const ACommand: TBCEditorCommand): Integer;
begin
  if (InsertIndex(ACommand, Result)) then
    Result := -1;
end;

function TBCEditorCommands.IndexOf(const AShortCut: TShortCut): Integer;
var
  LIndex: Integer;
  LShortCutIndex: Integer;
begin
  for LIndex := 0 to FCommands.Count - 1 do
    for LShortCutIndex := 0 to Length(FCommands[LIndex].ShortCuts) - 1 do
      if (FCommands[LIndex].ShortCuts[LShortCutIndex] = AShortCut) then
        Exit(LIndex);
  Result := -1;
end;

function TBCEditorCommands.InsertIndex(const ACommand: TBCEditorCommand; out AIndex: Integer): Boolean;
type
  Tstrcmp = function(lpString1, lpString2: PWideChar): Integer; stdcall;
var
  LLeft: Integer;
  LMid: Integer;
  LRight: Integer;
begin
  Result := True;

  if ((FCommands.Count = 0) or (ACommand > FCommands[FCommands.Count - 1].Command)) then
    AIndex := FCommands.Count
  else
  begin
    LLeft := 0;
    LRight := FCommands.Count - 1;
    while (LLeft <= LRight) do
    begin
      LMid := (LRight + LLeft) div 2;
      case (Sign(Ord(FCommands[FCommands.Count - 1].Command) - Ord(ACommand))) of
        -1: begin LLeft := LMid + 1;  AIndex := LMid + 1; end;
        0: begin Result := False; AIndex := LMid; break; end;
        1: begin LRight := LMid - 1; AIndex := LMid; end;
      end;
    end;
  end;
end;

function TBCEditorCommands.Recordable(const ACommand: TBCEditorCommand): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ACommand);
  if (LIndex < 0) then
    Result := False
  else
    Result := FCommands[LIndex].Recordable;
end;

procedure TBCEditorCommands.RegisterCommand(const ACommand: TBCEditorCommand;
  const ACommandCategory: TBCEditorCommandCategory; const AIdent: string;
  const AShortCut: TShortCut = 0; const AEnabledWhileRecording: Boolean = True;
  const ARecordable: Boolean = True);
var
  LCommand: TCommand;
  LIndex: Integer;
  LInsertIndex: Integer;
begin
  LCommand.Command := ACommand;
  LCommand.CommandCategory := ACommandCategory;
  LCommand.Ident := AIdent;
  LCommand.EnabledWhileRecording := AEnabledWhileRecording;
  LCommand.Recordable := ARecordable;

  if (InsertIndex(ACommand, LInsertIndex)) then
    FCommands.Insert(LInsertIndex, LCommand)
  else
    FCommands.List[LInsertIndex] := LCommand;

  if (AShortCut > 0) then
  begin
    repeat
      LIndex := IndexOf(AShortCut);
      if (LIndex >= 0) then
        SetLength(FCommands.List[LIndex].ShortCuts, 0);
    until (LIndex < 0);

    SetLength(FCommands.List[LInsertIndex].ShortCuts, 1);
    FCommands.List[LInsertIndex].ShortCuts[0] := AShortCut;
  end;
end;

procedure TBCEditorCommands.ResetShortCuts();
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
  RegisterCommand(ecScrollTo, eccScroll, 'ecScrollUp', 0, True, False);
  RegisterCommand(ecScrollUp, eccScroll, 'ecScrollUp', ShortCut(VK_UP, [ssCtrl]), True, False);

  RegisterCommand(ecBOL, eccMoveCaret, 'ecBOL', ShortCut(VK_HOME, []));
  RegisterCommand(ecBOF, eccMoveCaret, 'ecBOF', ShortCut(VK_HOME, [ssCtrl]));
  RegisterCommand(ecBOP, eccMoveCaret, 'ecBOP', ShortCut(VK_PRIOR, [ssCtrl]), False);
  RegisterCommand(ecDown, eccMoveCaret, 'ecDown', ShortCut(VK_DOWN, []));
  RegisterCommand(ecEOF, eccMoveCaret, 'ecEOF', ShortCut(VK_END, [ssCtrl]));
  RegisterCommand(ecEOL, eccMoveCaret, 'ecEOL', ShortCut(VK_END, []));
  RegisterCommand(ecEOP, eccMoveCaret, 'ecEOP', ShortCut(VK_NEXT, [ssCtrl]), False);
  RegisterCommand(ecFindBackward, eccMoveCaret, 'ecFindBackward');
  RegisterCommand(ecFindFirst, eccMoveCaret, 'ecFindFirst');
  RegisterCommand(ecFindForeward, eccMoveCaret, 'ecFindForeward');
  RegisterCommand(ecFindNext, eccMoveCaret, 'ecFindNext', ShortCut(VK_F3, []));
  RegisterCommand(ecFindPrevious, eccMoveCaret, 'ecFindPrevious', ShortCut(VK_F3, [ssShift]));
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

  RegisterCommand(ecBackspace, eccText, 'ecBackspace', ShortCut(VK_BACK, []));
  AddShortCut(ecBackspace, ShortCut(VK_BACK, [ssShift]));
  AddShortCut(ecBackspace, ShortCut(VK_BACK, [ssCtrl]));
  RegisterCommand(ecBlockComment, eccText, 'ecBlockComment', ShortCut(VK_OEM_2, [ssCtrl, ssShift]));
  RegisterCommand(ecBlockIndent, eccText, 'ecBlockIndent', ShortCut(Ord('I'), [ssCtrl, ssShift]));
  RegisterCommand(ecBlockUnindent, eccText, 'ecBlockUnindent', ShortCut(Ord('U'), [ssCtrl, ssShift]));
  RegisterCommand(ecChar, eccText, 'ecChar');
  RegisterCommand(ecClear, eccText, 'ecClear');
  RegisterCommand(ecDeleteToBOL, eccText, 'ecDeleteToBOL');
  RegisterCommand(ecDeleteChar, eccText, 'ecDeleteChar', ShortCut(VK_DELETE, []));
  RegisterCommand(ecDeleteToEOL, eccText, 'ecDeleteToEOL', ShortCut(Ord('Y'), [ssCtrl, ssShift]));
  RegisterCommand(ecDeleteLastWord, eccText, 'ecDeleteLastWord', ShortCut(VK_BACK, [ssCtrl]));
  RegisterCommand(ecDeleteLine, eccText, 'ecDeleteLine', ShortCut(Ord('Y'), [ssCtrl]));
  RegisterCommand(ecDeleteWord, eccText, 'ecDeleteWord', ShortCut(Ord('T'), [ssCtrl]));
  AddShortCut(ecDeleteWord, ShortCut(VK_DELETE, [ssCtrl]));
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

function TBCEditorCommands.ShortCutToCommand(const AShortCut: TShortCut;
  out ACommand: TBCEditorCommand): Boolean;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AShortCut);
  Result := LIndex >= 0;
  if (Result) then
    ACommand := FCommands[LIndex].Command;
end;

{ TCustomBCEditorMacroRecorder ************************************************}

procedure TCustomBCEditorMacroRecorder.AddCommand(const ACommand: TBCEditorCommand; const AData: TBytes);
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
  const ACommand: TBCEditorCommand; const AData: TBytes; var AHandled: Boolean);
begin
  if (ABefore) then
  begin
    case (ACommand) of
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
          Step();
          AHandled := True;
        end;
      ecSyncEdit:
        AHandled := True;
      else
        if ((FState = msRecording) and not GBCEditorCommands.EnabledWhileRecording(ACommand)) then
        begin
          MessageBeep($FFFFFFFF);
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
          if ((State = msRecording) and GBCEditorCommands.Recordable(ACommand)) then
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

function TCustomBCEditorMacroRecorder.GetIsEmpty(): Boolean;
begin
  Result := FItems.Count = 0;
end;

procedure TCustomBCEditorMacroRecorder.InsertCommand(const AIndex: Integer;
  const ACommand: TBCEditorCommand; const AData: TBytes);
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
    raise ERangeError.Create(SBCEditorCannotPlay);

  SetState(msPlaying);
  try
    while (FState = msPlaying) do
      Step();
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
    raise ERangeError.Create(SBCEditorNotTCustomBCEditor);

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
    FState := AValue;
    if Assigned(OnStateChange) then
      OnStateChange(Self);
  end;
end;

procedure TCustomBCEditorMacroRecorder.StartRecord();
begin
  if (FState in [msRecording, msPlaying]) then
    raise ERangeError.Create(SBCEditorCannotRecord);

  Clear();
  SetState(msRecording);
end;

function TCustomBCEditorMacroRecorder.Step(): Boolean;
begin
  Result := Step(False);
end;

function TCustomBCEditorMacroRecorder.Step(const APlay: Boolean): Boolean;
var
  LData: TBytes;
begin
  if ((FState = msRecording) or not APlay and (FState = msPlaying)) then
    raise ERangeError.Create(SBCEditorCannotPlay);

  Result := FCurrentCommand < FItems.Count;
  if (Result) then
  begin
    if (FItems[FCurrentCommand].DataSize = 0) then
      LData := nil
    else
      LData := BytesOf(@PAnsiChar(FData.Memory)[FItems[FCurrentCommand].DataPosition], FItems[FCurrentCommand].DataSize);
    TCustomBCEditor(FEditor).ProcessCommand(FItems[FCurrentCommand].Command, LData);
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
  GBCEditorCommands := TBCEditorCommands.Create();
finalization
  GBCEditorCommands.Free();
end.
