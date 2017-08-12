unit BCEditor.Commands;

interface {********************************************************************}

uses
  Classes, SysUtils, Types, Generics.Collections,
  Menus,
  BCEditor.Types;

const
  ecNone = 0;
  ecEditCommandFirst = 501;
  ecEditCommandLast = 1000;
  { Caret moving }
  ecLeft = 1;
  ecRight = 2;
  ecUp = 3;
  ecDown = 4;
  ecWordLeft = 5;
  ecWordRight = 6;
  ecLineBegin = 7;
  ecLineEnd = 8;
  ecPageUp = 9;
  ecPageDown = 10;
  ecPageTop = 13;
  ecPageBottom = 14;
  ecEditorTop = 15;
  ecEditorBottom = 16;
  ecShowGotoLine = 17;
  ecPosition = 20;
  { Selection }
  ecSelection = 100;
  ecSelectionLeft = ecLeft + ecSelection;
  ecSelectionRight = ecRight + ecSelection;
  ecSelectionUp = ecUp + ecSelection;
  ecSelectionDown = ecDown + ecSelection;
  ecSelectionWordLeft = ecWordLeft + ecSelection;
  ecSelectionWordRight = ecWordRight + ecSelection;
  ecSelectionLineBegin = ecLineBegin + ecSelection;
  ecSelectionLineEnd = ecLineEnd + ecSelection;
  ecSelectionPageUp = ecPageUp + ecSelection;
  ecSelectionPageDown = ecPageDown + ecSelection;
  ecSelectionPageTop = ecPageTop + ecSelection;
  ecSelectionPageBottom = ecPageBottom + ecSelection;
  ecSelectionEditorTop = ecEditorTop + ecSelection;
  ecSelectionEditorBottom = ecEditorBottom + ecSelection;
  ecSelectionWord = ecSelection + 21;
  ecSelectAll = ecSelection + 22;
  ecUnselect = ecSelection + 23;
  { Scrolling }
  ecScrollUp = 211;
  ecScrollDown = 212;
  ecScrollLeft = 213;
  ecScrollRight = 214;
  { Mode }
  ecInsertMode = 221;
  ecOverwriteMode = 222;
  ecToggleMode = 223;
  { Bookmark }
  ecGotoBookmark1 = 310;
  ecGotoBookmark2 = 311;
  ecGotoBookmark3 = 312;
  ecGotoBookmark4 = 313;
  ecGotoBookmark5 = 314;
  ecGotoBookmark6 = 315;
  ecGotoBookmark7 = 316;
  ecGotoBookmark8 = 317;
  ecGotoBookmark9 = 318;
  ecGotoBookmark0 = 319;
  ecSetBookmark1 = 320;
  ecSetBookmark2 = 321;
  ecSetBookmark3 = 322;
  ecSetBookmark4 = 323;
  ecSetBookmark5 = 324;
  ecSetBookmark6 = 325;
  ecSetBookmark7 = 326;
  ecSetBookmark8 = 327;
  ecSetBookmark9 = 328;
  ecSetBookmark0 = 329;
  ecGotoNextBookmark = 330;
  ecGotoPreviousBookmark = 331;
  { CompletionProposal }
  ecShowCompletionProposal = 470;
  { Deletion }
  ecBackspace = 501;
  ecDeleteChar = 502;
  ecDeleteWord = 503;
  ecDeleteLastWord = 504;
  ecDeleteBeginningOfLine = 505;
  ecDeleteEndOfLine = 506;
  ecDeleteLine = 507;
  ecClear = 508;
  { Insert }
  ecReturn = 509;
  ecInsertLine = 510;
  ecChar = 511;
  ecText = 512;
  { Clipboard }
  ecUndo = 601;
  ecRedo = 602;
  ecCopy = 603;
  ecCut = 604;
  ecPaste = 605;
  { Indent }
  ecBlockIndent = 610;
  ecBlockUnindent = 611;
  ecTab = 612;
  ecShiftTab = 613;
  { Case }
  ecUpperCase = 620;
  ecLowerCase = 621;
  { SyncEdit }
  ecSyncEdit = 701;
  { MacroRecorder }
  ecRecordMacro = 720;
  ecStopMacro = 721;
  ecPlaybackMacro = 722;
  ecStepMacro = 723;
  { Search }
  ecShowFind = 801;
  ecFindFirst = 802;
  ecFindNext = 803;
  ecFindPrevious = 804;
  ecFindForeward = 805;
  ecFindBackward = 806;
  ecShowReplace = 807;
  ecReplace = 808;
  { Comments }
  ecLineComment = 900;
  ecBlockComment = 901;

  ecUserFirst = 1001;

type
  TBCEditorCommand = type Word;


  { Command Data }

  PBCEditorCD = ^TBCEditorCD;
  TBCEditorCD = packed record
    Size: Int64; // Must be the first in EVERY command data!
  end;

  PBCEditorCDChar = ^TBCEditorCDChar;
  TBCEditorCDChar = packed record
    Size: Int64;
    Char: Char;
    Count: Int64;
  end;

  PBCEditorCDFind = ^TBCEditorCDFind;
  TBCEditorCDFind = packed record
    Size: Int64;
    Options: TBCEditorFindOptions;
  private
    FPattern: PChar;
    FPatternLength: Int64;
    function GetPattern(): string;
  public
    class function Create(const APattern: string;
      const AOptions: TBCEditorFindOptions): PBCEditorCDFind; static;
    procedure Free();
    property Pattern: string read GetPattern;
  end;

  PBCEditorCDItem = ^TBCEditorCDItem;
  TBCEditorCDItem = packed record
    Size: Int64;
    Item: Integer;
    Selection: Boolean;
  end;

  PBCEditorCDPosition = ^TBCEditorCDPosition;
  TBCEditorCDPosition = packed record
    Size: Int64;
    Pos: TPoint;
    Selection: Boolean;
  end;

  PBCEditorCDReplace = ^TBCEditorCDReplace;
  TBCEditorCDReplace = packed record
    Size: Int64;
    Options: TBCEditorReplaceOptions;
    Pattern: PChar;
    PatternLength: Int64;
    ReplaceText: PChar;
    ReplaceTextLength: Int64;
    class function Create(const APattern, AReplaceText: string;
      const AOptions: TBCEditorReplaceOptions): PBCEditorCDReplace; static;
    class procedure Decode(const AData: PBCEditorCDReplace; var APattern, AReplaceText: string;
      var AOptions: TBCEditorReplaceOptions); static;
    procedure Free();
  end;

  PBCEditorCDSelection = ^TBCEditorCDSelection;
  TBCEditorCDSelection = packed record
    Size: Int64;
    CaretPos: TPoint;
    BeginPos: TPoint;
    EndPos: TPoint;
  end;

  PBCEditorCDText = ^TBCEditorCDText;
  TBCEditorCDText = packed record
    Size: Int64;
    Selection: Boolean;
  private
    FText: PChar;
    FTextLength: Int64;
    function GetText(): string;
  public
    class function Create(const AText: string; const ASelection: Boolean): PBCEditorCDText; static;
    procedure Free();
    property Text: string read GetText;
  end;

  TBCEditorHookedCommandProc = procedure(const AEditor: Pointer; const ABefore: LongBool;
    const ACommand: Integer; const AData: PBCEditorCD; var AHandled: LongBool; const AHandlerData: Pointer); stdcall;
  TBCEditorHookedCommandObjectProc = procedure(ASender: TObject; const ABefore: Boolean;
    const ACommand: TBCEditorCommand; const AData: PBCEditorCD; var AHandled: Boolean) of object;
  TBCEditorProcessCommandEvent = procedure(ASender: TObject; const ACommand: TBCEditorCommand; const AData: PBCEditorCD; var AAllowed: Boolean) of object;

  TBCEditorHookedCommandHandler = record
    HandlerData: Pointer;
    ObjectProc: TBCEditorHookedCommandObjectProc;
    Proc: TBCEditorHookedCommandProc;
    class operator Equal(a, b: TBCEditorHookedCommandHandler): Boolean; inline;
  end;

  TBCEditorKeyCommand = class(TCollectionItem)
  strict private
    FCommand: TBCEditorCommand;
    FKey: Word;
    FSecondaryKey: Word;
    FSecondaryShiftState: TShiftState;
    FShiftState: TShiftState;
    function GetSecondaryShortCut: TShortCut;
    function GetShortCut: TShortCut;
    procedure SetCommand(const AValue: TBCEditorCommand);
    procedure SetKey(const AValue: Word);
    procedure SetSecondaryKey(const AValue: Word);
    procedure SetSecondaryShiftState(const AValue: TShiftState);
    procedure SetSecondaryShortCut(const AValue: TShortCut);
    procedure SetShiftState(const AValue: TShiftState);
    procedure SetShortCut(const AValue: TShortCut);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(ASource: TPersistent); override;
    property Key: Word read FKey write SetKey;
    property SecondaryKey: Word read FSecondaryKey write SetSecondaryKey;
    property SecondaryShiftState: TShiftState read FSecondaryShiftState write SetSecondaryShiftState;
    property ShiftState: TShiftState read FShiftState write SetShiftState;
  published
    property Command: TBCEditorCommand read FCommand write SetCommand;
    property SecondaryShortCut: TShortCut read GetSecondaryShortCut write SetSecondaryShortCut default 0;
    property ShortCut: TShortCut read GetShortCut write SetShortCut default 0;
  end;

  TBCEditorCommands = class(TCollection)
  strict private
    FOwner: TPersistent;
    function GetItem(AIndex: Integer): TBCEditorKeyCommand;
    procedure SetItem(AIndex: Integer; AValue: TBCEditorKeyCommand);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    procedure Add(const ACommand: TBCEditorCommand; const AShift: TShiftState; const AKey: Word);
    procedure Assign(ASource: TPersistent); override;
    function FindCommand(ACommand: TBCEditorCommand): Integer;
    function FindKeyCode(AKeyCode: Word; AShift: TShiftState): Integer;
    function FindKeyCodes(AKeyCode: Word; AShift: TShiftState; ASecondaryKeycode: Word; ASecondaryShift: TShiftState): Integer;
    function FindShortcut(AShortCut: TShortCut): Integer;
    function FindShortcuts(AShortCut, ASecondaryShortCut: TShortCut): Integer;
    function NewItem: TBCEditorKeyCommand;
    procedure ResetDefaults;
    property Items[AIndex: Integer]: TBCEditorKeyCommand read GetItem write SetItem; default;
  end;

function IdentToEditorCommand(const AIdent: string; var ACommand: LongInt): Boolean;
function EditorCommandToIdent(ACommand: LongInt; var AIdent: string): Boolean;

implementation {***************************************************************}

uses
  Windows,
  SysConst,
  BCEditor.Language;

resourcestring
  SBCEditorDuplicateShortcut = 'Shortcut already exists';

type
  TBCEditorCommandString = record
    Value: TBCEditorCommand;
    Name: string;
  end;

const
  EditorCommandStrings: array [0 .. 101] of TBCEditorCommandString = (
    (Value: ecNone; Name: 'ecNone'),
    (Value: ecLeft; Name: 'ecLeft'),
    (Value: ecRight; Name: 'ecRight'),
    (Value: ecUp; Name: 'ecUp'),
    (Value: ecDown; Name: 'ecDown'),
    (Value: ecWordLeft; Name: 'ecWordLeft'),
    (Value: ecWordRight; Name: 'ecWordRight'),
    (Value: ecLineBegin; Name: 'ecLineBegin'),
    (Value: ecLineEnd; Name: 'ecLineEnd'),
    (Value: ecPageUp; Name: 'ecPageUp'),
    (Value: ecPageDown; Name: 'ecPageDown'),
    (Value: ecPageTop; Name: 'ecPageTop'),
    (Value: ecPageBottom; Name: 'ecPageBottom'),
    (Value: ecEditorTop; Name: 'ecEditorTop'),
    (Value: ecEditorBottom; Name: 'ecEditorBottom'),
    (Value: ecShowGotoLine; Name: 'ecShowGotoLine'),
    (Value: ecPosition; Name: 'ecPosition'),
    (Value: ecSelection; Name: 'ecSelection'),
    (Value: ecSelectionLeft; Name: 'ecSelectionLeft'),
    (Value: ecSelectionRight; Name: 'ecSelectionRight'),
    (Value: ecSelectionUp; Name: 'ecSelectionUp'),
    (Value: ecSelectionDown; Name: 'ecSelectionDown'),
    (Value: ecSelectionWordLeft; Name: 'ecSelectionWordLeft'),
    (Value: ecSelectionWordRight; Name: 'ecSelectionWordRight'),
    (Value: ecSelectionLineBegin; Name: 'ecSelectionLineBegin'),
    (Value: ecSelectionLineEnd; Name: 'ecSelectionLineEnd'),
    (Value: ecSelectionPageUp; Name: 'ecSelectionPageUp'),
    (Value: ecSelectionPageDown; Name: 'ecSelectionPageDown'),
    (Value: ecSelectionPageTop; Name: 'ecSelectionPageTop'),
    (Value: ecSelectionPageBottom; Name: 'ecSelectionPageBottom'),
    (Value: ecSelectionEditorTop; Name: 'ecSelectionEditorTop'),
    (Value: ecSelectionEditorBottom; Name: 'ecSelectionEditorBottom'),
    (Value: ecSelectionWord; Name: 'ecSelectionWord'),
    (Value: ecSelectAll; Name: 'ecSelectAll'),
    (Value: ecScrollUp; Name: 'ecScrollUp'),
    (Value: ecScrollDown; Name: 'ecScrollDown'),
    (Value: ecScrollLeft; Name: 'ecScrollLeft'),
    (Value: ecScrollRight; Name: 'ecScrollRight'),
    (Value: ecInsertMode; Name: 'ecInsertMode'),
    (Value: ecOverwriteMode; Name: 'ecOverwriteMode'),
    (Value: ecToggleMode; Name: 'ecToggleMode'),
    (Value: ecGotoBookmark1; Name: 'ecGotoBookmark1'),
    (Value: ecGotoBookmark2; Name: 'ecGotoBookmark2'),
    (Value: ecGotoBookmark3; Name: 'ecGotoBookmark3'),
    (Value: ecGotoBookmark4; Name: 'ecGotoBookmark4'),
    (Value: ecGotoBookmark5; Name: 'ecGotoBookmark5'),
    (Value: ecGotoBookmark6; Name: 'ecGotoBookmark6'),
    (Value: ecGotoBookmark7; Name: 'ecGotoBookmark7'),
    (Value: ecGotoBookmark8; Name: 'ecGotoBookmark8'),
    (Value: ecGotoBookmark9; Name: 'ecGotoBookmark9'),
    (Value: ecGotoBookmark0; Name: 'ecGotoBookmark9'),
    (Value: ecSetBookmark1; Name: 'ecSetBookmark1'),
    (Value: ecSetBookmark2; Name: 'ecSetBookmark2'),
    (Value: ecSetBookmark3; Name: 'ecSetBookmark3'),
    (Value: ecSetBookmark4; Name: 'ecSetBookmark4'),
    (Value: ecSetBookmark5; Name: 'ecSetBookmark5'),
    (Value: ecSetBookmark6; Name: 'ecSetBookmark6'),
    (Value: ecSetBookmark7; Name: 'ecSetBookmark7'),
    (Value: ecSetBookmark8; Name: 'ecSetBookmark8'),
    (Value: ecSetBookmark9; Name: 'ecSetBookmark9'),
    (Value: ecSetBookmark0; Name: 'ecSetBookmark9'),
    (Value: ecGotoNextBookmark; Name: 'ecGotoNextBookmark'),
    (Value: ecGotoPreviousBookmark; Name: 'ecGotoPreviousBookmark'),
    (Value: ecBackspace; Name: 'ecBackspace'),
    (Value: ecDeleteChar; Name: 'ecDeleteChar'),
    (Value: ecDeleteWord; Name: 'ecDeleteWord'),
    (Value: ecDeleteLastWord; Name: 'ecDeleteLastWord'),
    (Value: ecDeleteBeginningOfLine; Name: 'ecDeleteBeginningOfLine'),
    (Value: ecDeleteEndOfLine; Name: 'ecDeleteEndOfLine'),
    (Value: ecDeleteLine; Name: 'ecDeleteLine'),
    (Value: ecClear; Name: 'ecClear'),
    (Value: ecReturn; Name: 'ecReturn'),
    (Value: ecInsertLine; Name: 'ecInsertLine'),
    (Value: ecChar; Name: 'ecChar'),
    (Value: ecText; Name: 'ecText'),
    (Value: ecUndo; Name: 'ecUndo'),
    (Value: ecRedo; Name: 'ecRedo'),
    (Value: ecCopy; Name: 'ecCopy'),
    (Value: ecCut; Name: 'ecCut'),
    (Value: ecPaste; Name: 'ecPaste'),
    (Value: ecBlockIndent; Name: 'ecBlockIndent'),
    (Value: ecBlockUnindent; Name: 'ecBlockUnindent'),
    (Value: ecTab; Name: 'ecTab'),
    (Value: ecShiftTab; Name: 'ecShiftTab'),
    (Value: ecUpperCase; Name: 'ecUpperCase'),
    (Value: ecLowerCase; Name: 'ecLowerCase'),
    (Value: ecSyncEdit; Name: 'ecSyncEdit'),
    (Value: ecRecordMacro; Name: 'ecRecordMacro'),
    (Value: ecStopMacro; Name: 'ecStopMacro'),
    (Value: ecPlaybackMacro; Name: 'ecPlaybackMacro'),
    (Value: ecStepMacro; Name: 'ecStepMacro'),
    (Value: ecShowFind; Name: 'ecShowFind'),
    (Value: ecFindFirst; Name: 'ecFindFirst'),
    (Value: ecFindNext; Name: 'ecFindNext'),
    (Value: ecFindPrevious; Name: 'ecFindPrevious'),
    (Value: ecFindForeward; Name: 'ecFindForeward'),
    (Value: ecFindBackward; Name: 'ecFindBackward'),
    (Value: ecShowReplace; Name: 'ecShowReplace'),
    (Value: ecReplace; Name: 'ecReplace'),
    (Value: ecLineComment; Name: 'ecLineComment'),
    (Value: ecBlockComment; Name: 'ecBlockComment'),
    (Value: ecShowCompletionProposal; Name: 'ecShowCompletionProposal')
  );

function IdentToEditorCommand(const AIdent: string; var ACommand: LongInt): Boolean;
var
  LIndex: Integer;
  LCommandString: TBCEditorCommandString;
begin
  Result := True;

  for LIndex := Low(EditorCommandStrings) to High(EditorCommandStrings) do
  begin
    LCommandString := EditorCommandStrings[LIndex];
    if CompareText(LCommandString.Name, AIdent) = 0 then
    begin
      ACommand := LCommandString.Value;
      Exit;
    end;
  end;

  Result := False;
end;

function EditorCommandToIdent(ACommand: LongInt; var AIdent: string): Boolean;
var
  LIndex: Integer;
  LCommandString: TBCEditorCommandString;
begin
  Result := True;

  for LIndex := Low(EditorCommandStrings) to High(EditorCommandStrings) do
  begin
    LCommandString := EditorCommandStrings[LIndex];
    if LCommandString.Value = ACommand then
    begin
      AIdent := LCommandString.Name;
      Exit;
    end;
  end;

  AIdent := IntToStr(ACommand);
  Result := False;
end;

function EditorCommandToCodeString(ACommand: TBCEditorCommand): string;
begin
  if not EditorCommandToIdent(ACommand, Result) then
    Result := IntToStr(ACommand);
end;

{ TBCEditorCDText *************************************************************}

class function TBCEditorCDText.Create(const AText: string; const ASelection: Boolean): PBCEditorCDText;
var
  LSize: Int64;
begin
  LSize := SizeOf(TBCEditorCDText) + Length(AText) * SizeOf(Char);
  GetMem(Result, LSize);
  Result^.Size := LSize;
  Result^.Selection := ASelection;
  Result^.FText := @PAnsiChar(Result)[SizeOf(TBCEditorCDText)];
  Result^.FTextLength := Length(AText);
  MoveMemory(Result^.FText, PChar(AText), Length(AText) * SizeOf(Char));
end;

procedure TBCEditorCDText.Free();
begin
  FreeMem(@Self);
end;

function TBCEditorCDText.GetText(): string;
begin
  SetString(Result, FText, FTextLength);
end;

{ TBCEditorCDFind *************************************************************}

class function TBCEditorCDFind.Create(const APattern: string;
  const AOptions: TBCEditorFindOptions): PBCEditorCDFind;
var
  LSize: Int64;
begin
  LSize := SizeOf(TBCEditorCDFind) + Length(APattern) * SizeOf(Char);
  GetMem(Result, LSize);
  Result^.Size := LSize;
  Result^.Options := AOptions;
  Result^.FPattern := @PAnsiChar(Result)[SizeOf(TBCEditorCDFind)];
  Result^.FPatternLength := Length(APattern);
  MoveMemory(Result^.FPattern, PChar(APattern), Length(APattern) * SizeOf(Char));
end;

procedure TBCEditorCDFind.Free();
begin
  FreeMem(@Self);
end;

function TBCEditorCDFind.GetPattern(): string;
begin
  SetString(Result, FPattern, FPatternLength);
end;

{ TBCEditorCDReplace **********************************************************}

class procedure TBCEditorCDReplace.Decode(const AData: PBCEditorCDReplace;
  var APattern, AReplaceText: string; var AOptions: TBCEditorReplaceOptions);
begin
  if (not Assigned(AData) or (AData^.Size < SizeOf(TBCEditorCDReplace))) then
    raise ERangeError.Create(SRangeError);

  SetString(APattern, AData^.Pattern, AData^.PatternLength);
  SetString(AReplaceText, AData^.ReplaceText, AData^.ReplaceTextLength);
  AOptions := AData^.Options;
end;

class function TBCEditorCDReplace.Create(const APattern, AReplaceText: string;
  const AOptions: TBCEditorReplaceOptions): PBCEditorCDReplace;
var
  LSize: Int64;
begin
  LSize := SizeOf(TBCEditorCDReplace) + Length(APattern) * SizeOf(Char) + Length(AReplaceText) * SizeOf(Char);
  GetMem(Result, LSize);
  Result^.Size := LSize;
  Result^.Options := AOptions;
  Result^.Pattern := @PAnsiChar(Result)[SizeOf(TBCEditorCDReplace)];
  Result^.PatternLength := Length(APattern);
  Result^.ReplaceText := @PAnsiChar(Result)[SizeOf(TBCEditorCDReplace) + Length(APattern) * SizeOf(Char)];
  Result^.ReplaceTextLength := Length(AReplaceText);
  MoveMemory(Result^.Pattern, PChar(APattern), Length(APattern) * SizeOf(Char));
  MoveMemory(Result^.ReplaceText, PChar(AReplaceText), Length(AReplaceText) * SizeOf(Char));
end;

procedure TBCEditorCDReplace.Free();
begin
  FreeMem(@Self);
end;

{ TBCEditorHookedCommandHandler ***********************************************}

class operator TBCEditorHookedCommandHandler.Equal(a, b: TBCEditorHookedCommandHandler): Boolean;
begin
  Result := (a.HandlerData = b.HandlerData)
    and (TMethod(a.ObjectProc) = TMethod(b.ObjectProc))
    and (Pointer((@a.Proc)^) = Pointer((@b.Proc)^));
end;

{ TBCEditorKeyCommand *********************************************************}

procedure TBCEditorKeyCommand.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorKeyCommand) then
  with ASource as TBCEditorKeyCommand do
  begin
    Self.FCommand := FCommand;
    Self.FKey := FKey;
    Self.FSecondaryKey := FSecondaryKey;
    Self.FShiftState := FShiftState;
    Self.FSecondaryShiftState := FSecondaryShiftState;
  end
  else
    inherited Assign(ASource);
end;

function TBCEditorKeyCommand.GetDisplayName: string;
begin
  Result := EditorCommandToCodeString(Command) + ' - ' + ShortCutToText(ShortCut);
  if SecondaryShortCut <> 0 then
    Result := Result + ' ' + ShortCutToText(SecondaryShortCut);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TBCEditorKeyCommand.GetSecondaryShortCut: TShortCut;
begin
  Result := Menus.ShortCut(SecondaryKey, SecondaryShiftState);
end;

function TBCEditorKeyCommand.GetShortCut: TShortCut;
begin
  Result := Menus.ShortCut(Key, ShiftState);
end;

procedure TBCEditorKeyCommand.SetCommand(const AValue: TBCEditorCommand);
begin
  if FCommand <> AValue then
    FCommand := AValue;
end;

procedure TBCEditorKeyCommand.SetKey(const AValue: Word);
begin
  if FKey <> AValue then
    FKey := AValue;
end;

procedure TBCEditorKeyCommand.SetSecondaryKey(const AValue: Word);
begin
  if FSecondaryKey <> AValue then
    FSecondaryKey := AValue;
end;

procedure TBCEditorKeyCommand.SetSecondaryShiftState(const AValue: TShiftState);
begin
  if FSecondaryShiftState <> AValue then
    FSecondaryShiftState := AValue;
end;

procedure TBCEditorKeyCommand.SetSecondaryShortCut(const AValue: TShortCut);
var
  LNewKey: Word;
  LNewShiftState: TShiftState;
  LDuplicate: Integer;
begin
  if AValue <> 0 then
  begin
    LDuplicate := TBCEditorCommands(Collection).FindShortcuts(ShortCut, AValue);
    if (LDuplicate <> -1) and (LDuplicate <> Self.Index) then
      raise ERangeError.Create(SBCEditOrduplicateShortcut);
  end;

  Menus.ShortCutToKey(AValue, LNewKey, LNewShiftState);
  if (LNewKey <> SecondaryKey) or (LNewShiftState <> SecondaryShiftState) then
  begin
    SecondaryKey := LNewKey;
    SecondaryShiftState := LNewShiftState;
  end;
end;

procedure TBCEditorKeyCommand.SetShiftState(const AValue: TShiftState);
begin
  if FShiftState <> AValue then
    FShiftState := AValue;
end;

procedure TBCEditorKeyCommand.SetShortCut(const AValue: TShortCut);
var
  LNewKey: Word;
  LNewShiftState: TShiftState;
  LDuplicate: Integer;
begin
  if AValue <> 0 then
  begin
    LDuplicate := TBCEditorCommands(Collection).FindShortcuts(AValue, SecondaryShortCut);
    if (LDuplicate <> -1) and (LDuplicate <> Self.Index) then
      raise ERangeError.Create(SBCEditorDuplicateShortcut);
  end;

  ShortCutToKey(AValue, LNewKey, LNewShiftState);

  if (LNewKey <> Key) or (LNewShiftState <> ShiftState) then
  begin
    Key := LNewKey;
    ShiftState := LNewShiftState;
  end;
end;

{ TBCEditorKeyCommands ********************************************************}

procedure TBCEditorCommands.Add(const ACommand: TBCEditorCommand; const AShift: TShiftState; const AKey: Word);
var
  LNewKeystroke: TBCEditorKeyCommand;
begin
  LNewKeystroke := NewItem;
  LNewKeystroke.Key := AKey;
  LNewKeystroke.ShiftState := AShift;
  LNewKeystroke.Command := ACommand;
end;

procedure TBCEditorCommands.Assign(ASource: TPersistent);
var
  LIndex: Integer;
  LKeyCommands: TBCEditorCommands;
begin
  if Assigned(ASource) and (ASource is TBCEditorCommands) then
  begin
    LKeyCommands := ASource as TBCEditorCommands;
    Self.Clear;
    for LIndex := 0 to LKeyCommands.Count - 1 do
      NewItem.Assign(LKeyCommands[LIndex]);
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorCommands.Create(AOwner: TPersistent);
begin
  inherited Create(TBCEditorKeyCommand);

  FOwner := AOwner;
end;

function TBCEditorCommands.FindCommand(ACommand: TBCEditorCommand): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to Count - 1 do
  if Items[LIndex].Command = ACommand then
    Exit(LIndex);
end;

function TBCEditorCommands.FindKeyCode(AKeycode: Word; AShift: TShiftState): Integer;
var
  LIndex: Integer;
  LKeyCommand: TBCEditorKeyCommand;
begin
  Result := -1;
  for LIndex := 0 to Count - 1 do
  begin
    LKeyCommand := Items[LIndex];
    if (LKeyCommand.Key = AKeyCode) and (LKeyCommand.ShiftState = AShift) and (LKeyCommand.SecondaryKey = 0) then
      Exit(LIndex);
  end;
end;

function TBCEditorCommands.FindKeyCodes(AKeyCode: Word; AShift: TShiftState; ASecondaryKeyCode: Word; ASecondaryShift: TShiftState): Integer;
var
  LIndex: Integer;
  LKeyCommand: TBCEditorKeyCommand;
begin
  Result := -1;
  for LIndex := 0 to Count - 1 do
  begin
    LKeyCommand := Items[LIndex];
    if (LKeyCommand.Key = AKeyCode) and (LKeyCommand.ShiftState = AShift) and (LKeyCommand.SecondaryKey = ASecondaryKeyCode) and
      (LKeyCommand.SecondaryShiftState = ASecondaryShift) then
      Exit(LIndex);
  end;
end;

function TBCEditorCommands.FindShortcut(AShortCut: TShortCut): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to Count - 1 do
  if Items[LIndex].ShortCut = AShortCut then
    Exit(LIndex);
end;

function TBCEditorCommands.FindShortcuts(AShortCut, ASecondaryShortCut: TShortCut): Integer;
var
  LIndex: Integer;
  LKeyCommand: TBCEditorKeyCommand;
begin
  Result := -1;
  for LIndex := 0 to Count - 1 do
  begin
    LKeyCommand := Items[LIndex];
    if (LKeyCommand.ShortCut = AShortCut) and (LKeyCommand.SecondaryShortCut = ASecondaryShortCut) then
      Exit(LIndex);
  end;
end;

function TBCEditorCommands.GetItem(AIndex: Integer): TBCEditorKeyCommand;
begin
  Result := TBCEditorKeyCommand(inherited GetItem(AIndex));
end;

function TBCEditorCommands.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorCommands.NewItem(): TBCEditorKeyCommand;
begin
  Result := TBCEditorKeyCommand(inherited Add);
end;

procedure TBCEditorCommands.ResetDefaults();
begin
  Clear;

  { Scrolling, caret moving and selection }
  Add(ecUp, [], VK_UP);
  Add(ecSelectionUp, [ssShift], VK_UP);
  Add(ecScrollUp, [ssCtrl], VK_UP);
  Add(ecDown, [], VK_DOWN);
  Add(ecSelectionDown, [ssShift], VK_DOWN);
  Add(ecScrollDown, [ssCtrl], VK_DOWN);
  Add(ecLeft, [], VK_LEFT);
  Add(ecSelectionLeft, [ssShift], VK_LEFT);
  Add(ecWordLeft, [ssCtrl], VK_LEFT);
  Add(ecSelectionWordLeft, [ssShift, ssCtrl], VK_LEFT);
  Add(ecRight, [], VK_RIGHT);
  Add(ecSelectionRight, [ssShift], VK_RIGHT);
  Add(ecWordRight, [ssCtrl], VK_RIGHT);
  Add(ecSelectionWordRight, [ssShift, ssCtrl], VK_RIGHT);
  Add(ecPageDown, [], VK_NEXT);
  Add(ecSelectionPageDown, [ssShift], VK_NEXT);
  Add(ecPageBottom, [ssCtrl], VK_NEXT);
  Add(ecSelectionPageBottom, [ssShift, ssCtrl], VK_NEXT);
  Add(ecPageUp, [], VK_PRIOR);
  Add(ecSelectionPageUp, [ssShift], VK_PRIOR);
  Add(ecPageTop, [ssCtrl], VK_PRIOR);
  Add(ecSelectionPageTop, [ssShift, ssCtrl], VK_PRIOR);
  Add(ecLineBegin, [], VK_HOME);
  Add(ecSelectionLineBegin, [ssShift], VK_HOME);
  Add(ecEditorTop, [ssCtrl], VK_HOME);
  Add(ecSelectionEditorTop, [ssShift, ssCtrl], VK_HOME);
  Add(ecLineEnd, [], VK_END);
  Add(ecSelectionLineEnd, [ssShift], VK_END);
  Add(ecEditorBottom, [ssCtrl], VK_END);
  Add(ecSelectionEditorBottom, [ssShift, ssCtrl], VK_END);
  Add(ecShowGotoLine, [ssAlt], Ord('G'));
  { Insert key alone }
  Add(ecToggleMode, [], VK_INSERT);
  { Clipboard }
  Add(ecUndo, [ssAlt], VK_BACK);
  Add(ecRedo, [ssAlt, ssShift], VK_BACK);
  Add(ecCopy, [ssCtrl], VK_INSERT);
  Add(ecCut, [ssShift], VK_DELETE);
  Add(ecPaste, [ssShift], VK_INSERT);
  { Deletion }
  Add(ecDeleteChar, [], VK_DELETE);
  Add(ecBackspace, [], VK_BACK);
  Add(ecBackspace, [ssShift], VK_BACK);
  Add(ecDeleteLastWord, [ssCtrl], VK_BACK);
  { Search }
  Add(ecShowFind, [ssCtrl], Ord('F'));
  Add(ecShowReplace, [ssCtrl], Ord('R'));
  Add(ecFindNext, [], VK_F3);
  Add(ecFindPrevious, [ssShift], VK_F3);
  { Enter (return) & Tab }
  Add(ecReturn, [], VK_RETURN);
  Add(ecReturn, [ssShift], VK_RETURN);
  Add(ecTab, [], VK_TAB);
  Add(ecShiftTab, [ssShift], VK_TAB);
  { Standard edit commands }
  Add(ecUndo, [ssCtrl], Ord('Z'));
  Add(ecRedo, [ssCtrl, ssShift], Ord('Z'));
  Add(ecCut, [ssCtrl], Ord('X'));
  Add(ecCopy, [ssCtrl], Ord('C'));
  Add(ecPaste, [ssCtrl], Ord('V'));
  Add(ecSelectAll, [ssCtrl], Ord('A'));
  { Block commands }
  Add(ecBlockIndent, [ssCtrl, ssShift], Ord('I'));
  Add(ecBlockUnindent, [ssCtrl, ssShift], Ord('U'));
  { Fragment deletion }
  Add(ecDeleteWord, [ssCtrl], Ord('T'));
  Add(ecDeleteWord, [ssCtrl], VK_DELETE);
  { Line operations }
  Add(ecInsertLine, [ssCtrl], Ord('M'));
  Add(ecDeleteLine, [ssCtrl], Ord('Y'));
  Add(ecDeleteEndOfLine, [ssCtrl, ssShift], Ord('Y'));
  { Bookmarks }
  Add(ecGotoBookmark1, [ssCtrl], Ord('1'));
  Add(ecGotoBookmark2, [ssCtrl], Ord('2'));
  Add(ecGotoBookmark3, [ssCtrl], Ord('3'));
  Add(ecGotoBookmark4, [ssCtrl], Ord('4'));
  Add(ecGotoBookmark5, [ssCtrl], Ord('5'));
  Add(ecGotoBookmark6, [ssCtrl], Ord('6'));
  Add(ecGotoBookmark7, [ssCtrl], Ord('7'));
  Add(ecGotoBookmark8, [ssCtrl], Ord('8'));
  Add(ecGotoBookmark9, [ssCtrl], Ord('9'));
  Add(ecGotoBookmark0, [ssCtrl], Ord('0'));
  Add(ecSetBookmark1, [ssCtrl, ssShift], Ord('1'));
  Add(ecSetBookmark2, [ssCtrl, ssShift], Ord('2'));
  Add(ecSetBookmark3, [ssCtrl, ssShift], Ord('3'));
  Add(ecSetBookmark4, [ssCtrl, ssShift], Ord('4'));
  Add(ecSetBookmark5, [ssCtrl, ssShift], Ord('5'));
  Add(ecSetBookmark6, [ssCtrl, ssShift], Ord('6'));
  Add(ecSetBookmark7, [ssCtrl, ssShift], Ord('7'));
  Add(ecSetBookmark8, [ssCtrl, ssShift], Ord('8'));
  Add(ecSetBookmark9, [ssCtrl, ssShift], Ord('9'));
  Add(ecSetBookmark0, [ssCtrl, ssShift], Ord('0'));
  Add(ecGotoNextBookmark, [], VK_F2);
  Add(ecGotoPreviousBookmark, [ssShift], VK_F2);
  { Comments }
  Add(ecLineComment, [ssCtrl], VK_OEM_2);
  Add(ecBlockComment, [ssCtrl, ssShift], VK_OEM_2);
  { Completion Proposal }
  Add(ecShowCompletionProposal, [ssCtrl], VK_SPACE);
  { SyncEdit }
  Add(ecSyncEdit, [ssCtrl, ssShift], Ord('J'));
  { MacroRecorder }
  Add(ecRecordMacro, [ssCtrl, ssShift], Ord('R'));
  Add(ecPlaybackMacro, [ssCtrl, ssShift], Ord('P'));
end;

procedure TBCEditorCommands.SetItem(AIndex: Integer; AValue: TBCEditorKeyCommand);
begin
  inherited SetItem(AIndex, AValue);
end;

initialization
  RegisterIntegerConsts(TypeInfo(TBCEditorCommand), IdentToEditorCommand, EditorCommandToIdent);
finalization
  UnregisterIntegerConsts(TypeInfo(TBCEditorCommand), IdentToEditorCommand, EditorCommandToIdent);
end.
