unit BCEditor.MacroRecorder;

interface {********************************************************************}

uses
  Classes, SysUtils, Generics.Collections,
  BCEditor, BCEditor.Commands;

type
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
      Data: PBCEditorCD;
    end;
    TState = (msStopped, msStepped, msRecording, msPlaying);

  strict private
    FCurrentCommand: Integer;
    FData: TMemoryStream;
    FDisallowedCommands: TList<TBCEditorCommand>;
    FEditor: TCustomBCEditor;
    FIgnoredCommands: TList<TBCEditorCommand>;
    FItems: TList<TItem>;
    FOnStateChange: TNotifyEvent;
    FState: TState;
    procedure EditorCommand(ASender: TObject; const ABefore: Boolean;
      const ACommand: TBCEditorCommand; const AData: PBCEditorCD; var AHandled: Boolean);
    function GetCommand(AIndex: Integer): TCommand;
    function GetCommandCount: Integer;
    function GetIsEmpty(): Boolean;
    procedure SetEditor(AValue: TCustomBCEditor);
    procedure SetState(AValue: TState);
    function Step(const APlay: Boolean): Boolean; overload;
  protected
    procedure Notification(AComponent: TComponent; aOperation: TOperation); override;
    property Editor: TCustomBCEditor read FEditor write SetEditor;
    property CommandCount: Integer read GetCommandCount;
    property Commands[AIndex: Integer]: TCommand read GetCommand;
    property IsEmpty: Boolean read GetIsEmpty;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property State: TState read FState;
  public
    procedure AddCommand(const ACommand: TBCEditorCommand; const AData: PBCEditorCD);
    procedure AllowCommandWhileRecording(const ACommand: TBCEditorCommand);
    procedure Clear();
    constructor Create(AOwner: TComponent); override;
    procedure DeleteCommand(const AIndex: Integer);
    destructor Destroy(); override;
    procedure DisallowCommandWhileRecording(const ACommand: TBCEditorCommand);
    procedure IgnoreCommand(const ACommand: TBCEditorCommand);
    procedure InsertCommand(const AIndex: Integer; const ACommand: TBCEditorCommand; const AData: PBCEditorCD);
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

implementation {***************************************************************}

uses
  Windows;

resourcestring
  SBCEditorCannotRecord = 'Cannot record macro: Already recording or playing';
  SBCEditorCannotPlay = 'Cannot play macro: Already recording or playing';

{ TCustomBCEditorMacroRecorder ************************************************}

procedure TCustomBCEditorMacroRecorder.AddCommand(const ACommand: TBCEditorCommand; const AData: PBCEditorCD);
begin
  InsertCommand(FItems.Count, ACommand, AData);
end;

procedure TCustomBCEditorMacroRecorder.AllowCommandWhileRecording(const ACommand: TBCEditorCommand);
begin
  FDisallowedCommands.Remove(ACommand);
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
  FData := TMemoryStream.Create();
  FDisallowedCommands := TList<TBCEditorCommand>.Create();
  FIgnoredCommands := TList<TBCEditorCommand>.Create();

  IgnoreCommand(ecShowGotoLine);
  IgnoreCommand(ecShowFind);
  IgnoreCommand(ecShowReplace);
  IgnoreCommand(ecShowCompletionProposal);

  DisallowCommandWhileRecording(ecSelection);
  DisallowCommandWhileRecording(ecPosition);
end;

procedure TCustomBCEditorMacroRecorder.DeleteCommand(const AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

destructor TCustomBCEditorMacroRecorder.Destroy();
begin
  FItems.Free();
  FData.Free();
  FDisallowedCommands.Free();
  FIgnoredCommands.Free();

  inherited;
end;

procedure TCustomBCEditorMacroRecorder.DisallowCommandWhileRecording(const ACommand: TBCEditorCommand);
begin
  if (FDisallowedCommands.IndexOf(ACommand) < 0) then
    FDisallowedCommands.Add(ACommand);
end;

procedure TCustomBCEditorMacroRecorder.EditorCommand(ASender: TObject; const ABefore: Boolean;
  const ACommand: TBCEditorCommand; const AData: PBCEditorCD; var AHandled: Boolean);
begin
  if (ABefore) then
  begin
    case (ACommand) of
      ecRecordMacro:
        if (ASender = FEditor) then
        begin
          if (FState = msRecording) then
            Stop()
          else
            StartRecord();
          AHandled := True;
        end;
      ecPlaybackMacro:
        if (ASender = FEditor) then
        begin
          Playback();
          AHandled := True;
        end;
      ecStepMacro:
        if (ASender = FEditor) then
        begin
          Step();
          AHandled := True;
        end;
      else
        if ((FState = msRecording) and (FDisallowedCommands.IndexOf(ACommand) >= 0)) then
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
          if ((State = msRecording) and (FIgnoredCommands.IndexOf(ACommand) < 0)) then
            AddCommand(ACommand, AData);
      end;
  end;
end;

function TCustomBCEditorMacroRecorder.GetCommand(AIndex: Integer): TCommand;
begin
  Result.Command := FItems[AIndex].Command;
  Result.Data := PBCEditorCD(@PAnsiChar(FData.Memory)[FItems[AIndex].DataPosition]);
end;

function TCustomBCEditorMacroRecorder.GetCommandCount(): Integer;
begin
  Result := FItems.Count;
end;

function TCustomBCEditorMacroRecorder.GetIsEmpty(): Boolean;
begin
  Result := FItems.Count = 0;
end;

procedure TCustomBCEditorMacroRecorder.IgnoreCommand(const ACommand: TBCEditorCommand);
begin
  if (FIgnoredCommands.IndexOf(ACommand) < 0) then
    FIgnoredCommands.Add(ACommand);
end;

procedure TCustomBCEditorMacroRecorder.InsertCommand(const AIndex: Integer;
  const ACommand: TBCEditorCommand; const AData: PBCEditorCD);
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
    LCommand.DataSize := AData^.Size;
    LCommand.DataPosition := FData.Position;
    FData.Write(AData^, AData^.Size);
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

procedure TCustomBCEditorMacroRecorder.SetEditor(AValue: TCustomBCEditor);
begin
  if (AValue <> FEditor) then
  begin
    if (Assigned(FEditor)) then
      FEditor.UnregisterCommandHandler(EditorCommand);
    FEditor := AValue;
    if (Assigned(FEditor)) then
      FEditor.RegisterCommandHandler(EditorCommand);
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
  LData: PBCEditorCD;
begin
  if ((FState = msRecording) or not APlay and (FState = msPlaying)) then
    raise ERangeError.Create(SBCEditorCannotPlay);

  Result := FCurrentCommand < FItems.Count;
  if (Result) then
  begin
    if (FItems[FCurrentCommand].DataSize = 0) then
      LData := nil
    else
      LData := PBCEditorCD(@PAnsiChar(FData.Memory)[FItems[FCurrentCommand].DataPosition]);
    FEditor.ProcessCommand(FItems[FCurrentCommand].Command, LData);
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

end.
