unit BCEditor.Editor.Replace;

interface

uses
  Classes,
  BCEditor.Types, BCEditor.Editor.Search;

type
  TBCEditorReplace = class(TPersistent)
  type
    TChangeEvent = procedure(Event: TBCEditorReplaceChanges) of object;
    TEvent = procedure(ASender: TObject; const APattern, AReplaceText: string; APosition: TBCEditorTextPosition;
      var AAction: TBCEditorReplaceAction) of object;
    TOptions = set of TBCEditorReplaceOption;

  strict private const
    DefaultOptions = [roPrompt];
  strict private
    FBeginPosition: TBCEditorTextPosition;
    FEndPosition: TBCEditorTextPosition;
    FEngine: TBCEditorSearchEngine;
    FOnChange: TChangeEvent;
    FOptions: TOptions;
    FPattern: string;
    FReplaceText: string;
    procedure SetEngine(const AValue: TBCEditorSearchEngine);
  protected
    property BeginPosition: TBCEditorTextPosition read FBeginPosition write FBeginPosition;
    property EndPosition: TBCEditorTextPosition read FEndPosition write FEndPosition;
    property OnChange: TChangeEvent read FOnChange write FOnChange;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorReplaceOption; const AEnabled: Boolean);
    property Pattern: string read FPattern write FPattern;
    property ReplaceText: string read FReplaceText write FReplaceText;
  published
    property Engine: TBCEditorSearchEngine read FEngine write SetEngine default seNormal;
    property Options: TOptions read FOptions write FOptions default DefaultOptions;
  end;

implementation

constructor TBCEditorReplace.Create;
begin
  inherited;

  FBeginPosition := InvalidTextPosition;
  FEndPosition := InvalidTextPosition;
  FEngine := seNormal;
  FOptions := DefaultOptions;
end;

procedure TBCEditorReplace.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorReplace) then
  with ASource as TBCEditorReplace do
  begin
    Self.FEngine := Engine;
    Self.FOptions := Options;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorReplace.SetOption(const AOption: TBCEditorReplaceOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorReplace.SetEngine(const AValue: TBCEditorSearchEngine);
begin
  if FEngine <> AValue then
  begin
    FEngine := AValue;
    if Assigned(FOnChange) then
      FOnChange(rcEngineUpdate);
  end;
end;

end.
