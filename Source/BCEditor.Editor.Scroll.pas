unit BCEditor.Editor.Scroll;

interface

uses
  Classes, UITypes,
  Forms,
  BCEditor.Types;

type
  TBCEditorScrollEvent = procedure(ASender: TObject; AScrollBar: TScrollBarKind) of object;

  TBCEditorScroll = class(TPersistent)
  type
    THint = class(TPersistent)
    strict private
      FFormat: TBCEditorScrollHintFormat;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Format: TBCEditorScrollHintFormat read FFormat write FFormat default shfTopLineOnly;
    end;

  strict private const
    DefaultOptions = [soMiddleClickMove];
  strict private
    FHint: TBCEditorScroll.THint;
    FMaxWidth: Integer;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorScrollOptions;
    procedure DoChange;
    procedure SetHint(const AValue: TBCEditorScroll.THint);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetOptions(const AValue: TBCEditorScrollOptions);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorScrollOption; const AEnabled: Boolean);
  published
    property Hint: TBCEditorScroll.THint read FHint write SetHint;
    property Options: TBCEditorScrollOptions read FOptions write SetOptions default DefaultOptions;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation {***************************************************************}

uses
  Graphics,
  BCEditor.Utils, BCEditor.Consts;

{ TBCEditorScroll.THint *******************************************************}

constructor TBCEditorScroll.THint.Create;
begin
  inherited;

  FFormat := shfTopLineOnly;
end;

procedure TBCEditorScroll.THint.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorScroll.THint then
  with ASource as TBCEditorScroll.THint do
    Self.FFormat := FFormat
  else
    inherited Assign(ASource);
end;

{ TBCEditorScroll *************************************************************}

constructor TBCEditorScroll.Create;
begin
  inherited;

  FOptions := DefaultOptions;
  FMaxWidth := 1024;
  FHint := TBCEditorScroll.THint.Create;
end;

destructor TBCEditorScroll.Destroy;
begin
  FHint.Free;

  inherited;
end;

procedure TBCEditorScroll.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorScroll then
  with ASource as TBCEditorScroll do
  begin
    Self.FHint.Assign(FHint);
    Self.FOptions := FOptions;
    Self.FMaxWidth := FMaxWidth;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorScroll.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorScroll.SetHint(const AValue: THint);
begin
  FHint.Assign(AValue);
end;

procedure TBCEditorScroll.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

procedure TBCEditorScroll.SetOption(const AOption: TBCEditorScrollOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorScroll.SetOptions(const AValue: TBCEditorScrollOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

end.
