unit BCEditor.Editor.Tabs;

interface {********************************************************************}

uses
  Classes,
  BCEditor.Types;

type
  TBCEditorTabs = class(TPersistent)
  strict private const
    DefaultOptions = [toColumns, toSelectedBlockIndent];
    DefaultWantTabs = True;
    DefaultWidth = 2;
  strict private
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorTabOptions;
    FWantTabs: Boolean;
    FWidth: Integer;
    procedure DoChange();
    procedure SetOptions(const AValue: TBCEditorTabOptions);
    procedure SetWantTabs(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Options: TBCEditorTabOptions read FOptions write SetOptions default DefaultOptions;
    property WantTabs: Boolean read FWantTabs write SetWantTabs default DefaultWantTabs;
    property Width: Integer read FWidth write SetWidth default DefaultWidth;
  end;

implementation {***************************************************************}

{ TBCEditorTabs ***************************************************************}

constructor TBCEditorTabs.Create;
begin
  inherited;

  FOptions := DefaultOptions;
  FWantTabs := DefaultWantTabs;
  FWidth := DefaultWidth;
end;

procedure TBCEditorTabs.Assign(ASource: TPersistent);
begin
  if (ASource is TBCEditorTabs) then
  begin
    FOptions := TBCEditorTabs(ASource).FOptions;
    FWantTabs := TBCEditorTabs(ASource).FWantTabs;
    FWidth := TBCEditorTabs(ASource).FWidth;
    DoChange();
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorTabs.DoChange;
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorTabs.SetOptions(const AValue: TBCEditorTabOptions);
begin
  if (FOptions <> AValue) then
  begin
    FOptions := AValue;
    DoChange();
  end;
end;

procedure TBCEditorTabs.SetWantTabs(const AValue: Boolean);
begin
  if (FWantTabs <> AValue) then
  begin
    FWantTabs := AValue;
    DoChange();
  end;
end;

procedure TBCEditorTabs.SetWidth(const AValue: Integer);
begin
  if ((FWidth <> AValue) and (1 <= AValue) and (AValue < 256)) then
  begin
    FWidth := AValue;
    DoChange();
  end;
end;

end.
