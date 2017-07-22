unit BCEditor.Editor.LeftMargin;

interface {********************************************************************}

uses
  Classes, UITypes,
  Graphics, ImgList,
  BCEditor.Types, BCEditor.Consts;

type
  TBCEditorLeftMargin = class(TPersistent)
  type
    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FBookmarkPanelBackground: TColor;
      FForeground: TColor;
      FLineStateModified: TColor;
      FLineStateLoaded: TColor;
    public
      constructor Create();
      procedure Assign(ASource: TPersistent); override;
    published
      property Background: TColor read FBackground write FBackground default clLeftMarginBackground;
      property BookmarkPanelBackground: TColor read FBookmarkPanelBackground write FBookmarkPanelBackground default clBtnFace;
      property Foreground: TColor read FForeground write FForeground default clLeftMarginForeground;
      property LineStateModified: TColor read FLineStateModified write FLineStateModified default clYellow;
      property LineStateLoaded: TColor read FLineStateLoaded write FLineStateLoaded default clLime;
    end;

    TBookMarks = class(TPersistent)
    strict private
      FOnChange: TNotifyEvent;
      FOwner: TComponent;
      FShortCuts: Boolean;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetVisible(AValue: Boolean);
    public
      constructor Create(AOwner: TComponent);
      procedure Assign(ASource: TPersistent); override;
    published
      property ShortCuts: Boolean read FShortCuts write FShortCuts default True;
      property Visible: Boolean read FVisible write SetVisible default True;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TMarks = class(TPersistent)
    strict private
      FDefaultImageIndex: Integer;
      FImages: TCustomImageList;
      FOnChange: TNotifyEvent;
      FOwner: TComponent;
      FShortCuts: Boolean;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetImages(const AValue: TCustomImageList);
      procedure SetVisible(AValue: Boolean);
    protected
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    public
      constructor Create(AOwner: TComponent);
      procedure Assign(ASource: TPersistent); override;
    published
      property DefaultImageIndex: Integer read FDefaultImageIndex write FDefaultImageIndex default -1;
      property Images: TCustomImageList read FImages write SetImages;
      property Visible: Boolean read FVisible write SetVisible default True;
    end;

    TLineNumbers = class(TPersistent)
    strict private const
      DefaultOptions = [lnoIntens];
      DefaultStartFrom = 1;
      DefaultVisible = True;
    strict private
      FOnChange: TNotifyEvent;
      FOptions: TBCEditorLeftMarginLineNumberOptions;
      FStartFrom: Integer;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetOptions(const AValue: TBCEditorLeftMarginLineNumberOptions);
      procedure SetStartFrom(const AValue: Integer);
      procedure SetVisible(const AValue: Boolean);
    protected
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    public
      constructor Create();
      procedure Assign(ASource: TPersistent); override;
      procedure SetOption(const AOption: TBCEditorLeftMarginLineNumberOption; const AEnabled: Boolean);
    published
      property Options: TBCEditorLeftMarginLineNumberOptions read FOptions write SetOptions default DefaultOptions;
      property StartFrom: Integer read FStartFrom write SetStartFrom default DefaultStartFrom;
      property Visible: Boolean read FVisible write SetVisible default DefaultVisible;
    end;

    TLineState = class(TPersistent)
    strict private const
      DefaultVisible = True;
    strict private
      FOnChange: TNotifyEvent;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetOnChange(AValue: TNotifyEvent);
      procedure SetVisible(const AValue: Boolean);
    protected
      property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    public
      constructor Create();
      procedure Assign(ASource: TPersistent); override;
    published
      property Visible: Boolean read FVisible write SetVisible default DefaultVisible;
    end;

  strict private
    FBookMarks: TBookmarks;
    FColors: TColors;
    FLineNumbers: TLineNumbers;
    FLineState: TLineState;
    FMarks: TMarks;
    FOnChange: TNotifyEvent;
    procedure DoChange();
    procedure SetBookMarks(const AValue: TBookmarks);
    procedure SetColors(const AValue: TColors);
    procedure SetMarks(const AValue: TMarks);
    procedure SetOnChange(AValue: TNotifyEvent);
  protected
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Bookmarks: TBCEditorLeftMargin.TBookmarks read FBookMarks write SetBookMarks;
    property Colors: TColors read FColors write SetColors;
    property LineNumbers: TLineNumbers read FLineNumbers write FLineNumbers;
    property LineState: TLineState read FLineState write FLineState;
    property Marks: TMarks read FMarks write SetMarks;
  end;

implementation {***************************************************************}

uses
  SysUtils, Math,
  BCEditor.Utils;

{ TBCEditorLeftMargin.TColors *************************************************}

procedure TBCEditorLeftMargin.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorLeftMargin.TColors then
  with ASource as TBCEditorLeftMargin.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FBookmarkPanelBackground := FBookmarkPanelBackground;
    Self.FLineStateModified := FLineStateModified;
    Self.FLineStateLoaded := FLineStateLoaded;
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorLeftMargin.TColors.Create();
begin
  inherited;

  FBackground := clLeftMarginBackground;
  FBookmarkPanelBackground := clBtnFace;
  FForeground := clLeftMarginForeground;
  FLineStateModified := clYellow;
  FLineStateLoaded := clLime;
end;

{ TBCEditorLeftMargin.TBookmarks **********************************************}

constructor TBCEditorLeftMargin.TBookmarks.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FShortCuts := True;
  FVisible := True;
end;

procedure TBCEditorLeftMargin.TBookmarks.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TBookmarks) then
  with ASource as TBCEditorLeftMargin.TBookmarks do
  begin
    Self.FShortCuts := FShortCuts;
    Self.FVisible := FVisible;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TBookmarks.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TBookmarks.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

{ TBCEditorLeftMargin.TMarks **************************************************}

constructor TBCEditorLeftMargin.TMarks.Create(AOwner: TComponent);
begin
  inherited Create();

  FOwner := AOwner;
  FDefaultImageIndex := -1;
  FShortCuts := True;
  FVisible := True;
end;

procedure TBCEditorLeftMargin.TMarks.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TMarks) then
  with ASource as TBCEditorLeftMargin.TMarks do
  begin
    Self.FDefaultImageIndex := FDefaultImageIndex;
    Self.FImages := FImages;
    Self.FShortCuts := FShortCuts;
    Self.FVisible := FVisible;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TMarks.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TMarks.SetImages(const AValue: TCustomImageList);
begin
  if (AValue <> FImages) then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
    DoChange();
  end;
end;

procedure TBCEditorLeftMargin.TMarks.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

{ TBCEditorLeftMargin.TLineNumbers ********************************************}

procedure TBCEditorLeftMargin.TLineNumbers.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TLineNumbers) then
  with ASource as TBCEditorLeftMargin.TLineNumbers do
  begin
    Self.FOptions := FOptions;
    Self.FStartFrom := FStartFrom;
    Self.FVisible := FVisible;

    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorLeftMargin.TLineNumbers.Create();
begin
  inherited;

  FOptions := DefaultOptions;
  FStartFrom := DefaultStartFrom;
  FVisible := DefaultVisible;
end;

procedure TBCEditorLeftMargin.TLineNumbers.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetOption(const AOption: TBCEditorLeftMarginLineNumberOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetOptions(const AValue: TBCEditorLeftMarginLineNumberOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetStartFrom(const AValue: Integer);
begin
  if (AValue <> FStartFrom) then
  begin
    FStartFrom := Max(0, AValue);
    DoChange();
  end;
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetVisible(const AValue: Boolean);
begin
  if (AValue <> FVisible) then
  begin
    FVisible := AValue;
    DoChange();
  end;
end;

{ TBCEditorLeftMargin.TLineState **********************************************}

constructor TBCEditorLeftMargin.TLineState.Create();
begin
  inherited;

  FVisible := DefaultVisible;
end;

procedure TBCEditorLeftMargin.TLineState.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TLineState) then
  with ASource as TBCEditorLeftMargin.TLineState do
  begin
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TLineState.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TLineState.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.TLineState.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

{ TBCEditorLeftMargin *********************************************************}

constructor TBCEditorLeftMargin.Create(AOwner: TComponent);
begin
  inherited Create;

  FColors := TColors.Create;

  FBookmarks := TBookmarks.Create(AOwner);
  FMarks := TMarks.Create(AOwner);
  FLineState := TLineState.Create;
  FLineNumbers := TLineNumbers.Create;
  FOnChange := nil;
end;

destructor TBCEditorLeftMargin.Destroy();
begin
  FBookmarks.Free();
  FMarks.Free();
  FColors.Free();
  FLineState.Free();
  FLineNumbers.Free();

  inherited;
end;

procedure TBCEditorLeftMargin.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorLeftMargin then
  with ASource as TBCEditorLeftMargin do
  begin
    Self.FBookmarks.Assign(FBookmarks);
    Self.FMarks.Assign(FMarks);
    Self.FColors.Assign(FColors);
    Self.FLineNumbers.Assign(FLineNumbers);
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.DoChange();
begin
  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.SetBookMarks(const AValue: TBookmarks);
begin
  FBookmarks.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetMarks(const AValue: TMarks);
begin
  FMarks.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  FBookmarks.OnChange := AValue;
  FLineState.OnChange := AValue;
  FLineNumbers.OnChange := AValue;
  FMarks.OnChange := AValue;
end;

end.
