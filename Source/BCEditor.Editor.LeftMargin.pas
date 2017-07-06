unit BCEditor.Editor.LeftMargin;

interface {********************************************************************}

uses
  Classes, UITypes,
  Graphics, ImgList,
  BCEditor.Editor.Marks, BCEditor.Types, BCEditor.Consts;

type
  TBCEditorLeftMargin = class(TPersistent)
  type
    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FBookmarkPanelBackground: TColor;
      FBorder: TColor;
      FForeground: TColor;
      FLineStateModified: TColor;
      FLineStateNormal: TColor;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Background: TColor read FBackground write FBackground default clLeftMarginBackground;
      property BookmarkPanelBackground: TColor read FBookmarkPanelBackground write FBookmarkPanelBackground default clBtnFace;
      property Border: TColor read FBorder write FBorder default clLeftMarginBackground;
      property Foreground: TColor read FForeground write FForeground default clLeftMarginForeground;
      property LineStateModified: TColor read FLineStateModified write FLineStateModified default clYellow;
      property LineStateNormal: TColor read FLineStateNormal write FLineStateNormal default clLime;
    end;

    TBookMarks = class(TPersistent)
    strict private
      FImages: TCustomImageList;
      FLeftMargin: Integer;
      FOnChange: TNotifyEvent;
      FOwner: TComponent;
      FShortCuts: Boolean;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetImages(const AValue: TCustomImageList);
      procedure SetVisible(AValue: Boolean);
    public
      constructor Create(AOwner: TComponent);
      procedure Assign(ASource: TPersistent); override;
    published
      property Images: TCustomImageList read FImages write SetImages;
      property LeftMargin: Integer read FLeftMargin write FLeftMargin default 2;
      property ShortCuts: Boolean read FShortCuts write FShortCuts default True;
      property Visible: Boolean read FVisible write SetVisible default True;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TMarks = class(TPersistent)
    type
      TPanel = class(TPersistent)
      strict private
        FOnChange: TNotifyEvent;
        FOptions: TBCEditorLeftMarginBookMarkPanelOptions;
        FVisible: Boolean;
        FWidth: Integer;
        procedure DoChange;
        procedure SetVisible(const AValue: Boolean);
        procedure SetWidth(AValue: Integer);
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property Options: TBCEditorLeftMarginBookMarkPanelOptions read FOptions write FOptions default [bpoToggleBookmarkByClick];
        property Visible: Boolean read FVisible write SetVisible default True;
        property Width: Integer read FWidth write SetWidth default 20;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
      end;

    strict private
      FDefaultImageIndex: Integer;
      FImages: TCustomImageList;
      FLeftMargin: Integer;
      FOnChange: TNotifyEvent;
      FOverlappingOffset: Integer;
      FOwner: TComponent;
      FShortCuts: Boolean;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetImages(const AValue: TCustomImageList);
      procedure SetVisible(AValue: Boolean);
    public
      constructor Create(AOwner: TComponent);
      procedure Assign(ASource: TPersistent); override;
    published
      property DefaultImageIndex: Integer read FDefaultImageIndex write FDefaultImageIndex default -1;
      property Images: TCustomImageList read FImages write SetImages;
      property LeftMargin: Integer read FLeftMargin write FLeftMargin default 2;
      property OverlappingOffset: Integer read FOverlappingOffset write FOverlappingOffset default 4;
      property Visible: Boolean read FVisible write SetVisible default True;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
    FMarksPanel: TMarks.TPanel;
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
    property MarksPanel: TMarks.TPanel read FMarksPanel write FMarksPanel;
  end;

implementation {***************************************************************}

uses
  SysUtils, Math,
  BCEditor.Utils;

{ TBCEditorLeftMargin.TColors *************************************************}

constructor TBCEditorLeftMargin.TColors.Create;
begin
  inherited;

  FBackground := clLeftMarginBackground;
  FBookmarkPanelBackground := clBtnFace;
  FBorder := clLeftMarginBackground;
  FLineStateModified := clYellow;
  FLineStateNormal := clLime;
end;

procedure TBCEditorLeftMargin.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorLeftMargin.TColors then
  with ASource as TBCEditorLeftMargin.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FBookmarkPanelBackground := FBookmarkPanelBackground;
    Self.FBorder := FBorder;
    Self.FLineStateModified := FLineStateModified;
    Self.FLineStateNormal := FLineStateNormal;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorLeftMargin.TBookmarks **********************************************}

constructor TBCEditorLeftMargin.TBookmarks.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FLeftMargin := 2;
  FShortCuts := True;
  FVisible := True;
end;

procedure TBCEditorLeftMargin.TBookmarks.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TBookmarks) then
  with ASource as TBCEditorLeftMargin.TBookmarks do
  begin
    Self.FImages := FImages;
    Self.FLeftMargin := FLeftMargin;
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

procedure TBCEditorLeftMargin.TBookmarks.SetImages(const AValue: TCustomImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
    DoChange;
  end;
end;

procedure TBCEditorLeftMargin.TBookmarks.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

{ TBCEditorLeftMargin.TMarks.TPanel *******************************************}

constructor TBCEditorLeftMargin.TMarks.TPanel.Create;
begin
  inherited;

  FWidth := 20;
  FOptions := [bpoToggleBookmarkByClick];
  FVisible := True;
end;

procedure TBCEditorLeftMargin.TMarks.TPanel.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TMarks.TPanel) then
  with ASource as TBCEditorLeftMargin.TMarks.TPanel do
  begin
    Self.FVisible := FVisible;
    Self.FWidth := FWidth;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TMarks.TPanel.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TMarks.TPanel.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.TMarks.TPanel.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange
  end;
end;

{ TBCEditorLeftMargin.TMarks **************************************************}

constructor TBCEditorLeftMargin.TMarks.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FDefaultImageIndex := -1;
  FLeftMargin := 2;
  FOverlappingOffset := 4;
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
    Self.FLeftMargin := FLeftMargin;
    Self.FOverlappingOffset := FOverlappingOffset;
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
  if FImages <> AValue then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
    DoChange;
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
  FMarksPanel := TMarks.TPanel.Create;
  FOnChange := nil;
end;

destructor TBCEditorLeftMargin.Destroy();
begin
  FBookmarks.Free();
  FMarks.Free();
  FColors.Free();
  FLineState.Free();
  FLineNumbers.Free();
  FMarksPanel.Free();

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
    Self.FMarksPanel.Assign(FMarksPanel);
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
  FMarksPanel.OnChange := AValue;
end;

end.
