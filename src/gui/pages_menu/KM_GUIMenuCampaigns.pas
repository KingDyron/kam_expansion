unit KM_GUIMenuCampaigns;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, SysUtils, Math,
  KM_Controls, KM_ControlsBase, KM_ControlsList, KM_ControlsMemo,
  KM_Pics,
  KM_Campaigns, KM_InterfaceDefaults, KM_InterfaceTypes;


type
  TKMMenuCampaigns = class(TKMMenuPageCommon)
  private
    fOnPageChange: TKMMenuChangeEventText; //will be in ancestor class

    fCampaigns: TKMCampaignsCollection;
    fScanCompleted: Boolean;

    procedure ListChange(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure BackClick(Sender: TObject);

    procedure ScanUpdate(Sender: TObject);
    procedure ScanTerminate(Sender: TObject);
  protected
    Panel_CampSelect: TKMPanel;
      Panel_Campaigns: TKMPanel;
        ColumnBox_Camps: TKMColumnBox;
        Image_CampsPreview: TKMImage;
        Memo_CampDesc: TKMMemo;
        Button_Camp_Start, Button_Camp_Back: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aCampaigns: TKMCampaignsCollection; aOnPageChange: TKMMenuChangeEventText);

    procedure RefreshList;
    procedure InitCampaignsScan;
    procedure Show;

    procedure UpdateState;
  end;


implementation
uses
  KM_ResTexts, KM_ResFonts, KM_ResTypes,
  KM_RenderUI,
  KM_GameSettings,
  KM_CampaignTypes;


{ TKMMainMenuInterface }
constructor TKMMenuCampaigns.Create(aParent: TKMPanel; aCampaigns: TKMCampaignsCollection; aOnPageChange: TKMMenuChangeEventText);
const
  PAD_W = 80;
  PAN_W = 1024 - PAD_W * 2;
  BTN_W = 300;
  BTN_PAD_W = 40;
  LIST_TOP = 30;
  LIST_W = 505;
  LIST_H = 540;
  COL_PAD = 14;
  RIGHT_W = PAN_W - LIST_W - COL_PAD;
  MAP_IMG_W = 337;
  MAP_IMG_H = 252;
  MAP_PAD = 4;
  DESC_PAD_H = 12;
  DESC_TOP = LIST_TOP + MAP_IMG_H + DESC_PAD_H;
  DESC_H = LIST_H - MAP_IMG_H - DESC_PAD_H;
  DESC_W = MAP_IMG_W + 2*MAP_PAD;
begin
  inherited Create(gpCampSelect);

  fCampaigns := aCampaigns;

  // Rescan campaigns on campaigns menu creation (f.e. on game start or game locale change)
  InitCampaignsScan;

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := BackClick;

  Panel_CampSelect := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_CampSelect.AnchorsStretch;
    Panel_Campaigns := TKMPanel.Create(Panel_CampSelect, PAD_W, 60, PAN_W, aParent.Height - 100);
    Panel_Campaigns.AnchorsStretch;

    TKMLabel.Create(Panel_Campaigns, 0, 0, Panel_Campaigns.Width, 20, gResTexts[TX_MENU_CAMP_HEADER], fntOutline, taCenter).AnchorsCenter;
    ColumnBox_Camps := TKMColumnBox.Create(Panel_Campaigns, 0, LIST_TOP, LIST_W, LIST_H, fntGrey, bsMenu);
    ColumnBox_Camps.SetColumns(fntOutline, [gResTexts[TX_MENU_CAMPAIGNS_TITLE],
                                             gResTexts[TX_MENU_CAMPAIGNS_MAPS_COUNT],
                                             gResTexts[1952]],
                                             [0, 305, 385]);
    ColumnBox_Camps.AnchorsCenter;
    ColumnBox_Camps.SearchColumn := 0;
    ColumnBox_Camps.OnChange := ListChange;
    ColumnBox_Camps.OnDoubleClick := StartClick;

    TKMBevel.Create(Panel_Campaigns, LIST_W + COL_PAD, 30, MAP_IMG_W + 2*MAP_PAD, MAP_IMG_H + 2*MAP_PAD).AnchorsCenter;
    Image_CampsPreview := TKMImage.Create(Panel_Campaigns, LIST_W + COL_PAD + MAP_PAD, 34, MAP_IMG_W, MAP_IMG_H, 0, rxGuiMain);
    Image_CampsPreview.ImageStretch;
    Image_CampsPreview.AnchorsCenter;

    Memo_CampDesc := TKMMemo.Create(Panel_Campaigns, LIST_W + COL_PAD, DESC_TOP, DESC_W, DESC_H, fntGame, bsMenu);
    Memo_CampDesc.AnchorsCenter;
    Memo_CampDesc.WordWrap := True;
    Memo_CampDesc.ItemHeight := 16;

    with TKMLabel.Create(Panel_Campaigns, 0, ColumnBox_Camps.Bottom + 15, 864, 40, gResTexts[TX_MENU_CAMP_HINT], fntGrey, taCenter) do
    begin
      AnchorsCenter;
      WordWrap := True;
    end;

    Button_Camp_Back := TKMButton.Create(Panel_Campaigns, BTN_PAD_W, 620, BTN_W, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_Camp_Back.AnchorsCenter;
    Button_Camp_Back.OnClick := BackClick;

    Button_Camp_Start := TKMButton.Create(Panel_Campaigns, PAN_W - BTN_W - BTN_PAD_W, 620, BTN_W, 30, gResTexts[TX_MENU_CAMP_START], bsMenu);
    Button_Camp_Start.AnchorsCenter;
    Button_Camp_Start.OnClick := StartClick;
end;


procedure TKMMenuCampaigns.RefreshList;
var
  I: Integer;
begin
  Image_CampsPreview.TexID := 0; //Clear preview image
  ColumnBox_Camps.Clear;
  Memo_CampDesc.Clear;
  for I := 0 to fCampaigns.Count - 1 do
    if not fCampaigns[I].IsMP then
    begin
      ColumnBox_Camps.AddItem(MakeListRow(
                          [fCampaigns[I].GetCampaignTitle, IntToStr(fCampaigns[I].MapCount), IntToStr(fCampaigns[I].GetProgress)],
                          [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF], I));
      if fCampaigns[I].ShortName = gGameSettings.MenuCampaignName then
      begin
        ColumnBox_Camps.ItemIndex := I;
        ListChange(nil);
      end;
    end;

  if ColumnBox_Camps.ItemIndex = -1 then
    Button_Camp_Start.Disable
  else
    Button_Camp_Start.Enable;
end;


procedure TKMMenuCampaigns.ListChange(Sender: TObject);
var
  cmp: TKMCampaignId;
  camp: TKMCampaign;
begin
  //Key press can cause ItemIndex = -1
  if ColumnBox_Camps.ItemIndex = -1 then
  begin
    Button_Camp_Start.Disable;
    Image_CampsPreview.TexID := 0;
    Memo_CampDesc.Clear;
  end
  else
  begin
    Button_Camp_Start.Enable;
    cmp := fCampaigns[ColumnBox_Camps.Rows[ColumnBox_Camps.ItemIndex].Tag].CampaignId;
    camp := fCampaigns.CampaignById(cmp);

    Image_CampsPreview.RX := camp.BackGroundPic.RX;
    Image_CampsPreview.TexID := camp.BackGroundPic.ID;

    Memo_CampDesc.Text := camp.GetCampaignDescription;
    gGameSettings.MenuCampaignName := camp.ShortName;
  end;
end;


procedure TKMMenuCampaigns.StartClick(Sender: TObject);
var
  cmp: UnicodeString;
begin
  //Get the caption and pass it to Campaign selection menu (it will be casted to TKMCampaignName there)
  //so that we avoid cast/uncast/cast along the event chain
  cmp := fCampaigns[ColumnBox_Camps.Rows[ColumnBox_Camps.ItemIndex].Tag].ShortName;
  fOnPageChange(gpCampaign, cmp);
end;


procedure TKMMenuCampaigns.BackClick(Sender: TObject);
begin
  fOnPageChange(gpSingleplayer);
end;


procedure TKMMenuCampaigns.ScanUpdate(Sender: TObject);
begin
  if not fScanCompleted then  // Don't refresh list, if scan was completed already
    RefreshList; //Don't jump to selected with each scan update
end;


procedure TKMMenuCampaigns.ScanTerminate(Sender: TObject);
begin
  if self = nil then
    Exit;
  fScanCompleted := True;
  RefreshList; //After scan complete jump to selected item
end;


procedure TKMMenuCampaigns.InitCampaignsScan;
begin
  //Terminate all
  fCampaigns.TerminateScan;

  //Reset scan variables
  fScanCompleted := False;

  fCampaigns.Refresh(ScanUpdate, ScanTerminate);
end;


procedure TKMMenuCampaigns.Show;
begin
  RefreshList;
  Panel_CampSelect.Show;
end;


procedure TKMMenuCampaigns.UpdateState;
begin
  fCampaigns.UpdateState;
end;


end.
