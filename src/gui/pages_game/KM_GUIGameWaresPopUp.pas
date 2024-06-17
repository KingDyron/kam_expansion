unit KM_GUIGameWaresPopUp;
{$I KaM_Remake.inc}
interface
uses
  KM_Controls, KM_ControlsBase, KM_ControlsEdit,
  KM_CommonClasses,
  KM_GUICommonWares,
  KM_ResTypes, KM_ControlsList,
  System.Classes;


type
  TKMGUIGameWaresPopUp = class
  private
    fOnShow : TNotifyEvent;
    procedure ClosePanel(Sender : TObject);
    procedure RefreshList;
    procedure OnExpandPanel(Sender : TObject);
  protected
    Pin_OpenV,
    Pin_Open : TKMButtonFlatPin;
    Panel_Wares : TKMPanel;
      Bevel_BackGround : TKMBevel;
      Image_ClosePanel: TKMImage;
      WaresV,
      Wares : TKMGUICommonWares;


  public
    constructor Create(aParent: TKMPanel; aOnShow : TNotifyEvent);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateState;
  end;


implementation
uses  KM_HandsCollection,
      KM_Pics,
      KM_AIGoals, KM_MapTypes,
      KM_Defaults, KM_GameParams,
      KM_ResFonts, KM_ResTexts,
      KM_RenderUI, KM_ResHouses,
      KM_CommonUtils, SysUtils, Math;

constructor TKMGUIGameWaresPopUp.Create(aParent: TKMPanel; aOnShow : TNotifyEvent);
begin
  inherited Create;
  fOnShow := aOnShow;
  Pin_Open := TKMButtonFlatPin.Create(aParent, 198, 170, 25, 32, 796);
  Pin_Open.OnClick := ClosePanel;
  Pin_Open.Hint := gResTexts[269];
  Pin_OpenV := TKMButtonFlatPin.Create(aParent, 198, 220, 25, 32, 724);
  Pin_OpenV.OnClick := ClosePanel;
  Pin_OpenV.Hint := gResTexts[1946];

  Panel_Wares := TKMPanel.Create(aParent, 250, 40 + 80, 185 + 20, 600);
  Panel_Wares.Hitable := false;

  Bevel_BackGround := TKMBevel.Create(Panel_Wares, 0, 0, Panel_Wares.Width, Panel_Wares.Height);

  Image_ClosePanel := TKMImage.Create(Panel_Wares, Panel_Wares.Width - 20, -5, 30, 30, 52);
  Image_ClosePanel.OnClick := ClosePanel;

  Wares := TKMGUICommonWares.Create(Panel_Wares, 5, 10, Panel_Wares.Width - 10, Panel_Wares.Height - 100, nil, false, false);
  Wares.Panel.OnExpand := OnExpandPanel;

  WaresV := TKMGUICommonWares.Create(Panel_Wares, 10, 25, Panel_Wares.Width - 10, Panel_Wares.Height - 100, nil, false, true);

  Bevel_BackGround.Height := Wares.Panel.Bottom + 30;
  Hide;
end;


procedure TKMGUIGameWaresPopUp.Show;
begin
  Panel_Wares.Show;
  if Assigned(fOnShow) then
    fOnShow(Self);
  RefreshList;
end;


procedure TKMGUIGameWaresPopUp.Hide;
begin
  Panel_Wares.Hide;
end;


function TKMGUIGameWaresPopUp.Visible: Boolean;
begin
  Result := WaresV.Visible or Wares.Visible or Panel_Wares.Visible;
end;

procedure TKMGUIGameWaresPopUp.OnExpandPanel(Sender: TObject);
begin
  Bevel_BackGround.Height :=  Wares.Panel.Bottom + 30;
end;

procedure TKMGUIGameWaresPopUp.ClosePanel(Sender: TObject);
begin
  if Sender = Pin_Open then
  begin
    if Visible and Wares.Visible then
        Hide
    else
    begin
      Show;
      Wares.Show;
      WaresV.Hide;
      Bevel_BackGround.Height :=  Wares.Panel.Bottom + 30;
    end;
  end else
  if Sender = Pin_OpenV then
  begin
    if Visible and WaresV.Visible then
        Hide
    else
    begin
      Show;
      Wares.Hide;
      WaresV.Show;
      Bevel_BackGround.Height :=  WaresV.Ware[high(gMySpectator.Hand.VWaresCount)].Bottom + 30;
    end;
  end else
    Hide;
end;

procedure TKMGUIGameWaresPopUp.RefreshList;
var I : Integer;
  WT: TKMWareType;
begin
  if Wares.Visible then
    for WT := WARE_MIN to WARE_MAX do
      Wares.SetWareCount(WT, gMySpectator.Hand.Stats.Wares[WT].ActualCnt);

  if WaresV.Visible then
    for I := 0 to high(gMySpectator.Hand.VWaresCount) do
      WaresV.SetWareCount(I, gMySpectator.Hand.VWaresCount[I]);
end;

procedure TKMGUIGameWaresPopUp.UpdateState;
begin
  if gGameparams.Tick mod 10 = 0 then
    RefreshList;
end;

end.


