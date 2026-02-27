unit KM_GUIGameDev;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Classes,
  KM_CommonClasses,
  KM_Defaults,
  KM_Controls, KM_ControlsBase,
  KM_GuiCommonDevelopment, KM_ResDevelopment;


type
  TKMButtonFlatDevGame = class(TKMButtonFlat)
  public
    Cost : Byte;
    Progress : Single;
    procedure Paint; override;
  end;

  TKMGUIGameDevelopment = class(TKMGUICommonDevelopment)
  protected
    Label_DevPoints : TKMLabelShadow;
    procedure SwitchPage(Sender : TObject); override;
    procedure DevClicked(Sender : TObject; Shift : TShiftState);

    function CreateButton(aParent : TKMPanel) : TKMButtonFlat; override;
    procedure SetUpButton(B : TKMButtonFlat; aDev : PKMDevelopment); override;
    procedure RefreshSingle(B : TKMButtonFlat; aDev : PKMDevelopment);  override;
  public
    constructor Create(aParent : TKMPanel);
    procedure ReloadTrees(aCurrentPageOnly : Boolean = true); override;
    procedure RefreshLabels;
    procedure Show; override;
  end;

implementation
uses
  Math,
  KM_CommonTypes,
  KM_HandsCollection, KM_HandLocks, KM_HandTypes,
  KM_Game, KM_GameParams, KM_GameInputProcess,
  KM_InterfaceGame,
  KM_ResTypes, KM_ResFonts, KM_ResTexts,
  KM_RenderUI;

constructor TKMGUIGameDevelopment.Create(aParent: TKMPanel);
begin
  Inherited Create(aParent, TB_PAD, 44, TB_WIDTH, aParent.Height - 50);
  OnButtonClickedShift := DevClicked;

  with TKMImage.Create(self, Width - 30, 5, 32, 31, 1081) do
  begin
    Hint := gResTexts[2300];
  end;
  Label_DevPoints := TKMLabelShadow.Create(self, Width - 31, 5 + 12, 30, 30, '', fntGrey, taCenter);
  Label_DevPoints.Hitable := false;
end;

procedure TKMGUIGameDevelopment.RefreshLabels;
var d : Integer;
begin
  d := gMySpectator.Hand.DevPoints;

  If d = 0 then
    Label_DevPoints.FontColor := $FF2222FF
  else
    Label_DevPoints.FontColor := $FFFFFFFF;

  If d > 9 then
    Label_DevPoints.Caption := '+9'
  else
    Label_DevPoints.Caption := gMySpectator.Hand.DevPoints.ToString;

end;

procedure TKMGUIGameDevelopment.Show;
begin
  Inherited Show;
  ReloadTrees;
end;

procedure TKMGUIGameDevelopment.SwitchPage(Sender: TObject);
begin
  Inherited;
  ReloadTrees;
end;

procedure TKMGUIGameDevelopment.ReloadTrees(aCurrentPageOnly: Boolean = True);
const GOLD_COLOR_UNLOCKED = $B700D9FF;
      GOLD_COLOR = $2700D9FF;
var dtt : TKMDevelopmentTreeType;
  locks : TKMHandLocks;
  unlockedList : TKMArray<PKMDevButton>;

  procedure SetButtonColor(aButton : TKMButtonFlat; aLineColor : Cardinal; aLineWidth : Byte  = 1; aBackColor : Cardinal = 0; aTag2 : Byte = 0);
  begin
    aButton.LineWidth := aLineWidth;
    aButton.DownColor := aLineColor;
    aButton.BackBevelColor := aBackColor;
    aButton.Tag2 := aTag2;
  end;

  procedure UnlockPrevious(aType : TKMDevelopmentTreeType; aToButton : PKMDevButton);
  var B : TKMButtonFlat;
    I : integer;
  begin
    with Tree[aType] do
      B := aToButton.Button_Tree;
    //B.Down := true;
    B.BackBevelColor := $7700FF00;
    B.DownColor := $FF00FF00;
    B.LineWidth := 4;
    TKMButtonFlatDevGame(B).Cost := aToButton.Dev.ID;
    for I := 0 to High(aToButton.Next) do
    If aToButton.Next[I].Button_Tree.DownColor <> UNLOCKED_COLOR_DOWN then
    begin
      IF aToButton.Next[I].Dev.IsSpecial then
        aToButton.Next[I].Button_Tree.BackBevelColor := GOLD_COLOR_UNLOCKED
      else
        SetButtonColor(aToButton.Next[I].Button_Tree, DEFAULT_COLOR, 2, DEFAULT_COLOR and $22FFFFFF, 1)
    end;
    IF aToButton.Dev.IsSpecial then
      B.BackBevelColor := GOLD_COLOR_UNLOCKED;
  end;

  procedure CheckNext(aType : TKMDevelopmentTreeType; var aToButton : TKMDevButton; aTop : Byte; aState : TKMHandDevLock);
  var I: integer;
    B : TKMButtonFlat;
    nextState, currState : TKMHandDevLock;
  begin
    with Tree[aType] do
      B := aToButton.Button_Tree;

    B.Enabled := aState in [dlNone, dlUnlocked, dlUnlockedSingle];
    //set default
    B.Down := false;
    B.BackBevelColor := 0;
    B.DownColor := TO_UNLOCK_COLOR;
    B.LineWidth := 2;

    B.Visible := aState <> dlNotVisible;
    B.Tag2 := 0;
    //TKMButtonFlatDevGame(B).Cost := aToButton.Dev.ID;

    TKMButtonFlatDevGame(B).Progress := -1;

    If gMySpectator.Hand.DevInProgress(aType, aToButton.Dev.ID) then
    begin
      TKMButtonFlatDevGame(B).Progress := gMySpectator.Hand.GetDevProgress(aType, aToButton.Dev.ID);
    end;

    currState := locks.DevelopmentLock[aType, aToButton.Dev.ID];
    If (aState = dlUnlockedSingle) or ((currState = dlUnlockedSingle){ and (aState <> dlNotVisible)}) then
    begin
      SetButtonColor(aToButton.Button_Tree, UNLOCKED_COLOR_DOWN, 3, UNLOCKED_COLOR_DOWN and $77FFFFFF, 1);
      B.Enabled := true;
      B.Visible := true;
      TKMButtonFlatDevGame(B).Cost := aToButton.Dev.ID;
    end else
    If aState = dlUnlocked then
      unlockedList.Add(@aToButton)
    else
    If (aToButton.Parent = nil) and (aState = dlNone) then
      SetButtonColor(aToButton.Button_Tree, DEFAULT_COLOR, 3, DEFAULT_COLOR and $44FFFFFF, 1);

    for I := 0 to High(aToButton.Dev.Next) do
    begin
      nextState := locks.DevelopmentLock[aType, aToButton.Dev.Next[I].ID];

      If (aState = dlBlocked) and (nextState <> dlNotVisible) then
        nextState := dlBlocked
      else
      If (aState = dlNotVisible) then
        nextState := dlNotVisible;

      CheckNext(aType, aToButton.Next[I], aTop + 1, nextState);

      If aToButton.Next[I].Button_Tree.DownColor <> UNLOCKED_COLOR_DOWN then
        if (aState in [dlUnlocked, dlUnlockedSingle]) and (nextState = dlNone)then
          SetButtonColor(aToButton.Next[I].Button_Tree, DEFAULT_COLOR, 3, DEFAULT_COLOR and $44FFFFFF, 1)
        else
        If (aState = dlNone) and (nextState = dlNone) then
          aToButton.Next[I].Button_Tree.DownColor := TO_UNLOCK_COLOR //can be unlocked
        else
        If (nextState = dlBlocked) then
          aToButton.Next[I].Button_Tree.DownColor := BLOCKED_COLOR; //path is blocked


    end;
    IF aToButton.Dev.IsSpecial then
      B.BackBevelColor := GOLD_COLOR;
  end;

  procedure CheckToUnlock(aType : TKMDevelopmentTreeType; var aToButton : TKMDevButton);
  var I : Integer;
  begin
    If aToButton.Button_Tree.DownColor = DEFAULT_COLOR then
    begin
      IF aToButton.Dev.IsSpecial then
        aToButton.Button_Tree.BackBevelColor := GOLD_COLOR
      else
      IF not gMySpectator.Hand.HasDevInProgress(aType)
      or gMySpectator.Hand.DevInProgress(aType, aToButton.Dev.ID) then
        aToButton.Button_Tree.BackBevelColor := DEFAULT_COLOR and $22FFFFFF
      else
        aToButton.Button_Tree.BackBevelColor := $770000A0;
    end;
    for I := 0 to High(aToButton.Next) do
      CheckToUnlock(aType, aToButton.Next[I]);

  end;

  procedure ReloadType(aType : TKMDevelopmentTreeType);
  var I : Integer;
  begin
    unlockedList.Clear;
    CheckNext(aType, Tree[aType].Button_Tree, 0, locks.DevelopmentLock[aType, 0]);
    for I := 0 to unlockedList.Count - 1 do
      UnlockPrevious(aType, unlockedList[I]);
    CheckToUnlock(aType, Tree[aType].Button_Tree);
  end;



begin
  If gMySpectator = nil then
    Exit;
  If not gMySpectator.Hand.Enabled then
    Exit;

  RefreshLabels;

  locks := gMySpectator.Hand.Locks;
  If aCurrentPageOnly then
    ReloadType(fLastPage)
  else
  for dtt := Low(Button_SwitchTree) to High(Button_SwitchTree) do
    ReloadType(dtt);
  RepositionVisible;
end;

procedure TKMGUIGameDevelopment.DevClicked(Sender: TObject; Shift : TShiftState);
var B : TKMButtonFlat;
begin
  If not (ssLeft in Shift) then
    Exit;
  B := TKMButtonFlat(Sender);
  //tag2 = 1, means that this development can be unlocked
  //tag2 = 0, try to unlock previous ones
  If not (B.Tag2 in [1]) then
    Exit;

  If gMySpectator = nil then
    Exit;

  If not gMySpectator.Hand.Enabled then
    Exit;

  If gMySpectator.Hand.Locks.DevelopmentLock[fLastPage, B.Tag] <> dlNone then
    Exit;

  //gMySpectator.Hand.UnlockDevelopment(fLastPage, B.Tag);
  gGame.GameInputProcess.CmdHand(gicUnlockDevelopment, byte(fLastPage), B.Tag, byte(ssShift in Shift));
end;


function TKMGUIGameDevelopment.CreateButton(aParent : TKMPanel) : TKMButtonFlat;
begin
  Result := TKMButtonFlatDevGame.Create(aParent, 0, 0, 0, 0, 0, rxGui);

end;

procedure TKMGUIGameDevelopment.SetUpButton(B : TKMButtonFlat; aDev : PKMDevelopment);
begin
  TKMButtonFlatDevGame(B).Cost := aDev.ID;
end;

procedure TKMGUIGameDevelopment.RefreshSingle(B : TKMButtonFlat; aDev : PKMDevelopment);
begin
  TKMButtonFlatDevGame(B).Cost := aDev.ID;
end;

procedure TKMButtonFlatDevGame.Paint;
begin
  inherited;
  //debuging option to know what ID has the development
  //TKMRenderUI.WriteText(AbsLeft + 3, AbsTop - 3, Width, Cost.ToString, fntGrey, taRight, CapColor);

  If Progress > 0 then
  begin
    TKMRenderUI.WritePicture(AbsLeft, AbsTop, Width, Height, [anLeft, anTop, anRight, anBottom], rxGui, 1206, true, BackBevelColor or $FF000000, 0, Progress);
  end;
end;

end.
