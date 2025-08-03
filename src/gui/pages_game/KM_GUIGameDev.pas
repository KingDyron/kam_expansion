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
    procedure Paint; override;
  end;

  TKMGUIGameDevelopment = class(TKMGUICommonDevelopment)
  protected
    Label_DevPointsCount : array[DEVELOPMENT_MIN..DEVELOPMENT_MAX] of TKMLabel;
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
  KM_Game, KM_GameInputProcess,
  KM_InterfaceGame,
  KM_ResTypes, KM_ResFonts,
  KM_RenderUI;

constructor TKMGUIGameDevelopment.Create(aParent: TKMPanel);
var dtt : TKMDevelopmentTreeType;
begin
  Inherited Create(aParent, TB_PAD, 44, TB_WIDTH, aParent.Height - 50);
  OnButtonClickedShift := DevClicked;

  for dtt := Low(Label_DevPointsCount) to High(Label_DevPointsCount) do
  begin
    Label_DevPointsCount[dtt] := TKMLabel.Create(self, Button_SwitchTree[dtt].Left + 3,
                                                  Button_SwitchTree[dtt].Top - 5,
                                                  Button_SwitchTree[dtt].Width, 20, '', fntGrey, taRight);
    Label_DevPointsCount[dtt].Hitable := false;
  end;

end;

procedure TKMGUIGameDevelopment.RefreshLabels;
var dtt : TKMDevelopmentTreeType;
begin
  for dtt := Low(Label_DevPointsCount) to High(Label_DevPointsCount) do
  begin
    Label_DevPointsCount[dtt].Caption := gMySpectator.Hand.DevPoints(dtt).ToString;
    If gMySpectator.Hand.DevPoints(dtt) = 0 then
      Label_DevPointsCount[dtt].SetColor($FF5151FF)
    else
      Label_DevPointsCount[dtt].SetColor($FF51FF53);
  end;
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
    TKMButtonFlatDevGame(B).Cost := 0;
    for I := 0 to High(aToButton.Next) do
    If aToButton.Next[I].Button_Tree.DownColor <> UNLOCKED_COLOR_DOWN then
    begin
      If not aToButton.Next[I].Button_Tree.Enabled then
        aToButton.Next[I].Button_Tree.DownColor := BLOCKED_COLOR//path is blocked
      else
        SetButtonColor(aToButton.Next[I].Button_Tree, DEFAULT_COLOR, 2, DEFAULT_COLOR and $22FFFFFF, 1)
      //begin
        {aToButton.Next[I].Button_Tree.DownColor := DEFAULT_COLOR;//path can be unlocked immediately
        aToButton.Next[I].Button_Tree.LineWidth := 2;
        aToButton.Next[I].Button_Tree.BackBevelColor := DEFAULT_COLOR and $AAFFFFFF;
      end;}
    end;

    //If aToButton.Parent <> nil then
      //UnlockPrevious(aType, aToButton.Parent);
  end;

  procedure CheckNext(aType : TKMDevelopmentTreeType; var aToButton : TKMDevButton; aTop : Byte; aState : TKMHandDevLock);
  var I: integer;
    B : TKMButtonFlat;
    nextState : TKMHandDevLock;
  begin
    with Tree[aType] do
      B := aToButton.Button_Tree;

    B.Enabled := aState in [dlNone, dlUnlocked];
    //set default
    B.Down := false;
    B.BackBevelColor := 0;
    B.DownColor := TO_UNLOCK_COLOR;
    B.LineWidth := 2;

    B.Visible := aState <> dlNotVisible;
    B.Tag2 := 0;
    If aState = dlUnlocked then
      unlockedList.Add(@aToButton)
    else
    If (aToButton.Parent = nil) and (aState = dlNone) then
      SetButtonColor(aToButton.Button_Tree, DEFAULT_COLOR, 3, DEFAULT_COLOR and $44FFFFFF, 1);

      //UnlockPrevious(aType, @aToButton);
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
        if (aState = dlUnlocked) and (nextState = dlNone) then
          SetButtonColor(aToButton.Next[I].Button_Tree, DEFAULT_COLOR, 3, DEFAULT_COLOR and $44FFFFFF, 1)
        else
        If (aState = dlNone) and (nextState = dlNone) then
          aToButton.Next[I].Button_Tree.DownColor := TO_UNLOCK_COLOR //can be unlocked
        else
        If (nextState = dlBlocked) then
          aToButton.Next[I].Button_Tree.DownColor := BLOCKED_COLOR; //path is blocked


    end;
  end;

  procedure CheckToUnlock(aType : TKMDevelopmentTreeType; var aToButton : TKMDevButton);
  var I : Integer;
  begin
    If aToButton.Button_Tree.DownColor = DEFAULT_COLOR then
    begin
      IF gMySpectator.Hand.DevPoints(aType) >= aToButton.Dev.Cost then
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
end;

procedure TKMGUIGameDevelopment.DevClicked(Sender: TObject; Shift : TShiftState);
var B : TKMButtonFlat;
begin
  If not (ssLeft in Shift) then
    Exit;
  B := TKMButtonFlat(Sender);
  If B.Tag2 <> 1 then  //tag2 = 1, means that this development can be unlocked
    Exit;

  If gMySpectator = nil then
    Exit;

  If not gMySpectator.Hand.Enabled then
    Exit;

  If gMySpectator.Hand.Locks.DevelopmentLock[fLastPage, B.Tag] <> dlNone then
    Exit;

  //gMySpectator.Hand.UnlockDevelopment(fLastPage, B.Tag);
  gGame.GameInputProcess.CmdHand(gicUnlockDevelopment, byte(fLastPage), B.Tag);
end;


function TKMGUIGameDevelopment.CreateButton(aParent : TKMPanel) : TKMButtonFlat;
begin
  Result := TKMButtonFlatDevGame.Create(aParent, 0, 0, 0, 0, 0, rxGui);

end;

procedure TKMGUIGameDevelopment.SetUpButton(B : TKMButtonFlat; aDev : PKMDevelopment);
begin
  TKMButtonFlatDevGame(B).Cost := aDev.Cost;
end;

procedure TKMGUIGameDevelopment.RefreshSingle(B : TKMButtonFlat; aDev : PKMDevelopment);
begin
  TKMButtonFlatDevGame(B).Cost := aDev.Cost;
end;

procedure TKMButtonFlatDevGame.Paint;
var textCol : Cardinal;
begin
  inherited;
  If Cost > 0 then
  begin
    textCol := IfThen(Enabled, CapColor, $FF888888);
    TKMRenderUI.WriteText(AbsLeft + 3, AbsTop - 3, Width, 'x' + Cost.ToString, fntGrey, taRight, textCol);
  end;
end;

end.
