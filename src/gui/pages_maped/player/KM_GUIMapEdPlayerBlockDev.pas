unit KM_GUIMapEdPlayerBlockDev;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_ControlsBase,
   KM_GUICommonDevelopment,
   KM_Defaults,
   KM_HandTypes,
   KM_Points;

type
  TKMButtonFlatBlock = class(TKMButtonFlat)
  public
    DevBlock : TKMHandDevLock;
    procedure Paint; override;
  end;

  TKMMapEdPlayerBlockDevs = class(TKMGUICommonDevelopment)
  protected
    function CreateButton(aParent : TKMPanel) : TKMButtonFlat; override;

    procedure SwitchPage(Sender : TObject); override;
    procedure DevClicked(Sender : TObject; Shift : TShiftState);
  public
    constructor Create(aParent : TKMPanel);

    procedure ReloadTrees(aCurrentPageOnly : Boolean = true); override;
    procedure Show; override;
  end;

implementation
uses
  KM_CommonClasses,
  KM_HandsCollection, KM_HandLocks,
  KM_Cursor, KM_RenderUI, KM_ResFonts, KM_Resource, KM_ResTypes, KM_ResDevelopment,
  KM_Utils, KM_CommonUtils,
  Math;


function TKMMapEdPlayerBlockDevs.CreateButton(aParent : TKMPanel) : TKMButtonFlat;
begin
  Result := TKMButtonFlatBlock.Create(aParent, 0, 0, 0, 0, 0, rxGui);
end;

procedure TKMButtonFlatBlock.Paint;
var id : Integer;
begin
  Inherited;
  id := 0;
  case DevBlock of
    dlNone : ;
    dlBlocked : id := 32;
    dlUnlocked : id := 33;
    dlNotVisible : id := 91;
    else id := 0;
  end;

  if id > 0 then
    TKMRenderUI.WritePicture(AbsLeft, AbsTop, 15, 15, [], rxGuiMain, id);


end;

constructor TKMMapEdPlayerBlockDevs.Create(aParent : TKMPanel);
begin
  Inherited Create(aParent, 15, 28 + 30, aParent.Width - 30, aParent.Height - 60);
  OnButtonClickedShift := DevClicked;
end;

procedure TKMMapEdPlayerBlockDevs.SwitchPage(Sender: TObject);
begin
  Inherited;
  ReloadTrees;
end;

procedure TKMMapEdPlayerBlockDevs.DevClicked(Sender : TObject; Shift : TShiftState);
var B : TKMButtonFlat;
  oldLock : TKMHandDevLock;
  locks : TKMHandLocks;
  I : Integer;
begin
  If gMySpectator = nil then
    Exit;
  If not gMySpectator.Hand.Enabled then
    Exit;

  locks := gMySpectator.Hand.Locks;
  B := TKMButtonFlat(Sender);
  oldLock := locks.DevelopmentLock[fLastPage,B.Tag];
  I := byte(oldLock);

  If ssRight in Shift then
    IncLoop(I, byte(low(TKMHandDevLock)), byte(high(TKMHandDevLock)), -1)
  else
  If ssShift in Shift then
    I := byte(dlUnlocked)
  else
  If ssCtrl in Shift then
    I := byte(dlNotVisible)
  else
  If ssAlt in Shift then
    I := byte(dlBlocked)
  else
    IncLoop(I, byte(low(TKMHandDevLock)), byte(high(TKMHandDevLock)), 1);


  If ssShift in Shift then
  begin
    locks.ForceUnlockDevMapEd(fLastPage, B.Tag);
  end else
    locks.DevelopmentLock[fLastPage,B.Tag] := TKMHandDevLock(I);

  If oldLock <> locks.DevelopmentLock[fLastPage,B.Tag] then
  begin
    //Locks.CheckDevLocksMapEd;
    ReloadTrees;
  end;
end;

procedure TKMMapEdPlayerBlockDevs.ReloadTrees(aCurrentPageOnly: Boolean = True);

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
    B.LineWidth := 3;
    TKMButtonFlatBlock(B).DevBlock := dlUnlocked;

    for I := 0 to High(aToButton.Next) do
    If aToButton.Next[I].Button_Tree.DownColor <> UNLOCKED_COLOR_DOWN then
    begin
      If aToButton.Next[I].Button_Tree.DownColor = BLOCKED_COLOR then
        aToButton.Next[I].Button_Tree.DownColor := BLOCKED_COLOR//path is blocked
      else
        SetButtonColor(aToButton.Next[I].Button_Tree, DEFAULT_COLOR, 2, DEFAULT_COLOR and $22FFFFFF, 1)
      //begin
        {aToButton.Next[I].Button_Tree.DownColor := DEFAULT_COLOR;//path can be unlocked immediately
        aToButton.Next[I].Button_Tree.LineWidth := 2;
        aToButton.Next[I].Button_Tree.BackBevelColor := DEFAULT_COLOR and $AAFFFFFF;
      end;}
    end;

    If aToButton.Parent <> nil then
      UnlockPrevious(aType, aToButton.Parent);
  end;

  procedure CheckNext(aType : TKMDevelopmentTreeType; var aToButton : TKMDevButton; aTop : Byte; aState : TKMHandDevLock);
  var I: integer;
    B : TKMButtonFlat;
    nextState : TKMHandDevLock;
  begin
    with Tree[aType] do
      B := aToButton.Button_Tree;

    B.Enabled := true;//aState in [dlNone, dlUnlocked];
    //set default
    B.Down := false;
    B.BackBevelColor := 0;
    B.DownColor := TO_UNLOCK_COLOR;
    B.LineWidth := 1;

    //B.Visible := aState <> dlNotVisible;
    B.Tag2 := 0;
    TKMButtonFlatBlock(B).DevBlock := aState;
    If aState = dlUnlocked then
      unlockedList.Add(@aToButton)
    else
    If (aToButton.Parent = nil) and (aState = dlNone) then
      SetButtonColor(aToButton.Button_Tree, DEFAULT_COLOR, 2, DEFAULT_COLOR and $44FFFFFF, 1);



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
          SetButtonColor(aToButton.Next[I].Button_Tree, DEFAULT_COLOR, 2, DEFAULT_COLOR and $44FFFFFF, 1)
        else
        If (aState = dlNone) and (nextState = dlNone) then
          aToButton.Next[I].Button_Tree.DownColor := TO_UNLOCK_COLOR //can be unlocked
        else
        If (nextState in [dlBlocked, dlNotVisible]) then
          aToButton.Next[I].Button_Tree.DownColor := BLOCKED_COLOR; //path is blocked


    end;
  end;

  procedure ReloadType(aType : TKMDevelopmentTreeType);
  var I : Integer;
  begin
    unlockedList.Clear;
    CheckNext(aType, Tree[aType].Button_Tree, 0, locks.DevelopmentLock[aType, 0]);
    for I := 0 to unlockedList.Count - 1 do
      UnlockPrevious(aType, unlockedList[I]);

  end;

begin
  If gMySpectator = nil then
    Exit;
  If not gMySpectator.Hand.Enabled then
    Exit;

  locks := gMySpectator.Hand.Locks;
  If aCurrentPageOnly then
    ReloadType(fLastPage)
  else
  for dtt := Low(Button_SwitchTree) to High(Button_SwitchTree) do
    ReloadType(dtt);
end;

procedure TKMMapEdPlayerBlockDevs.Show;
begin
  Inherited;
  ReloadTrees;
end;


end.
