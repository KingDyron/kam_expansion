unit KM_CampaignTypes;
{$I KaM_Remake.inc}
interface

type
  //Unique campaign identification, stored as 3 ANSI letters (TSK, TPR, etc)
  //3 bytes are used to avoid string types issues
  TKMCampaignId = array [0..2] of Byte;

  TKMCampaignFlagStyle = packed record
    ToUnlockID, UnlockedID, CompletedID, NodeID : Integer;
    LabelOffsetX, LabelOffsetY : Integer;
  end;

  procedure LoadCampaignFlags;




const
  CAMPAIGN_DATA_FILENAME = 'campaigndata';

  CAMPAIGN_SCRIPTS_FOLDER_NAME = 'Scripts';
  CAMPAIGN_TEXTS_FOLDER_NAME = 'Texts';

  CAMPAIGN_SOUNDS_FOLDER_NAME = 'Sounds';
  CAMPAIGN_FLAGS_JSON_NAME = 'CampaignFlags.json';


var CAMPAIGN_FLAG_STYLES : array of TKMCampaignFlagStyle;


implementation
uses KM_Defaults, SysUtils, IOUtils, KM_JsonHelpers;


procedure SetDefaultFlags;
begin
  SetLength(CAMPAIGN_FLAG_STYLES, 1);
  CAMPAIGN_FLAG_STYLES[0].ToUnlockID := 10;
  CAMPAIGN_FLAG_STYLES[0].UnlockedID := 93;
  CAMPAIGN_FLAG_STYLES[0].CompletedID := 11;
end;

procedure LoadCampaignFlags;
var path : String;
  I : Integer;
  nRoot, nFlag : TKMJsonObject;
  nFlags : TKMJsonArrayNew;
begin
  path := ExeDir + 'data' + PathDelim + 'defines' + PathDelim + CAMPAIGN_FLAGS_JSON_NAME;
  If not FileExists(path) then
  begin
    SetDefaultFlags;
    Exit;
  end;

  nRoot := TKMJsonObject.Create;
  try
    nRoot.LoadFromFile(path);
    nFlags := nRoot.A['Flags'];
    If nFlags.Count = 0 then
    begin
      SetDefaultFlags;
      Exit;
    end;
    SetLength(CAMPAIGN_FLAG_STYLES, nFlags.Count);
    for I := 0 to nFlags.Count - 1 do
    begin
      nFlag := nFlags.O[I];
      CAMPAIGN_FLAG_STYLES[I].ToUnlockID := nFlag.GetI('ToUnlock', 10);
      CAMPAIGN_FLAG_STYLES[I].UnlockedID := nFlag.GetI('Unlocked', 93);
      CAMPAIGN_FLAG_STYLES[I].CompletedID := nFlag.GetI('Completed', 11);
      CAMPAIGN_FLAG_STYLES[I].NodeID := nFlag.GetI('Node', 16);
      CAMPAIGN_FLAG_STYLES[I].LabelOffsetX := nFlag.GetI('LabelOffsetX', 10);
      CAMPAIGN_FLAG_STYLES[I].LabelOffsetY := nFlag.GetI('LabelOffsetY', 3);
    end;

  finally
    FreeAndNil(nRoot);
  end;

end;

end.
