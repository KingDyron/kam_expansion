unit KM_GameTypes;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults, KM_MapTypes, KM_CampaignTypes, KM_Points;

const
  SAVE_HEADER_MARKER = 'Header';
  SAVE_BODY_MARKER = 'BodyCompressed';

type
  TKMGameMode = (
    gmSingle,
    gmCampaign,
    gmMulti,        //Different GIP, networking,
    gmMultiSpectate,
    gmMapEd,        //Army handling, lite updates,
    gmReplaySingle, //No input, different results screen to gmReplayMulti
    gmReplayMulti   //No input, different results screen to gmReplaySingle
    );

  TKMAIType = (aitNone, aitClassic, aitAdvanced);
  TKMAITypeSet = set of TKMAIType;

  //Message kind determines icon and available actions for Message
  TKMMessageKind = (
    mkText, //Mission text message
    mkHouse,
    mkGroup,
    mkQuill, //Utility message (warnings in script loading)
    mkStyle1,
    mkStyle2,
    mkStyle3,
    mkStyle4,
    mkStyle5,
    mkStyle6,
    mkStyle7,
    mkStyle8,
    mkStyle9
  );
  const
    MSG_ICON: array [TKMMessageKind] of Word = (491, 492, 493, 495,
                                                760, 761, 762, 763, 764, 765, 766, 767, 768);

    MSG_KIND_HINT: array [TKMMessageKind] of Word = (1795, 1796, 1797, 1798,
                                                1799, 1800, 1801, 1802, 1803, 1804, 1805, 1806, 1807);

  Type
  TKMGameModeChangeEvent = procedure (aGameMode: TKMGameMode) of object;

  TKMNewSingleMapEvent = procedure (const aMissionFile, aGameName: UnicodeString; aDesiredLoc: ShortInt = -1;
                                    aDesiredColor: Cardinal = $00000000; aDifficulty: TKMMissionDifficulty = mdNone;
                                    aAIType: TKMAIType = aitNone; aBDifficulty: TKMMissionBuiltInDifficulty = mdbNormal) of object;

  TKMNewCampaignMapEvent = procedure (aCampaign: TKMCampaignId; aMap: Byte; aDifficulty: TKMMissionDifficulty = mdNone;
                                 aBDifficulty: TKMMissionBuiltInDifficulty = mdbNormal) of object;

  TKMNewMapEditorEvent = procedure (const aFileName: UnicodeString; aSizeX: Integer = 0; aSizeY: Integer = 0; aMapFullCRC: Cardinal = 0;
                                    aMapSimpleCRC: Cardinal = 0; aMultiplayerLoadMode: Boolean = False) of object;

  TKMGameShowMessageEvent = procedure (aKind: TKMMessageKind; aTextID: Integer; const aLoc: TKMPoint; aEntityUID: Integer; aHandIndex: TKMHandID) of object;

const
  // Location color 'magic value', which means we should not overwrite location color which was set in the map editor
  // Used on game start
  NO_OVERWRITE_COLOR = $00000000; // todo: refactor, get rid of 'magic color value'

  // Value we use to set hand / player without team choosen
  NO_TEAM = 0;


implementation

end.
