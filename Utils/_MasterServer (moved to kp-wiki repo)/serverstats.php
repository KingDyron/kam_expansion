<?php
require_once("serverlib.php");
require_once("consts.php");
require_once("db.php");

function ServerStats($format)
{
    if ($format == "ajaxupdate") {
        /*
        * the requester expects to receive a json object
        * some browsers withdraw the standard text/html that php returns by default
        */
        header('Cache-Control: no-cache, must-revalidate');
        header('Expires: Mon, 26 Jul 1997 05:00:00 GMT'); //expired in the past to prevent caching
        header('Content-type: application/json');
    }
    global $MAIN_VERSION, $STATS_REFRESH, $BASE_URL;
    $rev = $MAIN_VERSION;
    $con = db_connect();
    $data = $con->query("SELECT SUM(Players) AS PlayerSum, COUNT(IP) AS ServerCount FROM Servers WHERE Rev='$rev'");
    $data = $data->fetch_array();
    $TotalPlayerCount = intval($data["PlayerSum"]);
    $ServerCount = intval($data["ServerCount"]);
    switch ($format)
    {
        case "kamclub":
            echo '<html><head><META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8"><meta http-equiv="refresh" content="'.$STATS_REFRESH.'"></head><body><div style="font-size:11px; font-family:Arial,Tahoma"><b>Кол-во серверов:</b> '.$ServerCount.'<BR><b>Кол-во игроков:</b> '.$TotalPlayerCount.'</font></div></body></html>';
        break;
        case "kamclubeng":
            echo '<html><head><META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8"><meta http-equiv="refresh" content="'.$STATS_REFRESH.'"></head><body><div style="font-size:11px; font-family:Arial,Tahoma"><b>Server count:</b> '.$ServerCount.'<BR><b>Player count:</b> '.$TotalPlayerCount.'</font></div></body></html>';
        break;
        case "ajaxupdate":
            $data = json_encode(Array("pct"=>$TotalPlayerCount,"sct"=>$ServerCount));
            echo $_GET['jsonp_callback']."(".$data.")";
        break;
        case "csv":
            echo $ServerCount.','.$TotalPlayerCount;
        break;
        case "refresh":
            //user-side request after 30s with parameter ?format=ajaxupdate which then updates the numbers
            $startscript = '<script type="text/javascript">'."\n".
            'function updnr(){setTimeout(function (){jQuery.ajax({dataType: "jsonp",jsonp: "jsonp_callback",url: "'.$BASE_URL.'serverstats.php?format=ajaxupdate",success: function (data){jQuery("#scount").empty().append(data.sct);jQuery("#pcount").empty().append(data.pct);updnr();}});}, '.(1000*$STATS_REFRESH).');}'."\n".
            'jQuery(document).ready(function($){updnr();});</script>'."\n";
            echo $startscript.'There '.plural($ServerCount,'is','are',true).' <span id="scount">'.$ServerCount.'</span> '.plural($ServerCount,'server').' running and <span id="pcount">'.$TotalPlayerCount.'</span> '.plural($TotalPlayerCount,'player').' online';
        break;
        default:
            echo 'There '.plural($ServerCount,'is','are',true).' '.$ServerCount.' '.plural($ServerCount,'server').' running and '.$TotalPlayerCount.' '.plural($TotalPlayerCount,'player').' online';
        break;
    }
    $con->close();
}

//If we were called directly
if ( basename(__FILE__) == basename($_SERVER["SCRIPT_FILENAME"]) ) {
    $format = "";
    if(isset($_REQUEST["format"])) $format = $_REQUEST["format"];
    ServerStats($format);
}
