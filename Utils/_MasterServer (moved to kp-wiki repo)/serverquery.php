<?php
require_once("serverlib.php");
//require_once("flag.php");
require_once("Ip2Country/Ip2Country.php");
require_once("consts.php");
require_once("db.php");

function ServerQuery($format, $rev)
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
    global $MAIN_VERSION, $TABLE_REFRESH, $BASE_URL;

    if($rev == "") $rev = $MAIN_VERSION;
    $con = db_connect();
    $format = $con->real_escape_string($format);
    $rev = $con->real_escape_string($rev);
    if((!isset($format)) && (!CheckVersion($rev))) {
        $con->close();
        die("Invalid revision");
    }
    if($rev == "") $rev = $MAIN_VERSION;

    $ipc = new Ip2Country('/var/www/kam_hodgman_id_au/public_html/Ip2Country/data/ip2country.dat');
    $ipc->preload();

    $Result = "";
    $server_count = 0;
    /////////////////////////////////////////
    //              HEADER                 //
    /////////////////////////////////////////
    switch($format) {
        case "ajaxupdate":
            $Result = array();
        break;
        case "refresh":
            $Result = '<script type="text/javascript">'."\n".
            'function srvlsttim(dat){var x="<tr><td><strong>Name</strong></td><td><strong>Address</strong></td><td style=\"text-align: center\"><strong>Players</strong></td></tr>";for(var n=0;n<dat.cnt;n++){x+="<tr><td><img src=\"'.$BASE_URL.'flags/"+dat.srvs[n].c+".gif\" alt=\""+dat.srvs[n].c+"\" />&nbsp;"+dat.srvs[n].n+"</td><td>"+(dat.srvs[n].a=="0"?" <img src=\"'.$BASE_URL.'error.png\" alt=\"Server unreachable\" style=\"vertical-align:middle\" />":"")+dat.srvs[n].i+"</td><td style=\"text-align: center\">"+dat.srvs[n].p+"</td></tr>";jQuery("#ajxtbl").empty().append(x);}}'."\n".
            'function updsr(){setTimeout(function (){jQuery.ajax({dataType: "jsonp",jsonp: "jsonp_callback",url: "'.$BASE_URL.'serverquery.php?format=ajaxupdate&rev='.$rev.'",success: function (data){srvlsttim(data);updsr();}});}, '.(1000*$TABLE_REFRESH).");}\n".
            "jQuery(document).ready(function($){updsr();});</script>\n".
            '<table border="1" width="100%" id="ajxtbl"><tr><td><strong>Name</strong></td><td><strong>Address</strong></td><td style="text-align: center"><strong>Players</strong></td></tr>';
        break;
        case "table":
            $Result .= '<table border="1" width="100%" id="ajxtbl"><tr><td><strong>Name</strong></td><td><strong>Address</strong></td><td style="text-align: center"><strong>Players</strong></td></tr>';
        break;
        case "kamclub":
            $Result .= '<table border="1" width="100%" id="ajxtbl" style="font-size:11px; font-family:Arial,Tahoma"><tr><td><strong>Название сервера</strong></td><td><strong>Адрес</strong></td><td style="text-align: center"><strong>Кол-во игроков</strong></td></tr>';
        break;
        case "kamclubeng":
            $Result .= '<table border="1" width="100%" id="ajxtbl" style="font-size:11px; font-family:Arial,Tahoma"><tr><td><strong>Server name</strong></td><td><strong>Address</strong></td><td style="text-align: center"><strong>Players</strong></td></tr>';
        default:
        break;
    }
    /////////////////////////////////////////
    //                BODY                 //
    /////////////////////////////////////////
    Remove_Old_Servers($con);
    $data = $con->query("SELECT IP, Port, Name, Players, Dedicated, OS, Pingable FROM Servers WHERE Rev='$rev' ORDER BY Players DESC, Name ASC");
    while($row = $data->fetch_array())
    {
        $Name = $row['Name'];
        $IP = $row['IP'];
        $Port = $row['Port'];
        $PlayerCount = $row['Players'];
        $IsDedicated = $row['Dedicated'];
        $OS = $row['OS'];
        $Alive = $row['Pingable'];
        $ipCountry = $ipc->lookup($IP);
        $server_count++;

        switch($format)
        {
            case "refresh":
            case "kamclub":
            case "kamclubeng":
            case "table":
                //Clean color codes matching [$xxxxxx] or []
                $Name = preg_replace('/\\[\\$[0-9a-fA-F]{6}\\]|\\[\\]|\[\]/',"",$Name); //WTF regex
                $Name = htmlentities($Name);
                //$Country = IPToCountry($IP);
                $Warning = '';
                if(!$Alive) $Warning = ' <IMG src="'.$BASE_URL.'error.png" alt="Server unreachable" style="vertical-align:middle">';
                //$Result .= "<TR class=\"no_translate\"><TD><IMG src=\"".$BASE_URL."flags/".strtolower($Country).".gif\" alt=\"".GetCountryName($Country)."\">&nbsp;$Name</TD><TD>$Warning$IP</TD><TD style=\"text-align: center\">$PlayerCount</TD></TR>\n";
                $Result .= '<tr class="no_translate"><td><img src="' . $BASE_URL . 'flags/' . strtolower($ipCountry['short']) . '.gif" alt="' . $ipCountry['full'] .
                           '">&nbsp;' . $Name . '</td><td>' . $Warning . $IP . '</td><td style="text-align: center">' . $PlayerCount . "</td></tr>\n";
                break;
            case "ajaxupdate":
                //Clean color codes matching [$xxxxxx] or []
                $Name = preg_replace('/\\[\\$[0-9a-fA-F]{6}\\]|\\[\\]|\[\]/',"",$Name); //WTF regex
                $Name = htmlentities($Name);
                $srvsgl = array();
                $srvsgl['c'] = strtolower($ipCountry['short']);//strtolower(IPToCountry($IP));
                $srvsgl['n'] = $Name;
                if(!$Alive) { $srvsgl['a'] = "0"; } //$Alive could be '' to mean false
                else        { $srvsgl['a'] = "1"; }
                $srvsgl['i'] = $IP;
                //$Result['o'] = $Port; // not used yet
                $srvsgl['p'] = $PlayerCount;
                $Result[] = $srvsgl;
                break;
            default:
                if(substr($rev,1) >= 4878) //New server releases expect more parameters
                    $Result .= "$Name,$IP,$Port,$IsDedicated,$OS\n";
                else
                    $Result .= "$Name,$IP,$Port\n";
        }
    }
    /////////////////////////////////////////
    //              FOOTER                 //
    /////////////////////////////////////////
    switch($format) {
        case "ajaxupdate":
            $Result = json_encode(Array("cnt"=>$server_count,"srvs"=>$Result));
            $Result = $_GET['jsonp_callback']."(".$Result.")";
            break;
        case "kamclub":
        case "kamclubeng":
        case "refresh":
        case "table":
            $Result .= '</table>';
        default:
    }
    echo $Result;
    $con->close();
}

//If we were called directly
if ( basename(__FILE__) == basename($_SERVER["SCRIPT_FILENAME"]) ) {
    $format = "";
    $rev = "";
    if(isset($_REQUEST["format"])) $format = $_REQUEST["format"];
    if(isset($_REQUEST["rev"])) $rev = $_REQUEST["rev"];
    ServerQuery($format, $rev);
}
