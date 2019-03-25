package com.example.iotDna;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;

import static com.sun.identity.idm.AMIdentityRepository.debug;

public class iotDnaServer {
    static String accessTkn = null;
    static String iot_dna_url = null;

    public iotDnaServer(String url) {
        try {
            this.iot_dna_url = url;
            accessTkn = getAccessToken(); //
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static boolean verify(String usr, String guuid, String serial) {
        boolean attr = false;
        String payload = "";
        if (accessTkn.equals(null)) return false; // config is wrong somewhere since u don't have an access tkn
        try {
            HttpClient httpclient = HttpClients.createDefault();
            HttpPost http = new HttpPost(iot_dna_url + "/tenant/ImageWare/iot/verify/" + guuid); //todo port num should b configurable as well
            http.setHeader("Authorization",  "Bearer " + accessTkn); // grabbed on init'ion of this class
            http.setHeader("Content-Type", "application/json");
            http.setHeader("Accept", "application/json");

            StringEntity params =
                    new StringEntity("{ \"categories\": [ \"apk\" ], \"signature\":{\"signature\":\"" + usr+serial + "\"} }");
            http.setEntity(params);

            HttpResponse response = httpclient.execute(http);
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                payload = EntityUtils.toString(entity);
                log("      resp is " + payload);
            }
            if (payload.contains("Error")) {
                attr = false; //this is the default val
            } else if (payload.contains("true")) {
                attr = true;
            }
        } catch (Exception e) {
            log(e.toString());
        }
        return attr;
    }

    public static String getAccessToken() {
        String cook = "", payload;
        try {
            HttpClient httpclient = HttpClients.createDefault();
            HttpPost http = new HttpPost("https://gmi-ha.iwsinc.com/usermanager/oauth/token"); //rj? props file
            //  NOTE url for tkn is different than the one for verication!
            http.setHeader("Authorization", "Basic Zm9yZ2Vyb2NrOmRzMjQzIUAhSFlVaUg="); //todo pass these in as well via config
            http.setHeader("Content-Type", "application/x-www-form-urlencoded");

            ArrayList<NameValuePair> params;
            params = new ArrayList<NameValuePair>();
            params.add(new BasicNameValuePair("scope", "IGNORED"));
            params.add(new BasicNameValuePair("grant_type", "client_credentials"));
            http.setEntity(new UrlEncodedFormEntity(params, "UTF-8"));

            HttpResponse response = httpclient.execute(http);
            HttpEntity entity = response.getEntity();

            if (entity != null) {
                payload = EntityUtils.toString(entity);
                log("      resp is " + payload);
                if (payload.contains("Error")) {
                    return ""; //this is the default val
                } else if (payload.contains("access_token")) {
                    return stripNoise(payload, "access_token");
                }
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
        return cook;
    }

    private static String stripNoise(String parent, String child) {
        String noise = "";
        try {
            JSONObject jobj = new JSONObject(parent);
            Object idtkn = jobj.getString(child);
            noise = idtkn.toString();

            if (noise.startsWith("[")) { // get only 'value' from "["value"]"
                noise = noise.substring(1, noise.length() - 1);
            }
            if (noise.startsWith("\"")) {
                noise = noise.substring(1, noise.length() - 1);
            }
        } catch (JSONException e) {
            e.printStackTrace();
        } finally {
            return noise;
        }
    }

    public static void log(String str) {
        debug.error("\r\n           msg:" + str + "\r\n"); //todo change this to MESSAGE
    }

}