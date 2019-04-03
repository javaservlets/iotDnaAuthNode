package com.example.iotDna;

import static com.sun.identity.idm.AMIdentityRepository.debug;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.forgerock.openam.auth.node.api.NodeProcessException;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.util.ArrayList;

class iotDnaServer {
    private static iotDnaServer server = null;
    private static String accessTkn = null;
    private static String iot_dna_url = null;

    private iotDnaServer(String url) throws NodeProcessException {
        try {
            iot_dna_url = url;
            accessTkn = getAccessToken(); //
        } catch (Exception e) {
            throw new NodeProcessException(e);
        }
    }

    static iotDnaServer getInstance(String url) throws NodeProcessException {
        if (server == null) {
            server = new iotDnaServer(url);
        }
        return server;
    }

    boolean verify(String usr, String guuid, String serial) throws IOException {
        boolean attr = false;
        String payload = "";
        HttpClient httpclient = HttpClients.createDefault();
        HttpPost http = new HttpPost(iot_dna_url + "/tenant/ImageWare/iot/verify/" + guuid); //todo port num should b configurable as well
        //TODO Pull Authorization as constant as it is used multiple times
        http.setHeader("Authorization",  "Bearer " + accessTkn); // grabbed on init'ion of this class
        //TODO Pull Content-Type as constant as it is used multiple times
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
        //TODO Check the HTTP Status code in addition to the payload
        if (payload.contains("true")) {
            attr = true;
        }
        return attr;
    }

    private String getAccessToken() throws IOException, JSONException {
        HttpClient httpclient = HttpClients.createDefault();
        //TODO Pull out as config
        HttpPost http = new HttpPost("https://gmi-ha.iwsinc.com/usermanager/oauth/token"); //rj? props file
        //  NOTE url for tkn is different than the one for verification!
        //TODO Pull Authorization as constant as it is used multiple times
        http.setHeader("Authorization", "Basic Zm9yZ2Vyb2NrOmRzMjQzIUAhSFlVaUg="); //todo pass these in as well via config
        //TODO Pull Content-Type as constant as it is used multiple times
        http.setHeader("Content-Type", "application/x-www-form-urlencoded");

        ArrayList<NameValuePair> params = new ArrayList<NameValuePair>() {{
            add(new BasicNameValuePair("scope", "IGNORED"));
            add(new BasicNameValuePair("grant_type", "client_credentials"));
        }};

        http.setEntity(new UrlEncodedFormEntity(params, "UTF-8"));

        HttpResponse response = httpclient.execute(http);
        HttpEntity entity = response.getEntity();

        if (entity != null) {
            String payload = EntityUtils.toString(entity);
            log("      resp is " + payload);
            if (payload.contains("Error")) {
                return ""; //this is the default val
                //TODO Pull access_token as constant as it is used multiple times
            } else if (payload.contains("access_token")) {
                return stripNoise(payload);
            }
        }
        return "";
    }

    private static String stripNoise(String parent) throws JSONException {
        JSONObject jsonObject = new JSONObject(parent);
        //TODO Pull access_token as constant as it is used multiple times
        Object idToken = jsonObject.getString("access_token");
        String noise = idToken.toString();

        if (noise.startsWith("[")) { // get only 'value' from "["value"]"
            noise = noise.substring(1, noise.length() - 1);
        }
        if (noise.startsWith("\"")) {
            noise = noise.substring(1, noise.length() - 1);
        }
        return noise;
    }

    private static void log(String str) {
        debug.error("\r\n           msg:" + str + "\r\n"); //todo change this to MESSAGE
    }

}