package com.example.iotDna;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;

/*
This class gets passed an access server url, a bearer token, and a verification server url. On instantiation
it retrieves an access token and uses it later during a challenge (verification request). Latter request has
1. the user name
2. serial number of iot device (ie, wearable)
3. guuid that iotDna returned earlier during device registration
*/

class iotDnaServer {
    private static String access_url = null; // this is where you request access ...
    private static String bearer_tkn = null; //... with this bearer ...
    private static String access_tkn = null; // ... to get this token ...
    private static iotDnaServer server = null; // ... that you then can take to their verification server
    private static String iot_dna_url = null;
    private static final String AUTH_HEADER = "Authorization";
    private static final String CONTENT_TYPE = "Content-Type";
    private static final String PAYLOAD_TYPE = "access_token";

    private final Logger logger = LoggerFactory.getLogger(iotDnaServer.class);

    public iotDnaServer(String url, String access, String bearer) throws NodeProcessException {
        try {
            iot_dna_url = url;
            bearer_tkn = bearer;
            access_url = access;
            access_tkn = getAccessToken();
        } catch (Exception e) {
            throw new NodeProcessException(e);
        }
    }

    static iotDnaServer getInstance(String url, String access, String bearer) throws NodeProcessException {
        if (server == null) {
            server = new iotDnaServer(url, access, bearer);
        }
        return server;
    }

    boolean verify(String usr, String guuid, String serial) throws IOException {
        try {
            if (access_tkn.length() < 1 ) access_tkn = getAccessToken();
        } catch (Exception e) {
            log("can't get access token!!");
        }

        log("      verify req  is " + usr + " / " + guuid + " / " + serial + " accTkn: " + access_tkn);
        boolean attr = false;
        String payload = "";
        HttpClient httpclient = HttpClients.createDefault();
        HttpPost http = new HttpPost(iot_dna_url + "/tenant/ImageWare/iot/verify/" + guuid);
        http.setHeader(AUTH_HEADER,  "Bearer " + access_tkn); // grabbed on init'ion of this class e2ede5fc-b46c-4167-be17-6874666a681b
        http.setHeader(CONTENT_TYPE, "application/json");
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
        if (response.getStatusLine().getStatusCode() == HttpStatus.SC_OK && payload.contains("true")) {
            log("      we verified with " + payload);
            attr = true;
        }
        return attr;
    }

    private String getAccessToken() throws IOException, JSONException {
        HttpClient httpclient = HttpClients.createDefault();
        HttpPost http = new HttpPost(access_url);
        http.setHeader(AUTH_HEADER, "Basic " + bearer_tkn);
        http.setHeader(CONTENT_TYPE, "application/x-www-form-urlencoded");

        ArrayList<NameValuePair> params = new ArrayList<NameValuePair>() {{
            add(new BasicNameValuePair("scope", "IGNORED"));
            add(new BasicNameValuePair("grant_type", "client_credentials"));
        }};

        http.setEntity(new UrlEncodedFormEntity(params, "UTF-8"));
        HttpResponse response = httpclient.execute(http);
        HttpEntity entity = response.getEntity();

        if (entity != null) {
            String payload = EntityUtils.toString(entity);
            log("      access tkn resp is " + payload);
            if (payload.contains("Error")) {
                return ""; //this is the default val
            } else if (payload.contains(PAYLOAD_TYPE)) {
                return stripNoise(payload);
            }
        }
        return "";
    }

    private static String stripNoise(String parent) throws JSONException {
        JSONObject jsonObject = new JSONObject(parent);
        Object idToken = jsonObject.getString(PAYLOAD_TYPE);
        String noise = idToken.toString();

        if (noise.startsWith("[")) { // get only 'value' from "["value"]"
            noise = noise.substring(1, noise.length() - 1);
        }
        if (noise.startsWith("\"")) {
            noise = noise.substring(1, noise.length() - 1);
        }
        return noise;
    }

    private  void log(String str) {
        System.out.println(str);
        logger.debug("msg:" + str + "\r\n");
    }

}