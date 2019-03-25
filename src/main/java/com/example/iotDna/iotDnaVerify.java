/*
 * The contents of this file are subject to the terms of the Common Development and
 * Distribution License (the License). You may not use this file except in compliance with the
 * License.
 *
 * You can obtain a copy of the License at legal/CDDLv1.0.txt. See the License for the
 * specific language governing permission and limitations under the License.
 *
 * When distributing Covered Software, include this CDDL Header Notice in each file and include
 * the License file at legal/CDDLv1.0.txt. If applicable, add the following below the CDDL
 * Header, with the fields enclosed by brackets [] replaced by your own identifying
 * information: "Portions copyright [year] [name of copyright owner]".
 *
 * Copyright 2018 ForgeRock AS.
 */


package com.example.iotDna;

import com.google.inject.assistedinject.Assisted;
import com.iplanet.sso.SSOException;
import com.sun.identity.idm.AMIdentity;
import com.sun.identity.idm.IdRepoException;
import com.sun.identity.shared.debug.Debug;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.*;
import org.forgerock.openam.core.CoreWrapper;

import javax.inject.Inject;

import static org.forgerock.openam.auth.node.api.SharedStateConstants.REALM;
import static org.forgerock.openam.auth.node.api.SharedStateConstants.USERNAME;
import static javax.security.auth.callback.ConfirmationCallback.OK_CANCEL_OPTION;
import static javax.security.auth.callback.TextOutputCallback.ERROR;
import static javax.security.auth.callback.TextOutputCallback.INFORMATION;
import static javax.security.auth.callback.TextOutputCallback.WARNING;
import static org.forgerock.openam.auth.node.api.SharedStateConstants.USERNAME;


import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.ConfirmationCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.TextOutputCallback;

import org.forgerock.guava.common.collect.ImmutableList;
import org.forgerock.guava.common.collect.ImmutableSet;
import org.forgerock.json.JsonValue;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.Action;
import org.forgerock.openam.auth.node.api.Action.ActionBuilder;
import org.forgerock.openam.auth.node.api.Node;
import org.forgerock.openam.auth.node.api.NodeProcessException;
import org.forgerock.openam.auth.node.api.OutcomeProvider;
import org.forgerock.openam.auth.node.api.SharedStateConstants;
import org.forgerock.openam.auth.node.api.TreeContext;
import org.forgerock.openam.core.CoreWrapper;
import org.forgerock.openam.ldap.LDAPAuthUtils;
import org.forgerock.openam.ldap.LDAPUtilException;
import org.forgerock.openam.ldap.LDAPUtils;
import org.forgerock.openam.ldap.ModuleState;
import org.forgerock.openam.sm.annotations.adapters.Password;
import org.forgerock.opendj.ldap.Dn;
import org.forgerock.opendj.ldap.ResultCode;
import org.forgerock.util.i18n.PreferredLocales;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.assistedinject.Assisted;
import com.sun.identity.shared.debug.Debug;
import com.sun.identity.shared.locale.Locale;
import com.sun.identity.sm.RequiredValueValidator;

@Node.Metadata(outcomeProvider = iotDnaVerify.MyOutcomeProvider.class, configClass = iotDnaVerify.Config.class)

public class iotDnaVerify implements Node {

    private final Config config;
    private final CoreWrapper coreWrapper;
    private final static String DEBUG_FILE = "iotDnaNode";
    protected Debug debug = Debug.getInstance(DEBUG_FILE);

    @Override
    public Action process(TreeContext context) throws NodeProcessException {
        ActionBuilder action;
        JsonValue newState = context.sharedState.copy();
        String usr = newState.get("username").asString();
        String guuid = newState.get("UserAttribute").asString(); //this is the GUID that iotDna returned 2 us during dev enrollment
        String q_msg = newState.get("q_val").asString(); // this was retrieved by the QueueReader node, but populated by the usr interacting w/ their wearable app

        try {
            iotDnaServer server = new iotDnaServer(config.iotDnaAddress());
            boolean verified = server.verify(usr, guuid, q_msg); //rj? get rid of last param
            log("     iotDna match got " + verified); //return goTo(false).build(); //rj? why <> work?

            if (verified) {
                return goTo(MyOutcome.TRUE).build();
            } else {
                return goTo(MyOutcome.FALSE).build();
            }
        } catch (Exception e) {
            throw new NodeProcessException(e);//datastore decision 95-98
        }
    }


    public static class MyOutcomeProvider implements OutcomeProvider {
        @Override
        public List<Outcome> getOutcomes(PreferredLocales locales, JsonValue nodeAttributes) {
            //ResourceBundle bundle = locales.getBundleInPreferredLocale(iotDnaVerify.BUNDLE, iotDnaVerify.class.getClassLoader()); //rj? can't we use 'this'?
            return ImmutableList.of(
                    new Outcome(MyOutcome.TRUE.name(), "true"),
                    new Outcome(MyOutcome.FALSE.name(), "false")
             );
        }
    }

    private ActionBuilder goTo(MyOutcome outcome) {
        return Action.goTo(outcome.name());
    }

    public enum MyOutcome {
        TRUE,
        FALSE
    }


    public interface Config {
        @Attribute(order = 100)
        default String iotDnaAddress() {
            return "IotDna Server";
        } //https://iot-poc.iwsinc.com/
        @Attribute(order = 200)
        default String iotDnaBearer() {
            return "IotDna Bearer";
        } //Zm9yZ2Vyb2NrOmRzMjQzIUAhSFlVaUg=
        @Attribute(order = 300)
        default String expirationValue() {
            return "Minutes till expiry";
        } //todo delete since it won't b read
    }

    @Inject
    public iotDnaVerify(@Assisted Config config, CoreWrapper coreWrapper) throws NodeProcessException {
        this.config = config;
        this.coreWrapper = coreWrapper;
    }

    public iotDnaVerify() {
        this.config = new Config() {
            @Override
            public String toString() {
                return super.toString();
            }
        };
        this.coreWrapper = new CoreWrapper();
    }

    public void log(String str) {
        debug.error("\r\n           msg:" + str + "\r\n");
        //System.out.println("\n" + str);
    }

}