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

import org.forgerock.guava.common.collect.ImmutableList;
import org.forgerock.json.JsonValue;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.Action;
import org.forgerock.openam.auth.node.api.Action.ActionBuilder;
import org.forgerock.openam.auth.node.api.Node;
import org.forgerock.openam.auth.node.api.NodeProcessException;
import org.forgerock.openam.auth.node.api.OutcomeProvider;
import org.forgerock.openam.auth.node.api.TreeContext;
import org.forgerock.util.i18n.PreferredLocales;

import com.google.inject.assistedinject.Assisted;
import com.sun.identity.shared.debug.Debug;

import java.io.IOException;
import java.util.List;
import javax.inject.Inject;

@Node.Metadata(outcomeProvider = iotDnaVerify.MyOutcomeProvider.class, configClass = iotDnaVerify.Config.class)

public class iotDnaVerify implements Node {

    private final Config config;
    //TODO Prefer to use logger vs debug. Like this     private final Logger logger = LoggerFactory.getLogger
    // (iotDnaVerify.class);
    private final static String DEBUG_FILE = "iotDnaNode";
    private Debug debug = Debug.getInstance(DEBUG_FILE);


    public interface Config {

        //TODO default value should be example value
        @Attribute(order = 100)
        default String iotDnaAddress() {
            return "IotDna Server";
        } //https://iot-poc.iwsinc.com/

        //TODO Never used, default value should be example value
        @Attribute(order = 200)
        default String iotDnaBearer() {
            return "IotDna Bearer";
        }

        //TODO Never used, should be a int value
        //Zm9yZ2Vyb2NrOmRzMjQzIUAhSFlVaUg=
        @Attribute(order = 300)
        default String expirationValue() {
            return "Minutes till expiry";
        } //todo delete since it won't b read
    }


    @Inject
    public iotDnaVerify(@Assisted Config config) {
        this.config = config;
    }

    @Override
    public Action process(TreeContext context) throws NodeProcessException {
        JsonValue newState = context.sharedState.copy();
        String usr = newState.get("username").asString();
        String guuid = newState.get("UserAttribute").asString(); //this is the GUID that iotDna returned 2 us during dev enrollment
        String q_msg = newState.get("q_val").asString(); // this was retrieved by the QueueReader node, but populated by the usr interacting w/ their wearable app

        iotDnaServer server = iotDnaServer.getInstance(config.iotDnaAddress());

        boolean verified; //rj? get rid of last param
        try {
            verified = server.verify(usr, guuid, q_msg);
        } catch (IOException e) {
            throw new NodeProcessException(e);
        }
        log("     iotDna match got " + verified); //return goTo(false).build(); //rj? why <> work?

        try {

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
                    //TODO pass in displayNames as localized text via properties file
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

    //TODO Constructor never used
    public iotDnaVerify() {
        this.config = new Config() {
            @Override
            public String toString() {
                return super.toString();
            }
        };
    }

    //TODO Should not always log at error level, log depending on type of message
    private void log(String str) {
        debug.error("\r\n           msg:" + str + "\r\n");
        //System.out.println("\n" + str);
    }

}