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
import com.sun.identity.shared.debug.Debug;
import org.forgerock.json.JsonValue;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.IOException;

@Node.Metadata(outcomeProvider = AbstractDecisionNode.OutcomeProvider.class, configClass = iotDnaVerify.Config.class)

public class iotDnaVerify extends AbstractDecisionNode {

    private final Config config;
    private final Logger logger = LoggerFactory.getLogger(iotDnaVerify.class);
    private final static String DEBUG_FILE = "iotDnaNode";
    private Debug debug = Debug.getInstance(DEBUG_FILE);


    public interface Config {

        @Attribute(order = 100)
        default String iotDnaVerifyServer() {
            return "https://iot-poc.iwsinc.com";
        }

        @Attribute(order = 200)
        default String iotDnaAccessServer() {
            return "https://gmi-ha.iwsinc.com/usermanager/oauth/token";
        }

        @Attribute(order = 300)
        default String iotDnaBearerTkn() {
            return "Zm9yZ2Vyb2NrOmRzMjQzIUAhSFlVaUg=";
        }


    }

    @Inject
    public iotDnaVerify(@Assisted Config config) {
        this.config = config;
    }

    @Override
    public Action process(TreeContext context) throws NodeProcessException {
        JsonValue newState = context.sharedState.copy();
        String usr = newState.get("username").asString();
        String guuid = newState.get("UserAttribute").asString(); //this is the GUID that iotDna returned to us during dev enrollment
        String q_msg = newState.get("q_val").asString(); // this was retrieved by the QueueReader node, but populated by the usr interacting w/ their wearable app

        iotDnaServer server = iotDnaServer.getInstance(config.iotDnaVerifyServer(), config.iotDnaAccessServer(), config.iotDnaBearerTkn());

        boolean verified;
        try {
            verified = server.verify(usr, guuid, q_msg);
        } catch (IOException e) {
            throw new NodeProcessException(e);
        }
        log("     iotDna match got " + verified);

        try {
            if (verified) {
                return goTo(true).build();
            } else {
                return goTo(false).build();
            }
        } catch (Exception e) {
            throw new NodeProcessException(e);
        }
    }


    private void log(String str) {
        logger.debug("msg:" + str + "\r\n");
    }

}