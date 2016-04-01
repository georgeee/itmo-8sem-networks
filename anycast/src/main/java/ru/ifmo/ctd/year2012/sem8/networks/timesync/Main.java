package ru.ifmo.ctd.year2012.sem8.networks.timesync;

import org.apache.commons.cli.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Main {
    private static final int DEFAULT_PORT = 16969;
    private static final Logger log = LoggerFactory.getLogger(Main.class);
    private static final String ADDRESS_KEY = "addr";
    private static final String PORT_KEY = "port";
    private static final String HELP_KEY = "help";
    private static final String LAUNCH_CLIENT_KEY = "client";
    private static final String LAUNCH_SERVER_KEY = "server";
    private final Options options = new Options();
    private final CommandLine line;

    public Main(String[] args) throws ParseException {
        options.addOption("a", ADDRESS_KEY, true, "Anycast address");
        options.addOption("p", PORT_KEY, true, "Timesync port");
        options.addOption("h", HELP_KEY, false, "Help message");
        options.addOption("c", LAUNCH_CLIENT_KEY, false, "Launch client");
        options.addOption("s", LAUNCH_SERVER_KEY, false, "Launch server");
        line = parseArgs(args);
    }

    public static void main(String[] args) {
        try {
            new Main(args).run();
            System.exit(0);
        } catch (ParseException e) {
            log.error("Unexpected exception", e);
        }
    }

    private int getPort() {
        String pStr = line.getOptionValue(PORT_KEY);
        return pStr == null ? DEFAULT_PORT : Integer.parseInt(pStr);
    }

    private String getAddress() {
        String addr = line.getOptionValue(ADDRESS_KEY);
        if (addr == null) {
            throw new IllegalStateException("No address specified");
        }
        return addr;
    }

    public ConsoleListener startConsoleListener() {
        ConsoleListener listener = new ConsoleListener(getPort(), getAddress());
        listener.run();
        return listener;
    }

    public NetworkListener startNetworkListener() {
        NetworkListener listener = new NetworkListener(getPort());
        listener.start();
        return listener;
    }

    private CommandLine parseArgs(String[] args) throws ParseException {
        CommandLineParser parser = new DefaultParser();
        return parser.parse(options, args);
    }

    public void run() {
        if (!(options.hasOption(LAUNCH_CLIENT_KEY) || options.hasOption(LAUNCH_SERVER_KEY)) || line.hasOption(HELP_KEY)) {
            HelpFormatter formatter = new HelpFormatter();
            formatter.printHelp("timesync", options);
        } else {
            try {
                Thread serverThread = null;
                if (line.hasOption(LAUNCH_SERVER_KEY)) {
                    serverThread = startNetworkListener();
                }
                if (line.hasOption(LAUNCH_CLIENT_KEY)) {
                    startConsoleListener();
                }
                if (serverThread != null) serverThread.join();
            } catch (Exception e) {
                log.error("Exception occurred during execution", e);
            }
        }
    }

}
