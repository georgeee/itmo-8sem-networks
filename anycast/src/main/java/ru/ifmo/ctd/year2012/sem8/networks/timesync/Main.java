package ru.ifmo.ctd.year2012.sem8.networks.timesync;

public class Main {

    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("no argument");
        }
        new NetworkListener().start();
        new ConsoleListener(args[0]).start();
    }
}
