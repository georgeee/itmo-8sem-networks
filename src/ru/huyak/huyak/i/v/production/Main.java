package ru.huyak.huyak.i.v.production;

public class Main {

    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("no argument");
        }
        new NetworkListener().start();
        new ConsoleListener(args[0]).start();
    }
}
