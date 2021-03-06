package ru.ifmo.ctd.year2012.sem8.networks.timesync;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.Inet6Address;
import java.nio.ByteBuffer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Scanner;

public class ConsoleListener implements Runnable {
    private static final Logger log = LoggerFactory.getLogger(ConsoleListener.class);
    private final int port;
    private final String ipV6Address;

    public ConsoleListener(int port, String ipV6Address) {
        this.port = port;
        this.ipV6Address = ipV6Address;
    }

    @Override
    public void run() {
        DateFormat dateFormat = new SimpleDateFormat("HH:mm:ss.SSS");
        try (DatagramSocket socket = new DatagramSocket()) {
            socket.setSoTimeout(6969);
            String cmd;
            Scanner sc = new Scanner(System.in);
            while (true) {
                cmd = sc.nextLine();
                if (cmd.equals("exit")) {
                    System.exit(0);
                } else if (cmd.equals("time")) {
                    byte[] outData = "time".getBytes();
                    DatagramPacket outPacket = new DatagramPacket(outData, outData.length, Inet6Address.getByName(ipV6Address), port);
                    socket.send(outPacket);
                    try {
                        byte[] inData = new byte[256];
                        DatagramPacket inPacket = new DatagramPacket(inData, inData.length);
                        socket.receive(inPacket);
                        ByteBuffer buffer = ByteBuffer.wrap(inPacket.getData(), inPacket.getOffset(), inPacket.getLength());
                        long time = buffer.getLong(0);
                        String serverName = new String(inPacket.getData(), inPacket.getOffset() + Long.BYTES, inPacket.getLength() - Long.BYTES);
                        System.out.printf("Vremechko (server %s): %s%n", serverName, dateFormat.format(new Date(time)));
                    } catch (IOException e) {
                        log.error("Time getting error", e);
                    }
                } else {
                    System.err.println("Wrong command");
                }
            }
        } catch (IOException e) {
            log.error("Console listener exception", e);
        }
    }
}
