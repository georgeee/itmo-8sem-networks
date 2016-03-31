package ru.huyak.huyak.i.v.production;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.Inet6Address;
import java.nio.ByteBuffer;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Scanner;

public class ConsoleListener extends Thread {
    private String ipV6Address;

    public ConsoleListener(String ipV6Address) {
        this.ipV6Address = ipV6Address;
    }

    @Override
    public void run() {
        try (DatagramSocket socket = new DatagramSocket()) {
            socket.setSoTimeout(6969);
            String cmd = null;
            Scanner sc = new Scanner(System.in);
            while (true) {
                cmd = sc.nextLine();
                if (cmd.equals("exit")) {
                    System.exit(0);
                } else if (cmd.equals("time")) {
                    int port = 6969;
                    byte[] outData = "time".getBytes();
                    DatagramPacket outPacket = new DatagramPacket(outData, outData.length, Inet6Address.getByName(ipV6Address), port);
                    socket.send(outPacket);
                    try {
                        byte[] inData = new byte[8];
                        DatagramPacket inPacket = new DatagramPacket(inData, inData.length);
                        socket.receive(inPacket);
                        ByteBuffer buffer = ByteBuffer.allocate(Long.BYTES);
                        buffer.put(inData, 0, inData.length);
                        long time = buffer.getLong(0);
                        System.out.println("Vremechko: " + new SimpleDateFormat("HH:mm:ss.SSS").format(new Date(time)));
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                } else {
                    System.err.println("Wrong command");
                }
            }
        }
        catch (IOException e) {

        }
    }
}
